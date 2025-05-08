library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(fuzzyjoin)
library(openxlsx)
library(textstem)
library(stringr)
library(stopwords)
library(conflicted)
conflicts_prefer(dplyr::filter)
#functions------------------------

clean_job_title <- function(title) {
  title <- tolower(title)                      # Lowercase
  title <- gsub("[^a-z0-9 ]", " ", title)      # Remove special characters
  title <- str_squish(title)                   # Remove extra spaces
  title <- str_replace_all(title, "metallurgist", "metallurgical")#screws up matching
  tokens <- unlist(strsplit(title, " "))       # Tokenize
  lemmas <- lemmatize_words(tokens)            # Lemmatize
  stop_words <- stopwords("en")                # Get stop words
  lemmas <- lemmas[!lemmas %in% stop_words]    # Remove stop words
  lemmas <- lapply(lemmas, trimws)             # Remove empty strings
  return(lemmas)
}

normalized_dot <- function(vec1, vec2) {
  tab1 <- table(unlist(vec1))
  tab2 <- table(unlist(vec2))
  words <- union(names(tab1), names(tab2))
  v1 <- as.integer(tab1[words]); v1[is.na(v1)] <- 0
  v2 <- as.integer(tab2[words]); v2[is.na(v2)] <- 0
  dot <- sum(v1 * v2)
  total <- sum(v1) + sum(v2)
  dot / total
}


#' Based on Census proportions-------------------------------
noc_freq <- read_excel(here("data", "NOC NAICS.xlsx"))
colnames(noc_freq)[1] <- "noc"

noc_freq <- noc_freq|>
  select(noc, contains("2122"), contains("2123"))%>%
  filter(str_detect(noc, "^\\d{5}"))|>
  pivot_longer(cols = -noc, names_to = "naics", values_to = "count")|>
  group_by(noc)|>
  summarize(count = sum(count, na.rm = TRUE))|>
  filter(count>0)|>
  ungroup()|>
  mutate(census_frequency=count/sum(count))|>
  arrange(desc(census_frequency))

direct_employment <- read_excel(here("data", "Upcoming Mine Data - Employment.xlsx"), skip = 1)|>
  clean_names()|>
  select(mine=upcoming_mine, operational_jobs=estimated_direct_employment_operations)|>
  na.omit()|>
  arrange(desc(operational_jobs))

historic_proportions <- cross_join(noc_freq, direct_employment)|>
  mutate(noc_jobs=operational_jobs*census_frequency)|>
  select(noc, mine, noc_jobs)|>
  pivot_wider(names_from = mine, values_from = noc_jobs, values_fill = 0)|>
  adorn_totals("both")|>
  relocate(Total, .after=noc)

wb <- createWorkbook()
addWorksheet(wb, "Based NOC-NAICS table")
writeData(wb, "Based NOC-NAICS table", historic_proportions)
setColWidths(wb, sheet = "Based NOC-NAICS table", cols=1:30, widths = c(70, rep(5,29)))
style <- createStyle(numFmt = "0")
addStyle(wb, sheet = "Based NOC-NAICS table", style = style, cols = 2:30, rows=1:nrow(historic_proportions), gridExpand = TRUE)
freezePane(wb, sheet = "Based NOC-NAICS table", firstRow = TRUE, firstCol = TRUE)
saveWorkbook(wb, here("out","employment_by_mine_and_noc.xlsx"), overwrite = TRUE)


#' based on token overlap----------------------

noc_freq <- noc_freq|>
  separate(noc, into = c("noc", "noc_title"), sep = "\\s", extra = "merge")|>
  select(-count)

clean_jobs <- read_csv(here("data","noc_2021_version_1.0_elements.csv"))|>
  clean_names()|>
  filter(element_type_label_english %in% c("Illustrative example(s)", "All examples"))|>
  remove_empty()|>
  select(noc=code_noc_2021_v1_0, noc_title=class_title, statistics_canada_job_title=element_description_english)|>
  mutate(noc=str_pad(noc, 5, pad = "0"))|>
  right_join(noc_freq)|> #only nocs that show up in the mining column of NOC NAICS table
  distinct()|>
  group_by(noc, noc_title, census_frequency)|>
  summarize(statistics_canada_job_title=paste(statistics_canada_job_title, collapse = ", "))|>
  na.omit()|>#gets rid of seniors managers
  mutate(clean_tokens=map(statistics_canada_job_title, clean_job_title))

northisle_raw <- read_excel(here("data", "Northisle 2024 Labour Estimate.xlsx"), skip = 2)|>
  na.omit()|>
  filter(! Position %in% c("Total", "Head Office"))|>
  group_by(Position)|>
  summarize(Staff=sum(Staff))|>
  arrange(desc(Staff))|>
  rename(mining_company_job_title=Position)|>
  mutate(dirty_tokens=map(mining_company_job_title, clean_job_title))

northisle <- cross_join(clean_jobs, northisle_raw)|>
  mutate(overlap=map2_dbl(clean_tokens, dirty_tokens, normalized_dot),
         similarity=sqrt(overlap*census_frequency)
         )|>
  group_by(mining_company_job_title)|>
  filter(similarity>0.01)|>
  slice_max(similarity, n=7, with_ties = FALSE)|>
  select(noc, noc_title, mining_company_job_title, Staff, similarity)|>
  arrange(desc(Staff), mining_company_job_title, desc(similarity))

write_csv(northisle, here("out", "northisle.csv"))



