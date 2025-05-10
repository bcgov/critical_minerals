library(tidyverse)
library(tidytext)
library(widyr)
library(here)
library(readxl)
library(janitor)
library(tictoc)
library(conflicted)
conflicts_prefer(dplyr::filter)
tic()

meta <- read_excel(here("data", "Upcoming Mine Data - Employment.xlsx"), skip=1)|>
  clean_names()|>
  filter(upcoming_mine=="North Island")|>
  unite(meta, open_pit_or_underground_mine, commodities, sep="-")|>
  pull(meta)

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
  arrange(desc(census_frequency))|>
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
  unnest_tokens(word, statistics_canada_job_title)|>
  mutate(word=textstem::lemmatize_words(word))|>
  count(noc, word, sort = TRUE)|>
  bind_tf_idf(word, noc, n)

northisle_raw <- read_excel(here("data", "Northisle 2024 Labour Estimate.xlsx"), skip = 2)|>
  filter(!is.na(Position),
         !Position %in% c("Total", "Head Office"))|>
  mutate(category=if_else(is.na(Staff), Position, NA_character_))|>
  fill(category, .direction = "down")|>
  mutate(meta=meta)|>
  unite("Position", meta, category, Position, sep = " - ", na.rm = TRUE)|>
  na.omit()

northisle <- northisle_raw|>
  mutate(mining_company_job_title=Position)|>
  unnest_tokens(word, mining_company_job_title)|>
  mutate(word=str_replace_all(word, "metallurgist", "metallurgical"), #metallurgist missing from clean jobs.
         word=textstem::lemmatize_words(word))|>
  count(Position, word, sort = TRUE)|>
  bind_tf_idf(word, Position, n)

# Combine both sets with a label
jobs <- clean_jobs %>%
  mutate(doc_type = "noc", doc_id = noc) %>%
  select(doc_id, doc_type, word, tf_idf)

positions <- northisle %>%
  mutate(doc_type = "position", doc_id = Position) %>%
  select(doc_id, doc_type, word, tf_idf)

# Combine both into one tibble
all_docs <- bind_rows(jobs, positions)

# Compute pairwise cosine similarity between every position and every noc
similarities <- all_docs %>%
  pairwise_similarity(item = doc_id, feature = word, value = tf_idf,
                      diag = FALSE, upper = FALSE)%>%
  filter(item1 %in% clean_jobs$noc,
         item2 %in% northisle_raw$Position)|>
  full_join(noc_freq, by = c("item1" = "noc"))|>
  select(noc=item1, noc_title, Position=item2, similarity, census_frequency)|>
  mutate(composite = sqrt(similarity * census_frequency))|>
  group_by(Position) %>%
  slice_max(composite, n = 1)|>
  na.omit()|>
  full_join(northisle_raw, by = c("Position" = "Position"))|>
  select(noc, noc_title, Position, Staff, similarity=composite)|>
  arrange(desc(Staff), Position, desc(similarity))|>
  mutate(Position= str_remove_all(Position, paste(meta,"-")))

toc()








