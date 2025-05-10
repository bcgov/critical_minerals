library(tidyverse)
library(tidytext)
library(widyr)
library(here)
library(readxl)
library(janitor)
library(conflicted)
conflicts_prefer(dplyr::filter)


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
  na.omit()|>
  mutate(location=case_when(str_detect(category, "Admin") ~ "office",
                            category %in% c("Operations", "Maintenance") ~ "mill",
                            TRUE ~ "mine"),
         role=case_when(str_detect(Position, "(?i)Helper") ~ "helper",
                        str_detect(Position, "(?i)Operator") ~ "operator",
                        str_detect(Position, "(?i)Maintenance") ~ "maintenance",
                        TRUE ~ "other" ),
         noc=case_when(location=="mine"& role=="maintenance" ~ "72401",
                       location=="mine"& role=="operator" ~ "73400",
                       location=="mill"& role=="operator" ~ "73401",
                       location=="mine"& role=="helper" ~ "85110",
                       location=="mill"& role=="helper" ~ "86101",
                       str_detect(Position, "(?i)Driller") ~ "73402",
                       str_detect(Position, "(?i)Blaster") ~ "73403",
                       str_detect(Position, "(?i)Geologists") ~ "21331",
                       str_detect(Position, "(?i)Mechanic") & location=="mill" ~ "72401",
                       str_detect(Position, "(?i)Supervis") & location=="mine" ~ "82020",
                       str_detect(Position, "(?i)Electric") & location=="mill" ~"72201",
                       str_detect(Position, "(?i)Maintenance Plan") ~ "13201",


                       TRUE ~ "unknown" )
         )




