library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(openxlsx)
library(fuzzyjoin)
library(conflicted)
conflicts_prefer(dplyr::filter)
#functions-------------------------
source(here("R","functions.R"))
#mapping files----------------------
job_to_noc <- read_csv(here("data",
                                "mapping",
                                "add_new_job_titles_to_this_file.csv"),
                       locale = readr::locale(encoding = "UTF-8"))
nocs_to_names <- vroom::vroom(here("data", "mapping", "noc_2021_version_1.0_elements.csv"))|>
  select(noc=starts_with("Code"), noc_name=starts_with("Class"))|>
  distinct()
#write the mapping to a pretty excel file
left_join(job_to_noc, nocs_to_names, by = "noc")|>
  write_group_striped_excel("noc", here("out", "job_to_noc.xlsx"))

#brucejack------------------

brucejack_meta <- tibble(
  mine_type = "underground",
  commodities = "gold silver"
)

brucejack <- read_excel(here("data", "Labour Estimate - Newmont Red Chris and Brucejack.xlsx"), skip = 3, sheet = "Labour Estimate - Brucejack")|>
  clean_names()|>
  clean_text_column(category)|>
  clean_text_column(position)|>
  fill(category, .direction = "down")|>
  filter(category %in% c("general and administrative", "plant administration", "operations", "maintenance", "mining"))|>
  mutate(category=case_when(
    category %in% c("general and administrative", "plant administration") ~ "admin",
    category %in% c("operations", "maintenance") ~ "mill",
    TRUE ~ "mine"
  ))|>
  rename(location=category, staff=contains("staff"), non_standard_job_title=position)|>
  mutate(staff=as.numeric(staff))|>
  na.omit()|>
  crossing(brucejack_meta)|>
  map_nocs(job_to_noc)

agg_and_write(brucejack)

#red chris------------------

red_chris_meta <- tibble(
  mine_type = "open pit",
  commodities = "gold copper"
)

red_chris <- read_excel(here("data", "Labour Estimate - Newmont Red Chris and Brucejack.xlsx"), skip = 3, sheet = "Labour Estimate - Red Chris")|>
  clean_names()|>
  clean_text_column(category)|>
  clean_text_column(position)|>
  fill(category, .direction = "down")|>
  filter(category %in% c("general and administrative", "plant administration", "operations", "maintenance", "mining"))|>
  mutate(category=case_when(
    category %in% c("general and administrative", "plant administration") ~ "admin",
    category %in% c("operations", "maintenance") ~ "mill",
    TRUE ~ "mine"
  ))|>
  rename(location=category, staff=contains("staff"), non_standard_job_title=position)|>
  mutate(staff=as.numeric(staff))|>
  na.omit()|>
  crossing(red_chris_meta)|>
  map_nocs(job_to_noc)

agg_and_write(red_chris)

#north island----------------------
north_island_meta <- mine_type("North Island")

north_island <- read_excel(here("data", "North Island.xlsx"), skip = 2)|>
  clean_names()|>
  clean_text_column(position)|>
  filter(!is.na(position),
         !position %in% c("total"))|>
  mutate(category=if_else(is.na(staff), position, NA_character_))|>
  fill(category, .direction = "downup")|>
  na.omit()|>
  mutate(location=case_when(str_detect(category, "admin") ~ "admin",
                            category %in% c("operations", "maintenance") ~ "mill",
                            TRUE ~ "mine"))|>
  select(non_standard_job_title=position, staff, location)|>
  crossing(north_island_meta)|>
  map_nocs(job_to_noc)

agg_and_write(north_island)

#baptiste mine------------------

baptiste_meta <- mine_type("Baptiste Nickel Project")

baptiste_mine <- read_excel(here("data","Baptiste Nickel Project.xlsx"), sheet = "Mining Breakdown", skip = 3, n_max = 95)|>
  clean_names()|>
  select(non_standard_job_title=period, contains("year"))|>
  clean_text_column(non_standard_job_title)|>
  mutate(across(contains("year"), ~ as.numeric(.x)))|>
  filter(!str_detect(non_standard_job_title, "\\b(subtotal|total|absenteeism|annual)\\b"))|>
  mutate(location="mine")|>
  crossing(baptiste_meta)|>
  pivot_longer(cols = contains("Year"), names_to = "year", values_to = "staff")|>
  group_by(location, mine_type, commodities, non_standard_job_title)|>
  summarize(staff=round(mean(staff, na.rm = TRUE)), .groups = "drop")|>
  filter(staff>0)|>
  map_nocs(job_to_noc)

#baptiste mill------------------

baptiste_mill <- read_excel(here("data",
                                 "Baptiste Nickel Project.xlsx"),
                            sheet = "Plant - Tailings Breakdown",
                            skip = 2, n_max = 53)
colnames(baptiste_mill)[6] <- "phase 2"

baptiste_mill <- baptiste_mill|>
  clean_names()|>
  select(non_standard_job_title=position, phase_1=no_of_employee, phase_2)|>
  na.omit()|>
  clean_text_column(non_standard_job_title)|>
  mutate(across(contains("phase"), ~ as.numeric(.x)))|>
  filter(!str_detect(non_standard_job_title, "\\b(subtotal|total)\\b"))|>
  mutate(location="mill")|>
  crossing(baptiste_meta)|>
  pivot_longer(cols = contains("phase"), names_to = "year", values_to = "staff")|>
  group_by(location, mine_type, commodities, non_standard_job_title)|>
  summarize(staff=round(mean(staff, na.rm = TRUE)), .groups = "drop")|>
  filter(staff>0)|>
  map_nocs(job_to_noc)

#baptiste owner team----------------

baptiste_owner <- read_excel(here("data",
                                 "Baptiste Nickel Project.xlsx"),
                            sheet = "Owner Team - G&A Breakdown",
                            skip = 5,
                            col_names = c("non_standard_job_title", "staff", "phase1", "phase2", "onsite"))|>
  filter(staff=="Total")|>
  mutate(staff=(phase1+phase2)/2,
         location="admin")|>
  crossing(baptiste_meta)|>
  select(-phase1, -phase2, -onsite)|>
  map_nocs(job_to_noc)


baptiste <- bind_rows(baptiste_owner, baptiste_mine, baptiste_mill)|>
  arrange(noc)

agg_and_write(baptiste)



