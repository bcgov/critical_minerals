library(tidyverse)
library(here)
library(readxl)
library(janitor)

census_nocs <- read_excel(here("data", "NOC NAICS.xlsx"))|>
  select(noc=1, contains(c("2122", "2123")))|>#filter only 5 digit codes
  filter(str_detect(noc, "^\\d{5}"))|>
  pivot_longer(cols = -noc, names_to = "naics")|>
  group_by(noc)|>
  summarize(value=sum(value))|>
  filter(value>0)|>
  separate(noc, into = c("noc", "noc_desc"), sep = " ", extra = "merge")|>
  select(noc)

mapped_nocs <- read_excel(here("out", "job_to_noc.xlsx"), col_types = "text")|>
  select(noc)|>
  distinct()

anti_join(mapped_nocs, census_nocs)
