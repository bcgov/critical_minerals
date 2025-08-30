library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(magrittr)

construction <- read_excel(here("data", "Baptiste Nickel Project.xlsx"), sheet = "Constn Lbr - Camp count", skip = 4, col_names = c("thing", 1:36))|>
  filter(thing=="Overall Project")|>
  pivot_longer(cols = -thing, names_to = "month", values_to = "count")|>
  mutate(month=as.numeric(month),
         divided_count=count/35,
         division=list(1:35))|>
  unnest(division)|>
  mutate(index=row_number())


get_props <- function(tbbl, span){
 tbbl|>
  mutate(year=ceiling(index/(1260/span)))|>
  group_by(year)|>
  summarize(count=sum(count))|>
  mutate(prop=count/sum(count))
}

results <- tibble(span=seq(1.5, 5, .5),
                  construction=list(construction))|>
  mutate(props=map2(construction, span, get_props))|>
  select(span, props)|>
  unnest(props)|>
  mutate(span= paste("Construction spanning", span, "years"),
         year=as.character(year))

(ggplot(results, aes(as.character(year), prop)) +
  geom_col() +
  geom_label(aes(label = round(prop, 2))) +
  facet_wrap(~span, scales = "free_y") +
  labs(
    title = "Proportion of construction workers by year",
    x = "Year",
    y = "Proportion of total construction workers"
  ) +
  theme_minimal()) %>%
  ggsave(here("out", "construction_prop_breakdown.svg"),
               plot = ., bg = "white", width = 10, height = 6)

write_csv(results, "construction_props.csv")

