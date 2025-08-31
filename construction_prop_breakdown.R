library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(magrittr)

construction <- read_excel(here("data", "Baptiste Nickel Project.xlsx"), sheet = "Constn Lbr - Camp count", skip = 4, col_names = c("thing", 1:36))|>
  filter(thing=="Overall Project")|>
  pivot_longer(cols = -thing, names_to = "month", values_to = "count")|>
  mutate(month=as.numeric(month),
         divided_count=count/420,
         division=list(1:420))|>
  unnest(division)|>
  mutate(index=row_number())


get_props <- function(tbbl, span){
 tbbl|>
  mutate(month=ceiling(index/(15120/span)))|>
  group_by(month)|>
  summarize(count=sum(divided_count))|>
  mutate(prop=count/sum(count))
}

results <- tibble(span=seq(18, 60, 6),
                  construction=list(construction))|>
  mutate(props=map2(construction, span, get_props))|>
  select(span, props)|>
  unnest(props)|>
  mutate(span= paste("Construction spanning", span, "months"))

(plt <- ggplot(results, aes(month, prop)) +
  geom_col(alpha=.5) +
  facet_wrap(~span, nrow=2) +
  labs(
    title = "Proportion of construction workers by year",
    x = "Month",
    y = "Proportion of total construction workers"
  ) +
  theme_minimal())

ggsave(filename=here("out", "construction_prop_breakdown.svg"),
               plot = plt, bg = "white", width = 10, height = 6)

write_csv(results, "construction_props.csv")

