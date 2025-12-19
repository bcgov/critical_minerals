library(tidyverse)
library(here)
library(readxl)
library(janitor)

construction <- read_excel(here("data", "Baptiste Nickel Project.xlsx"), sheet = "Constn Lbr - Camp count", skip = 4, col_names = c("thing", 1:36))|>
  filter(thing=="Overall Project")|>
  pivot_longer(cols = -thing, names_to = "month", values_to = "count")|>
  mutate(month=as.numeric(month),
         quarter= (month - 1) %/% 3 + 1)|>
  group_by(quarter)|>
  summarize(count=sum(count))|>
  mutate(divided_count=count/720720, #least common multiple (LCM) of 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
         division=list(1:720720))|>
  unnest(division)|>
  mutate(index=row_number())

get_props <- function(tbbl, span){
 tbbl|>
  mutate(quarter=ceiling(index/(12*720720/span)))|>
  group_by(quarter)|>
  summarize(count=sum(divided_count))|>
  mutate(prop=count/sum(count))
}

results <- tibble(span=2:16,
                  construction=list(construction))|>
  mutate(props=map2(construction, span, get_props))|>
  select(span, props)|>
  unnest(props)

(plt <- ggplot(results, aes(quarter, prop)) +
  geom_col(alpha=.5) +
  facet_wrap(~span,
             labeller = labeller(span = function(x) paste("Construction spanning ", x, "Quarters"))
             ) +
  labs(
    title = "Proportion of construction workers by quarter",
    x = "Quarter",
    y = "Proportion of total construction workers"
  ))

ggsave(filename=here("out", "quarterly_prop_breakdown.svg"),
               plot = plt, bg = "white", width = 10, height = 6)

write_csv(results, "quarterly_props.csv")

