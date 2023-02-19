library(tidyverse)
library(here)
library(readxl)
library(janitor)

hb_age_pop <- read_xlsx(here("raw_data/mid-year-pop-est-21-time-series-data.xlsx"),
                       sheet = "Table_2", skip = 5, col_names = TRUE) %>% 
  clean_names() %>% 
  filter(year == 2021)


under_75 <- hb_age_pop %>% select(x0:x74)
over_75 <- hb_age_pop %>% select(x75:x90_and_over)

hb_age_pop_over_under_75 <- hb_age_pop %>% 
  mutate(under_75 = rowSums(under_75)) %>% 
  mutate(over_75 = rowSums(over_75)) 

write_csv(hb_age_pop_over_under_75, here("clean_data/hb_population_2021_by_age.csv"))
