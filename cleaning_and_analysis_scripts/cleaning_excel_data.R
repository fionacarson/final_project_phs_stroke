library(tidyverse)
library(here)
library(janitor)
library(readxl)

incidence <- read_xlsx(here("raw_data/table3_incidence_stroke_2023-01-24.xlsx"),
                             sheet = "data")

incidence %>% 
  select(where(is.character)) %>% 
  map(unique)

