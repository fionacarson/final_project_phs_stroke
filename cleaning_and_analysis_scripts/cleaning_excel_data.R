library(tidyverse)
library(here)
library(janitor)
library(readxl)

incidence <- read_xlsx(here("raw_data/table3_incidence_stroke_2023-01-24.xlsx"),
                             sheet = "data")

incidence %>% 
  select(where(is.character)) %>% 
  map(unique)

# No missing values

# TIDY AGE GROUP COLUMN
incidence <- incidence %>% 
  rename("age" = "ageband",
         "diagnosis" = "cond",
         "hbr_name" = "hb",
         "sex" = "sexdesc",
         "incidence" = "inc",
         "crude_rate" = "crude") %>% 
  mutate(age = recode(age, 
                      "<75" = "under 75",
                      "75+" = "over 75",
                      "ALL AGES" = "All"),
         sex = recode(sex, 
                      "MALE" = "Male",
                      "FEMALE" = "Female",
                      "BOTH SEXES" = "All"))


write_csv(incidence, here("clean_data/incidence.csv"))

