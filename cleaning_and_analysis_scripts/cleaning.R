library(tidyverse)
library(here)
library(janitor)


# activity_hb -------------------------------------------------------------

# The following file was supplied by CodeClan. It contains 10 years of data
# up to 2018/19

activity_hb <- read_csv(here("raw_data/stroke_activitybyhbr.csv")) %>% 
  clean_names() %>% 
# Have checked all the qualifiers and they can be removed  
  select(!ends_with("qf"))

# The following file is the most recent 10 years of data available from 
# PHS website and goes up to 2021/22

activity_hb_2022 <- read_csv(here("raw_data/stroke_activitybyhbr_up_to_2022.csv")) %>% 
  clean_names() %>% 
  select(!ends_with("qf"))


# Each year contains 4320 records. There are 10 years worth of data in each file 
# so that equates to the 43200 observations we have in our datasets. 

# Each year is broken down into:
# 15 health boards (14 and all Scotland)
# 3 sexes (M, F, All)
# 6 age groups (0-44, 45-64, 65-74 years, 75plus, under 75, all)
# 4 diagnoses (cvd, stroke, subarachnoid haemorrhage, TIAs and related syndromes)
# 4 admission types (elective, emergency, transfer, all)
# 15 * 3 * 6 * 4 * 4 = 4320

# Add extra 3 years of data to create dataset with 56160 observations

activity_2019_to_2022 <- activity_hb_2022 %>% 
  filter(financial_year %in% c("2019/20", "2020/21", "2021/22"))

activity_hb <- bind_rows(activity_hb, activity_2019_to_2022)

rm(activity_hb_2022, activity_2019_to_2022)

# Diagnosis column contains Cerebrovascular Disease which is the equivalent of "All"
# in the sex and age columns. This is divided into stroke, subarachnoid haemorrhage, 
# and TIAs but there are other diagnoses not included in these categories so we 
# need to manufacture an "other cvd" categoriy and calculate values. 

# Sum all the sub-categories we do have
stroke_TIA_haemorrhage <- activity_hb %>% 
  filter(!diagnosis == "Cerebrovascular Disease") %>% 
  group_by(financial_year, hbr, admission_type, age_group, sex) %>% 
  summarise(number_of_discharges_stroke_etc = sum(number_of_discharges),
            crude_rate_stroke_etc = sum(crude_rate),
            easr_stroke_etc = sum(easr)) 

stroke_TIA_haemorrhage %>% 
  filter(hbr == "S08000025")

check <- activity_hb %>% 
  filter(hbr == "S08000025")

# Need the cvd only values so we can the subtract the summed values created above
cvd_only <- activity_hb %>% 
  filter(diagnosis == "Cerebrovascular Disease")

# Join the cvd_only and exisitng sub-cateogories datasets so we can perform subtractions
other_cvd <- left_join(stroke_TIA_haemorrhage, cvd_only, 
                 by = c("financial_year", "hbr", "admission_type", "age_group", "sex"))

# Subtract summed sub-categories values from cvd only to get other cvd values
other_cvd <- other_cvd %>% 
  mutate(number_of_discharges = number_of_discharges - number_of_discharges_stroke_etc,
         crude_rate = crude_rate - crude_rate_stroke_etc, 
         easr = easr - easr_stroke_etc) %>% 
  select(-ends_with("stroke_etc")) %>% 
  mutate(diagnosis = recode(diagnosis, "Cerebrovascular Disease" = "Other CVD"))

# Add other cvd data into original dataset so we now have the total (called 
# "Cerebrovascular Disease) and 4 sub-categories (stroke, TIA, haemorrhage and other)
activity_hb <- bind_rows(activity_hb, other_cvd)

rm(other_cvd, stroke_TIA_haemorrhage, cvd_only)

write_csv(activity_hb, "clean_data/activity_hb.csv")

# mortality_hb ------------------------------------------------------------

mortality_hb <- read_csv(here("raw_data/stroke_mortalitybyhbr.csv")) %>% 
  clean_names() %>% 
  select(!ends_with("qf"))

mortality_hb_2022 <- read_csv(here("raw_data/stroke_mortalitybyhbr_up_to_2022.csv")) %>% 
  clean_names() %>% 
  select(!ends_with("qf"))

activity_2019_to_2022 <- activity_hb_2022 %>% 
  filter(financial_year %in% c("2019/20", "2020/21", "2021/22"))

activity_hb <- bind_rows(activity_hb, activity_2019_to_2022)

rm(activity_hb_2022, activity_2019_to_2022)