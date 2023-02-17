
library(tidyverse)
library(here)
library(janitor)



# activity by health board NAs dropped-----------------------------------------------------

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

# ADD MOST RECENT 3 YEARS OF DATA

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

# MISSING DATA
# Data is missing for confidentiality reasons because the numbers are so low 
# they would allow identification of individuals. Original plan was to change 
# values to 1 but this is complicated by the fact a lot of the values are 
# derived so, for example, we'd need to change Male and Female to one and then 
# All to 2 and then add this to Scotland total. 
# All missing data is from Orkney, Shetland and Western Isles. 
# 3739 rows with missing data.  
# There is no age group data for the 3 health boards but there is data for 
# age_group = All. 

# Let change all NAs to zero, the numbers are clearly low and we can't justify 
# guessing an actual number. 

activity_hb <- activity_hb %>% 
  drop_na(number_of_discharges)

# CONSOLIDATE NAMES IN SEX COLUMN
activity_hb <- activity_hb %>% 
  mutate(sex = recode(sex,"Males" = "Male",  "Females" = "Female"))

# CONSOLIDATE HEALTH BOARD CODES (USE NEWER ONES)
activity_hb <- activity_hb %>% 
  mutate(hbr = recode(hbr,
                      "S08000018" = "S08000029", 
                      "S08000021" = "S08000031",
                      "S08000023" = "S08000032"))

# TIDY AGE GROUP COLUMN
activity_hb <- activity_hb %>% 
  rename("age" = "age_group") %>% 
  mutate(age = str_remove(age, " years")) %>% 
  mutate(age = recode(age, 
                      "under75" = "under 75",
                      "75plus" = "over 75"))

# CREATING "OTHER CVD" SUB-CATEGORY

# Diagnosis column contains Cerebrovascular Disease which is the equivalent of "All"
# in the sex and age columns. This is divided into stroke, subarachnoid haemorrhage, 
# and TIAs but there are other diagnoses not included in these categories so we 
# need to manufacture an "other cvd" category and calculate values. 

# Sum all the sub-categories we do have
stroke_TIA_haemorrhage <- activity_hb %>% 
  filter(!diagnosis == "Cerebrovascular Disease") %>% 
  group_by(financial_year, hbr, admission_type, age, sex) %>% 
  summarise(number_of_discharges_stroke_etc = sum(number_of_discharges),
            crude_rate_stroke_etc = sum(crude_rate),
            easr_stroke_etc = sum(easr)) 

# Need the CVD only values so we can the subtract the summed values created above
cvd_only <- activity_hb %>% 
  filter(diagnosis == "Cerebrovascular Disease")

# Join the cvd_only and existing sub-categories datasets so we can perform subtractions
other_cvd <- left_join(stroke_TIA_haemorrhage, cvd_only, 
                       by = c("financial_year", "hbr", "admission_type", "age", "sex"))

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

activity_hb <- activity_hb %>% 
  mutate(crude_rate = round(crude_rate, 0),
         easr = round(easr, 0))

# Adding health board names into dataset to make analysis more intuitive
activity_hb <- activity_hb %>% 
  mutate(hbr_name = case_when(
    hbr == "S92000003" ~ "Scotland", 
    hbr == "S08000015" ~ "Ayrshire & Arran", 
    hbr == "S08000016" ~ "Borders", 
    hbr == "S08000017" ~ "Dumfries & Galloway",
    hbr == "S08000019" ~ "Forth Valley",
    hbr == "S08000020" ~ "Grampian", 
    hbr == "S08000022" ~ "Highland", 
    hbr == "S08000024" ~ "Lothian", 
    hbr == "S08000025" ~ "Orkney", 
    hbr == "S08000026" ~ "Shetland",
    hbr == "S08000028" ~ "Western Isles",
    hbr == "S08000029" ~ "Fife",
    hbr == "S08000030" ~ "Tayside",
    hbr == "S08000031" ~ "Greater Glasgow & Clyde",
    hbr == "S08000032" ~ "Lanarkshire"
  ), .after = hbr)

write_csv(activity_hb, "clean_data/activity_hb_na_dropped.csv")