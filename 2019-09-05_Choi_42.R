# 2019-09-05_Choi_42.R

# More specifically, may I request for the number of pt.s in the 
# registry - by the following characteristics?
# Gender: XX women; XX men
# Age : XX <age 65; XX>= age65
# Cognitive functioning status among those 65+ (if not known by the age group, 
# among all 740 pt.s)
# - Normal: XX
# - MCI: XX
# - Dementia: XX
# Race among those 65+ (if not known by the age group, among 740 pt.s)
# - White: XX
# - African American: XX
# - Others: XX
# Ethnicity among those 65+ (if not known by the age group, among 740 pt.s)
# - Hispanic: XX
# - Non-Hispanic: XX


# Packages ----
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(openssl)
library(lubridate)


# Globals / Helpers ----
source("~/Box/Documents/R_helpers/config.R")
source("~/Box/Documents/R_helpers/helpers.R")


# Retrieve Data ----
fields_ms_raw <-
  c(
    "subject_id"
    , "date"
    , "birth_date"
    , "sex_value"
    , "race_value"
    , "clin_diagnosis"
    , "uds_dx"
  )

fields_ms <- fields_ms_raw %>% paste(collapse = ",")

fields_u3_raw <- 
  c(
    "ptid"
    , "form_date"
  )
fields_u3 <- fields_u3_raw %>% paste(collapse = ",")

json_ms <- 
  export_redcap_records(uri    = REDCAP_API_URI,
                        token  = REDCAP_API_TOKEN_MINDSET,
                        fields = fields_ms)

json_u3 <-
  export_redcap_records(uri    = REDCAP_API_URI,
                        token  = REDCAP_API_TOKEN_UDS3n,
                        fields = fields_u3)

df_ms <-
  json_ms %>% 
  jsonlite::fromJSON() %>% 
  as_tibble %>% 
  na_if("") %>% 
  type_convert(col_types = cols(.default = col_guess()))
glimpse(df_ms)

df_u3 <-
  json_u3 %>% 
  jsonlite::fromJSON() %>% 
  as_tibble %>% 
  na_if("") %>% 
  type_convert(col_types = cols(.default = col_guess()))
glimpse(df_u3)

df_ms_cln <-
  df_ms %>% 
  select(-redcap_event_name) %>% 
  rename(sex = sex_value, race = race_value) %>% 
  filter(date >= as.Date("2015-01-01"))

df_u3_cln <-
  df_u3 %>% 
  select(-redcap_event_name) %>% 
  filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
  filter(!is.na(form_date))

ids_u3 <-
  df_u3_cln %>% 
  distinct(ptid) %>%
  pull(ptid)

df_ms_cln_flt <-
  df_ms_cln %>% 
  filter(!(subject_id %in% ids_u3)) %>% 
  get_visit_n(subject_id, date, n = Inf)

df_ms_cln_flt_mut <-
  df_ms_cln_flt %>% 
  mutate(age = 
           interval(birth_date, Sys.Date()) %>% time_length(unit = "year")) %>% 
  mutate(age_categ = if_else(age < 65, "< 65", ">= 65")) %>% 
  mutate(subject_id = openssl::sha256(subject_id)) %>% 
  mutate(sex = case_when(
    sex == 1 ~ "M",
    sex == 2 ~ "F",
    TRUE ~ NA_character_
  )) %>% 
  mutate(race = case_when(
    # 1, White | 2, Black | 3, Asian | 4, Hispanic | 5, Other | 6, Unknown
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race == 3 ~ "Asian",
    race == 4 ~ "Hispanic",
    race == 5 ~ "Other",
    race == 6 ~ "Unknown",
    TRUE ~ NA_character_
  )) %>% 
  mutate(clin_diagnosis = case_when(
    clin_diagnosis == 1  ~ "probable AD",
    clin_diagnosis == 2  ~ "probable AD/EP",
    clin_diagnosis == 3  ~ "FTD",
    clin_diagnosis == 4  ~ "FTD/EP",
    clin_diagnosis == 5  ~ "HD",
    clin_diagnosis == 6  ~ "MCI",
    clin_diagnosis == 7  ~ "MID",
    clin_diagnosis == 8  ~ "PD",
    clin_diagnosis == 9  ~ "PDD",
    clin_diagnosis == 10 ~ "PSP",
    clin_diagnosis == 11 ~ "OPCA",
    clin_diagnosis == 12 ~ "MSA",
    clin_diagnosis == 13 ~ "NL",
    clin_diagnosis == 14 ~ "CBD",
    clin_diagnosis == 15 ~ "Memory Concerns",
    clin_diagnosis == 16 ~ "No Memory Concerns",
    clin_diagnosis == 17 ~ "Other Cognitive Disorder",
    clin_diagnosis == 18 ~ "Other Movement Disorder",
    clin_diagnosis == 19 ~ "Other Mixed Cognitive/Movement Disorder",
    clin_diagnosis == 20 ~ "Other Dementia",
    clin_diagnosis == 21 ~ "Pending",
    clin_diagnosis == 22 ~ "Irrelevant",
    clin_diagnosis == 23 ~ "Probable DLB",
    clin_diagnosis == 24 ~ "PPA",
    clin_diagnosis == 25 ~ "Lewy Body Dementia",
    TRUE ~ NA_character_
  )) %>% 
  mutate(uds_dx = case_when(
    uds_dx == 1 ~ "Amnestic MCI-memory only",
    uds_dx == 2 ~ "Amnestic MCI-memory plus",
    uds_dx == 3 ~ "Probable AD",
    uds_dx == 4 ~ "Possible AD",
    uds_dx == 5 ~ "Dem with Lewy bodies",
    uds_dx == 6 ~ "Vascular dem",
    uds_dx == 7 ~ "Alcohol-related dem",
    uds_dx == 8 ~ "Undetermined dem",
    uds_dx == 9 ~ "FTD",
    uds_dx == 10 ~ "Primary progressive aphasia",
    uds_dx == 11 ~ "PSP",
    uds_dx == 12 ~ "CBD",
    uds_dx == 13 ~ "HD",
    uds_dx == 14 ~ "Prion",
    uds_dx == 15 ~ "Cog dysfunction from med",
    uds_dx == 16 ~ "Cog dysfunction from illness",
    uds_dx == 17 ~ "Depression",
    uds_dx == 18 ~ "Other psych illness",
    uds_dx == 19 ~ "Down's",
    uds_dx == 20 ~ "PD",
    uds_dx == 21 ~ "Stroke",
    uds_dx == 22 ~ "Hydrocephalus",
    uds_dx == 23 ~ "Traumatic brain injury",
    uds_dx == 24 ~ "CNS neoplasm",
    uds_dx == 25 ~ "Other",
    uds_dx == 26 ~ "NL",
    uds_dx == 27 ~ "Non-Amnestic MCI-single domain",
    uds_dx == 28 ~ "Non-Amnestic MCI-multiple domains",
    uds_dx == 29 ~ "Impaired, not MCI",
    uds_dx == 30 ~ "Per Center Decision-patient did not come to consensus-milestoned out of study",
    uds_dx == 31 ~ "Amnestic MCI, single domain",
    uds_dx == 32 ~ "Patient never came to consensus",
    uds_dx == 33 ~ "Amnestic multidomain dementia syndrome",
    uds_dx == 34 ~ "Amnestic MCI, multiple domains",
    uds_dx == 35 ~ "Behavioral variant FTD (bvFTD) syndrome",
    TRUE ~ NA_character_
  )) %>% 
  mutate(dx = coalesce(uds_dx, clin_diagnosis)) %>% 
  select(-birth_date) %>% 
  select(subject_id, date, age, age_categ, sex, race, dx, clin_diagnosis, uds_dx)

# df_ms_cln_flt_mut %>% 
#   group_by(sex) %>% 
#   summarize(n = n())

# df_ms_cln_flt_mut %>% 
#   group_by(race) %>% 
#   summarize(n = n())

group_by_summarize <- function(df, group_var) {
  group_var_quo <- enquo(group_var) 
  df %>% 
    group_by(!!group_var_quo) %>% 
    summarize(n = n())
}

group_bys_summarize <- function(df, ...) {
  group_var_quos <- enquos(...)
  df %>% 
    group_by(!!!group_var_quos) %>% 
    summarize(n())
}


group_by_summarize(df_ms_cln_flt_mut, age_categ) %>% 
  write_csv("MiNDSet_Registry--AgeCateg.csv", na = "n/a")

group_by_summarize(df_ms_cln_flt_mut, sex)
group_bys_summarize(df_ms_cln_flt_mut, age_categ, sex) %>% 
  tidyr::spread(key = age_categ, value = `n()`) %>% 
  write_csv(x = ., "MiNDSet_Registry--AgeCateg_X_Sex.csv", na = "n/a")

group_by_summarize(df_ms_cln_flt_mut, race)
group_bys_summarize(df_ms_cln_flt_mut, age_categ, race) %>% 
  tidyr::spread(key = age_categ, value = `n()`) %>% 
  write_csv("MiNDSet_Registry--AgeCateg_X_Race.csv", na = "n/a")

group_by_summarize(df_ms_cln_flt_mut, dx)
group_bys_summarize(df_ms_cln_flt_mut, age_categ, dx) %>% 
  tidyr::spread(key = age_categ, value = `n()`) %>% 
  write_csv("MiNDSet_Registry--AgeCateg_X_Dx.csv", na = "n/a")













