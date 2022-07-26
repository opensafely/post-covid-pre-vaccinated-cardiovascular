##################################################################################
# 
# Description: This script reads in the input data and prepares it for data cleaning.
#
# Input: output/input.feather
# Output: output/
#
##################################################################################

# Load libraries ---------------------------------------------------------------

library(magrittr)
library(tidyverse)
library(lubridate)

# FILE PATHS

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review"))

# Define general start date and end date
cohort_start_date = as.Date("2020-01-01")
cohort_end_date = as.Date("2021-06-18") # General End date: 2021-06-18 (date last JCVI group eligible for vaccination - Decision on Jan 18th 2022)


## Load dataset
df <- arrow::read_feather(file = "output/input.feather")

## Define cohort start and end dates
df <- df %>%
  mutate(index_date = cohort_start_date,
         cohort_end_date = cohort_end_date)

# QC for consultation variable
# max to 365 (average of one per day)

print("Consultation variable before QC")
summary(df$cov_num_consulation_rate)

df <- df %>%
  mutate(cov_num_consulation_rate = replace(cov_num_consulation_rate, cov_num_consulation_rate > 365, 365))

print("Consultation variable after QC")
summary(df$cov_num_consulation_rate)

# Combine BMI variables to create one history of obesity variable ---------------

df <- df %>%
  mutate(cov_bin_obesity = ifelse(cov_bin_obesity == TRUE | cov_cat_bmi_groups == "Obese", TRUE, FALSE)) %>%
  dplyr::select(- c(cov_num_bmi,cov_num_bmi_date_measured,cov_cat_bmi_groups))

# Format columns -----------------------------------------------------
# dates, numerics, factors, logicals

df <- df %>% mutate(across(contains('_date'), ~ as.Date(as.character(.))),
                    across(contains('_birth_year'), ~ format(as.Date(.), "%Y")),
                    across(contains('_num'), ~ as.numeric(.)),
                    across(contains('_cat'), ~ as.factor(.)),
                    across(contains('_bin'), ~ as.factor(.)))

print("Columns formatted successfully")

# Define COVID-19 severity --------------------------------------------------------------

df <- df %>%
  mutate(sub_cat_covid19_hospital = 
           ifelse(!is.na(exp_date_covid19_confirmed) &
                    !is.na(sub_date_covid19_hospital) &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed >= 0 &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed < 29, "hospitalised",
                  ifelse(!is.na(exp_date_covid19_confirmed), "non_hospitalised", 
                         ifelse(is.na(exp_date_covid19_confirmed), "no_infection", NA)))) %>%
  mutate(across(sub_cat_covid19_hospital, factor))


# Restrict columns and save analysis dataset ---------------------------------

df1 <- df %>% 
  dplyr::select(- vax_jcvi_age_1, - vax_jcvi_age_2, vax_cat_jcvi_group) %>% #  remove JCVI variables
  # select patient id, death date and variables: subgroups, exposures, outcomes, covariates, quality assurance and vaccination
  dplyr::select(patient_id, death_date,index_date,cohort_end_date,
                contains(c("sub_", "exp_", "out_", "cov_", "qa_", "vax_"))) %>%
  dplyr::select(-contains("df_out_")) %>%
  dplyr::select(-contains("tmp_"))

# Describe data --------------------------------------------------------------

sink(paste0("output/not-for-review/describe_input_stage0.txt"))
print(Hmisc::describe(df1))
sink()

# SAVE

saveRDS(df1, file = paste0("output/input.rds"))

print("Dataset saved successfully")

# Save Venn diagram input dataset -----------------------
df <- df %>% select(starts_with(c("patient_id","tmp_out_date","out_date")))

# Describe data --------------------------------------------------------------

sink(paste0("output/not-for-review/describe_venn_input.txt"))
print(Hmisc::describe(df))
sink()

saveRDS(df, file = paste0("output/venn.rds"))

print("Venn dataset saved successfully")
