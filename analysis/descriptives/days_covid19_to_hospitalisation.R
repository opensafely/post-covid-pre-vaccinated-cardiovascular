##################################################################################
# 
# Description: This script reads in the input data from stage 1 and provides
#              the distribution of days from COVID-19 infection to hospitalisation
#              for those considered as hospitalised COVID.
#
# Input: output/input_stage1.rds
# Output: output/review/descriptives/days_COVID19_to_hospitalisation_pre_vaccination.csv"
#
##################################################################################

library(readr)
library(tidyverse)


cohort_name <- "pre_vaccination"

# Load relevant data
input <- read_rds(paste0("output/input_stage1.rds"))

# Get number of days between Covid diagnosis and hospital admission for hospitalised Covid cases
input <- input %>%
  mutate(days_Covid19_to_hospitalisation = 
           ifelse(sub_cat_covid19_hospital == "hospitalised", input$sub_date_covid19_hospital - input$exp_date_covid19_confirmed, NA)
  )
table_N_days <- input %>% dplyr::count(days_Covid19_to_hospitalisation)

# Add in suppression for counts <=5
table_N_days_for_disclosure <- table_N_days %>%
  mutate(n_people = 
           ifelse(n >5, n, "[Redacted]")
  ) %>%
  select(days_Covid19_to_hospitalisation,n_people)

write.csv(table_N_days_for_disclosure, file=paste0("output/review/descriptives/days_COVID19_to_hospitalisation_",cohort_name, ".csv"), row.names = F)