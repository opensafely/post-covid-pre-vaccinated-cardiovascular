## =============================================================================
## Project:     Post covid unvaccinated project
##
## Purpose:  Apply stage 1. Data cleaning
##  - Prepare variables
##  - Apply QA rules
##  - Apply inclusion exclusion criteria
##  - Create cleaned dataset
## 
## Content: 
## 0. Load relevant libraries and read data/arguments
## 1. Prepare all variables (re-factoring, re-typing)
## 2. Apply QA rules
## 3. Apply exclusion/inclusion criteria
## 4. Output flowchart CSV and create the final stage 1 dataset 
## =============================================================================

###############################################
# 0. Load relevant libraries and read in data #
###############################################

library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)

# Define general start date and end date
start_date = as.Date("2020-01-01")
end_date = as.Date("2021-06-18") # General End date: 2021-06-18 (date last JCVI group eligible for vaccination - Decision on Jan 18th 2022)

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "descriptives"))

# Input dataset
input <-read_rds("output/input.rds")

# NOTE: no censoring of end date for death/event at this stage

######################################################
# 1. Prepare all variables (re-factoring, re-typing) # 
######################################################

# Handle missing values

input <- input %>% mutate(cov_cat_smoking_status = as.character(cov_cat_smoking_status)) %>%
  mutate(cov_cat_smoking_status = replace_na(cov_cat_smoking_status, "M")) %>%
  mutate(cov_cat_smoking_status = as.factor(cov_cat_smoking_status))

input <- input %>% mutate(cov_cat_region = as.character(cov_cat_region)) %>%
  mutate(cov_cat_region = replace_na(cov_cat_region, "Missing")) %>%
  mutate(cov_cat_region = as.factor(cov_cat_region))

# For categorical factors, specify references

cat_factors <- colnames(input)[grepl("_cat_",colnames(input))]
input[,cat_factors] <- lapply(input[,cat_factors], function(x) factor(x, ordered = FALSE))

## cov_cat_ethnicity
levels(input$cov_cat_ethnicity) <- list("Missing" = "0", "White" = "1", "Mixed" = "2", "South Asian" = "3", "Black" = "4", "Other" = "5")
input$cov_cat_ethnicity <- ordered(input$cov_cat_ethnicity, levels = c("White","Mixed","South Asian","Black","Other","Missing"))

## cov_cat_deprivation
levels(input$cov_cat_deprivation)[levels(input$cov_cat_deprivation)==1 | levels(input$cov_cat_deprivation)==2] <-"1-2 (most deprived)"
levels(input$cov_cat_deprivation)[levels(input$cov_cat_deprivation)==3 | levels(input$cov_cat_deprivation)==4] <-"3-4"
levels(input$cov_cat_deprivation)[levels(input$cov_cat_deprivation)==5 | levels(input$cov_cat_deprivation)==6] <-"5-6"
levels(input$cov_cat_deprivation)[levels(input$cov_cat_deprivation)==7 | levels(input$cov_cat_deprivation)==8] <-"7-8"
levels(input$cov_cat_deprivation)[levels(input$cov_cat_deprivation)==9 | levels(input$cov_cat_deprivation)==10] <-"9-10 (least deprived)"
input$cov_cat_deprivation <- ordered(input$cov_cat_deprivation, levels = c("1-2 (most deprived)","3-4","5-6","7-8","9-10 (least deprived)"))

## cov_cat_smoking_status
levels(input$cov_cat_smoking_status) <- list("Ever smoker" = "E", "Missing" = "M", "Never smoker" = "N", "Current smoker" = "S")
input$cov_cat_smoking_status <- ordered(input$cov_cat_smoking_status, levels = c("Never smoker","Ever smoker","Current smoker","Missing"))

## cov_cat_sex
levels(input$cov_cat_sex) <- list("Female" = "F", "Male" = "M")
input$cov_cat_sex <- relevel(input$cov_cat_sex, ref = "Female")

## Set reference level for binary covariates
bin_factors <- colnames(input)[grepl("cov_bin_",colnames(input))]
input[,bin_factors] <- lapply(input[,bin_factors], function(x) factor(x, levels = c("FALSE","TRUE")))

print("Variable preparation performed successfully")

#####################
# 2. Apply QA rules #
#####################

#Rule 1: Year of birth is after year of death or patient only has year of death
input$rule1=NA
input$rule1=((input$qa_num_birth_year > (format(input$death_date, format="%Y")) & is.na(input$qa_num_birth_year)== FALSE & is.na(input$death_date) == FALSE)|(is.na(input$qa_num_birth_year)== TRUE & is.na(input$death_date) == FALSE))

#Rule 2: Year of birth predates NHS established year or year of birth exceeds current date
input$rule2=NA
input$rule2=((input$qa_num_birth_year <1793 |(input$qa_num_birth_year >format(Sys.Date(),"%Y"))) & is.na(input$qa_num_birth_year) == FALSE)

#Rule 3: Date of death is NULL or invalid (on or before 1/1/1900 or after current date)
input$rule3=NA
input$rule3=((input$death_date <="1900-01-01"|input$death_date > format(Sys.Date(),"%Y-%m-%d")) & is.na(input$death_date) == FALSE)

#Rule 4: Pregnancy/birth codes for men
input$rule4=NA
input$rule4=(input$qa_bin_pregnancy == TRUE & input$cov_cat_sex=="Male")

#Rule 5: HRT or COCP meds for men
input$rule5=NA
input$rule5=((input$cov_cat_sex=="Male" & input$cov_bin_hormone_replacement_therapy==TRUE)|(input$cov_cat_sex=="Male" & input$cov_bin_combined_oral_contraceptive_pill==TRUE))

#Rule 6: Prostate cancer codes for women
input$rule6=NA
input$rule6=(input$qa_bin_prostate_cancer == TRUE & input$cov_cat_sex=="Female")

# Remove rows that are TRUE for at least one rule

input_QA <- input %>% filter(rule1 == FALSE & rule2 == FALSE & rule3 == FALSE & rule4 == FALSE & rule5 == FALSE & rule6 == FALSE) 

print("QA filtering performed successfully")

# Produce QA summary

QA_summary <- data.frame(matrix(ncol = 2))
colnames(QA_summary) <- c('Rule', 'N rule TRUE')
QA_summary[1:7, 1] <- c("Rule 1", "Rule 2", "Rule 3", "Rule 4", "Rule 5", "Rule 6", "Total excluded from QA")
QA_summary[1,2]=nrow(input%>%filter(rule1==T))
QA_summary[2,2]=nrow(input%>%filter(rule2==T))
QA_summary[3,2]=nrow(input%>%filter(rule3==T))
QA_summary[4,2]=nrow(input%>%filter(rule4==T))
QA_summary[5,2]=nrow(input%>%filter(rule5==T))
QA_summary[6,2]=nrow(input%>%filter(rule6==T))
QA_summary[7,2]=nrow(input)-nrow(input_QA)

#Save QA summary as .csv

write.csv(QA_summary, file = file.path("output/review/descriptives", "QA_summary.csv") , row.names=F)

print("QA summary saved successfully")

# Remove QA variables from dataset

input <- input_QA %>%
  select(-c(rule1,rule2,rule3,rule4,rule5,rule6,
            qa_num_birth_year, qa_bin_pregnancy, qa_bin_prostate_cancer))

# Save meta data (after QA rules have been applied)

describe_vars <- tidyselect::vars_select(names(input), contains(c('_cat_', 'cov_bin','cov_cat','qa_bin','exp_cat','vax_cat', 'step_'), ignore.case = TRUE))
describe_vars_num <- tidyselect::vars_select(names(input), contains(c('_num'), ignore.case = TRUE))
meta_data_factors <- lapply(input[,describe_vars], table)
meta_data_factors_num <- lapply(input[,describe_vars_num], summary)
meta_data_factors <- c(meta_data_factors, meta_data_factors_num)
sink(file = file.path("output/not-for-review/", "meta_data_factors.csv"))
print(meta_data_factors)
sink()

print("Meta data saved successfully")

#########################################
# 3. Apply exclusion/inclusion criteria #
#########################################

# Define the cohort flow

cohort_flow <- data.frame(N = numeric(),
                          N_removed = numeric(),
                          Description = character(),
                          stringsAsFactors = FALSE)
cohort_flow[nrow(cohort_flow)+1,] <- c(as.numeric(as.numeric(nrow(input)) + as.numeric(QA_summary[7,2])), 0, "Study defined sample size before QA checks")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(QA_summary[7,2]) ,"Study defined sample size after QA checks")

#---------------------------------------#
# Apply criteria listed in the protocol #
#---------------------------------------#

# Inclusion criteria 1: Alive on the first day of follow up

input <- input %>% filter(index_date < death_date)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[2,1]) - nrow(input),"Criteria 1 (Inclusion): Alive on the first day of follow up") # Feed into the cohort flow

# Inclusion criteria 2: Known age between 18 and 110 on 01/01/2020 

input <- input %>% 
  filter(cov_num_age >= 18 & cov_num_age <= 110)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[3,1]) - nrow(input), "Criteria 2 (Inclusion): Known age between 18 and 110 on 01/01/2020") # Feed into the cohort flow

# Inclusion criteria 3: Known sex

input <- input %>% 
  filter(cov_cat_sex == "Male" | cov_cat_sex == "Female")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[4,1]) - nrow(input), "Criteria 3 (Inclusion): Known sex")

# Inclusion criteria 4: Known deprivation 

input <- input %>% 
  drop_na(cov_cat_deprivation)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[5,1]) - nrow(input), "Criteria 4 (Inclusion): Known deprivation")

# Inclusion criteria 5: Registered in an English GP with TPP software for at least 6 months prior to the study start date
# NOTE: Dealt with in Study definition
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[6,1]) - nrow(input), "Criteria 5 (Inclusion): Registered in an English GP with TPP software for at least 6 months prior to the study start date")

#Inclusion criteria 6: Known region
input <- input %>% mutate(cov_cat_region = as.character(cov_cat_region)) %>%
  filter(cov_cat_region != "Missing")%>%
  mutate(cov_cat_region = as.factor(cov_cat_region))

input$cov_cat_region <- relevel(input$cov_cat_region, ref = "East")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[7,1]) - nrow(input), "Criteria 6 (Inclusion): Known region")

#Inclusion criteria 7: No known history of COVID-19 prior to the start of follow-up
input=input%>%filter(sub_bin_covid19_confirmed_history == FALSE)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[8,1]) - nrow(input), "Criteria 7 (Inclusion): No known history of COVID-19 prior to study start date")

#--------------------------#
# 3.e. Generate histograms #
#--------------------------#
# generate histograms for numerical variables

numeric_vars <- input %>% dplyr::select(contains("_num")) 

svglite::svglite(file = file.path("output/not-for-review/", paste0("numeric_histograms.svg")))
g <- ggplot(gather(numeric_vars), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
print(g)
dev.off()

print("Histograms saved successfully")

##############
# 4. Outputs #
##############

# Create csv file 

write.csv(cohort_flow, file = file.path("output/review/descriptives", paste0("cohort_flow.csv")), row.names=F)

# Create the final stage 1 dataset 

saveRDS(input, file = file.path("output", paste0("input_stage1.rds")))

print("Cohort flow and stage 1 saved successfully")

