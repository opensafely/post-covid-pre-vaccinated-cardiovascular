## =============================================================================
## Project:     Post covid unvaccinated project
##
## Purpose:  Apply stage 1. Data cleaning
##  - Prepare variables
##  - Apply QA rules
##  - Apply inclusion exclusion criteria
##  - Create cleaned dataset
## 
## Authors: Kurt Taylor
## Reviewer: Rochelle Knight
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

stage2 <- function(group) {
  # Input dataset
  input <-read_rds("output/input.rds")
  
  input <- input %>%
    mutate(index_date = start_date,
           end_date = end_date)
  
  # NOTE: no censoring of end date for death/event at this stage
  
  ######################################################
  # 1. Prepare all variables (re-factoring, re-typing) # 
  ######################################################
  
  # Get the names of variables which are factors
  factor_names <- tidyselect::vars_select(names(input), contains(c('_cat_'), ignore.case = TRUE))
  
  input <- input %>%
    # handle missing smoking and BMI values 
    mutate(cov_cat_smoking_status = replace_na(cov_cat_smoking_status, "M"),
           # cov_cat_bmi_groups = replace_na(cov_cat_bmi_groups, "Missing"),
           # Replace " " with "_"     
           cov_cat_region = gsub(" ", "_", cov_cat_region)) %>% 
    # handle missing region values
    mutate(cov_cat_region = replace_na(cov_cat_region, "Missing")) %>%
    # Set the variables that should be factor variables as factor
    mutate(across(any_of(factor_names), function(x) factor(x, ordered = FALSE))) %>%
    # Re-code vars and specify references
    mutate(sub_cat_covid19_hospital = ordered(sub_cat_covid19_hospital, levels = c("non_hospitalised","hospitalised","no_infection")),
           # ethnicity
           cov_cat_ethnicity = case_when(cov_cat_ethnicity == 0 ~ "Missing",
                                         cov_cat_ethnicity == 1 ~ "White",
                                         cov_cat_ethnicity == 2 ~ "Mixed",
                                         cov_cat_ethnicity == 3 ~ "South Asian",
                                         cov_cat_ethnicity == 4 ~ "Black",
                                         cov_cat_ethnicity == 5 ~ "Other")) %>%
    mutate(cov_cat_ethnicity = ordered(cov_cat_ethnicity, levels = c("White","Mixed","South Asian","Black","Other","Missing")),
           # smoking 
           cov_cat_smoking_status = case_when(cov_cat_smoking_status == "E" ~ "Ever smoker",
                                              cov_cat_smoking_status == "M" ~ "Missing",
                                              cov_cat_smoking_status == "N" ~ "Never smoker",
                                              cov_cat_smoking_status == "S" ~ "Current smoker")) %>%
    mutate(cov_cat_smoking_status = ordered(cov_cat_smoking_status, levels = c("Never smoker","Ever smoker","Current smoker","Missing")),
           # BMI
           cov_cat_bmi_groups = ordered(cov_cat_bmi_groups, levels = c("Healthy_weight", "Underweight", "Overweight", "Obese", "Missing")),
           # region
           cov_cat_region = relevel(factor(cov_cat_region), ref = "London"),
           # sex
           cov_cat_sex = case_when(cov_cat_sex == "F" ~ "Female",
                                   cov_cat_sex == "M" ~ "Male")) %>%
    mutate(cov_cat_sex = relevel(factor(cov_cat_sex), ref = "Female"),
           # cat jcvi group
           vax_cat_jcvi_group = ordered(vax_cat_jcvi_group, levels = c("12","11","10","09","08","07","06","05","04","03","02","01","99"))) %>%
    # deprivation: First - most deprived; fifth -least deprived
    mutate(cov_cat_deprivation = ifelse(cov_cat_deprivation == 1 | cov_cat_deprivation == 2, "1-2 (most deprived)",
                                        ifelse(cov_cat_deprivation == 3 | cov_cat_deprivation == 4, "3-4",
                                               ifelse(cov_cat_deprivation == 5 | cov_cat_deprivation == 6, "5-6",
                                                      ifelse(cov_cat_deprivation == 7 | cov_cat_deprivation == 8, "7-8",
                                                             ifelse(cov_cat_deprivation == 9 | cov_cat_deprivation == 10, "9-10 (least deprived)", NA)))))) %>%
    mutate_at(vars(cov_cat_deprivation), as.factor) %>%
    mutate(cov_cat_deprivation = ordered(cov_cat_deprivation, levels = c("1-2 (most deprived)","3-4","5-6","7-8","9-10 (least deprived)")))
  
  print("Variable preparation performed successfully")
  
  #####################
  # 2. Apply QA rules #
  #####################
  
  input <- input %>%
    # Rule 1: Year of birth is after year of death or patient only has year of death
    mutate(rule1 = ifelse((qa_num_birth_year > (format(death_date, format="%Y")) &
                             ! is.na(qa_num_birth_year) & 
                             ! is.na(death_date) | is.na(qa_num_birth_year) &
                             ! is.na(death_date)), TRUE, FALSE),
           # Rule 2: Year of birth predates NHS established year or year of birth exceeds current date       
           rule2 = ifelse((qa_num_birth_year < 1793 | (qa_num_birth_year > format(Sys.Date(),"%Y"))) & ! is.na(qa_num_birth_year), TRUE, FALSE),
           # Rule 3: Date of death is NULL or invalid (on or before 1/1/1900 or after current date)     
           rule3 = ifelse((death_date <= "1900-01-01" | death_date > format(Sys.Date(),"%Y-%m-%d")) & ! is.na(death_date), TRUE, FALSE),
           # Rule 4: Pregnancy/birth codes for men
           rule4 = ifelse(qa_bin_pregnancy == TRUE & cov_cat_sex=="Male", TRUE, FALSE),
           # Rule 5: HRT or COCP meds for men
           rule5 = ifelse(cov_cat_sex=="Male" & cov_bin_hormone_replacement_therapy==TRUE | cov_cat_sex=="Male" & cov_bin_combined_oral_contraceptive_pill == TRUE, TRUE, FALSE),
           # Rule 6: Prostate cancer codes for women
           rule6 = ifelse(qa_bin_prostate_cancer == TRUE & cov_cat_sex=="Female", TRUE, FALSE))
  # should we add a rule to remove those with COVID history prior to 2020?
  
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
  
  input <- input %>%
    mutate(start_alive = ifelse(death_date < index_date, 0, 1)) %>%
    mutate(start_alive = replace_na(start_alive, 1)) %>%
    filter(start_alive == 1)
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
  
  input$cov_cat_region <- relevel(input$cov_cat_region, ref = "London")
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[7,1]) - nrow(input), "Criteria 6 (Inclusion): Known region")
  
  # Exclusion criteria: SARS-CoV-2 infection recorded prior to the start of follow-up
  # No COVID cases prior to 1st Jan 2020
  
  #--------------------------#
  # 3.e. Generate histograms #
  #--------------------------#
  # generate histograms for numerical variables
  
  numeric_vars <- input %>% dplyr::select(contains("_num")) %>%
    mutate(cov_num_tc_hdl_ratio = replace(cov_num_tc_hdl_ratio, which(cov_num_tc_hdl_ratio < 0), NA)) # assign negative ages to NA - should only matter for dummy data)
  
  svglite::svglite(file = file.path("output/not-for-review/", paste0("numeric_histograms_", group, ".svg")))
  g <- ggplot(gather(numeric_vars), aes(value)) + 
    geom_histogram(bins = 10) + 
    facet_wrap(~key, scales = 'free_x')
  print(g)
  dev.off()
  
  print("Histograms saved successfully")
  #--------------------------------------------#
  # Apply outcome specific exclusions criteria #
  #--------------------------------------------#
  
  if (group == "diabetes"){
    # Exclude individuals with a recorded diagnosis of diabetes prior to 1st January 2020 
    input <- input %>% 
      filter(! out_date_t1dm < index_date | is.na(out_date_t1dm)) %>%
      filter(! out_date_t2dm < index_date | is.na(out_date_t2dm)) %>%
      filter(! out_date_otherdm < index_date | is.na(out_date_otherdm))
    cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),as.numeric(cohort_flow[8,1]) - nrow(input), "Diabetes specific criteria: Remove those with diabetes prior to study start date")
    
  } else if (group == "diabetes_prediabetes"){
    # Restrict to only those with a history of pre diabetes
    input <- input %>% 
      filter(! out_date_t1dm < index_date | is.na(out_date_t1dm)) %>%
      filter(! out_date_t2dm < index_date | is.na(out_date_t2dm)) %>%
      filter(! out_date_otherdm < index_date | is.na(out_date_otherdm))  %>%
      filter(cov_bin_prediabetes == TRUE) %>%
      # change name of t2dm variable to avoid duplicated cox actions
      dplyr::rename(out_date_t2dm_pd = out_date_t2dm)
    cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),as.numeric(cohort_flow[8,1]) - nrow(input), "Diabetes specific criteria: Remove those with diabetes prior to study start date AND restrict to those with a history of prediabetes")
    
  } else if (group == "diabetes_no_prediabetes"){
    # Restrict to only those with NO history of pre diabetes
    input <- input %>% 
      filter(! out_date_t1dm < index_date | is.na(out_date_t1dm)) %>%
      filter(! out_date_t2dm < index_date | is.na(out_date_t2dm)) %>%
      filter(! out_date_otherdm < index_date | is.na(out_date_otherdm)) %>%
      filter(cov_bin_prediabetes == FALSE) %>%
      # change name of t2dm variable to avoid duplicated cox actions
      dplyr::rename(out_date_t2dm_pd_no = out_date_t2dm)
    cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),as.numeric(cohort_flow[8,1]) - nrow(input), "Diabetes specific criteria: Remove those with diabetes prior to study start date AND restrict to those with NO history of prediabetes")
    
  } else if (group == "diabetes_obesity"){
    # Restrict to only those with a history of pre diabetes
    input <- input %>% 
      filter(! out_date_t1dm < index_date | is.na(out_date_t1dm)) %>%
      filter(! out_date_t2dm < index_date | is.na(out_date_t2dm)) %>%
      filter(! out_date_otherdm < index_date | is.na(out_date_otherdm)) %>%
      filter(cov_cat_bmi_groups == "Obese") %>%
      # change name of t2dm variable to avoid duplicated cox actions
      dplyr::rename(out_date_t2dm_obes = out_date_t2dm)
    cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),as.numeric(cohort_flow[8,1]) - nrow(input), "Diabetes specific criteria: Remove those with diabetes prior to study start date AND restrict to those with obesity")
    
  } else if (group == "diabetes_no_obesity"){
    # Restrict to only those with NO history of pre diabetes
    input <- input %>% 
      filter(! out_date_t1dm < index_date | is.na(out_date_t1dm)) %>%
      filter(! out_date_t2dm < index_date | is.na(out_date_t2dm)) %>%
      filter(! out_date_otherdm < index_date | is.na(out_date_otherdm)) %>%
      filter(cov_cat_bmi_groups != "Obese") %>%
      # change name of t2dm variable to avoid duplicated cox actions
      dplyr::rename(out_date_t2dm_obes_no = out_date_t2dm)
    cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),as.numeric(cohort_flow[8,1]) - nrow(input), "Diabetes specific criteria: Remove those with diabetes prior to study start date AND restrict to those with NO obesity")
    
  } else if (group == "diabetes_gestational"){
    # Exclude men from gestational diabetes analysis
    input <- input %>% 
      filter(! out_date_t1dm < index_date | is.na(out_date_t1dm)) %>%
      filter(! out_date_t2dm < index_date | is.na(out_date_t2dm)) %>%
      filter(! out_date_otherdm < index_date | is.na(out_date_otherdm)) %>%
      filter(cov_cat_sex == "Female")
    cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[8,1]) - nrow(input), "Gestational diabetes: The study population will be restricted to women.")
    
  } else if (group == "mental_health"){
    # Mental health analyses exclusion criteria
    input <- input %>% 
      filter()
    cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input), as.numeric(cohort_flow[8,1]) - nrow(input), "Mental health: .")
  }
  
  ##############
  # 4. Outputs #
  ##############
  
  # Create csv file 
  
  write.csv(cohort_flow, file = file.path("output/review/descriptives", paste0("cohort_flow_",group,".csv")), row.names=F)
  
  # Create the final stage 1 dataset 
  
  saveRDS(input, file = file.path("output", paste0("input_stage1_",group,".rds")))
  
  print("Cohort flow and stage 1 saved successfully")
  
  # END
}

# Run function using outcome group
active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>% filter(active==TRUE)
group <- unique(active_analyses$outcome_group)

for(i in group){
  stage2(i)
}