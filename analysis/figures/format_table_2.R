library(dplyr)
library(janitor)
# Load data --------------------------------------------------------------------

results_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/OS-outputs/OS-output-9june2022/"
output_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/OS-outputs/OS-output-9june2022/figures/"

group <- c("diabetes", "diabetes_gestational")

for(i in group){
  table2_raw <- read.csv(paste0(results_dir,"table2_", i, ".csv"))
  
  table2_raw <- table2_raw %>% select(subgroup,event,unexposed_event_count, post_exposure_event_count)%>%
    filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
  
  table2_pre_expo <- table2_raw %>% select(subgroup, event, unexposed_event_count)
  table2_pre_expo$period <- "unexposed"
  table2_pre_expo <- table2_pre_expo %>% dplyr::rename(event_count = unexposed_event_count)
  
  table2_post_expo <- table2_raw %>% select(subgroup, event, post_exposure_event_count)
  table2_post_expo$period <- "post_expo"
  table2_post_expo <- table2_post_expo %>% dplyr::rename(event_count = post_exposure_event_count)
  
  table2 <- rbind(table2_pre_expo,table2_post_expo)
  
  rm(table2_pre_expo,table2_post_expo,table2_raw)
  
  
  table2$period <- ifelse(table2$period=="unexposed","No COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_hospitalised","After hospitalised COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_non_hospitalised","After non-hospitalised COVID-19",table2$period)
  
  table2[,"subgroup"] <- NULL
  table2 <- table2[!duplicated(table2), ]
  
  # Make columns for exposure time -----------------------------------------------
  
  table2 <- tidyr::pivot_wider(table2, names_from = "period", values_from = "event_count")
  table2 <- clean_names(table2)
  
  #Add totals columm
  table2 <- table2 %>% mutate_all(na_if, "[Redacted]")
  cols <- c("no_covid_19","after_hospitalised_covid_19","after_non_hospitalised_covid_19")
  table2[cols] <- lapply(table2[cols], as.numeric)
  table2$Total <- rowSums(table2[,c(2,3,4)], na.rm = TRUE)

  # Tidy event labels ------------------------------------------------------------
  active_analyses <- readr::read_rds("lib/active_analyses.RDS")
  
  table2 <- table2 %>% left_join(active_analyses %>% select(outcome_variable,outcome), by=c("event"="outcome_variable"))
  table2$event <- NULL
  
  # Re-order rows and add empty rows ---------------------------------------------------------------
  table2 <- table2[,c(5,1,2,3,4)] 
  table2 <- table2  %>% dplyr::rename(Outcome = outcome)
  
  # table2 <- rbind(table2,c("Other vascular events",rep(NA,4)))
  # table2 <- rbind(table2,c("Arterial thrombosis events",rep(NA,4)))
  # table2 <- rbind(table2,c("Venous thromboembolism events",rep(NA,4)))
  
  # table2$Outcome <- factor(table2$Outcome, levels=c("Arterial thrombosis events",
  #                                                   "Arterial thrombosis event",
  #                                                   "Acute myocardial infarction",
  #                                                   "Ischaemic stroke",
  #                                                   "Venous thromboembolism events",
  #                                                   "Venous thrombosis event",
  #                                                   "Pulmonary embolism",
  #                                                   "Deep vein thrombosis",
  #                                                   "Other vascular events",
  #                                                   "Heart failure",
  #                                                   "Angina",
  #                                                   "Transient ischaemic attack",
  #                                                   "Subarachnoid haemorrhage and haemorrhagic stroke")) 
  # 
  # table2 <- table2[order(table2$Outcome),]
  
  # Save -------------------------------------------------------------------------
  
  data.table::fwrite(table2,paste0(output_dir,"formatted_main_table_2_",i,".csv"))
  
}
