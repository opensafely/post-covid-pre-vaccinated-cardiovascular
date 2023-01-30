library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
#library(dplyr)

###########################
# Load information to use #
###########################

## defaults ----
defaults_list <- list(
  version = "3.0",
  expectations= list(population_size=100000L)
)

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses_table <- subset(active_analyses, active_analyses$active =="TRUE")
outcomes_model <- active_analyses_table$outcome_variable %>% str_replace("out_date_", "")
cohort_to_run <- c("pre_vaccination")
analyses <- c("main", "subgroups")
analyses_to_run_stata <- read.csv("lib/analyses_to_run_in_stata.csv")
analyses_to_run_stata$subgroup <- ifelse(analyses_to_run_stata$subgroup=="hospitalised","covid_pheno_hospitalised",analyses_to_run_stata$subgroup)
analyses_to_run_stata$subgroup <- ifelse(analyses_to_run_stata$subgroup=="non_hospitalised","covid_pheno_non_hospitalised",analyses_to_run_stata$subgroup)
analyses_to_run_stata <- analyses_to_run_stata %>% filter(cohort %in% cohort_to_run
                                                          & time_periods == "reduced")



# create action functions ----

############################
## generic action function #
############################
action <- function(
    name,
    run,
    dummy_data_file=NULL,
    arguments=NULL,
    needs=NULL,
    highly_sensitive=NULL,
    moderately_sensitive=NULL
){
  
  outputs <- list(
    moderately_sensitive = moderately_sensitive,
    highly_sensitive = highly_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL
  
  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    dummy_data_file = dummy_data_file,
    needs = needs,
    outputs = outputs
  )
  action[sapply(action, is.null)] <- NULL
  
  action_list <- list(name = action)
  names(action_list) <- name
  
  action_list
}


## create comment function ----
comment <- function(...){
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}


## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt){
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}


#################################################
## Function for typical actions to analyse data #
#################################################
# Updated to a typical action running Cox models for one outcome

apply_model_function <- function(outcome,cohort){
  splice(
    comment(glue("Apply cox model for {outcome}")),
    action(
      name = glue("Analysis_cox_{outcome}"),
      run = "r:latest analysis/model/01_cox_pipeline.R",
      arguments = c(outcome),
      needs = list("stage1_data_cleaning", "stage1_end_date_table"),
      moderately_sensitive = list(
        analyses_not_run = glue("output/review/model/analyses_not_run_{outcome}_pre_vaccination.csv"),
        compiled_hrs_csv = glue("output/review/model/suppressed_compiled_HR_results_{outcome}_pre_vaccination.csv"),
        compiled_hrs_csv_to_release = glue("output/review/model/suppressed_compiled_HR_results_{outcome}_pre_vaccination_to_release.csv"),
        compiled_event_counts_csv = glue("output/review/model/suppressed_compiled_event_counts_{outcome}_pre_vaccination.csv"),
        compiled_event_counts_csv_non_supressed = glue("output/review/model/compiled_event_counts_{outcome}_pre_vaccination.csv"),
        describe_data_surv = glue("output/not-for-review/describe_{outcome}_*_time_periods.txt")
      ),
      highly_sensitive = list(
        sampled_data = glue("output/input_sampled_data_{outcome}_*_{cohort}_*_time_periods.csv"),
        cox_input_data = glue("output/input_{outcome}_*_{cohort}_*_time_periods.csv")
      )
    )
  )
}

stata_actions <- function(outcome, cohort, subgroup, time_periods, day0, extf){
  splice(
    action(
      name = glue("stata_cox_model_{outcome}_{subgroup}_{cohort}_{time_periods}_day0_{day0}_extf_{extf}"),
      run = glue("stata-mp:latest analysis/cox_model.do input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods {day0} {extf}"),
      needs = list(glue("Analysis_cox_{outcome}")),
      moderately_sensitive = list(
        medianfup = glue("output/input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods_stata_median_fup_day0{day0}_extf{extf}.csv"),
        stata_output = glue("output/input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods_cox_model_day0{day0}_extf{extf}.txt")
      )
    )
  )
}

##########################################################
## Define and combine all actions into a list of actions #
##########################################################
actions_list <- splice(
  
  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create_project_actions.R",
          "Edit and run create_project_actions.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),
  
  #comment("Generate vaccination eligibility information"),
  action(
    name = glue("vax_eligibility_inputs"),
    run = "r:latest analysis/vax_eligibility_inputs.R",
    highly_sensitive = list(
      vax_study_dates_json = glue("output/vax_study_dates.json"),
      vax_jcvi_groups= glue("output/vax_jcvi_groups.csv"),
      vax_eligible_dates= ("output/vax_eligible_dates.csv")
    )
  ),
  
  #comment("Generate dummy data for study_definition"),
  action(
    name = "generate_study_population",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition --output-format feather",
    needs = list("vax_eligibility_inputs"),
    highly_sensitive = list(
      cohort = glue("output/input.feather")
    )
  ),
  
  #comment("Preprocess data"),
  action(
    name = "preprocess_data",
    run = "r:latest analysis/preprocess/preprocess_data.R",
    needs = list("generate_study_population"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_stage0.txt")),
    highly_sensitive = list(
      cohort = glue("output/input.rds"),
      venn = glue("output/venn.rds")
    )
  ), 
  
  #comment("Stage 1 - Data cleaning"),
  action(
    name = "stage1_data_cleaning",
    run = "r:latest analysis/preprocess/Stage1_data_cleaning.R",
    needs = list("preprocess_data"),
    moderately_sensitive = list(
      QA_rules = glue("output/review/descriptives/QA_summary.csv"),
      refactoring = glue("output/not-for-review/meta_data_factors.csv"),
      IE_criteria = glue("output/review/descriptives/cohort_flow.csv"),
      histograms = glue("output/not-for-review/numeric_histograms.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_stage1.rds")
    )
  ),
  
  #comment("Stage 1 - End date table"),
  action(
    name = "stage1_end_date_table",
    run = "r:latest analysis/preprocess/create_follow_up_end_date.R",
    needs = list("preprocess_data","stage1_data_cleaning"),
    highly_sensitive = list(
      end_date_table = glue("output/follow_up_end_dates.rds")
    )
  ),
  
  #comment("Stage 2 - Missing - Table 1"),
  action(
    name = "stage2_missing_table1",
    run = "r:latest analysis/descriptives/Stage2_Missing_Table1.R",
    needs = list("stage1_data_cleaning"),
    moderately_sensitive = list(
      Missing_RangeChecks = glue("output/not-for-review/Check_missing_range.csv"),
      DateChecks = glue("output/not-for-review/Check_dates_range.csv"),
      Descriptive_Table = glue("output/review/descriptives/Table1_pre_vaccination_cvd.csv")
    )
  ),
  
  #comment("Table 2)
  action(
    name = "table_2_original_follow_up_any_position_events",
    run = "r:latest analysis/descriptives/table_2.R",
    arguments = list("original_follow_up","any_position"),
    needs = list("stage1_data_cleaning","stage1_end_date_table"),
    moderately_sensitive = list(
      table_2 = "output/review/descriptives/table2_pre_vaccination_original_follow_up_any_position_events.csv"
      
    )
  ),
  
  action(
    name = "table_2_extended_follow_up_any_position_events",
    run = "r:latest analysis/descriptives/table_2.R",
    arguments = list("extended_follow_up","any_position"),
    needs = list("stage1_data_cleaning","stage1_end_date_table"),
    moderately_sensitive = list(
      table_2 = "output/review/descriptives/table2_pre_vaccination_extended_follow_up_any_position_events.csv"
      
    )
  ),
  
  #comment("Table 2)
  action(
    name = "table_2_original_follow_up_primary_position_events",
    run = "r:latest analysis/descriptives/table_2.R",
    arguments = list("original_follow_up","primary_position"),
    needs = list("stage1_data_cleaning","stage1_end_date_table"),
    moderately_sensitive = list(
      table_2 = "output/review/descriptives/table2_pre_vaccination_original_follow_up_primary_position_events.csv"
      
    )
  ),
  
  action(
    name = "table_2_extended_follow_up_primary_position_events",
    run = "r:latest analysis/descriptives/table_2.R",
    arguments = list("extended_follow_up","primary_position"),
    needs = list("stage1_data_cleaning","stage1_end_date_table"),
    moderately_sensitive = list(
      table_2 = "output/review/descriptives/table2_pre_vaccination_extended_follow_up_primary_position_events.csv"
      
    )
  ),
  
  #comment("Stage 4 - Venn diagrams"),
  action(
    name = "venn_diagram",
    run = "r:latest analysis/descriptives/venn_diagram.R",
    needs = list("preprocess_data","stage1_data_cleaning","stage1_end_date_table"),
    moderately_sensitive = list(
      venn_diagram = glue("output/review/venn-diagrams/venn_diagram_*"))
  ),
  
  action(
    name = "days_to_event_histogram_original_follow_up",
    run = "r:latest analysis/descriptives/histogram_data_post_exposure_days_to_event.R",
    arguments = "original_follow_up",
    needs =  list("stage1_data_cleaning","stage1_end_date_table"),
    moderately_sensitive = list(
      histogram_data = "output/review/descriptives/histogram_data_pre_vaccination_original_follow_up.csv")
  ),
  
  action(
    name = "days_to_event_histogram_extended_follow_up",
    run = "r:latest analysis/descriptives/histogram_data_post_exposure_days_to_event.R",
    arguments = "extended_follow_up",
    needs =  list("stage1_data_cleaning","stage1_end_date_table"),
    moderately_sensitive = list(
      histogram_data = "output/review/descriptives/histogram_data_pre_vaccination_extended_follow_up.csv")
  ),
    
  # action(
  #   name = "event_counts_by_time_period",
  #   run = "r:latest analysis/descriptives/event_counts_by_time_period.R",
  #   needs = list("stage1_data_cleaning", "stage1_end_date_table"),
  #   moderately_sensitive = list(
  #     event_counts = "output/review/descriptives/event_counts_by_time_period_pre_vaccination.csv")
  # ),
  
  #comment("Stage 5 - Apply models"),
  splice(
    # over outcomes
    unlist(lapply(outcomes_model, function(x) splice(unlist(lapply(cohort_to_run, function(y) apply_model_function(outcome = x, cohort = y)), recursive = FALSE))
    ),recursive = FALSE)),

  #Stata cox model
  splice(unlist(lapply(1:nrow(analyses_to_run_stata),
                       function(i) stata_actions(outcome = analyses_to_run_stata[i, "outcome"],
                                                 subgroup = analyses_to_run_stata[i, "subgroup"],
                                                 cohort = analyses_to_run_stata[i, "cohort"],
                                                 time_periods = analyses_to_run_stata[i, "time_periods"],
                                                 day0 = analyses_to_run_stata[i, "day0"],
                                                 extf = analyses_to_run_stata[i, "extf"])),
                recursive = FALSE)),

  #comment("Format Stata output"),
  action(
    name = "format_stata_output",
    run = "r:latest analysis/format_stata_output.R",
    needs = as.list(paste0("stata_cox_model_",analyses_to_run_stata$outcome,"_",analyses_to_run_stata$subgroup,"_",analyses_to_run_stata$cohort,"_",analyses_to_run_stata$time_periods,"_day0_",analyses_to_run_stata$day0,"_extf_",analyses_to_run_stata$extf)),
    moderately_sensitive = list(
      stata_output = "output/stata_output.csv")
  ),
  
  action(
    name = "format_R_output",
    run = "r:latest analysis/model/07_combine_HRs_to_one_file.R",
    needs = paste0("Analysis_cox_",outcomes_model),
    moderately_sensitive = list(
      R_output = "output/review/model/R_HR_output.csv",
      R_event_counts = "output/review/model/R_event_count_output.csv")
  )
  
)


## combine everything ----
project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

#####################################################################################
## convert list to yaml, reformat comments and white space, and output a .yaml file #
#####################################################################################
as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")
