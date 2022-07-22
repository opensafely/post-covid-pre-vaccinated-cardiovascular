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
cohort_to_run <- c("")

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
apply_model_function <- function(outcome){
  splice(
    comment(glue("Apply cox model for {outcome}")),
    action(
      name = glue("Analysis_cox_{outcome}"),
      run = "r:latest analysis/model/01_cox_pipeline.R",
      arguments = c(outcome),
      needs = list("stage1_data_cleaning", glue("stage1_end_date_table")),
      moderately_sensitive = list(
        analyses_not_run = glue("output/review/model/analyses_not_run_{outcome}.csv"),
        compiled_hrs_csv = glue("output/review/model/suppressed_compiled_HR_results_{outcome}.csv"),
        compiled_hrs_csv_to_release = glue("output/review/model/suppressed_compiled_HR_results_{outcome}_to_release.csv"),
        compiled_event_counts_csv = glue("output/review/model/suppressed_compiled_event_counts_{outcome}.csv")
      ),
      highly_sensitive = list(
        compiled_hrs = glue("output/review/model/compiled_HR_results_{outcome}.csv"),
        compiled_event_counts = glue("output/review/model/compiled_event_counts_{outcome}.csv")
      )
    )
  )
}

table2 <- function(cohort){
  splice(
    comment(glue("Stage 4 - Table 2")),
    action(
      name = glue("stage4_table_2"),
      run = "r:latest analysis/descriptives/table_2.R",
      arguments = c(cohort),
      needs = list("stage1_data_cleaning",glue("stage1_end_date_table")),
      moderately_sensitive = list(
        input_table_2 = glue("output/review/descriptives/table2*.csv")
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
      IE_criteria = glue("output/review/descriptives/cohort_flow*.csv"),
      histograms = glue("output/not-for-review/numeric_histograms_*.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_stage1*.rds")
    )
  ),
  
  #comment("Stage 1 - End date table"),
  action(
    name = "stage1_end_date_table",
    run = "r:latest analysis/preprocess/create_follow_up_end_date.R",
    needs = list("preprocess_data","stage1_data_cleaning"),
    highly_sensitive = list(
      end_date_table = glue("output/follow_up_end_dates*.rds")
    )
  ),

  #comment("Stage 2 - Missing - Table 1"),
  action(
    name = "stage2_missing_table1",
    run = "r:latest analysis/descriptives/Stage2_Missing_Table1.R",
    needs = list("stage1_data_cleaning"),
    moderately_sensitive = list(
      Missing_RangeChecks = glue("output/not-for-review/Check_missing_range*.csv"),
      DateChecks = glue("output/not-for-review/Check_dates_range*.csv"),
      Descriptive_Table = glue("output/review/descriptives/Table1*.csv")
    )
  ),

  #comment("Stage 3 - Diabetes flow"),  

  action(
    name = "stage3_diabetes_flow",
    run = "r:latest analysis/descriptives/diabetes_flowchart.R",
    needs = list("stage1_data_cleaning"),
    moderately_sensitive = list(
      flow_df = glue("output/review/figure-data/diabetes_flow_values*.csv")
      # flow_fig = glue("output/diabetes_flow.png"),
    ),
  ),
  
  #comment("Stage 4 - Create input for table2"),
  splice(
    # over outcomes
    unlist(lapply(cohort_to_run, function(x) table2(cohort = x)), recursive = FALSE)
  ),
  
  #comment("Stage 4 - Venn diagrams"),
  action(
    name = "stage4_venn_diagram",
    run = "r:latest analysis/descriptives/venn_diagram.R",
    needs = list("preprocess_data", "stage1_data_cleaning", "stage1_end_date_table"),
    moderately_sensitive = list(
      venn_diagram = glue("output/review/venn-diagrams/venn_diagram_*")
    )
  ),

  #comment("Stage 5 - Apply models"),
  splice(
    # over outcomes
    unlist(lapply(outcomes_model, function(x) splice(unlist(lapply(cohort_to_run, function(y) apply_model_function(outcome = x)), recursive = FALSE))
      ),recursive = FALSE)))
  

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
