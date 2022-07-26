# Post-Covid-Events Cox Model Scripts

## Points to note for all scripts below:
* **outputs**: See project yaml for an up to date list of outputs generated by these scripts.
* If working with multiple cohorts and/or outcomes, the scripts below are structured as a large function so that the function can then run multiple times for each cohort/outcome using specified command arguments at the top of the script. 
*  These scripts are based on ones written by Samantha Ip, see the following repo's for original scripts: https://github.com/BHFDSC/CCU002_01 & https://github.com/BHFDSC/CCU002_03

## 01_cox_pipeline.R
* Calls all  other cox scripts and runs them from all from here.
* Reads in the arguments for which cohort and outcome event to run

## 02_01_analyses_to_run.R
* Reads in the active_analyses.rds file which contains what analyses needs to be run for each outcome
* Creates a dataframe (analyses_to_run) for the outcome of interest with the analyses that needs to be run which can then be read into a later function.

## 02_02_cox_load_data.R
* Loads the input data from stage 1
* Specifies all the main parameters i.e. age breaks, study start and end date, time periods for HRs
* Creates an empty dataframe (analyses_not_run) that analyses that were unable to run get added to i.e. if there were too few post-exposure events to meet the threshold.

## 02_03_cox_timepoint_param.R
* For each analyses in the analyses_to_run df, this script calculates the number of post-exposure events and determines which time periods (normal or reduced) it should be run using. 
* All analyses are run using the reduced time periods and if there are enough post-exposure events we additionally run using the normal time periods.
* If there are between 50 to 400 (>=50 & <400), we use the reduced time periods. If there are 400 or more (>=400) post-exposure events we use the normal time periods (in addition to running the reduced time periods as well).
* If there are less than 50 (<50) post-exposure events we do not run the analyses 

## 03_01_cox_subgrouping.R
* This is the first script to run in the main cox function. It uses the inputs from the analyses_to_run dataframe and subgroups the data accordingly.
* For the hospitalised/non-hospitalised analyses, for the phenotoype **not of interest** we censor patients on the day before exposure so that we have all their non-exposed follow-up.
* All filtering of patients/removal of dates outside of follow up is done by the end of this script.

## 04_01_(a)_cox_fit_model.R
* Script contains two functions: fit_model_reducedcovariates and coxfit
* *fit_model_reducedcovariates* calls the script 04_01_(b)_cox_format_survival_data.R which formats the survival dataset
* Left joins the covariates if the maximally adjusted model is being run
* Collapses categorical covariates/decides what covariates to fit for the saved stata dataset
* Calls the coxfit function to run the cox model
* *coxfit* fits the cox model to the dataset
* Removes binary covariates with less than 3 post-exposure events at either level; collapses categorical covariates if any of levels have less than 3 post-exposure events 
* Fits 3 cox models: age/sex, age/sex/region, maximally adjusted
* Outputs a results file with HR estimates, SE and CI's


## 04_01_(b)_cox_format_survival_data.R
* Samples the dataset if there are more that 4 million people
* Formats the data for the cox model
* Calculates event counts, follow up, median follow up for figure plotting
* Saves event count file used for redacting HRs in 05_cox_format_tbls_HRs.R

## 05_cox_format_tbls_HRs.R
* Once all the analyses has run, this script combines all the results into one file ready for output
* Combines HR files to release and event count files
* If any time periods have less than 5 post exposure then these HRs are redacted

## 06_cox_extra_functions.R
* Contains extra functions used throughout the scripts
* *rm_lowvar_covars* Outputs list of binary covariates with less than 3 post exposure events at either level to be removed from the analysis 
* *collapse_categorical_covars* If any level of the categorical covariates (cov_cat_deprivation & cov_cat_smoking_status) has less than 3 post exposure events at any level, collapsed the levels
* *covariate_exploration* Prints the number of post exposure events at each covariate level before the cox model is fit 

