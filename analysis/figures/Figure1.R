############
# Figure 1 #
############
#libraries
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Using local path - for testing
#dir <- "C:/Users/gic30/OneDrive - University of Cambridge/2. Long Covid/Code/Post-covid-vaccinated - stage 6 - Figure 1 - 2022.02"
#setwd(dir)

results_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/OS-outputs/OS-output-9june2022/gestational"
output_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/OS-outputs/OS-output-9june2022/figures/"

#------------------#
# 1. Load argument #
#------------------#
# NOTE: No action written in project.yaml for running figures so no need for arguments
#args = commandArgs(trailingOnly=TRUE)

# Define cohort(s)
cohort=c("unvaccinated")

#-------------------------#
# 2. Get outcomes to plot #
#-------------------------#
active_analyses <- read_rds("lib/active_analyses.rds")
# active_analyses_table <- subset(active_analyses, active_analyses$active =="TRUE" & active_analyses$outcome_group=="CVD")
active_analyses_table <- subset(active_analyses, active_analyses$outcome_group=="diabetes_gestational")
outcome_name_table <- active_analyses_table %>% 
  select(outcome, outcome_variable,outcome_group) %>% 
  mutate(outcome_name=active_analyses_table$outcome_variable %>% str_replace("out_date_", ""))

# Focus on first 8 CVD outcomes (remove ate and vte)
outcome_to_plot <- outcome_name_table$outcome_name


#---------------------------------------------#
# 3. Load and combine all estimates in 1 file #
#---------------------------------------------#
hr_files=list.files(path = results_dir, pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(results_dir,"/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

# Get estimates for main analyses and list of outcomes from active analyses
main_estimates <- subset(estimates, subgroup == "main" & event %in% outcome_to_plot & term %in% term[grepl("^days",term)])
rm(estimates,hr_file_paths)

#--------------------------Format the results-----------------------------------
main_estimates <- main_estimates %>% mutate(across(c("estimate","conf.low","conf.high"), as.numeric))

#--------------------------------------#
# 4. Specify time in weeks (mid-point) #
#--------------------------------------#
term_to_time <- data.frame(term = c("days0_7","days7_14", "days14_28", "days28_56", "days56_84", "days84_197", "days197_365", "days365_535",
                                    "days0_28","days28_535"),
                           time = c(0.5,1.5,3,6,10,20,40,65,
                                    2,65))
main_estimates <- merge(main_estimates, term_to_time, by = c("term"), all.x = TRUE)

#------------------------------------------#
# 4. Specify groups and their line colours #
#------------------------------------------#
# Specify colours
main_estimates$colour <- ""
main_estimates$colour <- ifelse(main_estimates$model=="mdl_agesex","#bababa",main_estimates$colour) # Grey
main_estimates$colour <- ifelse(main_estimates$model=="mdl_max_adj","#000000",main_estimates$colour) # Black

# Factor variables for ordering
main_estimates$model <- factor(main_estimates$model, levels=c("mdl_agesex", "mdl_max_adj")) 
main_estimates$colour <- factor(main_estimates$colour, levels=c("#bababa", "#000000"))

# Rename adjustment groups
levels(main_estimates$model) <- list("Age/sex/region adjustment"="mdl_agesex", "Extensive adjustment"="mdl_max_adj")

#-------------------------#
# 5. Specify outcome name #
#-------------------------#
# Use the nice names from active_analyses table i.e. outcome_name_table
main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name, outcome_group), by = c("event"="outcome_name"))

#---------#
# 6. Plot #
#---------#
c="unvaccinated"
group="diabetes"
for(c in cohort){
  for (group in unique(main_estimates$outcome_group)) {
    df=main_estimates %>% filter(cohort ==c & outcome_group == group)
    
    ggplot2::ggplot(data=df,
                    mapping = ggplot2::aes(x=time, y = estimate, color = model, shape=model, fill=model))+
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
      ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
      ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                                    ymax = ifelse(conf.high>64,64,conf.high),  
                                                    width = 0), 
                             position = ggplot2::position_dodge(width = 1))+   
      ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) +    
      #    ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
      ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.5,1,2,4,8,16,32,64), trans = "log") +
      ggplot2::scale_x_continuous(lim = c(0,88), breaks = seq(0,88,8)) +
      ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$model))+ 
      ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$model)) +
      ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$model)) +
      ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol = 2, byrow = TRUE)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.spacing.x = ggplot2::unit(0.5, "lines"),
                     panel.spacing.y = ggplot2::unit(0, "lines"),
                     legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                     legend.title = ggplot2::element_blank(),
                     legend.position="bottom",
                     plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +    
      ggplot2::facet_wrap(outcome~., ncol = 2)
    
    ggplot2::ggsave(paste0(output_dir,"Figure1","_",c,"_",group,".png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)
  }
}
    

