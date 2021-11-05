          ### MASTER R SCRIPT ###

## Options

options(warn = -1) # Turn off warnings
options(scipen=999) # Suppress scientific notation


## Create folders to store results

dir.create(file.path("output"), showWarnings = FALSE)
dir.create(file.path("output/female_exposure"), showWarnings = FALSE)
dir.create(file.path("output/validity_tests"), showWarnings = FALSE)
dir.create(file.path("output/incumbency_advantage"), showWarnings = FALSE)
dir.create(file.path("output/descriptive_stats"), showWarnings = FALSE)


## General data cleaning

print("Data cleaning")

source("script/data_cleaning/nuances.R")
source("script/data_cleaning/town_characteristics.R", encoding="utf-8")
source("script/data_cleaning/data_cleaning.R")
source("script/data_cleaning/add_ranks.R")
source("script/data_cleaning/merge_dataset.R")


## Called in other scripts

# source("script/results/rd_results.R") # To create data frames for analysis
# source("script/results/get_new_df.R") # For OLS tables 


### GET TABLES ###

print("Tables")

# Summary statistics

source("script/results/summary_statistics.R")
female_summary_stats()
incumbency_summary_stats()

# Pre-treatment characteristics - xtable

source("script/validity_tests/validity_test_i.R")
incumbency_pretreatment_char()
female_pretreatment_char()

# Probability of victory in final round 2008 - stargazer

source("script/validity_tests/validity_test_iii.R")
incumbency_prob_win()
female_prob_win()

# Incumbency - OLS - stargazer

source("script/results/controls_incumbency.R")
incumbency_baseline()
incumbency_placebo()
incumbency_mechanism()

# Female exposure effect - OLS - stargazer

source("script/results/controls_female.R")
female_baseline()
female_placebo()
female_mechanism()

# Results for optimal bandwidth - xtable

source("script/results/optimal_bandwidth_results.R")
incumbency_opt_bw()
female_opt_bw()

# Mechanism - Female exposure effect

source("script/results/controls_female_candidates.R")
female_candidates_mechanism()



### GET FIGURES ###

print("Figures")

# McCrary plot
source("script/validity_tests/validity_test_ii.R")

# RD plots
source("script/results/get_rd_plots.R")

# Robustness plots
source("script/results/robustness_plots.R")
incumbency_main_graphs()
female_main_graphs()

# Descriptive statistics
source("script/results/desc_stats_by_election.R")

# Sankey diagram
source("script/results/sankey_diagram_party.R")

# 

## Turn on warnings 

options(warn = 0)