### VALIDITY TEST I: PRE-TREATMENT CHARACTERISTICS ###

## Load libraries

library(tidyverse)
library(rdrobust)
library(xtable)

# ---------------------------------------------------------------- # 

## Load data and functions (from incumbency and female RD)

source("script/results/rd_results.R")

town_char <- readRDS("clean_data/town_characteristics.RDS")
town_char <- town_char %>% select(-town_name_census, -town_name_emploi, -town_name_comptes, 
                                  -town_name_revenus, -town_name_structure)

town_var_full <- c("Electoral roll",
                   "Total population",
                   "Number of employed",
                   "Number of women employed",
                   "Public sector employment",
                   "Number of women in public sector",
                   "Number of men in public sector", 
                   "Number of wage earners", 
                   "Debt per capita", 
                   "Tax revenue per capita", 
                   "Number of fiscal households",
                   "First-quartile income",
                   "Median income", 
                   "Third-quartile income",
                   "Interquartile range for income",
                   "Mean income",
                   "Gini coefficient",
                   "Taxed households (perc)",
                   "Wages/total income (perc)",
                   "Unemployment benefits/total income (perc)",
                   "Pensions/total income (perc)",
                   "Profits/total income (perc)",
                   "Other income/total income (perc)")


# ---------------------------------------------------------------- # 

## Create the function to run t-tests

run_all_tests <- function(df1, df2, party1, party2, status, 
                          full_sample = FALSE, bandwidth = "optimal",
                          caption = "", label = NULL) {
  
  # Create data set
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status)
  
  # If not full sample
  
  if (full_sample == FALSE) {
      
    # Run RD reg to get the bandwidth - based on vote share
    
    y_var <- mov$perc_votes_exp_E02.ME
    
    if (is.numeric(bandwidth) == FALSE) {
        
      rd_reg <- rdrobust(x = mov$mov_E01, 
                         y = y_var, 
                         c = 0,
                         kernel = "tri", 
                         bwselect = "mserd",
                         vce = "nn", 
                         cluster = NULL,
                         level = 95,
                         p = 1,
                         q = 2)
      
      left_bw <- rd_reg$bws[1]
      right_bw <- rd_reg$bws[3]
      
      # Change bandwidth
      
      if (bandwidth == "half") {
        left_bw <- left_bw / 2
        right_bw <- right_bw / 2
      }
    
    # If numeric bandwidth    
    } else {
      left_bw <- bandwidth
      right_bw <- bandwidth
    }
  
  # If full sample
    
  } else if (full_sample == TRUE) {
    left_bw <- 100
    right_bw <- 100
  }
      
  # Filter based on bandwidth
  
  mov <- mov %>% 
    mutate("Female candidate" = female_E01.ME) %>%
    mutate("Left candidate" = party_left_E01.ME) %>%
    mutate("Right candidate" = party_right_E01.ME) %>%
    select(geo_code, town_name, mov_E01,
           `Female candidate`, `Left candidate`, `Right candidate`,
           inscrits)
 
  towns_left <- mov %>% 
    filter(mov_E01 < 0) %>% 
    filter(mov_E01 >= -left_bw) %>%
    select(-mov_E01)
  
  towns_right <- mov %>% 
    filter(mov_E01 >= 0) %>% 
    filter(mov_E01 <= right_bw) %>%
    select(-mov_E01)
  
  # Merge to get town characteristics
  
  towns_left <- left_join(towns_left, town_char, by = "geo_code")
  towns_right <- left_join(towns_right, town_char, by = "geo_code")
  
  town_var <- colnames(towns_left)
  
  if (status == "party_rank") {
    town_var <- town_var[c(3, 6: length(town_var))] # Start the t-tests at inscrits
  } else if (status == "gender_rank") {
    town_var <- town_var[c(4:length(town_var))]
  }
  
  # Run the t-tests
  
  results <- c()
  
  for (var in town_var) {
    test <- t.test(towns_right[var], towns_left[var])
    results <- append(results, c(test$estimate[1], 
                                 test$estimate[2], 
                                 test$estimate[2]-test$estimate[1],
                                 test$stderr,
                                 test$p.value,
                                 (left_bw + right_bw)/2,
                                 nrow(towns_right) + nrow(towns_left)))
  }
  
  # Format results from t-test
  
  results <- as.data.frame(matrix(results, 7))
  results <- t(results)
  
  colnames(results) <- c('group1', 
                         'group2', 
                         'Difference', 
                         'Standard error', 
                         'P-value',
                         'Bandwidth',
                         'N')
  
  rownames(results) <- town_var
  
  results <- as.data.frame(results)
  
  ## Round numbers

  results$group1 <- sprintf("%.2f", results$group1)
  results$group2 <- sprintf("%.2f", results$group2)
  results$Difference <- sprintf("%.2f", results$Difference)
  results$`Standard error` <- sprintf("%.2f", results$`Standard error`)
  results$`P-value` <- sprintf("%.3f", results$`P-value`)
  results$`Bandwidth` <- sprintf("%.2f", results$`Bandwidth`)
  results$N <- sprintf("%.f", results$N)
  
  # Rename columns
  
  colnames(results) <- c(paste('Mayor:', party1), 
                         paste('Mayor:', party2), 
                         'Difference', 
                         'Standard error', 
                         'P-value',
                         'Bandwidth',
                         'N')

  # Put stars next to Difference
  
  results <- results %>% 
    mutate(Difference = ifelse(`P-value` <= 0.01, 
                               paste0(Difference, '$^{***}$'), 
                               Difference)) %>%
    mutate(Difference = ifelse((`P-value` > 0.01) & (`P-value` <= 0.05), 
                               paste0(Difference, '$^{**}$'), 
                               Difference)) %>%
    mutate(Difference = ifelse((`P-value` > 0.05) & (`P-value` <= 0.10), 
                               paste0(Difference, '$^{*}$'), 
                               Difference))
    
  # Delete bandwidth for full sample
  if (full_sample == TRUE) {
    results <- results[, -6]
  }
  
  if (status == "party_rank") {
    var_full <- c("Female candidate", town_var_full)
  } else if (status == "gender_rank") {
    var_full <- c("Left candidate", "Right candidate", town_var_full)
  }
  
  rownames(results) <- var_full
  
  # Get thousands separator
  for (i in seq(1, 4)) {
    results[,i] <- prettyNum(results[,i], big.mark=",", preserve.width="none")
  }
  results$N <- prettyNum(results$N, big.mark=",", preserve.width="none")

  res_latex <- xtable(x = results, 
                      caption = caption,
                      label = label)
  
  if (full_sample == FALSE) {
    align(res_latex) <- "lccccccc"
  } else {
    align(res_latex) <- "lcccccc"
  }
  
  writeClipboard(print(res_latex, 
        floating = TRUE, 
        latex.environments = "center", 
        booktabs = TRUE,
        sanitize.text.function=identity,
        caption.placement = "top"))
  
  return(results)
}

# ---------------------------------------------------------------- # 

## Create functions to get tables

incumbency_pretreatment_char <- function() {
  
  incumbency <- run_all_tests(m2008, m2014_t1, "Left", "Right", "party_rank", 
                              full_sample = FALSE, 
                              bandwidth = 5,
                              caption = "Difference in municipality characteristics based on party (MOV $\\leq$ 5\\%)",
                              label = "table:incumbency_pre-treatment_characteristics" )
}

female_pretreatment_char <- function() {
  
  female_ <- run_all_tests(m2008, m2014_t1, "F", "M", "gender_rank", 
                           full_sample = FALSE,
                           bandwidth = 20,
                           caption = "Difference in municipality characteristics for female and male mayors (MOV $\\leq$ 20\\%)",
                           label = "table:female_pre-treatment_characteristics")
  
}

# ---------------------------------------------------------------- # 

## Run the t-tests

if (FALSE) {
  
  ## Female exposure
  
  # Not included
  female_full <- run_all_tests(m2008, m2014_t1, "F", "M", "gender_rank", 
                               full_sample = TRUE, bandwidth = "optimal",
                               caption = "Difference in municipality characteristics for female and male mayors (full sample)")
  
  ## Incumbency effect
  
  # Appendix
  incumbency_20 <- run_all_tests(m2008, m2014_t1, "Left", "Right", "party_rank", 
                                 full_sample = FALSE, 
                                 bandwidth = 20,
                                 caption = "Difference in municipality characteristics based on party (MOV $\\leq$ 20\\%)")
  
  # Main
  incumbency_10 <- run_all_tests(m2008, m2014_t1, "Left", "Right", "party_rank", 
                                      full_sample = FALSE, 
                                      bandwidth = 10,
                                      caption = "Difference in municipality characteristics based on party (MOV $\\leq$ 10\\%)")
  
  # Not included
  incumbency_full <- run_all_tests(m2008, m2014_t1, "Left", "Right", "party_rank", 
                                   full_sample = TRUE)

}

# ---------------------------------------------------------------- # 

## Complete

print("Complete.")