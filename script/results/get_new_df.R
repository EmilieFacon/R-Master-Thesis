## FUNCTION TO GET DATA SET FOR CONTROLS RESULTS TABLE ##

## Load functions and data 

source("script/results/rd_results.R")

# ---------------------------------------------------------------- #

## Function to get data set and controls

get_new_df <- function(df1, df2, status, party1, party2,
                       outcome_var, na_as_zero = TRUE) {
  
  # Load intial data sets
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status)
  
  # Outcome variable
  
  if (outcome_var == "vote share") {
    y_var <- mov$perc_votes_exp_E02.ME
    
  } else if (outcome_var == "first rank") {
    mov <- mov %>% 
      mutate(party1_wins = ifelse(rank_E02.ME == 1, 1, 0))
    y_var <- mov$party1_wins
    
  } else if (outcome_var == "margin of victory") {
    y_var <- mov$mov_E02
    
  } else if (outcome_var == "two-party vote share") {
    y_var <- mov$perc_votes_exp_E02.ME / (mov$perc_votes_exp_E02.ME + mov$perc_votes_exp_E02.YOU)
    
  } else if (outcome_var == "probability of election") {
    mov <- mov %>%
      mutate(party1_elected = ifelse(perc_votes_exp_E02.ME > 50, 1, 0))
    y_var <- mov$party1_elected
    
  } else if (outcome_var == "number of female candidates") {
    y_var <- mov$n_female_E02.ME
    
  } else if (outcome_var == "proportion of female candidates") {
    y_var <- mov$n_female_E02.ME / mov$n_lists_E02.ME
  }
  
  if (na_as_zero == TRUE) {
    y_var[is.na(y_var)] <- 0
  }
  
  # Get controls
  
  rd_controls <- get_controls_df(df1, df2, status, party1, party2, 
                                 outcome_var, controls = 2, na_as_zero = na_as_zero)
  
  # Incumbent or female incumbent
  
  mov <- mov %>% mutate(incumbent = ifelse(mov_E01 >= 0, 1, 0))
  
  # Select variables
  
  margin_of_victory <- mov$mov_E01
  incumbent <- mov$incumbent
  same_candidate.ME <- mov$same_candidate.ME
  same_candidate.YOU <- mov$same_candidate.YOU
  n_lists_2008 <- mov$n_lists_E01.ME
  n_female_2008 <- mov$n_female_E01.ME
  n_male_2008<- mov$n_male_E01.ME
  
  # New df
  
  new_df <- data.frame(y_var,
                       margin_of_victory,
                       incumbent,
                       same_candidate.ME,
                       same_candidate.YOU,
                       n_lists_2008,
                       n_female_2008,
                       n_male_2008,
                       rd_controls)
  
  return(new_df)
}