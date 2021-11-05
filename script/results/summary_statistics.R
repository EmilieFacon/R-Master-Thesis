### SUMMARY STATISTICS

## Load functions and data 

source("script/results/rd_results.R")

## Load library

library(stargazer)

# ---------------------------------------------------------------- #

## Format stargazer

format_stargazer <- function(text){
  
  # Remove backticks
  text <- gsub("([^A-z]+)`", "\\1", text, perl = TRUE)
  text <- gsub("`([^A-z]+)", "\\1", text, perl = TRUE)
  text <- gsub("(^`)|(`$)", "", text, perl = TRUE)
  
  # Replace tabular with mytabular and remove line space
  text <- gsub("tabular", "mytabular", text, fixed = TRUE)
  text <- gsub("\\\\[-2.1ex]", " ", text, fixed = TRUE)
  
  return(text)
}

# ---------------------------------------------------------------- #

## Female exposure effect

female_summary_stats <- function() {
  
  df1 <- m2008
  df2 <- m2014_t1
  status <- "gender_rank"
  party1 <- "F"
  party2 <- "M"
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status) 
  
  mov <- mov %>%
    mutate(at_least_one_female = ifelse(is.na(female_E02.ME), 0, 1)) %>% 
    mutate(at_least_one_male = ifelse(is.na(female_E02.YOU), 0, 1)) %>%
    mutate(female_incumbent_vote_share = ifelse(rank_E01.ME == 1, 
                                                perc_votes_exp_E02.ME,
                                                NA)) %>%
    mutate(female_challenger_vote_share = ifelse(rank_E01.ME == 2,
                                                 perc_votes_exp_E02.ME,
                                                 NA)) %>%
    mutate(male_incumbent_vote_share = ifelse(rank_E01.YOU == 1,
                                              perc_votes_exp_E02.YOU, 
                                              NA)) %>% 
    mutate(male_challenger_vote_share = ifelse(rank_E01.YOU == 2, 
                                               perc_votes_exp_E02.YOU,
                                               NA)) %>%
    mutate(female_incumbent_at_least_one_female = ifelse(rank_E01.ME == 1,
                                                         at_least_one_female,
                                                         NA)) %>% 
    mutate(female_challenger_at_least_one_female = ifelse(rank_E01.ME == 2,
                                                          at_least_one_female,
                                                          NA)) %>% 
    mutate(female_incumbent_repeat = ifelse(rank_E01.ME == 1,
                                            same_candidate.ME,
                                            NA)) %>% 
    mutate(female_challenger_repeat = ifelse(rank_E01.ME == 2, 
                                             same_candidate.ME, 
                                             NA)) %>% 
    mutate(male_incumbent_repeat = ifelse(rank_E01.YOU == 1, 
                                          same_candidate.YOU, 
                                          NA)) %>% 
    mutate(male_challenger_repeat = ifelse(rank_E01.YOU == 2,
                                           same_candidate.YOU,
                                           NA)) %>% 
    mutate(female_incumbent = ifelse(rank_E01.ME == 1, 1, 0)) %>% 
    mutate(male_incumbent = ifelse(rank_E01.YOU == 1, 1, 0))
    
  small_df <- mov %>% select(perc_votes_exp_E01.ME, 
                             perc_votes_exp_E01.YOU, 
                             female_incumbent,
                             male_incumbent,
                             perc_votes_exp_E02.ME, 
                             female_incumbent_vote_share, 
                             female_challenger_vote_share,
                             perc_votes_exp_E02.YOU, 
                             male_incumbent_vote_share,
                             male_challenger_vote_share,
                             same_candidate.ME, 
                             female_incumbent_repeat,
                             female_challenger_repeat,
                             same_candidate.YOU,
                             male_incumbent_repeat,
                             male_challenger_repeat,
                             n_lists_E01.ME, n_lists_E02.ME,
                             at_least_one_female, 
                             female_incumbent_at_least_one_female,
                             female_challenger_at_least_one_female,
                             at_least_one_male)
  
  cov_names <- c("Female vote share in 2008 (final round)", 
                 "Male vote share in 2008 (final round)", 
                 "Female victory in 2008 (final round)",
                 "Male victory in 2008 (final round)",
                 "Female vote share in 2014 (first round)",
                 "Female vote share in 2014 for female incumbents",
                 "Female vote share in 2014 for female challengers", 
                 "Male vote share in 2014 (first round)",
                 "Male vote share in 2014 for male incumbents",
                 "Male vote share in 2014 for male challengers",
                 "Repeat female candidate",
                 "Repeat female candidate for female incumbents",
                 "Repeat female candidate for female challengers",
                 "Repeat male candidate",
                 "Repeat male candidate for male incumbents",
                 "Repeat male candidate for male challengers",
                 "Number of lists in 2008 (final round)", 
                 "Number of lists in 2014 (first round)",
                 "At least one female candidate in 2014",
                 "At least one female candidate for female incumbents",
                 "At least one female candidate for female challengers",
                 "At least one male candidate in 2014")
  
  my_results <- stargazer(small_df,
                          title = "Female exposure effect: Summary statistics",
                          label = "table:female_summary_stats",
                          style = "aer",
                          covariate.labels = cov_names,
                          column.sep.width = "0pt",
                          table.placement = "ht!",
                          no.space = TRUE, 
                          digits = 2)
  
  my_results <- format_stargazer(my_results)
  writeLines(my_results)
  writeClipboard(my_results)
}

# ---------------------------------------------------------------- #

## Incumbency advantage

incumbency_summary_stats <- function() {
  
  df1 <- m2008
  df2 <- m2014_t1
  status <- "party_rank"
  party1 <- "Left"
  party2 <- "Right"
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status)
  
  mov <- mov %>% 
    mutate(left_incumbent_vote_share = ifelse(rank_E01.ME == 1, 
                                              perc_votes_exp_E02.ME,
                                              NA)) %>%
    mutate(left_challenger_vote_share = ifelse(rank_E01.ME == 2,
                                                 perc_votes_exp_E02.ME,
                                                 NA)) %>%
    mutate(right_incumbent_vote_share = ifelse(rank_E01.YOU == 1,
                                              perc_votes_exp_E02.YOU, 
                                              NA)) %>% 
    mutate(right_challenger_vote_share = ifelse(rank_E01.YOU == 2, 
                                               perc_votes_exp_E02.YOU,
                                               NA))
  
  small_df <- mov %>% select(perc_votes_exp_E01.ME, 
                             perc_votes_exp_E01.YOU, 
                             perc_votes_exp_E02.ME, 
                             left_incumbent_vote_share, 
                             left_challenger_vote_share,
                             perc_votes_exp_E02.YOU, 
                             right_incumbent_vote_share,
                             right_challenger_vote_share,
                             same_candidate.ME, same_candidate.YOU,
                             n_lists_E01.ME, n_lists_E02.ME)
  
  cov_names <- c("Left vote share in 2008 (final round)", 
                 "Right vote share in 2008 (final round)", 
                 "Left vote share in 2014 (first round)",
                 "Left vote share in 2014 for incumbents",
                 "Left vote share in 2014 for challengers", 
                 "Right vote share in 2014 (first round)",
                 "Right vote share in 2014 for incumbents",
                 "Right vote share in 2014 for challengers",
                 "Repeat Left candidate",
                 "Repeat Right candidate",
                 "Number of lists in 2008 (final round)", 
                 "Number of lists in 2014 (first round)")
  
  my_results <- stargazer(small_df,
                          title = "Incumbency advantage: Summary statistics",
                          label = "table:incumbency_summary_stats",
                          style = "aer",
                          covariate.labels = cov_names,
                          column.sep.width = "0pt",
                          table.placement = "ht!",
                          no.space = TRUE,
                          digits = 2)
  
  my_results <- format_stargazer(my_results)
  writeLines(my_results)
  writeClipboard(my_results)
  
  ## Naive OLS
  
  mov <- mov %>%
    mutate(incumbent = ifelse(rank_E01.ME == 1, 1, 0))
  
  reg <- lm(mov$perc_votes_exp_E02.ME ~ mov$incumbent)
}

# ---------------------------------------------------------------- #

## Run the functions

if (FALSE) {
  female_summary_stats()
  incumbency_summary_stats()
}