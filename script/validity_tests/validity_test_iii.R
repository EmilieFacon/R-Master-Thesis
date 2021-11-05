### VALIDITY TEST III: PROBABILITY OF WINNING CLOSE ELECTIONS ###

## Load data and functions (from incumbency and female RD)

source("script/results/rd_results.R")

## Load libraries

library(stargazer)
library(sandwich)

# ---------------------------------------------------------------- # 

## Probability of winning close elections for incumbent
# Better to use 2001 and 2008, because density test is on 2008

incumbency_prob_win <- function() {

  df1_usual <- m2001
  df2_usual <- m2008
  
  # Switch df1 and df2, because we want left merge on 2008
  df1 <- df2_usual
  df2 <- df1_usual
  
  # Create data set
  
  df <- merge_by_status(df1, df2, "party_rank")
  mov <- create_mov_data(df, "Left", "Right", "party_rank")
  
  # New df
  
  new_df <- mov %>%
    select(geo_code, town_name, mov_E01, mov_E02,
           party.ME, rank_E01.ME, perc_votes_exp_E01.ME, rank_E02.ME,
           party.YOU, rank_E01.YOU, perc_votes_exp_E01.YOU, rank_E02.YOU)
    
  # Reshape
  
  new_df <- reshape(as.data.frame(new_df),
                    direction = "long",
                    varying = list(party = c("party.ME", "party.YOU"),
                                   rank_E01 = c("rank_E01.ME", "rank_E01.YOU"),
                                   vote_share_E01 = c("perc_votes_exp_E01.ME", "perc_votes_exp_E01.YOU"),
                                   rank_E02 = c("rank_E02.ME", "rank_E02.YOU")),
                    v.names = c("party", "rank_E01", "vote_share_E01", "rank_E02"))
  
  new_df <- new_df %>% select(-time, -id) 
  new_df <- new_df[order(new_df$geo_code),]
  
  # Incumbent is rank in 2001 (rank_E02 because other way around)
  new_df <- new_df %>%
    mutate(Incumbent = ifelse(rank_E02 == 1, 1, 0)) %>%
    mutate(Incumbent = ifelse(is.na(Incumbent), 0, Incumbent)) %>%
    mutate(Victory = ifelse(rank_E01 == 1, 1, 0)) %>%
    rename(mov = mov_E01)
  
  ## Regressions ##
  
  # Full sample
  reg_full <- lm(Victory ~ Incumbent, 
                 data = new_df)
  summary(reg_full)
  se_full <- sqrt(diag(vcovHC(reg_full, type = "HC3")))

  # MOV <= 20%
  reg_20 <- lm(Victory ~ Incumbent, 
               data = new_df, 
               subset = (mov >= -20) & (mov <= 20))
  summary(reg_20)
  se_20 <- sqrt(diag(vcovHC(reg_20, type = "HC3")))
  
  # MOV <= 10%
  reg_10 <- lm(Victory ~ Incumbent, 
               data = new_df, 
               subset = (mov >= -10) & (mov <= 10))
  summary(reg_10)
  se_10 <- sqrt(diag(vcovHC(reg_10, type = "HC3")))
  
  # MOV <= 5%
  reg_5 <- lm(Victory ~ Incumbent, 
               data = new_df, 
               subset = (mov >= -5) & (mov <= 5))
  summary(reg_5)
  se_5 <- sqrt(diag(vcovHC(reg_5, type = "HC3")))
  
  # Stargazer
  my_results <- stargazer(reg_full, reg_20, reg_10, reg_5,
            title = "Incumbency advantage - Probability of victory by incumbency status",
            label = "table:incumbency_prob_victory",
            column.labels = c("Full sample", "\\MOV $\\leq$ 20\\%", "\\MOV $\\leq$ 10\\%", "\\MOV $\\leq$ 5\\%"),
            align = FALSE,
            style = "aer",
            omit.table.layout = "n",
            dep.var.caption = "",
            column.sep.width = "0pt",
            table.placement = "ht!",
            omit.stat = c("adj.rsq", "f", "ser"),
            dep.var.labels.include = FALSE, 
            no.space = TRUE,
            intercept.bottom = TRUE,
            intercept.top = FALSE,
            se = list(se_full, se_20, se_10, se_5))
  
  
  format_stargazer <- function(text){
    # Replace tabular with boxed and remove line space
    text <- gsub("tabular", "boxed", text, fixed = TRUE)
    text <- gsub("\\\\[-2.1ex]", " ", text, fixed = TRUE)
    return(text)
  }
  
  my_results <- format_stargazer(my_results)
  writeLines(my_results)
  writeClipboard(my_results)
}

# ---------------------------------------------------------------- # 

## Female probability of winning
# Are women more likely to win (close) mixed-gender elections

female_prob_win <- function() {
  
  # Create data frames
  
  df <- merge_by_status(m2008, m2014, "gender_rank")
  mov <- create_mov_data(df, "F", "M", "gender_rank")
  
  # New df
  
  new_df <- mov %>%
    select(geo_code, town_name, mov_E01, 
           female_E01.ME, surname_E01.ME, first_name_E01.ME, rank_E01.ME, perc_votes_exp_E01.ME,
           female_E01.YOU, surname_E01.YOU, first_name_E01.YOU, rank_E01.YOU, perc_votes_exp_E01.YOU)
  
  new_df <- reshape(as.data.frame(new_df),
                    direction = "long",
                    varying = list(female = c("female_E01.ME", "female_E01.YOU"),
                                   surname = c("surname_E01.ME", "surname_E01.YOU"),
                                   first_name = c("first_name_E01.ME", "first_name_E01.YOU"),
                                   rank = c("rank_E01.ME", "rank_E01.YOU"),
                                   vote_share = c("perc_votes_exp_E01.ME", "perc_votes_exp_E01.YOU")),
                    v.names = c("female", "surname", "first_name", "rank", "vote_share"))
  
  new_df <- new_df %>% select(-time, -id) %>%
    mutate(Victory = ifelse(rank == 1, 1, 0)) %>%
    rename(mov = mov_E01) %>%
    mutate(Male = ifelse(female == 1, 0, 1)) %>%
    rename(Female = female)
  
  new_df <- new_df[order(new_df$geo_code),]
  
  ## Regressions ##
  
  # Full sample
  reg_full <- lm(Victory ~ Female, 
               data = new_df)
  summary(reg_full)
  coeftest(reg_full, vcov = vcovHC(reg_full, type="HC3"))
  se_full <- sqrt(diag(vcovHC(reg_full, type = "HC3")))
  
  # MOV <= 20%
  reg_20 <- lm(Victory ~ Female, 
               data = new_df, 
               subset = (mov >= -20) & (mov <= 20))
  summary(reg_20)
  se_20 <- sqrt(diag(vcovHC(reg_20, type = "HC3")))

  # MOV <= 10%
  reg_10 <- lm(Victory ~ Female, 
              data = new_df, 
              subset = (mov >= -10) & (mov <= 10))
  summary(reg_10)
  se_10 <- sqrt(diag(vcovHC(reg_10, type = "HC3")))
  
  # Stargazer
  
  my_results <- stargazer(reg_full, reg_20, reg_10,
            title = "Female exposure effect - Probability of victory by gender",
            label = "table:female_prob_victory",
            column.labels = c("Full sample", "\\MOV $\\leq$ 20\\%", "\\MOV $\\leq$ 10\\%"),
            align = FALSE,
            style = "aer",
            dep.var.caption = "",
            omit.table.layout = "n",
            column.sep.width = "0pt",
            table.placement = "ht!",
            omit.stat = c("adj.rsq", "f", "ser"),
            dep.var.labels.include = FALSE, 
            no.space = TRUE,
            intercept.bottom = TRUE,
            intercept.top = FALSE,
            se = list(se_full, se_20, se_10))

  format_stargazer <- function(text){
    # Replace tabular with boxed and remove line space
    text <- gsub("tabular", "boxed", text, fixed = TRUE)
    text <- gsub("\\\\[-2.1ex]", " ", text, fixed = TRUE)
    return(text)
  }
  
  my_results <- format_stargazer(my_results)
  writeLines(my_results)
  writeClipboard(my_results)
}

# ---------------------------------------------------------------- # 

## Call the functions

# Need to inverse column number and column name manually

if (FALSE) {
  incumbency_prob_win()
  female_prob_win()
}