### ADD CONTROLS | INCUMBENCY ADVANTAGE ###

## Load functions and data 

source("script/results/get_new_df.R")

## Load libraries

library(sandwich)
library(stargazer)
library(mgsub)

# ---------------------------------------------------------------- #

# Format tables 

incumbency_table <- function(new_df,
                              mechanism = TRUE, 
                              placebo = FALSE,
                              outcome_var = "vote share", 
                              bw = 5,
                              party1 = "Left") {

  
  new_df <- new_df %>% mutate(victory_2001 = incumbent_party.ME)
    
  # Baseline
  
  reg_1 <- lm(y_var ~ incumbent * margin_of_victory, 
              data = new_df, 
              subset = margin_of_victory > -bw & margin_of_victory < bw)
  
  coeftest(reg_1, vcov = vcovHC(reg_1, type="HC3"))
  
  cov <- vcovHC(reg_1, type = "HC3")
  se_1 <- sqrt(diag(cov))
  
  # Repeat candidate
  
  if (mechanism == TRUE) {
    
    reg_2 <- lm(y_var ~  incumbent * margin_of_victory + 
                     same_candidate.ME * incumbent,
                   data = new_df, 
                   subset = margin_of_victory > -bw & margin_of_victory < bw)
    
    coeftest(reg_2, vcov = vcovHC(reg_2, type="HC3"))
    
    cov <- vcovHC(reg_2, type = "HC3")
    se_2 <- sqrt(diag(cov))
  
  }
  
  # Add other party controls
  
  if (mechanism == TRUE) {
    reg_3 <- lm(y_var ~  incumbent * margin_of_victory + 
                     same_candidate.ME * incumbent + 
                     victory_2001 +
                     vote_share_E00.ME,
                   data = new_df, 
                   subset = margin_of_victory > -bw & margin_of_victory < bw)
    
    coeftest(reg_3, vcov = vcovHC(reg_3, type="HC3"))
    
    cov <- vcovHC(reg_3, type = "HC3")
    se_3 <- sqrt(diag(cov))
    
  } else {
    
    # No interactions
    reg_3a <- lm(y_var ~  incumbent * margin_of_victory + 
                   victory_2001 +
                   vote_share_E00.ME,
                 data = new_df, 
                 subset = margin_of_victory > -bw & margin_of_victory < bw)
    
    coeftest(reg_3a, vcov = vcovHC(reg_3a, type="HC3"))
    cov <- vcovHC(reg_3a, type = "HC3")
    se_3a <- sqrt(diag(cov))
    
    # Interactions
    reg_3b <- lm(y_var ~  incumbent * margin_of_victory + 
                     victory_2001 * incumbent +
                     vote_share_E00.ME * incumbent,
                   data = new_df, 
                   subset = margin_of_victory > -bw & margin_of_victory < bw)
    
    coeftest(reg_3b, vcov = vcovHC(reg_3b, type="HC1"))
    cov <- vcovHC(reg_3b, type = "HC1")
    se_3b <- sqrt(diag(cov))
  }
  
  # Add town controls
  
  if (mechanism == TRUE) {
    
    reg_4 <- lm(y_var ~ incumbent * margin_of_victory + 
                     same_candidate.ME * incumbent + 
                     victory_2001 +
                     vote_share_E00.ME  + 
                     total_pop + employment + pub_sector + debt_pc + 
                     tax_rev_pc +
                     mean_income + gini_coef + perc_unemployment_benefits + 
                     perc_profits + perc_other_income, 
                   data = new_df, 
                   subset = margin_of_victory > -bw & margin_of_victory < bw)
    
    coeftest(reg_4, vcov = vcovHC(reg_4, type="HC3"))
    cov <- vcovHC(reg_4, type = "HC3")
    se_4 <- sqrt(diag(cov))
    
  } else {
    
    # No interaction
    reg_4a <- lm(y_var ~ incumbent * margin_of_victory + 
                     victory_2001 +
                     vote_share_E00.ME + 
                     total_pop + employment + pub_sector + debt_pc + 
                     tax_rev_pc +
                     mean_income + gini_coef + perc_unemployment_benefits + 
                     perc_profits + perc_other_income, 
                   data = new_df, 
                   subset = margin_of_victory > -bw & margin_of_victory < bw)  
    
    coeftest(reg_4a, vcov = vcovHC(reg_4a, type="HC3"))
    cov <- vcovHC(reg_4a, type = "HC3")
    se_4a <- sqrt(diag(cov))
    
    # Party interactions
    reg_4b <- lm(y_var ~ incumbent * margin_of_victory + 
                   victory_2001 * incumbent +
                   vote_share_E00.ME * incumbent + 
                   total_pop + employment + pub_sector + debt_pc + 
                   tax_rev_pc  +
                   mean_income  + gini_coef + perc_unemployment_benefits + 
                   perc_profits + perc_other_income, 
                 data = new_df, 
                 subset = margin_of_victory > -bw & margin_of_victory < bw)  
    
    coeftest(reg_4b, vcov = vcovHC(reg_4b, type="HC3"))
    cov <- vcovHC(reg_4b, type = "HC3")
    se_4b <- sqrt(diag(cov))
    
    # Interactions with significant terms for placebo regression
    reg_4c <- lm(y_var ~ incumbent * margin_of_victory + 
                   victory_2001 * incumbent +
                   vote_share_E00.ME * incumbent + 
                   total_pop * incumbent + employment + pub_sector + debt_pc + 
                   tax_rev_pc * incumbent +
                   mean_income  + gini_coef + perc_unemployment_benefits + 
                   perc_profits + perc_other_income, 
                 data = new_df, 
                 subset = margin_of_victory > -bw & margin_of_victory < bw)  
    
    coeftest(reg_4c, vcov = vcovHC(reg_4c, type="HC3"))
    cov <- vcovHC(reg_4c, type = "HC3")
    se_4c <- sqrt(diag(cov))
    
  }
  

  
  ## Get table
  
  # Covariates names

  if (mechanism == TRUE) {
    cov_top <- c("Incumbent party", 
                 "Margin of victory (MOV)", 
                 "Repeat candidate")
  } else {
    cov_top <- c("Incumbent party", 
                 "Margin of victory (MOV)")
  }
  
  if (placebo == TRUE) {
    cov_top <- c("First rank in 2014", 
                 "Margin of victory (MOV) in 2014")
  }
  
  cov_names <- c(cov_top,
                 "Victory in 2001", 
                 "Vote share in 2001",
                 "Total population (thousands)",
                 "Employment (fraction)", 
                 "Public sector employment (fraction)",
                 "Debt per capita (thousands)", 
                 "Tax revenue per capita (thousands)",
                 "Mean income (thousands)", 
                 "Gini coefficient",
                 "Unemployment benefits/total income (perc)",
                 "Profits/total income (perc)",
                 "Other income/total income (perc)")
  
  ## Stargazer
  
  bw <- sprintf("%.2f", bw)
    
  if (mechanism == TRUE) {
    
    # Mechanism
    models <- list(reg_1, reg_2, reg_3, reg_4)
    title <- paste0("Incumbency advantage - Mechanism: Personal incumbency advantage (", outcome_var, ")")
    label <- "table:incumbency_mechanism"
    add_lines <- list(c("Town controls", "No", "No", "No", "Yes"),
                      c("Bandwidth", bw, bw, bw, bw))
    se_list <- list(se_1, se_2, se_3, se_4)

  } else {
    
    # Baseline
    models <- list(reg_1, reg_3a, reg_4a, reg_4b)
    title <- paste0("Incumbency advantage - Baseline results (", outcome_var, ")")
    label <- "table:incumbency_ols"
    add_lines <- list(c("Town controls", "No", "No", "Yes", "Yes"),
                      c("Bandwidth", bw, bw, bw, bw))
    se_list <- list(se_1, se_3a, se_4a, se_4b)
  }
  
  if (placebo == TRUE) {
    
    # Placebo
    models <- list(reg_1, reg_3a, reg_4a, reg_4c)
    title <- paste0("Incumbency advantage - Placebo regressions (", outcome_var, ")")
    label <- "table:incumbency_placebo"
    add_lines <- list(c("Town controls", "No", "No", "Yes", "Yes"),
                      c("Bandwidth", bw, bw, bw, bw))
    se_list <- list(se_1, se_3a, se_4a, se_4c)
    
  }
  
  my_results <- stargazer(models,
            title = title,
            label = label,
            digits = 2,
            align = FALSE,
            style = "aer",
            omit.table.layout = "n",
            dep.var.caption = "",
            dep.var.labels.include = FALSE, 
            covariate.labels = cov_names,
            column.sep.width = "0pt",
            table.placement = "ht!",
            omit.stat = c("adj.rsq", "f", "ser"),
            no.space = TRUE,
            intercept.bottom = TRUE,
            intercept.top = FALSE,
            add.lines = add_lines, 
            se = se_list)
  
  format_stargazer <- function(text){
    
    # Remove backticks
    text <- gsub("([^A-z]+)`", "\\1", text, perl = TRUE)
    text <- gsub("`([^A-z]+)", "\\1", text, perl = TRUE)
    text <- gsub("(^`)|(`$)", "", text, perl = TRUE)
    
    # Replace tabular with mytabular and remove line space
    text <- gsub("tabular", "mytabular", text, fixed = TRUE)
    text <- gsub("\\\\[-2.1ex]", " ", text, fixed = TRUE)
    
    # Rename interaction variables
    text <- mgsub(string = text,
                  pattern = c("same\\\\_candidate.ME",
                              "victory\\\\_2001",
                              "vote\\\\_share\\\\_E00.ME",
                              "total\\\\_pop",
                              "tax\\\\_rev\\\\_pc",
                              "mean\\\\_income"),
                  replacement = c("Repeat candidate",
                                  "Victory in 2001",
                                  "Vote share in 2001",
                                  "Total population",
                                  "Tax revenue per capita",
                                  "Mean income"))
    if (placebo == TRUE) {
      text <- mgsub(text,
                    c("incumbent", 
                      "margin\\\\_of\\\\_victory"),
                    c("First rank in 2014", 
                      "MOV in 2014"))
    } else {
      text <- mgsub(text,
                    c("incumbent", 
                      "margin\\\\_of\\\\_victory"),
                    c("Incumbent party",
                      "MOV"))
    }
    
    return(text)
  }

  my_results <- format_stargazer(my_results)
  writeLines(my_results)
  writeClipboard(my_results)

}

# ---------------------------------------------------------------- # 

## Create the function to print Latex code for tables

# Baseline results

incumbency_baseline <- function() {
  
  # New df
  new_df <- get_new_df(m2008, m2014_t1, "party_rank", "Left", "Right", "vote share")
  
  # Adding covariates
  incumbency_table(new_df,
                    mechanism = FALSE, 
                    placebo = FALSE,
                    outcome_var = "vote share",
                    bw = 5,
                    party1 = "Left")
}

# Mechanism

incumbency_mechanism <- function() {
  
  # New df
  new_df <- get_new_df(m2008, m2014_t1, "party_rank", "Left", "Right", "vote share")

  # Mechanism
  incumbency_table(new_df,
                    mechanism = TRUE,
                    placebo = FALSE,
                    outcome_var = "vote share",
                    bw = 5, 
                    party1 = "Left")
}

# Placebo

incumbency_placebo <- function() {
  
  # Placebo df
  new_df <- get_new_df(m2014_t1, m2008, "party_rank", "Left", "Right", "vote share")  
  
  # Placebo
  incumbency_table(new_df,
                    mechanism = FALSE, 
                    placebo = TRUE, 
                    outcome_var = "vote share",
                    bw = 5, 
                    party1 = "Left")
}
# ---------------------------------------------------------------- # 

## Run the function

if (FALSE) {
  incumbency_baseline()
  incumbency_mechanism()
  incumbency_placebo()
}
