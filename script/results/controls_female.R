### ADD CONTROLS | FEMALE EXPOSURE EFFECT ###

## Load functions and data 

source("script/results/get_new_df.R")

## Load libraries

library(sandwich)
library(stargazer)

# ---------------------------------------------------------------- #

## Prepare results table

female_table <- function(new_df, 
                              mechanism = TRUE, 
                              placebo = FALSE,
                              outcome_var = "vote share", 
                              bw = 5) {
  
  # Rename
  
  new_df <- new_df %>%
    mutate("Female incumbent" = incumbent) %>%
    mutate("Female victory" = incumbent) %>%
    mutate("MOV" = margin_of_victory) %>%
    mutate("Left" = party_left_E01.ME) %>%
    mutate("Right" = party_right_E01.ME) %>%
    mutate("Other party" = ifelse(Left == 0 & Right == 0, 1, 0)) %>%
    mutate("Repeat candidate" = same_candidate.ME)
  
  # Baseline
  
  reg_1 <- lm(y_var ~ `Female incumbent` * MOV, 
              data = new_df, 
              subset = MOV > -bw & MOV < bw)
  
  coeftest(reg_1, vcov = vcovHC(reg_1, type="HC3"))
  coefci(reg_1, vcov = vcovHC(reg_1, type="HC3"))
  
  cov <- vcovHC(reg_1, type = "HC3")
  se_1 <- sqrt(diag(cov))
  
  # Repeat candidate
  
  if (mechanism == TRUE) {
    
    reg_2 <- lm(y_var ~  `Female incumbent` * MOV + 
                  `Repeat candidate` * `Female incumbent`,
                data = new_df, 
                subset = MOV > -bw & MOV < bw)
    
    coeftest(reg_2, vcov = vcovHC(reg_2, type="HC3"))
    
    cov <- vcovHC(reg_2, type = "HC3")
    se_2 <- sqrt(diag(cov))
    
  }
  
  # Add other party controls
  
  if (mechanism == TRUE) {
    reg_3 <- lm(y_var ~  `Female incumbent` * MOV + 
                  `Repeat candidate` * `Female incumbent` + 
                  incumbent_party.ME +
                  vote_share_E00.ME +
                  Right +
                  `Other party`,
                data = new_df, 
                subset = MOV > -bw & MOV < bw)
    
    coeftest(reg_3, vcov = vcovHC(reg_3, type="HC3"))
    
    cov <- vcovHC(reg_3, type = "HC3")
    se_3 <- sqrt(diag(cov))
    
  } else {
    
    # No interactions
    reg_3a <- lm(y_var ~  `Female incumbent`* MOV + 
                   incumbent_party.ME +
                   vote_share_E00.ME +
                   Right +
                   `Other party`,
                 data = new_df, 
                 subset = MOV > -bw & MOV < bw)
    
    coeftest(reg_3a, vcov = vcovHC(reg_3a, type="HC3"))
    coefci(reg_3a, vcov = vcovHC(reg_3a, type="HC3"))
    cov <- vcovHC(reg_3a, type = "HC3")
    se_3a <- sqrt(diag(cov))
    
    # Interactions
    reg_3b <- lm(y_var ~  `Female incumbent`* MOV + 
                   incumbent_party.ME +
                   vote_share_E00.ME +
                   Right * `Female incumbent`+
                   `Other party` * `Female incumbent`,
                 data = new_df, 
                 subset = MOV > -bw & MOV < bw)
    
    coeftest(reg_3b, vcov = vcovHC(reg_3b, type="HC1"))
    cov <- vcovHC(reg_3b, type = "HC1")
    se_3b <- sqrt(diag(cov))
  }
  
  # Add town controls
  
  if (mechanism == TRUE) {
    
    reg_4 <- lm(y_var ~ `Female incumbent`* MOV + 
                  `Repeat candidate` * `Female incumbent`+ 
                  incumbent_party.ME +
                  vote_share_E00.ME +
                  Right +
                  `Other party` +
                  total_pop + employment + pub_sector + debt_pc + 
                  tax_rev_pc + mean_income + gini_coef + perc_unemployment_benefits + 
                  perc_profits + perc_other_income, 
                data = new_df, 
                subset = MOV > -bw & MOV < bw)
    
    coeftest(reg_4, vcov = vcovHC(reg_4, type="HC3"))
    cov <- vcovHC(reg_4, type = "HC3")
    se_4 <- sqrt(diag(cov))
    
  } else {
    
    # No interactions
    reg_4a <- lm(y_var ~ `Female incumbent`* MOV + 
                   incumbent_party.ME +
                   vote_share_E00.ME +
                   Right +
                   `Other party` +
                   total_pop + employment + pub_sector + debt_pc + 
                   tax_rev_pc + mean_income + gini_coef + perc_unemployment_benefits + 
                   perc_profits + perc_other_income, 
                 data = new_df, 
                 subset = MOV > -bw & MOV < bw)  
    
    coeftest(reg_4a, vcov = vcovHC(reg_4a, type="HC3"))
    coefci(reg_4a, vcov = vcovHC(reg_4a, type="HC3"))
    cov <- vcovHC(reg_4a, type = "HC3")
    se_4a <- sqrt(diag(cov))
    
    # Interactions with incumbent
    reg_4b <- lm(y_var ~ `Female incumbent`* MOV + 
                  incumbent_party.ME +
                  vote_share_E00.ME +
                  Right * `Female incumbent`+
                  `Other party` * `Female incumbent`+
                  total_pop + employment + pub_sector + debt_pc + 
                  tax_rev_pc + mean_income + gini_coef + perc_unemployment_benefits + 
                  perc_profits + perc_other_income, 
                data = new_df, 
                subset = MOV > -bw & MOV < bw)  
    
    coeftest(reg_4b, vcov = vcovHC(reg_4b, type="HC1"))
    coefci(reg_4b, vcov = vcovHC(reg_4b, type="HC1"))
    cov <- vcovHC(reg_4b, type = "HC1")
    se_4b <- sqrt(diag(cov))
    
    # Interactions with MOV
    reg_4c <- lm(y_var ~ `Female incumbent`* MOV + 
                   incumbent_party.ME +
                   vote_share_E00.ME +
                   Right * MOV +
                   `Other party` * MOV +
                   total_pop + employment + pub_sector + debt_pc + 
                   tax_rev_pc + mean_income + gini_coef + perc_unemployment_benefits + 
                   perc_profits + perc_other_income, 
                 data = new_df, 
                 subset = MOV > -bw & MOV < bw)  
    
    coeftest(reg_4c, vcov = vcovHC(reg_4c, type="HC1"))
    cov <- vcovHC(reg_4c, type = "HC1")
    se_4c <- sqrt(diag(cov))
    
    # Remove 2 not Left or Right and interact with incumbent
    
    new_df_2 <- new_df %>% filter(!(Left == 0 & Right == 0))
    
    reg_4d <- lm(y_var ~ `Female incumbent`* MOV + 
                   incumbent_party.ME +
                   vote_share_E00.ME +
                   Right * `Female incumbent`+
                   total_pop + employment + pub_sector + debt_pc + 
                   tax_rev_pc + mean_income + gini_coef + perc_unemployment_benefits + 
                   perc_profits + perc_other_income, 
                 data = new_df_2, 
                 subset = MOV > -bw & MOV < bw)  
    
    coeftest(reg_4d, vcov = vcovHC(reg_4d, type="HC1"))
    cov <- vcovHC(reg_4d, type = "HC1")
    se_4d <- sqrt(diag(cov))
    
  }
  
  
  
  ## Get table
  
  # Covariates names
  
  if (mechanism == TRUE) {
    cov_top <- c("Female incumbent", 
                 "Margin of victory (MOV)", 
                 "Repeat candidate")
  } else if (mechanism == FALSE & placebo == FALSE) {
    cov_top <- c("Female incumbent", 
                 "Margin of victory (MOV)")
  }
  
  if (placebo == TRUE) {
    cov_top <- c("Female victory in 2014", 
                 "Margin of victory (MOV) in 2014")
    cov_middle <-  c("Female's party in 2014: Right",
                     "Female's party in 2014: Other party")
  } else {
    cov_middle <- c("Female's party in 2008: Right",
                    "Female's party in 2008: Other party")
  }
  
  cov_names <- c(cov_top,
                 "Female's party victory in 2001", 
                 "Female's party vote share in 2001",
                 cov_middle,
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
  
  # Note: 
  # No column label here
  # Change to \begin{tabular}{lccc}
  # Delete notes
  # Remove \\[-2.1ex] before results
  
  bw <- sprintf("%.2f", bw)
  
  if (mechanism == TRUE) {
    
    # Mechanism
    models <- list(reg_1, reg_2, reg_3, reg_4)
    title <- paste0("Female exposure effect - Mechanism: Personal incumbency advantage (", outcome_var, ")")
    label <- "table:female_mechanism"
    add_lines <- list(c("Party controls", "No", "No", "Yes", "Yes"),
                      c("Town controls", "No", "No", "No", "Yes"),
                      c("Bandwidth", bw, bw, bw, bw))
    se_list <- list(se_1, se_2, se_3, se_4)
    
  } else {
    
    # Baseline
    models <- list(reg_1, reg_3a, reg_4a, reg_4b)
    title <- paste0("Female exposure effect - Baseline results (", outcome_var, ")")
    label <- "table:female_ols"
    add_lines <- list(c("Party controls", "No", "Yes", "Yes", "Yes"),
                      c("Town controls", "No", "No", "Yes", "Yes"),
                      c("Bandwidth", bw, bw, bw, bw))
    se_list <- list(se_1, se_3a, se_4a, se_4b)
  }
  
  if (placebo == TRUE) {
    models <- list(reg_1, reg_3a, reg_4a)
    title <- paste0("Female exposure effect - Placebo regressions (", outcome_var, ")")
    label <- "table:female_placebo"
    add_lines <- list(c("Party controls", "No", "Yes", "Yes"),
                      c("Town controls", "No", "No", "Yes"),
                      c("Bandwidth", bw, bw, bw))
    se_list <- list(se_1, se_3a, se_4a)
  }
  
  format_stargazer <- function(text){
    
    # Remove backticks
    text <- gsub("([^A-z]+)`", "\\1", text, perl = TRUE)
    text <- gsub("`([^A-z]+)", "\\1", text, perl = TRUE)
    text <- gsub("(^`)|(`$)", "", text, perl = TRUE)
    
    # Replace tabular with mytabular and remove line space
    text <- gsub("tabular", "mytabular", text, fixed = TRUE)
    text <- gsub("\\\\[-2.1ex]", " ", text, fixed = TRUE)
    
    # Placebo
    if (placebo == TRUE) {
      text <- gsub("Female incumbent", "Female victory", text, fixed = TRUE)
    }
    
    return(text)
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
  
  my_results <- format_stargazer(my_results)
  writeLines(my_results)
  writeClipboard(my_results)
}


# ---------------------------------------------------------------- # 

## Create the function to print Latex code for tables

# Baseline results

female_baseline <- function() {

  # New df
  new_df <- get_new_df(m2008, m2014_t1, "gender_rank", "F", "M", "vote share")
  
  # Adding covariates
  female_table(new_df, 
                    mechanism = FALSE, 
                    placebo = FALSE,
                    outcome_var = "vote share",
                    bw = 5)
}

# Mechanism 

female_mechanism <- function() {
  
  # New df
  new_df <- get_new_df(m2008, m2014_t1, "gender_rank", "F", "M", "vote share")
  
  # Mechanism
  female_table(new_df,
                    mechanism = TRUE,
                    placebo = FALSE,
                    outcome_var = "vote share",
                    bw = 5)
}

# Placebo

female_placebo <- function() {
  
  # New df for placebo - other way round
  new_df <- get_new_df(m2014_t1, m2008, "gender_rank", "F", "M", "vote share")  
  
  # Placebo
  female_table(new_df,
                    mechanism = FALSE, 
                    placebo = TRUE, 
                    outcome_var = "vote share",
                    bw = 5) 
}

# ---------------------------------------------------------------- # 

## Run the functions

if (FALSE) {
  female_baseline()
  female_mechanism()
  female_placebo()
}