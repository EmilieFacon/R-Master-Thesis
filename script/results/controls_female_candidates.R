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
    mutate("Repeat candidate" = same_candidate.ME) %>% 
    mutate(prop_female_2008 = n_female_2008/n_lists_2008)
    
  
  # Baseline
  
  reg_1a <- lm(y_var ~ `Female incumbent` * MOV,
              data = new_df, 
              subset = MOV > -bw & MOV < bw)
  
  coeftest(reg_1a, vcov = vcovHC(reg_1a, type="HC3"))
  coefci(reg_1a, vcov = vcovHC(reg_1a, type="HC3"))
  
  cov <- vcovHC(reg_1a, type = "HC3")
  se_1a <- sqrt(diag(cov))
  
  # Baseline + Proportion
  
  reg_1b <- lm(y_var ~ `Female incumbent` * MOV + prop_female_2008,
              data = new_df, 
              subset = MOV > -bw & MOV < bw)
  
  coeftest(reg_1b, vcov = vcovHC(reg_1b, type="HC3"))
  coefci(reg_1b, vcov = vcovHC(reg_1b, type="HC3"))
  
  cov <- vcovHC(reg_1b, type = "HC3")
  se_1b <- sqrt(diag(cov))
  
  # Repeat candidate
  
  if (mechanism == TRUE) {
    
    reg_2 <- lm(y_var ~  `Female incumbent` * MOV + prop_female_2008 +
                  `Repeat candidate` * `Female incumbent`,
                data = new_df, 
                subset = MOV > -bw & MOV < bw)
    
    coeftest(reg_2, vcov = vcovHC(reg_2, type="HC3"))
    
    cov <- vcovHC(reg_2, type = "HC3")
    se_2 <- sqrt(diag(cov))
    
  }
  
  # Add other party controls
  
  if (mechanism == TRUE) {
    reg_3 <- lm(y_var ~  `Female incumbent` * MOV + prop_female_2008 +
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
    
  }
  
  # Add town controls
  
  if (mechanism == TRUE) {
    
    reg_4 <- lm(y_var ~ `Female incumbent`* MOV + prop_female_2008 +
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
    
  } 
  
  ## Get table
  
  # Covariates names
  
  if (mechanism == TRUE) {
    cov_top <- c("Female incumbent", 
                 "Margin of victory (MOV)", 
                 "Proportion of female candidates in 2008",
                 "Repeat candidate")
    
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
    models <- list(reg_1a, reg_1b, reg_2, reg_3, reg_4)
    title <- paste0("Female exposure effect - Mechanism: Personal incumbency advantage (", outcome_var, ")")
    label <- "table:female_mechanism_candidates"
    add_lines <- list(c("Party controls", "No", "No", "No", "Yes", "Yes"),
                      c("Town controls", "No", "No", "No", "No", "Yes"),
                      c("Bandwidth", bw, bw, bw, bw, bw))
    se_list <- list(se_1a, se_1b, se_2, se_3, se_4)
    
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

# Number of candidates

female_candidates_mechanism <- function() {
  
  # New df
  new_df <- get_new_df(m2008, m2014_t1, "gender_rank", "F", "M", 
                       "proportion of female candidates")
  
  # Adding covariates
  female_table(new_df, 
               mechanism = TRUE, 
               placebo = FALSE,
               outcome_var = "proportion of female candidates",
               bw = 5)
}



# ---------------------------------------------------------------- # 

## Run the functions

if (FALSE) {
  female_candidates_mechanism()
}