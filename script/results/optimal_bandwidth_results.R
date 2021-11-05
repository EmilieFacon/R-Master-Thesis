### RESULTS FOR OPTIMAL BANDWIDTH ###

## Load functions and data 

source("script/results/rd_results.R")
library(xtable)

# ---------------------------------------------------------------- #

## Format results

format_results <- function(rd_reg, controls, p) {
  
  # Get values
  coef <- rd_reg$coef[1]
  se <- rd_reg$se[1]
  p_value <- rd_reg$pv[1]
  N <- sum(rd_reg$N_h)
  bw <- rd_reg$bws[1]
  
  # Round them
  coef <- sprintf("%.2f", coef)
  se <- paste0("(", sprintf("%.2f", se), ")")
  bw <- sprintf("%.2f", bw)
  
  # Put stars next to p-values
  if (p_value <= 0.01) {
    coef <- paste0(coef, "$^{***}$")
  } else if (p_value <= 0.05) {
    coef <- paste0(coef, "$^{**}$")
  } else if (p_value <= 0.10) {
    coef <- paste0(coef, "$^{*}$")
  }
  
  # Controls
  party_controls <- ifelse(controls == 0, "No", "Yes")
  town_controls <- ifelse(controls == 2, "Yes", "No")
  
  # Return
  res <- c(coef, se, party_controls, town_controls, p, bw, N)
  return(res)
}

# ---------------------------------------------------------------- #

## Optimal bandwidth table

opt_bw_table <- function(df1, df2, status, party1, party2,
                         caption, label) {
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status)
  
  p_list <- c(1, 1, 1, 3)
  controls_list <- c(0, 1, 2, 2)
  
  results <- c()
  
  for (i in seq(1, 4)) {
    
    p <- p_list[i]
    controls <- controls_list[i]
    
    rd_reg <- rd_and_plot(mov, df1, df2, status, party1, party2, 
                          outcome_var = "vote share",
                          bandwidth = "optimal",
                          p = p,
                          controls = controls,
                          kernel = "triangular",
                          print_results = FALSE,
                          print_graph = FALSE,
                          save_png = FALSE,
                          save_pdf = FALSE)
    
    res <- format_results(rd_reg, controls, p)
    results <- append(results, res)
  }
  
  if (status == "party_rank") {
    effect <- "Incumbent party"
  } else {
    effect <- "Female incumbent"
  }
  
  res_mat <- matrix(results, 
                    nrow = 7,
                    dimnames = list(c(effect,
                                      "",
                                      "Party controls", 
                                      "Town controls", 
                                      "Degree of local polynomial", 
                                      "Bandwidth size",
                                      "Observations"),
                                    c("(1)", "(2)", "(3)", "(4)")))
  
  res_latex <- xtable(res_mat, 
                      caption = caption,
                      label = label)
  
  align(res_latex) <- "lcccc"
  
  writeClipboard(print(res_latex, 
        floating = TRUE, 
        latex.environments = "center", 
        booktabs = TRUE,
        sanitize.text.function = identity,
        caption.placement = "top"))
}

# ---------------------------------------------------------------- #

## Create summary functions

incumbency_opt_bw <- function() {
  opt_bw_table(m2008, m2014_t1, "party_rank", "Left", "Right",
               caption = "Incumbency advantage - Results using optimal bandwidth (vote share)",
               label = "table:incumbency_opt_bw")
}

female_opt_bw <- function() {
  opt_bw_table(m2008, m2014_t1, "gender_rank", "F", "M",
               caption = "Female exposure effect - Results using optimal bandwidth (vote share)",
               label = "table:female_opt_bw")
}

# ---------------------------------------------------------------- #

## Run it

if (FALSE) {
  incumbency_opt_bw()
  female_opt_bw()
}
