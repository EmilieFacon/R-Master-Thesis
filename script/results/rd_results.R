### INCUMBENCY ADVANTAGE AND FEMALE EXPOSURE - RD RESULTS ###

## Load libraries

library(tidyverse)
library(rdrobust)
library(rdd)

# ---------------------------------------------------------------- #

## Load data

m2001 <- readRDS("clean_data/m2001.RDS")
m2001_t1 <- readRDS("clean_data/m2001_t1.RDS")
m2001_t2 <- readRDS("clean_data/m2001_t2.RDS")

m2008 <- readRDS("clean_data/m2008.RDS")
m2008_t1 <- readRDS("clean_data/m2008_t1.RDS")
m2008_t2 <- readRDS("clean_data/m2008_t2.RDS")

m2014 <- readRDS("clean_data/m2014.RDS")
m2014_t1 <- readRDS("clean_data/m2014_t1.RDS")
m2014_t2 <- readRDS("clean_data/m2014_t2.RDS")

town_char <- readRDS("clean_data/town_characteristics.RDS")

# ---------------------------------------------------------------- #

## Prepare the control variables

town_char <- readRDS("clean_data/town_characteristics.RDS")

town_char <- town_char %>%
  mutate(employment = actifs_occupes/total_pop) %>%
  mutate(pub_sector = fct_pub/actifs_occupes)

controls_town <- c("total_pop", "employment", "pub_sector", 
                   "debt_pc", "tax_rev_pc", 
                   "mean_income", 
                   "gini_coef",
                   "perc_unemployment_benefits",
                   "perc_profits",
                   "perc_other_income")

town_char <- town_char[, c("geo_code", controls_town)]

controls_label <- c("Total population",
                    "Employment", 
                    "Public sector employment (perc)",
                    "Debt per capita", 
                    "Tax revenue per capita",
                    "Mean income", 
                    "Gini coefficient",
                    "Unemployment benefits/total income (perc)",
                    "Profits/total income (perc)",
                    "Other income/total income (perc)")

# ---------------------------------------------------------------- #

## Keep only key variables

keep_var_01 <- function(df) {
  df <- df %>% 
    select(geo_code, town_name, inscrits, 
           party_rank, party, party_left, party_right,
           perc_votes_exp, rank, n_lists, election, details)
  return(df)
}

keep_var_08 <- function(df) {
  df <- df %>% 
    mutate(perc_seats = 100 * seats / n_seats) %>%
    select(geo_code, town_name, inscrits, 
           gender_rank, gender, female, surname, first_name,
           party_rank, party, party_left, party_right,
           perc_votes_exp, seats, perc_seats, rank, 
           n_lists, n_female, n_male,
           election, details)
  return(df)
}

# ---------------------------------------------------------------- # 

## Merge E01 and E02 based on status (party rank OR gender)

merge_by_status <- function(df1, df2, status, na_as_zero = TRUE){

  # Keep key variables
  if (identical(df1, m2001) | identical(df2, m2001)) {
    m08 <- FALSE
    df1 <- keep_var_01(df1)
    df2 <- keep_var_01(df2)
  } else {
    m08 <- TRUE
    df1 <- keep_var_08(df1)
    df2 <- keep_var_08(df2)
  }
  
  # Drop details for df1
  df1 <- df1 %>% select(-details)
  
  # Drop variables for df2 (otherwise duplicates)
  df2 <- df2 %>% select(-town_name, -inscrits)
    
  if (status == 'party_rank') {
    df2 <- df2 %>% select(-party)
  } else if (status == 'gender_rank') {
    df2 <- df2 %>% select(-gender)
  }
  
  # Merge
  df <- merge(df1, df2,
              by = c('geo_code', status),
              all.x = TRUE,
              all.y = FALSE,
              suffixes = c("_E01", "_E02"))

  # Personal incumbency dummy
  if (m08 == TRUE) {
    df <- df %>%
    mutate(same_candidate = ifelse((surname_E01 == surname_E02) & (first_name_E01 == first_name_E02),
                                   1, 
                                   0))
  }
  
  # Put zeros instead of NAs - For all
  
  
  if (na_as_zero == TRUE) {
    
    if (m08 == TRUE) {
      df <- df %>% 
        mutate(same_candidate = ifelse(is.na(same_candidate), 0, same_candidate)) %>%
        mutate(party_left_E02 = ifelse(is.na(party_left_E02), 0, party_left_E02)) %>%
        mutate(party_right_E02 = ifelse(is.na(party_right_E02), 0, party_right_E02)) # Put seats to 0 when NA
    }
    
  }
  
  # Return
  
  return(df)
}

# ---------------------------------------------------------------- # 

## Create MOV data set

create_mov_data <- function(df, party1, party2, status) {
  
  # Select top 2 lists for E01
  
  df <- df %>% 
    filter(n_lists_E01 >= 2) %>%
    filter(rank_E01 <= 2)

  # Select elections

  if (status == 'party_rank') { # If party_rank: select elections with party1 and party2
    
    party_list <- c("Extreme Left", "Left", "Centre", "Right", "Extreme right", "Other")
  
    if (party1 == "Other") {
      party1 <- party_list[!grepl(paste0("^", party2, "$"), party_list)]
    } 
    if (party2 == "Other") {
      party2 <- party_list[!grepl(paste0("^", party1, "$"), party_list)]
    }
  
    df <- df %>% 
      mutate(match_party = ifelse(party %in% party1, 1, 0)) %>%
      mutate(match_party = ifelse(party %in% party2, 2, match_party)) %>%
      group_by(geo_code) %>%
      filter(n() == 2) %>%
      filter(sum(match_party) == 3) %>%
      select(-match_party) %>%
      mutate(party_reshape = ifelse(party %in% party1, "ME", "YOU"))  
    
  } else if (status == 'gender_rank') {  # If gender_rank: select mixed-gender elections
    
    df <- df %>% 
      mutate(match_party = ifelse(gender == "F", 1, 0)) %>%
      group_by(geo_code) %>%
      filter(n() == 2) %>%
      filter(sum(match_party) == 1) %>%
      select(-match_party) %>%
      mutate(party_reshape = ifelse(gender %in% party1, "ME", "YOU"))  
  }
  
  # Reshape
  
  mov <- reshape(as.data.frame(df), 
                 idvar = c("geo_code", "town_name", "inscrits"), 
                 timevar = "party_reshape", 
                 direction = "wide")
  
  # Check no duplicates
  
  if (! identical(mov[duplicated(mov$geo_code), 'geo_code'], character(0))) {
    print("There are duplicates - please check the data.")
    return()
  }
 
  # Calculate MOV

  mov <- mov %>%
    mutate(mov_E01 = perc_votes_exp_E01.ME - perc_votes_exp_E01.YOU) %>%
    mutate(mov_E02 = perc_votes_exp_E02.ME - perc_votes_exp_E02.YOU) 
  
  if ("seats_E01" %in% colnames(mov)) {
    mov <- mov %>%
      mutate(seats_mov_E01 = perc_seats_E01.ME - perc_seats_E01.YOU) %>%
      mutate(seats_mov_E02 = perc_seats_E02.ME - perc_seats_E02.YOU)
  }
  
  # Return
  return(mov)
}


# ---------------------------------------------------------------- #

## Function to get data set and controls

get_controls_df <- function(df1, df2, status, party1, party2,
                            outcome_var, controls = 2, 
                            na_as_zero = TRUE) {
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status)
  
  ## CONTROLS
  
  # Merge 2001 data set to get incumbent party 
  
  m2001_temp <- m2001 %>% 
    mutate(incumbent_party = ifelse(rank == 1, 1, 0)) %>%
    mutate(vote_share = perc_votes_exp) %>%
    select(geo_code, party_rank, incumbent_party, vote_share) 
  
  if (status == "party_rank") {
    mov <- mov %>%
      mutate(party_rank_E01.ME = party_rank.ME) %>%
      mutate(party_rank_E01.YOU = party_rank.YOU)
  }
  
  # First merge for ME
  
  m2001_ME <- m2001_temp %>%
    rename(party_rank_E01.ME = party_rank, 
           incumbent_party.ME = incumbent_party,
           vote_share_E00.ME = vote_share)
  
  mov <- merge(mov, m2001_ME,
               by = c("geo_code", "party_rank_E01.ME"),
               all.x = TRUE,
               all.y = FALSE)
  
  # Then merge for YOU
  
  m2001_YOU <- m2001_temp %>%
    rename(party_rank_E01.YOU = party_rank, 
           incumbent_party.YOU = incumbent_party,
           vote_share_E00.YOU = vote_share)
  
  mov <- merge(mov, m2001_YOU,
               by = c("geo_code", "party_rank_E01.YOU"),
               all.x = TRUE,
               all.y = FALSE)
  
  # Change NAs - Check if results sensitive
  
  if (na_as_zero == TRUE) {
    mov <- mov %>%
      mutate(incumbent_party.ME = ifelse(is.na(incumbent_party.ME), 0, incumbent_party.ME)) %>%
      mutate(incumbent_party.YOU = ifelse(is.na(incumbent_party.YOU), 0, incumbent_party.YOU)) %>% 
      mutate(vote_share_E00.ME = ifelse(is.na(vote_share_E00.ME), 0, vote_share_E00.ME)) %>%
      mutate(vote_share_E00.YOU = ifelse(is.na(vote_share_E00.YOU), 0, vote_share_E00.YOU))
  }
  
  if (status == "party_rank") {
    rd_controls <- mov[, c("incumbent_party.ME", "vote_share_E00.ME")]  
    
  } else if (status == "gender_rank") {
    rd_controls <- mov[, c("incumbent_party.ME", "vote_share_E00.ME", 
                           "party_left_E01.ME", "party_right_E01.ME")]
  }
  
  if (controls == 1) {
    return(rd_controls)
  
  } else {
    
    # Add town controls
    
    controls_town <- c("total_pop", "employment", "pub_sector", 
                       "debt_pc", "tax_rev_pc", 
                       "mean_income", 
                       "gini_coef",
                       "perc_unemployment_benefits",
                       "perc_profits",
                       "perc_other_income")
    
    mov <- merge(mov, town_char,
                 by = c('geo_code'),
                 all.x = TRUE,
                 all.y = FALSE)
    
    # Rescale
    
    mov <- mov %>% 
      mutate(total_pop = total_pop/1000) %>%
      mutate(debt_pc = debt_pc / 1000) %>%
      mutate(tax_rev_pc = tax_rev_pc / 1000) %>%
      mutate(mean_income = mean_income / 1000) 
    
    rd_controls <- cbind(rd_controls, mov[, controls_town])
    
    return(rd_controls)
  }
}

# ---------------------------------------------------------------- # 

## Function to run regressions and plots automatically
  
rd_and_plot <- function(mov, df1, df2, status, party1, party2, 
                        outcome_var = "vote share",
                        bandwidth = "optimal",
                        p = 1,
                        controls = 0,
                        kernel = "triangular",
                        na_as_zero = TRUE,
                        print_results = TRUE,
                        print_graph = TRUE,
                        save_png = FALSE,
                        save_pdf = FALSE,
                        graph_name = NULL) {

  ## PREPARE ## 
  
  # Year 
  
  year_E01 <- mean(mov$election_E01.ME, na.rm = TRUE)
  year_E02 <- mean(mov$election_E02.ME, na.rm = TRUE)

  # Details
  
  details <- max(mov$details.ME, na.rm = TRUE)
    
  # Graph name, title, and folder

  Outcome_var <- paste(toupper(substring(outcome_var, 1,1)), 
                       substring(outcome_var, 2), sep="", collapse=" ")
  
  if (is.null(graph_name)) {
    graph_name <- paste("rdplot", year_E02, "-",
                        party1, party2, "-", 
                        outcome_var, "-", details)
  }
  
  if (status == 'party_rank') {
    
    folder_name <- 'incumbency_advantage'
    graph_title <- "Incumbency advantage: Regression discontinuity plot"    
    x_label <- paste("Margin of victory in", year_E01, "election")
    y_label <- paste0(Outcome_var, " in ", year_E02, " election - ", details)
    
  } else if (status == 'gender_rank') {
    
    folder_name <- 'female_exposure'
    graph_title <- "Female exposure effect: Regression discontinuity plot"
    x_label <- paste("Female margin of victory in", year_E01, "election")
    y_label <- paste0(Outcome_var, " of top female candidate in ", year_E02, " election - ", details)
    
  }
  
  # Outcome variable

  x_var <- mov$mov_E01
  
  if (outcome_var == "vote share") {
    y_var <- mov$perc_votes_exp_E02.ME
    
  } else if (outcome_var == "first rank") {
    mov <- mov %>% 
      mutate(party1_wins = ifelse(rank_E02.ME == 1, 1, 0))
    y_var <- mov$party1_wins
    
  } else if (outcome_var == "margin of victory") {
    y_var <- mov$mov_E02
    
  } else if (outcome_var == "two-party vote share") {
    y_var <- 100 * mov$perc_votes_exp_E02.ME / (mov$perc_votes_exp_E02.ME + mov$perc_votes_exp_E02.YOU)
    
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
  
  # Add controls
  
  if (controls == 0) {
    rd_controls <- NULL
  
  } else { # Both controls == 1 and 2
    
    if (year_E01 == 2001) { 
      print("Error: No controls for year 2001.")
      return()
    }
    
    # Get controls 
    rd_controls <- get_controls_df(df1, df2, status, party1, party2,
                                   outcome_var, controls = controls, 
                                   na_as_zero = na_as_zero)
  }
  
    
  ## REGRESSION ##
  
  if (bandwidth != "optimal") {
    
    h <- bandwidth 

    rd_reg <- rdrobust(x = x_var, 
                       y = y_var, 
                       c = 0,
                       kernel = kernel, 
                       h = h, # specify h
                       vce = "nn", 
                       cluster = NULL,
                       level = 95,
                       p = p,
                       covs = rd_controls,
                       masspoints = "adjust") # default when h is specified
      
  } else { # Optimal bandwidth
    
    rd_reg <- rdrobust(x = x_var, 
                       y = y_var, 
                       c = 0,
                       kernel = kernel, 
                       bwselect = "mserd",
                       vce = "nn", 
                       cluster = NULL,
                       level = 95,
                       p = p,
                       q = p+1,
                       covs = rd_controls, 
                       covs_drop = TRUE)
    
    h <- rd_reg$bws[1]
  }
    
    # Print coefficients
    
  if (print_results == TRUE) {
    
    res <- c(rd_reg$coef,
             rd_reg$se,
             rd_reg$pv,
             rd_reg$ci)
    res[16:18] <- ifelse(res[7:9] < 0.01, 
                         0.01, 
                         ifelse(res[7:9] < 0.05, 0.05, NA))
    results <- matrix(round(res, 3),
                      nrow = 3,
                      dimnames = list(Estimate = c("Conventional", "Bias-Corr.", "Robust"),
                                      c("Coefficient \t", "se", "p-val", "lower", "upper", "sig")))
    print(results)
    
  }
    
  ## GRAPH ##
  
  if (print_graph == TRUE) {
    
    # Save graph - open pdf/png
    
    if (save_png) {
      png(file = paste0("output/", folder_name, "/", graph_name, ".png"),
          width = 800, # pdf - 7
          height = 450) # pdf - 4.5
    } 
    if (save_pdf) {
      pdf(file = paste0("output/", folder_name, "/", graph_name, ".pdf"),
          width = 7, # pdf - 7
          height = 4.5)
    }
    
    # Graph RD plot
    
    rdplot(x = x_var, 
           y = y_var, 
           c = 0,
           binselect = "esmv", 
           kernel = kernel, 
           x.lim = c(-15, 15),
           p = p, # Change from 2 to 1 - this is what we use in the rdrobust regression
           h = h, # Add bandwidth
           title = graph_title,
           x.label = x_label,
           y.label = y_label,
           covs = NULL, 
           nbins = 60)
      
    # Save graph
    if (save_png == TRUE | save_pdf == TRUE) {
      dev.off()
    } 
  }
  
  return(rd_reg)
  
}  

# ---------------------------------------------------------------- # 

## Full function 

if (FALSE) {
  
  df1 <- m2008
  df2 <- m2014_t1
  status <- "gender_rank"
  party1 <- "F"
  party2 <- "M"
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status)
  
  rd_reg <- rd_and_plot(mov, status, party1, party2, 
                        outcome_var = "vote share", 
                        bandwidth = "optimal",
                        p = 1, 
                        controls = 0,
                        print_results = TRUE,
                        print_graph = TRUE,
                        save_png = FALSE)
}

# ---------------------------------------------------------------- # 


## Complete

print("Complete.")