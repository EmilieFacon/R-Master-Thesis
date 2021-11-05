### VALIDITY TEST II: DISCONTINUITY IN DENSITY ###

## Load libraries

library(tidyverse)
library(rdrobust)
library(xtable)

# ---------------------------------------------------------------- # 

## Load data and functions (from incumbency and female RD)

source("script/results/rd_results.R")

# ---------------------------------------------------------------- # 

## Discontinuity in density

check_discontinuity <- function(df1, df2, party1, party2, status, 
                                donut = FALSE, 
                                save_png = FALSE, save_pdf = FALSE,
                                graph_name = NULL) {
  
  # Create data set
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status) 
  
  # Donut
  if (donut == TRUE) {
    mov <- mov %>% 
      filter((mov_E01 > 1) | (mov_E01 < -1))
  }
  
  # Prepare plot
  year_E01 <- mean(mov$election_E01.ME, na.rm = TRUE)
  year_E02 <- mean(mov$election_E02.ME, na.rm = TRUE)
  
  if (status == "party_rank") {
    effect_of_interest <- "Incumbency advantage"
    parties <- paste(party1, "vs", party2)
    x_lab <- paste("Margin of victory for", party1, "party in", year_E01, "election")
  }
  
  else if (status == "gender_rank") {
    effect_of_interest <- "Female exposure effect"
    parties <- ""
    x_lab <- paste("Female margin of victory in", year_E01, "election")
  }
  
  if (donut == FALSE) {
    donut_note <- ""
  } else {
    donut_note <- "(donut design)"
  }
  
  if (is.null(graph_name)) {
    graph_name <- paste(effect_of_interest, parties, "-", year_E01, "election")
  }
  
  # Save graph
  if (save_png == TRUE) {
    png(file = paste0("output/validity_tests/", graph_name, ".png"),
        width = 800, # pdf - 7
        height = 450) # pdf - 4.5
  } 
  if (save_pdf == TRUE) {
    pdf(file = paste0("output/validity_tests/", graph_name, ".pdf"),
        width = 7, # pdf - 7
        height = 4.5) # pdf - 4.5
  }

  # McCrary plot
  p_value <- DCdensity(runvar = mov$mov_E01,
                       cutpoint = 0,
                       plot = TRUE)
  abline(v = 0)
  title(main = paste(effect_of_interest, "- McCrary density plot", donut_note),
        sub = paste("P-value:", round(p_value, 3)),
        xlab = x_lab,
        ylab = "Density")
  
  # Print results
  print(graph_name)
  print(paste("P-value:", round(p_value, 3)))
  
  # Save graph
  if (save_png == TRUE | save_pdf == TRUE) {
    dev.off()
  } 
}

# ---------------------------------------------------------------- # 

## Investigate the discontinuity for female exposure effect

check_outliers_female <- function(){ 
  
  # Define variables
  
  df1 <- m2008
  df2 <- m2014
  status <- "gender_rank"
  party1 <- "F"
  party2 <- "M"
  
  # Create data set
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status) 
  
  # Donut
  
  mov <- mov %>% filter((mov_E01 <= 1) & (mov_E01 >= -1))
  
  # Select and rename variables
  
  new_df <- mov %>%
    mutate(f_win = ifelse(rank_E01.ME == 1, 1, 0)) %>%
    mutate(dep = sprintf("%.2s", geo_code)) %>%
    select(geo_code, town_name, dep, f_win, mov_E01,
           party_E01.ME, perc_votes_exp_E01.ME,
           party_E01.YOU) %>%
    rename(mov = mov_E01,
           f_party = party_E01.ME,
           f_vote_share = perc_votes_exp_E01.ME,
           m_party = party_E01.YOU)
  
  # Check victories - 14 vs 6
  
  sum_female_victories <- new_df %>% 
    group_by(f_win) %>%
    summarise(n = n())
  print(sum_female_victories)
  
  # Check parties - Same
  
  sum_female_party <- new_df %>% 
    group_by(f_party) %>%
    summarise(n = n())
  print(sum_female_party)
  
  sum_male_party <- new_df %>% 
    group_by(m_party) %>%
    summarise(n = n())
  print(sum_male_party)
  
  ## Check 2001 results
  
  # Get data frame for 2001
  towns_list <- mov$geo_code
  towns_2001 <- m2001 %>% filter(geo_code %in% towns_list)
  towns_2001 <- towns_2001 %>%
    group_by(geo_code) %>%
    filter(rank == 1) %>%
    select(geo_code, town_name, party, perc_votes_exp) %>%
    rename("winning_party_2001" = party,
           "vote_share_2001" = perc_votes_exp)
  
  # Merge 2001 results
  new_df <- merge(new_df, 
                  towns_2001,
                  by = "geo_code",
                  all.x = TRUE,
                  all.y = FALSE)
  new_df <- new_df %>% 
    select(-town_name.y, -geo_code) %>%
    rename(town_name = town_name.x) %>%
    select(dep, town_name, everything())
  
  # Check same parties - Same
  new_df <- new_df %>% 
    mutate("Same party as female candidate" = ifelse(f_party == winning_party_2001, 1, 0)) %>%
    mutate("Same party as male candidate" = ifelse(m_party == winning_party_2001, 1, 0))
  print(paste("Same party as female candidate:",
              sum(new_df$`Same party as female candidate`)))
  print(paste("Same party as male candidate:",
              sum(new_df$`Same party as male candidate`)))
  new_df <- new_df %>%
    select(-`Same party as female candidate`, - `Same party as male candidate`)
  
  # Format for Latex print
  new_df <- new_df %>% 
    mutate(f_win = sprintf("%.f", f_win))

  # Order by margin of victory
  new_df <- new_df[order(new_df$mov, decreasing = TRUE),] 
  
  # Capitalise party name
  new_df <- new_df %>%
    mutate(f_party = str_to_sentence(f_party)) %>%
    mutate(m_party = str_to_sentence(m_party)) %>%
    mutate(winning_party_2001 = str_to_sentence(winning_party_2001))
  
  ## Tests
  
  # Binomial test
  binomial_test <- binom.test(14, 20, p=0.5)
  summary(binomial_test)
  
  # Rename columns
  new_df <- new_df %>%
    rename("Dep" = dep,
           "Town name" = town_name,
           "MOV" = mov,
           "Female winner" = f_win,
           "Female party" = f_party,
           "Female vote share" = f_vote_share,
           "Male party" = m_party,
           "Winning party in 2001" = winning_party_2001,
           "Winner's vote share in 2001" = vote_share_2001)
  
  # Print Latex
  
  res_latex <- xtable(x = new_df, 
                      caption = "Female exposure effect - Towns with female margin of victory between -1\\% and +1\\%")
  align(res_latex) <- "llcccccccc"
  print(res_latex, 
        floating = TRUE, 
        latex.environments = "center", 
        booktabs = TRUE,
        include.rownames = FALSE)
  
  # Note: heads of columns
  # Dep & Town name                    & \begin{tabular}[c]{@{}c@{}}Female\\ winner\end{tabular} & MOV   & \begin{tabular}[c]{@{}c@{}}Female \\ party\end{tabular} & \begin{tabular}[c]{@{}c@{}}Female \\ vote \\ share\end{tabular} & \begin{tabular}[c]{@{}c@{}}Male \\ party\end{tabular} & \begin{tabular}[c]{@{}c@{}}Winning \\ party \\ in 2001\end{tabular} & \begin{tabular}[c]{@{}c@{}}Winner's\\ vote share \\ in 2001\end{tabular} \\ \midrule

  # Return new_df
  
  return(new_df)
}

# ---------------------------------------------------------------- # 


## Run the tests

if (FALSE) {
  
  # PNG for all
  
  check_discontinuity(m2008, m2014_t1, "F", "M", "gender_rank", save_png = TRUE)
  
  check_discontinuity(m2008, m2014_t1, "Left", "Right", "party_rank", save_png = TRUE)
  check_discontinuity(m2008, m2014_t1, "Left", "Other", "party_rank", save_png = TRUE)
  check_discontinuity(m2008, m2014_t1, "Right", "Left", "party_rank", save_png = TRUE)
  check_discontinuity(m2008, m2014_t1, "Right", "Other", "party_rank", save_png = TRUE)

  check_discontinuity(m2001, m2008_t1, "Left", "Right", "party_rank", save_png = TRUE)
  check_discontinuity(m2001, m2008_t1, "Left", "Other", "party_rank", save_png = TRUE)
  check_discontinuity(m2001, m2008_t1, "Right", "Left", "party_rank", save_png = TRUE)
  check_discontinuity(m2001, m2008_t1, "Right", "Other", "party_rank", save_png = TRUE)
  
  # Check outliers for female exposure effect
  
  close_races <- check_outliers_female()
  
}

# ---------------------------------------------------------------- # 

# PDF for main 

check_discontinuity(m2008, m2014_t1, "F", "M", "gender_rank", 
                    save_pdf = TRUE,
                    graph_name = "female_mccrary_full")

check_discontinuity(m2008, m2014_t1, "F", "M", "gender_rank", 
                    donut = TRUE, 
                    save_pdf = TRUE,
                    graph_name = "female_mccrary_donut")

check_discontinuity(m2008, m2014_t1, "Left", "Right", "party_rank", 
                    save_pdf = TRUE,
                    graph_name = "incumbency_mccrary")

# ---------------------------------------------------------------- # 

## Complete

print("Complete.")