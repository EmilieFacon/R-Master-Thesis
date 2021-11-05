### INCUMBENCY ADVANTAGE AND FEMALE EXPOSURE - DESCRIPTIVE STATISTICS ###

## Load libraries

library(tidyverse)

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

# ---------------------------------------------------------------- #

## Descriptive statistics - Vote shares

# Get results

get_national_shares <- function(df) {
  res <- df %>%
    group_by(party) %>%
    summarise(Election = factor(mean(election, na.rm = TRUE)),
              party_sum = sum(exp)) %>%
    mutate(party_perc = 100 * party_sum / sum(party_sum)) %>%
    ungroup() %>%
    mutate(party = factor(party, 
                          levels = c("Extreme Left", "Left", "Centre", "Right", "Extreme Right", "Other"),
                          ordered = TRUE))
  return(res)
}

# Plot results

plot_vote_shares <- function(df_2001, df_2008, df_2014, graph_name, round) {
  
  df <- rbind(get_national_shares(df_2001),
              get_national_shares(df_2008),
              get_national_shares(df_2014))
  
  my_plot <- ggplot(df, aes(x = party, y = party_perc, fill = Election)) +
    geom_col(position = "dodge") + 
    labs(x = "Party",
         y = "National vote share", 
         title = paste("Vote share by party -", round))
  
  print(my_plot)
  
  ggsave(my_plot,
         filename = paste0("output/descriptive_stats/", graph_name),
         width = 7, 
         height = 4.5,
         units = "in")
}

# Get plots

plot_vote_shares(m2001_t1, m2008_t1, m2014_t1, 
             graph_name = "vote_share_first_round.pdf",
             round = "First-round results")

plot_vote_shares(m2001_t2, m2008_t2, m2014_t2, 
             graph_name = "vote_share_second_round.png",
             round = "Second-round results")

plot_vote_shares(m2001, m2008, m2014, 
             graph_name = "vote_share_final_round.pdf",
             round = "Final-round results")

# ---------------------------------------------------------------- #

## Descriptive statistics - Number of cities won


# Get results

get_towns_won <- function(df) {
  res <- df %>%
    filter(perc_votes_exp > 50) %>%
    group_by(party) %>%
    summarise(Election = factor(mean(election, na.rm = TRUE)),
              towns_won = n()) %>%
    mutate(towns_perc = 100 * towns_won / sum(towns_won)) %>%
    ungroup() %>%
    mutate(party = factor(party, 
                          levels = c("Extreme Left", "Left", "Centre", "Right", "Extreme Right", "Other"),
                          ordered = TRUE))
  return(res)
}

# Plot results

plot_towns_won <- function(df_2001, df_2008, df_2014, graph_name, round) {
  
  df <- rbind(get_towns_won(df_2001),
              get_towns_won(df_2008),
              get_towns_won(df_2014))
  
  my_plot <- ggplot(df, aes(x = party, y = towns_perc, fill = Election)) +
    geom_col(position = "dodge") + 
    labs(x = "Party",
         y = "Percentage of large towns won", 
         title = paste("Percentage of large towns won -", round))
  
  print(my_plot)
  
  ggsave(my_plot,
         filename = paste0("output/descriptive_stats/", graph_name),
         width = 7, 
         height = 4.5,
         units = "in")
}

plot_towns_won(m2001_t1, m2008_t1, m2014_t1,
               "towns_won_first_round.pdf",
               "First-round results")

plot_towns_won(m2001_t2, m2008_t2, m2014_t2,
               "towns_won_second_round.png",
               "Second-round results")

plot_towns_won(m2001, m2008, m2014,
               "towns_won_final_round.pdf",
               "Final-round results")
