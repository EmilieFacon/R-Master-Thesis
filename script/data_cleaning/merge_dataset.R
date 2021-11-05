### DATA CLEANING | MERGE FIRST AND SECOND ROUNDS ###

## Load libraries

library(tidyverse)

# ---------------------------------------------------------------- #

## Define function to merge

merge_rounds <- function(round1, round2, rds_name) {
  
  # Merge
  df <- rbind(round1, round2)
  
  # Keep only most recent round
  df <- df %>%
    group_by(geo_code) %>%
    mutate(max_round = max(round)) %>%
    ungroup() %>%
    filter(round == max_round) %>%
    select(-max_round)
  
  # Order
  df[order(df$geo_code),]
  
  # Change details
  df <- df %>% mutate(details = "final results")
  
  # Save and return
  saveRDS(df, file = rds_name)
  return(df)
}

# ---------------------------------------------------------------- #

## Load data and call function

# Load data

m2001_t1 <- readRDS("clean_data/m2001_t1.RDS")
m2001_t2 <- readRDS("clean_data/m2001_t2.RDS")
m2008_t1 <- readRDS("clean_data/m2008_t1.RDS")
m2008_t2 <- readRDS("clean_data/m2008_t2.RDS")
m2014_t1 <- readRDS("clean_data/m2014_t1.RDS")
m2014_t2 <- readRDS("clean_data/m2014_t2.RDS")

# Call function

m2001 <- merge_rounds(m2001_t1, m2001_t2, "clean_data/m2001.RDS")
m2008 <- merge_rounds(m2008_t1, m2008_t2, "clean_data/m2008.RDS")
m2014 <- merge_rounds(m2014_t1, m2014_t2, "clean_data/m2014.RDS")

# Clean

remove(m2001_t1)
remove(m2001_t2)
remove(m2008_t1)
remove(m2008_t2)
remove(m2014_t1)
remove(m2014_t2)
remove(merge_rounds)

# Complete

print("Complete.")