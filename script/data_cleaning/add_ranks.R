        ### DATA CLEANING - ADD RANKS ###


## Load libraries

library(tidyverse)

## Load data

m2001_t1 <- readRDS("clean_data/m2001_t1.RDS")
m2001_t2 <- readRDS("clean_data/m2001_t2.RDS")

m2008_t1 <- readRDS("clean_data/m2008_t1.RDS")
m2008_t2 <- readRDS("clean_data/m2008_t2.RDS")

m2014_t1 <- readRDS("clean_data/m2014_t1.RDS")
m2014_t2 <- readRDS("clean_data/m2014_t2.RDS")

# --------------------------------------------------------- #

## Get ranks

get_ranks <- function(df, gender_column = TRUE) {
  
  # Rank
  df <- df %>% 
    group_by(geo_code) %>%
    mutate(rank = min_rank(desc(perc_votes_exp))) %>%
    mutate(n_lists = n()) %>%
    ungroup() 
  
  # Party rank
  df <- df %>%
    group_by(geo_code, party) %>%
    mutate(party_rank = min_rank(desc(perc_votes_exp))) %>%
    ungroup() %>%
    mutate(party_rank = paste(party, party_rank))
  
  # Gender rank and number of women
  if (gender_column == TRUE) {
    df <- df %>%
      group_by(geo_code, gender) %>%
      mutate(gender_rank = min_rank(desc(perc_votes_exp))) %>%
      ungroup() %>%
      mutate(gender_rank = paste(gender, gender_rank)) 
    
    df <- df %>% 
      group_by(geo_code) %>% 
      mutate(n_female = sum(female)) %>% 
      mutate(n_male = sum(1-female))
  }
  
  # Return
  return(df)
}

# --------------------------------------------------------- #

## Election 2001 - rounds 1 & 2

m2001_t1 <- get_ranks(m2001_t1, gender_column = FALSE)
m2001_t2 <- get_ranks(m2001_t2, gender_column = FALSE)

# Check for ties - No problem
m2001_t1 %>% group_by(geo_code) %>% filter(max(rank) != 1) %>% filter(max(rank) == min(rank))
m2001_t2 %>% group_by(geo_code) %>% filter(max(rank) != 1) %>% filter(max(rank) == min(rank))
  
# More than one observation with rank 1/2
m2001_t1 %>% group_by(geo_code) %>% filter(rank == 1) %>% mutate(n = n()) %>% filter(n > 1)
# Bordères - Perfect equality in round 1
m2001_t1 %>% group_by(geo_code) %>% filter(rank == 2) %>% mutate(n = n()) %>% filter(n > 1)
m2001_t2 %>% group_by(geo_code) %>% filter(rank == 1) %>% mutate(n = n()) %>% filter(n > 1)
m2001_t2 %>% group_by(geo_code) %>% filter(rank == 2) %>% mutate(n = n()) %>% filter(n > 1)


# --------------------------------------------------------- #

## Election 2008 - Round 1

m2008_t1 <- get_ranks(m2008_t1)

# Check for more than 2 lists 
# Some perfect equality in round 1

## Election 2008 - Round 2

m2008_t2 <- get_ranks(m2008_t2)

# Check for more than 2 lists - Banyuls-sur-Mer
m2008_t2 %>% filter(n_lists >= 2) %>% filter(rank <= 2) %>%
  group_by(geo_code) %>%
  mutate(num = n()) %>%
  filter(num != 2) %>%
  select(geo_code, town_name, surname, seats, votes, rank, n_lists, round)
m2008_t2[m2008_t2$geo_code == "66016" & m2008_t2$surname == "RULLS", "rank"] <- 3

# Check for ties - Givry
m2008_t2 %>% filter(n_lists >= 2) %>% filter(rank <= 2) %>%
  group_by(geo_code) %>%
  filter(max(rank) == min(rank)) %>%
  select(geo_code, town_name, surname, seats, votes, rank, n_lists, round)
# Daniel VILLERET elected, Philippe CHERPION 2nd
m2008_t2[m2008_t2$geo_code == "71221" & m2008_t2$surname == "CHERPION", "rank"] <- 2

# Check no 0 seats in total
m2008_t2 %>% filter(n_seats == 0)
# Ajaccio - Seats wrong - https://www.lexpress.fr/resultats-elections/municipales-2008-ajaccio-20000_374308.html
m2008_t2[m2008_t2$geo_code == "2A004" & m2008_t2$surname == "RUAULT", "seats"] <- 4
m2008_t2[m2008_t2$geo_code == "2A004" & m2008_t2$surname == "RENUCCI", "seats"] <- 38
m2008_t2[m2008_t2$geo_code == "2A004" & m2008_t2$surname == "CORTEY", "seats"] <- 3
m2008_t2 <- m2008_t2 %>% mutate(n_seats = ifelse(geo_code == "2A004", 45, n_seats))

# --------------------------------------------------------- #

## Election 2014 - Round 1

m2014_t1 <- get_ranks(m2014_t1)

# More than one observation with rank 1 - No problem
m2014_t1 %>% group_by(geo_code) %>% filter(rank == 1) %>% mutate(n = n()) %>% filter(n > 1)

## Election 2014 - Round 2

m2014_t2 <- get_ranks(m2014_t2)

# More than one observation with rank 1
m2014_t2 %>% group_by(geo_code) %>% 
  filter(rank == 1) %>% mutate(n = n()) %>% filter(n > 1) %>%
  select(geo_code, town_name, votes, seats, party, party_rank)

# Lescar
m2014_t2[m2014_t2$geo_code == "64335" & m2014_t2$surname == "COY", "rank"] <- 2

# Saint-Loup
m2014_t2[m2014_t2$geo_code == "70467" & m2014_t2$surname == "BAVARD", "rank"] <- 2
m2014_t2[m2014_t2$geo_code == "70467" & m2014_t2$surname == "BAVARD", "party_rank"] <- "left 2"

# --------------------------------------------------------- #

## Add years

m2001_t1['election'] <- 2001
m2001_t2['election'] <- 2001

m2008_t1['election'] <- 2008
m2008_t2['election'] <- 2008

m2014_t1['election'] <- 2014
m2014_t2['election'] <- 2014

## Add details

m2001_t1['details'] <- "round 1"
m2001_t2['details'] <- "round 2"

m2008_t1['details'] <- "round 1"
m2008_t2['details'] <- "round 2"

m2014_t1['details'] <- "round 1"
m2014_t2['details'] <- "round 2"

## Save and clean

remove(get_ranks)

saveRDS(m2001_t1, "clean_data/m2001_t1.RDS")
saveRDS(m2001_t2, "clean_data/m2001_t2.RDS")

saveRDS(m2008_t1, "clean_data/m2008_t1.RDS")
saveRDS(m2008_t2, "clean_data/m2008_t2.RDS")

saveRDS(m2014_t1, "clean_data/m2014_t1.RDS")
saveRDS(m2014_t2, "clean_data/m2014_t2.RDS")


## Complete

print("Complete.")