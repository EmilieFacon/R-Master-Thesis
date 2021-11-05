          ### DATA CLEANING ###

# Note:
# munic_20xx_tx - Wide data
# m20xx_tx - Long data


    ### Load libraries and data

library(tidyverse)
library(readxl)

nuances_2001 <- readRDS("clean_data/nuances_2001.RDS")
nuances_2008 <- readRDS("clean_data/nuances_2008.RDS")
nuances_2014 <- readRDS("clean_data/nuances_2014.RDS")


    ### Create function to clean and reshape data


## Clean 2001 - part of clean_data function

clean_2001 <- function(df) {
  
  names(df)[1:16] <- c("dep_code", 
                       "dep_name", 
                       "town_code",
                       "town_name", 
                       "pop",
                       "inscrits",
                       "abstentions", 
                       "perc_abs_ins",
                       "votants", 
                       "perc_vot_ins",
                       "blancs",
                       "perc_blancs_ins",
                       "perc_blancs_vot", 
                       "exp",
                       "perc_exp_ins", 
                       "perc_exp_vot")
  
  # Drop overseas 
  df <- df %>% 
    mutate(overseas = grepl("Z", dep_code, fixed = TRUE)) %>%
    filter(overseas == FALSE) %>%
    select(-overseas)
  
  # Select 
  df <- df %>% select(-dep_name, -perc_abs_ins, -perc_vot_ins, -perc_blancs_ins, -perc_blancs_vot, -perc_exp_ins, -perc_exp_vot, -pop)
  df <- df %>% select(dep_code, town_code, town_name, inscrits, abstentions, votants, blancs, exp, everything())
  
  # Return
  return(df)
}


## Clean 2008 - part of clean_data function

clean_2008 <- function(df) {
  
  names(df)[1:15] <- c("dep_code", 
                       "dep_name", 
                       "town_code",
                       "town_name", 
                       "inscrits",
                       "abstentions", 
                       "perc_abs_ins",
                       "votants", 
                       "perc_vot_ins",
                       "blancs",
                       "perc_blancs_ins",
                       "perc_blancs_vot", 
                       "exp",
                       "perc_exp_ins", 
                       "perc_exp_vot")
  
  # Drop overseas 
  df <- df %>% 
    mutate(overseas = grepl("Z", dep_code, fixed = TRUE)) %>%
    filter(overseas == FALSE) %>%
    select(-overseas)
  
  # Select 
  df <- df %>% select(-dep_name, -perc_abs_ins, -perc_vot_ins, -perc_blancs_ins, -perc_blancs_vot, -perc_exp_ins, -perc_exp_vot)
  df <- df %>% select(dep_code, town_code, town_name, inscrits, abstentions, votants, blancs, exp, everything())
  
  # Return
  return(df)
}


## Clean 2014 - part of clean_data function

clean_2014 <- function(df) {
  
  names(df)[1:17] <- c("export_date",
                       "dep_code", 
                       "scrutin_mode",
                       "dep_name", 
                       "town_code",
                       "town_name", 
                       "inscrits",
                       "abstentions", 
                       "perc_abs_ins",
                       "votants", 
                       "perc_vot_ins",
                       "blancs",
                       "perc_blancs_ins",
                       "perc_blancs_vot", 
                       "exp",
                       "perc_exp_ins", 
                       "perc_exp_vot")
  
  # Exclude different type of vote - Polynésie française
  df <- df %>% filter(scrutin_mode != "PL2") 
  
  # Department code for Corsica
  df <- df %>% 
    mutate(dep_code = ifelse(dep_name == "CORSE DU SUD", "2A", dep_code)) %>%
    mutate(dep_code = ifelse(dep_name == "HAUTE CORSE", "2B", dep_code))
  
  # Drop overseas
  df <- df %>%
    filter(!is.na(dep_code))
  
  # Select
  df <- df %>% select(-export_date, -scrutin_mode, -dep_name, -perc_abs_ins, -perc_vot_ins, -perc_blancs_ins, -perc_blancs_vot, -perc_exp_ins, -perc_exp_vot)
  df <- df %>% select(dep_code, town_code, town_name, inscrits, abstentions, votants, blancs, exp, everything())
  
  # Return
  return(df)
}


## Remove empty columns - part of clean_data function

remove_empty_columns <- function(df, election) {

  start_col <- 10
  end_col <- ncol(df)
  
  if (election == 2001) {
    # No columns to remove
    return(df)
  } 
  else if (election == 2008) {
    b <- 9
  }
  else if (election == 2014) {
    b <- 11
  }
  
  # Create a list of logicals to keep or not the column
  list_keep <- c(rep(TRUE, start_col-1))   
  
  # List of logicals: TRUE if column only contains NA
  column_is_na <- colSums(is.na(df)) != nrow(df) 
  
  # Loop through blocks
  for (first_col in seq(start_col, end_col, b)) {
    keep <- any(column_is_na[first_col : (first_col+b-1)])
    list_keep <- append(list_keep, rep(keep, b))
  }
  
  # Keep columns
  df <- df[,list_keep]
  
  # Return
  return(df)
} 


## Clean data function

clean_data <- function(df, election) {
  
  # Year-specific data cleaning
  if (election == 2001) {
    df <- clean_2001(df)
  }
  else if (election == 2008) {
    df <- clean_2008(df)
  }
  else if (election == 2014) {
    df <- clean_2014(df)
  } 
  else {
    print("Error")
  }
  
  # Drop sections and arrondissements
  df <- df %>%
    filter(!is.na(as.numeric(town_code))) %>%
    mutate(town_code = sprintf("%03d", as.numeric(town_code)))
  
  # Create geo_code
  df <- df %>%
    mutate(dep_code_new = sprintf("%02d", as.integer(dep_code))) %>%
    mutate(dep_code_new = ifelse(dep_code_new == "NA", dep_code, dep_code_new)) %>%
    select(-dep_code) %>%
    rename(dep_code = dep_code_new) %>%
    mutate(geo_code = paste(dep_code, town_code, sep = ""))  
  
  # Select
  df <- df %>% select(geo_code, dep_code, everything())
  
  # Remove empty columns
  df <- remove_empty_columns(df, election)
  
  # Return
  return(df)
}


## Reshape 2001 - part of clean_reshape function

reshape_2001 <- function(df) {
  
  start_col <- 10
  end_col <- ncol(df)
  b <- 5
  
  df <- reshape(as.data.frame(df), 
                idvar = "geo_code", 
                direction = "long", 
                varying = list(code_nuance =    seq(from = start_col, to = (end_col-4), by = b),
                               list =           seq(from = (start_col+1), to = (end_col-3), by = b),
                               votes =          seq(from = (start_col+2), to = (end_col-2), by = b),
                               perc_votes_ins = seq(from = (start_col+3), to = (end_col-1), by = b),
                               perc_votes_exp = seq(from = (start_col+4), to = end_col, by = b)),
                v.names = c("code_nuance", 
                            "list", 
                            "votes", 
                            "perc_votes_ins", 
                            "perc_votes_exp"))
  
  return(df)
}


## Reshape 2008 - part of clean_reshape function

reshape_2008 <- function(df) {

  start_col <- 10
  end_col <- ncol(df)
  b <- 9
  
  df <- reshape(as.data.frame(df), 
                idvar = "geo_code", 
                direction = "long", 
                varying = list(code_nuance =    seq(from = start_col, to = (end_col-8), by = b),
                               gender =         seq(from = (start_col+1), to = (end_col-7), by = b),
                               surname =        seq(from = (start_col+2), to = (end_col-6), by = b),
                               first_name =     seq(from = (start_col+3), to = (end_col-5), by = b),
                               list =           seq(from = (start_col+4), to = (end_col-4), by = b),
                               seats =          seq(from = (start_col+5), to = (end_col-3), by = b),
                               votes =          seq(from = (start_col+6), to = (end_col-2), by = b),
                               perc_votes_ins = seq(from = (start_col+7), to = (end_col-1), by = b),
                               perc_votes_exp = seq(from = (start_col+8), to = end_col, by = b)),
                v.names = c("code_nuance", 
                            "gender", 
                            "surname", 
                            "first_name", 
                            "list", 
                            "seats", 
                            "votes", 
                            "perc_votes_ins", 
                            "perc_votes_exp"))

  return(df)
}


## Reshape 2014 - part of clean_reshape function

reshape_2014 <- function(df) {

  start_col <- 10
  end_col <- ncol(df)
  b <- 11
  
  df <- reshape(as.data.frame(df), 
                idvar = "geo_code", 
                direction = "long", 
                varying = list(code_nuance =    seq(from = start_col, to = (end_col-10), by = b),
                               gender =         seq(from = (start_col+1), to = (end_col-9), by = b),
                               surname =        seq(from = (start_col+2), to = (end_col-8), by = b),
                               first_name =     seq(from = (start_col+3), to = (end_col-7), by = b),
                               list =           seq(from = (start_col+4), to = (end_col-6), by = b),
                               seats =          seq(from = (start_col+5), to = (end_col-5), by = b),
                               seats_sector =   seq(from = (start_col+6), to = (end_col-4), by = b),
                               seats_CC =       seq(from = (start_col+7), to = (end_col-3), by = b),
                               votes =          seq(from = (start_col+8), to = (end_col-2), by = b),
                               perc_votes_ins = seq(from = (start_col+9), to = (end_col-1), by = b),
                               perc_votes_exp = seq(from = (start_col+10), to = end_col, by = b)),
                v.names = c("code_nuance", 
                            "gender", 
                            "surname", 
                            "first_name", 
                            "list", 
                            "seats", 
                            "seats_sector",
                            "seats_CC",
                            "votes", 
                            "perc_votes_ins", 
                            "perc_votes_exp"))
  
  df <- df %>% select(-seats_sector, -seats_CC)
  
  df$perc_votes_ins <- gsub(',' ,'.', df$perc_votes_ins)
  df$perc_votes_ins <- as.numeric(df$perc_votes_ins)
  
  df$perc_votes_exp <- gsub(',' ,'.', df$perc_votes_exp)
  df$perc_votes_exp <- as.numeric(df$perc_votes_exp)
  
  return(df)
}


## Clean reshape - part of clean_and_reshape_function

clean_reshape <- function(df, election, round) {
  
  # Reshape
  if (election == 2001) {
    df <- reshape_2001(df)
    df <- left_join(df, nuances_2001, by = 'code_nuance')
  }
  else if (election == 2008) {
    df <- reshape_2008(df)
    df <- left_join(df, nuances_2008, by = 'code_nuance')
  }
  else if (election == 2014) {
    df <- reshape_2014(df)
    df <- left_join(df, nuances_2014, by = 'code_nuance')
  }
  else {
    print("Error.")
  }
  
  # Order the columns and sort the data
  df <- df %>% select(geo_code, time, dep_code, town_code, town_name,
                      inscrits, abstentions, votants, blancs, exp,
                      code_nuance, party, everything()) # Order columns
  df <- df[with(df, order(geo_code, time)), ] # Sort data by geo_code and time
  df <- df %>% select(-time) # Drop time variable
  
  # Drop empty observations
  start_col <- 11
  end_col <- ncol(df)
  df <- df[complete.cases(df[, start_col:end_col]),]
  rownames(df) <- 1:nrow(df) # Reindex
  
  # Generate round and number of seats variable
  df$round <- round
  if (election != 2001) {
    df <- df %>% 
      group_by(geo_code) %>%
      mutate(n_seats = sum(seats))
  }  
  
  # Return
  return(df)
}


## Full function - clean, reshape, and save

clean_and_reshape <- function(df, election, round, rds_name) {
  
  df <- clean_data(df, election)
  df <- clean_reshape(df, election, round)
  
  # Create dummy variables
  
  df <- df %>%
    mutate(party_left = ifelse(party == "Left", 1, 0)) %>%
    mutate(party_right = ifelse(party == "Right", 1, 0))
  
  if (election == 2008 | election == 2014) {
    df <- df %>% 
      mutate(female = ifelse(gender == "F", 1, 0))
  }
  
  # Save
  
  saveRDS(df, file = rds_name)
  return(df)
}


      ### Load data and call function

## Load data

munic_2001_t1 <- read_excel("data/elections_municipales/municipales_2001.xls", sheet = "Tour 1")
munic_2001_t2 <- read_excel("data/elections_municipales/municipales_2001.xls", sheet = "Tour 2")
munic_2008_t1 <- read_excel("data/elections_municipales/municipales_2008.xls", sheet = "Tour 1")
munic_2008_t2 <- read_excel("data/elections_municipales/municipales_2008.xls", sheet = "Tour 2")
munic_2014_t1 <- read_excel("data/elections_municipales/municipales_2014_t1.xlsx")
munic_2014_t2 <- read_excel("data/elections_municipales/municipales_2014_t2.xlsx")

## Call function - save

m2001_t1 <- clean_and_reshape(munic_2001_t1, 2001, 1, "clean_data/m2001_t1.RDS")
m2001_t2 <- clean_and_reshape(munic_2001_t2, 2001, 2, "clean_data/m2001_t2.RDS")
m2008_t1 <- clean_and_reshape(munic_2008_t1, 2008, 1, "clean_data/m2008_t1.RDS")
m2008_t2 <- clean_and_reshape(munic_2008_t2, 2008, 2, "clean_data/m2008_t2.RDS")
m2014_t1 <- clean_and_reshape(munic_2014_t1, 2014, 1, "clean_data/m2014_t1.RDS")
m2014_t2 <- clean_and_reshape(munic_2014_t2, 2014, 2, "clean_data/m2014_t2.RDS")


## Keep only towns that are in 2008 data set

towns <- unlist(unique(m2008_t1['geo_code']))

m2014_t1 <- m2014_t1 %>% filter(geo_code %in% towns)
saveRDS(m2014_t1, "clean_data/m2014_t1.RDS")

m2014_t2 <- m2014_t2 %>% filter(geo_code %in% towns)
saveRDS(m2014_t2, "clean_data/m2014_t2.RDS")

remove(towns)

## Clean data

remove(munic_2001_t1)
remove(munic_2001_t2)
remove(munic_2008_t1)
remove(munic_2008_t2)
remove(munic_2014_t1)
remove(munic_2014_t2)
remove(nuances_2001)
remove(nuances_2008)
remove(nuances_2014)

## Remove functions

remove(clean_2001)
remove(clean_2008)
remove(clean_2014)
remove(clean_and_reshape)
remove(clean_data)
remove(clean_reshape)
remove(remove_empty_columns)
remove(reshape_2001)
remove(reshape_2008)
remove(reshape_2014)

## Complete

print("Complete.")