### VALIDITY OF RD DESIGN - TOWN PRETREATMENT CHARACTERISTICS###

## Load libaries

library(tidyverse)
library(readxl)
library(stringr)

# ---------------------------------------------------------------- # 

## I: Pre-treatment municipality characteristics (Table A4-A5, Fig A1)

# Population
# Land area
# Debt pc
# Tax revenues pc
# Total employment pc
# Female share, total employment
# Local gov employment pc
# Female share, local gov employment
# Manufacturing / total employment
# Female share, manufacturing

# ---------------------------------------------------------------- # 

## Census 2007 - Population

# Load data
census_2007 <- read_excel("data/recensement_2007.xls",
                          sheet = "Communes",
                          skip = 7)

# Rename variables
census_2007 <- census_2007 %>%
  rename(dep_code = `Code département`,
         town_name_census = `Nom de la commune`,
         town_code = `Code commune`,
         total_pop = `Population totale`) %>%
  select(dep_code, town_code, town_name_census, total_pop)

# Drop overseas
census_2007 <- census_2007 %>% 
  filter(dep_code != "971") %>%
  filter(dep_code != "972") %>%
  filter(dep_code != "973") %>%
  filter(dep_code != "974")

# Create geo_code
census_2007 <- census_2007 %>%
  mutate(geo_code = paste(dep_code, town_code, sep = "")) %>%
  select(geo_code, everything()) %>%
  select(-dep_code, -town_code)

# ---------------------------------------------------------------- # 

## Data on towns - Employment data

# Total employment pc
# Female share, total employment
# Local gov employment pc
# Female share, local gov employment
# Manufacturing / total employment
# Female share, manufacturing

# Load data
emploi_2007 <- read_excel("data/emploi_2007/base-cc-carac-emploi-2007.xls",
                          sheet = "COM_2007",
                          skip = 4)

# Skip first observations - variable names
emploi_2007 <- emploi_2007[-1,] 

# Select variables
emploi_2007 <- emploi_2007 %>%
  select(`Département`, 
         `Libellé géographique`, 
         `Code géographique`,
         `Actifs occupés 15 ans ou plus en 2007 (princ)`,
         `Actifs occupés 15 ans ou plus Femmes en 2007 (princ)`,
         `Salariés 15 ans ou plus Femmes Fonct publ, CDI en 2007 (princ)`,
         `Salariés 15 ans ou plus Hommes Fonct publ, CDI en 2007 (princ)`,
         `Salariés 15 ans ou plus en 2007 (princ)`) %>%
  rename(dep_code = `Département`, 
         town_name_emploi = `Libellé géographique`, 
         geo_code = `Code géographique`)

# Rename and round variables
emploi_2007 <- emploi_2007 %>%
  mutate(actifs_occupes = round(as.numeric(`Actifs occupés 15 ans ou plus en 2007 (princ)`))) %>%
  mutate(actifs_occupes_femmes = round(as.numeric(`Actifs occupés 15 ans ou plus Femmes en 2007 (princ)`))) %>%
  mutate(fct_pub_femmes = round(as.numeric(`Salariés 15 ans ou plus Femmes Fonct publ, CDI en 2007 (princ)`))) %>%
  mutate(fct_pub_hommes = round(as.numeric(`Salariés 15 ans ou plus Hommes Fonct publ, CDI en 2007 (princ)`))) %>%
  mutate(salaries = round(as.numeric(`Salariés 15 ans ou plus en 2007 (princ)`))) %>%
  mutate(fct_pub = (fct_pub_femmes + fct_pub_hommes)/2) %>%
  select(dep_code, town_name_emploi, geo_code, actifs_occupes, actifs_occupes_femmes, fct_pub, fct_pub_femmes, fct_pub_hommes, salaries)
  
# Drop overseas
emploi_2007 <- emploi_2007 %>% 
  filter(dep_code != "971") %>%
  filter(dep_code != "972") %>%
  filter(dep_code != "973") %>%
  filter(dep_code != "974")

# Alrady has geo_code
emploi_2007 <- emploi_2007 %>%
  select(geo_code, everything()) %>%
  select(-dep_code)

# ---------------------------------------------------------------- # 

## Data on towns - Debt and budget

# Debt pc
# Tax revenues pc

# Load data
comptes_2007 <- read.csv("data/comptes_2007/donneesbpse_dobp_2007.csv",
                         sep = ";",
                         dec = ",",
                         encoding = "UTF-8")

# Select and rename variables
comptes_2007 <- comptes_2007 %>%
  mutate(dep_code = str_remove(dep, "^0")) %>%
  mutate(town_code = sprintf("%03d", icom)) %>%
  select(dep_code, town_code, fdette, fimpo1, inom) %>%
  rename(debt_pc = fdette,
         tax_rev_pc = fimpo1, 
         town_name_comptes = inom) %>%
  mutate(debt_pc = as.numeric(as.character(debt_pc))) %>%
  mutate(tax_rev_pc = as.numeric(as.character(tax_rev_pc))) %>%
  mutate(town_name_comptes = as.character(town_name_comptes))

# Drop overseas
comptes_2007 <- comptes_2007 %>%
  filter(dep_code != "101") %>%
  filter(dep_code != "102") %>%
  filter(dep_code != "103") %>%
  filter(dep_code != "104") 

# Generate geo_code
comptes_2007 <- comptes_2007 %>%
  mutate(geo_code = paste(dep_code, town_code, sep = "")) %>%
  select(geo_code, town_name_comptes, everything()) %>%
  select(-dep_code, -town_code)

# ---------------------------------------------------------------- # 

## Socio-cultural variables - Part I

# Load data
revenus_2007 <- read_excel("data/revenus_2007/RFLM_2007 - Communes et ARM/Donnees/COM/RFDM2007COM.xls",
                          sheet = 2,
                          skip = 6)
# Revenus fiscaux localisés des ménages - Indicateurs de distribution par ménage

# Select and rename
revenus_2007 <- revenus_2007 %>%
  rename(geo_code = COM,
         town_name_revenus = LIBGEO, 
         n_fiscal_households = NBMEN07,
         q1_income = RFMQ107,
         median_income = RFMQ207, 
         q3_income = RFMQ307,
         q_range_income = RFMIQ07,
         mean_income = RFMMO07,
         gini_coef = RFMGI07) %>%
  select(geo_code, town_name_revenus, n_fiscal_households, q1_income, median_income, q3_income,
         q_range_income, mean_income, gini_coef)

# Drop overseas - No overseas 
# Already has geo_code

# ---------------------------------------------------------------- # 

## Socio-cultural variables - Part II

# Load data
structure_2007 <- read_excel("data/revenus_2007/RFLM_2007 - Communes et ARM/Donnees/COM/RFST2007COM.xls",
                           sheet = 2,
                           skip = 6)
# Revenus fiscaux localisés des ménages - Indicateurs de structure par ménage

# Select and rename
structure_2007 <- structure_2007 %>%
  rename(geo_code = COM,
         town_name_structure = LIBGEO,
         perc_taxed_households = PMIMP07,
         perc_wages = PTSA07,
         perc_unemployment_benefits = PCHO07,
         perc_pensions = PPEN07,
         perc_profits = PBEN07,
         perc_other_income = PAUT07) %>%
  select(geo_code, town_name_structure, perc_taxed_households, perc_wages, 
         perc_unemployment_benefits, perc_pensions, perc_profits, perc_other_income)
  
# Already has geo_code
# No overseas

# ---------------------------------------------------------------- # 

## Merge

# Census and employment data
town_char <- inner_join(x = census_2007, 
                        y = emploi_2007, 
                        by = "geo_code")

# Add comptes
town_char <- inner_join(x = town_char,
                        y = comptes_2007,
                        by = "geo_code")

# Add menages
town_char <- inner_join(x = town_char,
                        y = revenus_2007, 
                        by = "geo_code")

# Add structure
town_char <- inner_join(x = town_char,
                        y = structure_2007, 
                        by = "geo_code")

# Order columns
town_char <- town_char %>%
  select(geo_code, town_name_census, town_name_emploi, town_name_comptes, 
         town_name_revenus, town_name_structure, everything())

# Clean 

remove(census_2007)
remove(comptes_2007)
remove(emploi_2007)
remove(revenus_2007)
remove(structure_2007)

# Save

saveRDS(town_char, "clean_data/town_characteristics.RDS")