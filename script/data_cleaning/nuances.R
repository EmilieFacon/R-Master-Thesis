          ##### INCUMBENCY ADVANTAGE - POLITICAL NUANCES #####

## Load library

library(tidyverse)


## Nuances 2001 - https://www.interieur.gouv.fr/content/download/1856/19466/file/liste_nuances_municipales_2001.pdf

nuances_2001 <- read.delim("data/elections_municipales/nuances_2001.txt",
                           encoding = 'UTF-8') 

nuances_2001 <- nuances_2001[, 1:2]

colnames(nuances_2001) <- c("code_nuance", "nuance_meaning")

nuances_2001["party_detail"] <- c("Extreme Left",
                                  "Left",
                                  "Left", 
                                  "Green", 
                                  "Green",
                                  "Other", 
                                  "Other", 
                                  "Right", 
                                  "Right", 
                                  "Extreme Right", 
                                  "Extreme Right") 

nuances_2001 <- nuances_2001 %>%
  mutate(party = ifelse(party_detail == "Centre Right", "Right", party_detail)) %>%
  mutate(party = ifelse(party_detail == "centre left", "Left", party)) %>%
  mutate(party = ifelse(party_detail == "Green", "Other", party))

nuances_2001 <- nuances_2001 %>% 
  select(code_nuance, party) %>%
  mutate(code_nuance = as.character(code_nuance))

nuances_2001 <- rbind(nuances_2001, c("LNC", NA)) # Non communiqué

saveRDS(nuances_2001, "clean_data/nuances_2001.RDS")

          
## Nuances 2008: https://www.interieur.gouv.fr/Elections/Les-resultats/Municipales/elecresult__municipales_2008/(path)/municipales_2008/nuances.html 

nuances_2008 <- read.delim("data/elections_municipales/nuances_2008.txt",
                           encoding = 'UTF-8') 

colnames(nuances_2008) <- c("code_nuance", "nuance_meaning")

nuances_2008["party_detail"] <- c("Extreme Left",
                                  "Extreme Left", 
                                  "Left",
                                  "Left",
                                  "Green",
                                  "Left",
                                  "Centre Left",
                                  "Other", 
                                  "Other", 
                                  "Centre", 
                                  "Centre Right",
                                  "Right", 
                                  "Right", 
                                  "Extreme Right", 
                                  "Extreme Right") 

nuances_2008 <- nuances_2008 %>%
  mutate(party = ifelse(party_detail == "Centre Right", "Right", party_detail)) %>%
  mutate(party = ifelse(party_detail == "Centre Left", "Left", party)) %>%
  mutate(party = ifelse(party_detail == "Green", "Other", party))

nuances_2008 <- nuances_2008 %>% 
  select(code_nuance, party) %>%
  mutate(code_nuance = as.character(code_nuance))

saveRDS(nuances_2008, "clean_data/nuances_2008.RDS")


## Nuances 2014: https://www.interieur.gouv.fr/Elections/Les-resultats/Municipales/elecresult__MN2014/(path)/MN2014/nuances.html

nuances_2014 <- read.delim("data/elections_municipales/nuances_2014.txt",
                           encoding = 'UTF-8')

colnames(nuances_2014) <- c("code_nuance", "nuance_meaning")

nuances_2014["party_detail"] <- c("Extreme Left",
                                  "Extreme Left", 
                                  "Extreme Left",
                                  "Extreme Left", 
                                  "Left", 
                                  "Left",
                                  "Left",
                                  "Green",
                                  "Other",
                                  "Centre",
                                  "Centre",
                                  "Centre Right", 
                                  "Right",
                                  "Right",
                                  "Right",
                                  "Extreme Right",
                                  "Extreme Right") 

nuances_2014 <- nuances_2014 %>%
  mutate(party = ifelse(party_detail == "Centre Right", "Right", party_detail)) %>%
  mutate(party = ifelse(party_detail == "Green", "Other", party))

nuances_2014 <- nuances_2014 %>% 
  select(code_nuance, party) %>%
  mutate(code_nuance = as.character(code_nuance))

saveRDS(nuances_2014, "clean_data/nuances_2014.RDS")