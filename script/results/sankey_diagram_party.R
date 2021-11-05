### SANKEY DIAGRAM

## Load packages

library(tidyverse)
library(networkD3)

# ---------------------------------------------------------------- #

## Load data 

m2008 <- readRDS("clean_data/m2008.RDS")
m2014_t1 <- readRDS("clean_data/m2014_t1.RDS")

# ---------------------------------------------------------------- #

## Prepare diagram

# Link 2008 and 2014 by town

m2008 <- m2008 %>%
  filter(rank == 1) %>%
  mutate(party = ifelse(party == "Left" | party == "Right", party, "Other")) %>% 
  select(geo_code, party_2008 = party) 

m2014_t1 <- m2014_t1 %>%
  filter(rank == 1) %>%
  mutate(party = ifelse(party == "Left" | party == "Right", party, "Other")) %>% 
  select(geo_code, party_2014 = party)

df <- merge(m2008, m2014_t1) # Inner join

# Get aggregate statistics

df <- df %>%
  group_by(party_2008, party_2014) %>%
  summarise(n = n())

# Create nodes data frame

parties <- unique(as.character(df$party_2008))

my_nodes <- data.frame(node = c(0:5),
                       name = c(parties, parties))

my_nodes_2008 <- data.frame(node = c(0, 1, 2), 
                            name = parties)

my_nodes_2014 <- data.frame(node = c(3, 4, 5), 
                            name = parties)

# Create links data frame

df <- merge(df, my_nodes_2008, by.x = "party_2008", by.y = "name")
df <- merge(df, my_nodes_2014, by.x = "party_2014", by.y = "name")
my_links <- df[, c("node.x", "node.y", "n")]
colnames(my_links) <- c("source", "target", "value")

## Create diagram

# Add a 'group' column to the nodes data frame:
my_nodes$group <- as.factor(c("L", "O", "R", "L", "O", "R"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["L", "O", "R"]) .range(["#E6003E", "white", "#0051B0"])'

# Show diagram

my_diagram <- sankeyNetwork(Links = my_links, 
              Nodes = my_nodes, 
              Source = 'source', 
              Target = 'target', 
              Value = 'value', 
              NodeID = 'name',
              units = 'n',
              colourScale = my_color, 
              NodeGroup = "group",
              fontSize = 14,
              fontFamily = "sans-serif")

my_diagram
