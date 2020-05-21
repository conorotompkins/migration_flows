library(tidyverse)
library(janitor)
library(tidygraph)
library(ggraph)
library(sf)
library(tidycensus)
library(ggrepel)
library(gganimate)

options(scipen = 999, digits = 4)

df_states <- get_acs(geography = "state", variables = "B19013_001", geometry = TRUE)

df_states

df_state_centroids <- cbind(df_states, st_coordinates(st_centroid(df_states))) %>% 
  st_drop_geometry()

df_migration <- read_csv("output/migration_data_cleaned.csv") %>% 
  #filter(year == 2018) %>% 
  select(state_from, state_to, year, migration)

graph <- df_migration %>% 
  as_tbl_graph() %>% 
  ggraph() +
    geom_node_point() +
    geom_edge_fan()

graph +
  transition_states(year)

df_nodes <- df_migration %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  mutate(node_id = row_number()) %>% 
  as_tibble()

df_migration %>% 
  as_tbl_graph() %>% 
  activate(edges) %>% 
  left_join(df_nodes, by = c("from" = "node_id")) %>% 
  rename(state_from = name) %>% 
  filter(year == 2018,
         state_from %in% c("Pennsylvania", "California")) %>% 
  ggraph() +
    geom_node_point() +
    geom_edge_fan() +
    facet_edges(~state_from)
