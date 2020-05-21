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

graph
# graph +
#   transition_states(year)

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
    geom_node_label(aes(label = name)) +
    geom_edge_fan(aes(edge_width = migration, edge_alpha = migration)) +
    #facet_edges(~state_from)
    theme_bw() +
    transition_states(state_from)


####test animate by year
#this works
node_positions <- df_state_centroids %>% 
  select(NAME, x = X, y = Y)

#2017
graph_object_2017 <- df_migration %>% 
  filter(year == 2017) %>% 
  arrange(state_from, year, desc(migration)) %>% 
  group_by(state_from, year) %>% 
  slice(1:3) %>% 
  ungroup() %>% 
  semi_join(node_positions, by = c("state_from" = "NAME")) %>% 
  semi_join(node_positions, by = c("state_to" = "NAME")) %>% 
  as_tbl_graph(directed = TRUE)


#2018
edges_2018 <- df_migration %>% 
  filter(year == 2018) %>% 
  arrange(state_from, year, desc(migration)) %>% 
  group_by(state_from, year) %>% 
  slice(1:3) %>% 
  ungroup() %>% 
  semi_join(node_positions, by = c("state_from" = "NAME")) %>% 
  semi_join(node_positions, by = c("state_to" = "NAME")) %>% 
  as_tbl_graph(directed = TRUE) %>% 
  activate(edges) %>% 
  as_tibble()

graph_object_combined <- graph_object_2017 %>% 
  bind_edges(edges_2018)

manual_layout <- create_layout(graph = graph_object_combined,
                                    layout = node_positions)



graph <- ggraph(manual_layout) + 
  geom_sf(data = df_states) +
  geom_node_point(alpha = 0) +
  geom_edge_fan(aes(edge_alpha = migration, edge_width = migration, color = as.factor(year))) +
                # arrow = arrow(length = unit(4, 'mm')),
                # start_cap = circle(1, 'mm'),
                # end_cap = circle(1, 'mm')) +
  coord_sf(xlim = c(-178.490999, -63.310338), ylim = c(13.659001, 73.846275)) +
  scale_edge_alpha_continuous(range = c(.3, .9)) +
  scale_edge_width_continuous(range = c(0, 3)) +
  theme_bw() #+
  #facet_edges(~year)#+
  
network_animation <- graph +
  transition_states(year) +
  labs(title = "{closest_state}")

anim_save("output/test_network_animation_year.gif", animation = network_animation, width = 1200, height = 1200)
