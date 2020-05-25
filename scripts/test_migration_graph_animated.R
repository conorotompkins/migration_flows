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
  scale_edge_width_continuous(range = c(0, 5)) +
  theme_bw() #+
  #facet_edges(~year)#+
  
network_animation <- graph +
  transition_states(year) +
  labs(title = str_c("Year:", "{closest_state}", sep = " "))

anim_save("output/test_network_animation_year.gif", animation = network_animation, width = 1200, height = 1200)

#compare pennsylvania and california in 2017

node_positions <- df_state_centroids %>% 
  select(NAME, x = X, y = Y)

#create comparison df
graph_object_state_compare <- df_migration %>% 
  filter(year == 2017,
         state_from %in% c("Pennsylvania", "California")) %>% 
  arrange(state_from, year, desc(migration)) %>% 
  group_by(state_from, year) %>% 
  slice(1:15) %>% 
  ungroup() %>% 
  semi_join(node_positions, by = c("state_from" = "NAME")) %>% 
  semi_join(node_positions, by = c("state_to" = "NAME")) %>% 
  as_tbl_graph()
  

df_nodes <- graph_object_state_compare %>%
  activate(nodes) %>%
  as_tibble() %>%
  mutate(node_id = row_number())

# df_nodes <- node_positions %>% 
#   select(name = NAME)

df_edges <- graph_object_state_compare %>% 
  activate(edges) %>% 
  as_tibble() %>% 
  left_join(df_nodes %>% rename(from_state = name), by = c("from" = "node_id")) %>% 
  left_join(df_nodes %>% rename(to_state = name), by = c("to" = "node_id")) %>%
  left_join(node_positions %>% mutate(node_id = row_number()) %>% rename(node_id_from = node_id), 
            by = c("from_state" = "NAME")) %>% 
  left_join(node_positions %>% mutate(node_id = row_number()) %>% rename(node_id_to = node_id), 
            by = c("to_state" = "NAME")) %>% 
  select(node_id_from, node_id_to, year, migration) %>% 
  rename(from = node_id_from, to = node_id_to)

graph_object_compare <- tbl_graph(nodes = node_positions %>% select(name = NAME), edges = df_edges) %>% 
  activate(edges) %>% 
  left_join(node_positions %>% mutate(node_id = row_number()) %>% rename(node_id_from = node_id, from_state = NAME), 
            by = c("from" = "node_id_from"))
  
manual_layout_compare <- create_layout(graph = graph_object_compare,
                               layout = node_positions)  

manual_layout_compare %>% 
  ggraph() +
    geom_sf(data = df_states) +
    geom_node_point(alpha = 0) +
    geom_edge_fan(aes(edge_alpha = migration, edge_width = migration, color = as.factor(from_state)), 
                  strength = 2, n = 10, check_overlap = TRUE, lineend = "round") +
    #geom_edge_parallel(aes(edge_alpha = migration, edge_width = migration, color = as.factor(from_state))) +
    facet_wrap(~from_state) +
    # arrow = arrow(length = unit(4, 'mm')),
    # start_cap = circle(1, 'mm'),
    # end_cap = circle(1, 'mm')) +
    coord_sf(xlim = c(-178.490999, -63.310338), ylim = c(13.659001, 73.846275)) +
    scale_edge_alpha_continuous(range = c(.3, .9)) +
    scale_edge_width_continuous(range = c(0, 3)) +
    theme_bw() 

compare_states_graph <- manual_layout_compare %>% 
  ggraph() +
  geom_sf(data = df_states) +
  geom_node_point(alpha = 0) +
  geom_edge_fan(aes(edge_alpha = migration, edge_width = migration, color = as.factor(from_state)), 
                lineend = "round") +
  coord_sf(xlim = c(-178.490999, -63.310338), ylim = c(13.659001, 73.846275)) +
  scale_edge_alpha_continuous(range = c(.3, .9)) +
  scale_edge_width_continuous(range = c(1, 3)) +
  theme_bw() 

network_animation_compare_states <- compare_states_graph +
  transition_states(from_state) +
  labs(title = str_c("State from:", "{closest_state}", sep = " "))

anim_save("output/test_network_animation_compare_states.gif", animation = network_animation_compare_states, width = 1200, height = 1200)
