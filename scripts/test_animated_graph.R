library(tidyverse) # For everything
library(gganimate) # For animating graphs
library(ggraph) # For plotting graphs
library(magick) # For exporting gif
library(tidygraph) # For manipulating graphs
library(colorspace) # For sequential_hcl
library(igraph) # For random walk and as_ids



random_traverse_fxn <- function(i, graph, steps = 6){
  # Return table of edges traversed from a random walk starting at node i of a graph
  rand_walk  <- random_walk(gr, i, steps = steps, stuck = "return", mode = "all") # actual random walk
  trav_nodes <- as_ids(rand_walk) # Get nodes that were traversed 
  trav_nodes_tbl <- enframe(name = NULL, trav_nodes) %>% rename(from = value) # Put nodes in table
  trav_edges <- trav_nodes_tbl %>%     # Make edges from node list
    mutate(to = lead(from)) %>%      # Initial node + next node = traverse edge
    filter(!is.na(to))               # Last lead row has NA, remove it
  trav_edges$step <- seq(1, steps - 1) # Order that each edge was traversed
  trav_edges$walk_start <- i # Node that the path started on
  return(trav_edges)
}

set.seed(42)

# Make a graph
# play_islands makes a graph with dense cliques for the walker to get stuck in
gr <- play_islands(5, 10, 0.8, 3) %>% 
  mutate(name = rownames(.N())) # create node names

gr

# Number of steps for the walker to take
steps = 51

# I'm starting just one walker, but option to start multiple walkers with a vector in first argument
# ex. map_df(c(1,1,1,1,1), random_traverse_fxn, gr, 5) starts 5 walkers at node 1 that each take 5 steps
#     map_df(c(1,5,11,16,18,25), random_traverse_fxn, gr, 3) starts 6 walkers at different nodes that each take 3 steps. 
# This line starts one walker that takes 51 steps through the graph
walked_path <- map_df(1, random_traverse_fxn, gr, steps) %>% mutate(walked_edge = TRUE)

walked_path

# Turn edges from random_traverse_fxn into graph
gr_walked_path <- as_tbl_graph(walked_path)
gr_walked_path

# Get only the edges to add onto the original graph, don't want nodes
walked_path_edges <- gr_walked_path %>% activate('edges') %>% as_tibble()

walked_path_edges

# Add walked edges to the original graph
gr_w_walked_path <- bind_edges(gr, walked_path_edges) # Doesn't change nodes

gr_w_walked_path

# Preformatting for plot
# I want the full network to be displayed the whole time in grey
# Approach: have the first frame be the whole graph
#           and the following frames the walked paths
gr_w_walked_path <- gr_w_walked_path %>% activate("edges") %>%
  mutate(step = case_when(is.na(step) ~ 0, TRUE ~ as.numeric(step))) %>% # Make unwalked edges appear first
  mutate(walk_start = case_when(is.na(walk_start) ~ 0, TRUE ~ as.numeric(walk_start))) %>% # Only really needed for multiple walkers
  mutate(edge_alpha = case_when(walked_edge == TRUE ~ 0.5)) # Forcing alpha for walked edges

gr_w_walked_path

# A sequential palette where the first value is grey
custom_palette <- c("grey50", rev(sequential_hcl(palette = "OrYel", n = steps - 1)))

p <- ggraph(gr_w_walked_path, 'kk') + 
  geom_edge_link0(aes(edge_color = as.factor(step), edge_width = walked_edge, alpha = edge_alpha)) + 
  scale_edge_colour_manual(values = custom_palette) +
  scale_edge_width_discrete(range = c(1.5,1.5), na.value = 0.5) + 
  scale_alpha(range=c(0,1),  na.value = 1) + 
  ggtitle('Random walker - Step {closest_state}') +
  theme_graph() + 
  theme(plot.background = element_rect(fill = 'black'), 
        plot.subtitle = element_text(color = '#F39B4C'),
        plot.title = element_text(color = '#F39B4C'),
        legend.position = "none") +
  transition_states(step)+  #If multiple walkers, can add walk_start here
  shadow_mark() + # Necessary so that the whole graph (first frame) stays the whole time
  NULL

anim <- animate(p, start_pause = 10, end_pause = 10,height = 500, width = 600, nframes = 150)

anim_save("output/random_walker.gif", anim)
