## Project: NK Prison Database Analysis
## Log:
##    - 2024-10-02 Make dyads of detainees and peerpetrators
##    - 2024-10-08 Make wide to long for other variables (e.g., violation type)

## Load packages in use
if (!require("pacman")) install.packages("pacman")
pacman::p_load(janitor, lubridate, sf, gridExtra, tools, 
               viridis, patchwork, scales, knitr, igraph, tidyverse)

perpetrators_cl |> 
  dplyr::select(perp_id, perp_related_penal_facility) |> 
  mutate(perp_related_penal_facility_split = str_split(perp_related_penal_facility, "\\|")) |> 
  unnest_wider(perp_related_penal_facility_split, names_sep = "_") |>
  dplyr::select(perp_id, contains("perp_related_penal_facility_split")) |> 
  tidyr::pivot_longer(
    cols = c(perp_related_penal_facility_split_1:perp_related_penal_facility_split_3),
    names_to = "number",
    values_to = "facility"
  ) |> dplyr::select(-number) |> drop_na(facility) -> 
  perp_A

perp_A

perp_B <- perp_A

perp_A <- perp_A |> rename(perpA_id = perp_id)
perp_B <- perp_B |> rename(perpB_id = perp_id)

merged_data <- perp_A %>%
  inner_join(perp_B, by = "facility") %>%
  filter(perpA_id != perpB_id)

library(igraph)
library(ggraph)
library(tidygraph)
# Create the graph from dyadic data (no facility coloring)
graph_data <- merged_data %>%
  select(perpA_id, perpB_id)  # Only select perpetrator pairs

# Create an igraph object (undirected graph)
g <- graph_from_data_frame(d = graph_data, directed = FALSE)

# Step 2: Use community detection to find clusters (using Louvain algorithm)
communities <- cluster_louvain(g)

# Add community membership as a vertex attribute
V(g)$community <- membership(communities)
# Visualize the graph with ggraph, coloring nodes by facility
# Visualize the graph with ggraph
ggraph(g, layout = "fr") +  # Fruchterman-Reingold layout for better clustering
  geom_edge_link(colour='#cfcbc4', edge_width=1, show.legend = F) +  # Transparent edges
  geom_node_point(aes(color = factor(community)), size = 3, show.legend = F) +  # Color nodes by community
  geom_node_text(aes(label = name), repel = TRUE, size = 2) +  # Labels with repulsion to avoid overlap
  theme_void() +  # Clean background
  viridis::scale_color_viridis(discrete = T)+ 
  theme(legend.position = "right")  # Show legend for community
