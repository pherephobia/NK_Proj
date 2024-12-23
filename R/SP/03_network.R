## Project: NK Prison Database Analysis
## Log:
##    - 2024-10-02 Make dyads of detainees and perpetrators
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


### Re-do Jieun's one -----------------------------------------------
### Notes for both
### node size: by centrality (igraph: outdegree)
### clusters: find clusters within clusters (facilities) --> igraph: louvin community detection --> colors
perpetrators <- read_csv("Data/NKPD_perpetrators.csv") |> janitor::clean_names()
detainees_perpetrator <- readRDS("Data/sp_analysis_data/detainees_perpetrator.rds")

### perpetrator data ---------------
detainees_perpetrator_cl <- detainees_perpetrator %>%
  dplyr::select(
    id = perp_id,
    syear = det_start_year,
    eyear = det_end_year,
    facility = perp_related_penal_facility,
    perp_nationality,
    related_detainee = det_id,
    perp_organisational_affiliation,
    perp_age_range,
    perp_rank,
    perp_affiliated_province,
    perp_type_of_penal_facility
  ) %>%
  separate_rows(facility, sep = "\\|") %>%
  mutate(
    id = gsub("perp-", "", id),
    time = purrr::map2(syear, eyear, ~ seq(.x, .y))
  ) %>%
  dplyr::select(id, time, facility, everything())

dyadsedge <- detainees_perpetrator_cl %>%
  unnest(time) %>% 
  group_by(facility, time) %>% 
  summarise(ids = list(id), .groups = "drop") %>% 
  filter(lengths(ids) > 1) %>%
  mutate(dyads = map(ids, ~ combn(.x, 2, simplify = FALSE))) %>%
  unnest(dyads) %>%
  unnest_wider(dyads, names_sep = "_") %>%
  rename(from = dyads_1, to = dyads_2) %>%
  filter(from != to) %>%
  dplyr::select(from, to, time, facility)

dyads <- dyadsedge %>% 
  group_by(from, to, facility) %>% 
  summarise(
    weight = n(),
    time_range = paste0(min(time), "-", max(time)), 
    .groups = "drop"
  ) %>% 
  arrange(facility, from, to) # Arrange for better readability

hist(dyads$weight)
table(dyadsedge$time)


dyads_noweight <- dyads |> dplyr::filter(weight > 2)

actors <- data.frame(
  names = unique(dyads_noweight$from)
)
dyads
summary(dyads_noweight$weight)

g <- graph_from_data_frame(dyads_clean |> 
                             dplyr::filter(weight > 6),
                           directed=TRUE, vertices=actors$names)
table(vertex_degree)
E(g)$width <- log(E(g)$weight+1) / 3
V(g)$label <- ifelse(vertex_degree > 57, V(g)$name, NA)
V(g)$size=igraph::degree(g)%>%log()*2
V(g)$color <- ifelse(
  vertex_degree <= 4, 
  "grey80", 
  ifelse(
    vertex_degree <= 15, 
    "gold", 
    ifelse(
      vertex_degree <= 21,
      "#7BBC53FF",
      "#DE6736FF"
    )
  )
)

vertex_degree |> as.data.frame() |> rownames_to_column() |> arrange(desc(vertex_degree))

library(ggraph)

ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.5, size = 0.2) +
  geom_node_point(size = log(igraph::degree(g)+1)*2, 
                  aes(fill = V(g)$color),
                  alpha = 0.8, shape = 21, color = "grey20") +
  geom_node_label(
    aes(label = V(g)$label), hjust = 0, vjust = 0, repel = T) +
  scale_fill_manual(labels = c("Degree <= 4", 
                               "Degree > 4 & Degree <=15",
                               "Degree > 15 & Degree <= 21", 
                               "Degree > 21"),
                    values = c("#DE6736FF", "gold", "#7BBC53FF", "grey80")) +
#  scale_fill_manual(values = carto_pal(7, "Vivid")) +
  theme_void()  
plot(g,
     edge.arrow.size = 0.1,
     vertex.color = V(g2)$color,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = .5,
     vertex.label.dist = 0,
     edge.curved = 0.5,
     layout = co)

