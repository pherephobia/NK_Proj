perp <- read_csv("Data/NKPD_perpetrators.csv")
det <- read_csv("Data/NKPD_detainees.csv")
detainees_perpetrator_viol_wide <- readRDS("Data/sp_analysis_data/detainees_perpetrator_viol_wide.rds")

## Load packages in use
if (!require("pacman")) install.packages("pacman")
pacman::p_load(janitor, lubridate, sf, gridExtra, tools, 
               viridis, patchwork, scales, knitr, igraph, tidyverse)
detainees_perpetrator_viol_wide |> 
  mutate(date = paste0(det_start_year, "-", det_end_year)) -> detainees_perpetrator_viol_wide
names(detainees_perpetrator_viol_wide)

detainees_perpetrator_viol_wide |> 
  dplyr::filter(Torture == 1L) -> toture


toture |> dplyr::select(
  det_id, perp_id, date, Torture, facility = perp_related_penal_facility,
  perp_age_range,
  perp_rank
) -> perp_facility_torture


library(dplyr)
library(tidyr)

# 1. date를 연도 단위로 분리
perp_facility_torture <- perp_facility_torture %>%
  separate(date, into = c("start_year", "end_year"), sep = "-", convert = TRUE) %>%
  mutate(end_year = ifelse(is.na(end_year), start_year, end_year))

torture_expanded <- perp_facility_torture %>%
  rowwise() %>%
  mutate(year_range = list(seq(start_year, end_year))) %>%
  unnest(year_range) %>%
  rename(year = year_range) |> 
  dplyr::select(-start_year, -end_year, -Torture)

# 2. 각 연도를 포함하는 범위로 확장
perp_expanded <- perp_facility_torture %>%
  rowwise() %>%
  mutate(year_range = list(seq(start_year, end_year))) %>%
  unnest(year_range) %>%
  rename(year = year_range) %>%
  select(-start_year, -end_year)

# 3. 동일한 연도, det_id에서 공통적으로 활동한 perpA와 perpB 식별
dyads <- perp_expanded %>%
  rename(perpA = perp_id) %>%
  inner_join(perp_expanded, by = c("det_id", "year")) %>%
  dplyr::filter(perpA < perp_id) %>%  # 중복 제거: perpA < perpB 조건 추가
  rename(perpB = perp_id) %>%
  distinct(perpA, perpB, det_id, year)
getwd()
write_csv(dyads, "Data/sp_analysis_data/dyads.csv")
# 4. 결과물 확인
print(dyads, n = 20)

link_data <- dyads %>% 
  select(perpA, perpB, year) %>% # 필요한 열만 선택
  rename(perp_A = perpA, perp_B = perpB) %>% # 열 이름 변경
  mutate(link = 1) |>  # link 열 추가 및 1로 설정 
  arrange(perp_A, perp_B, year)

# Remove rows where perpA and perpB are the same
dyads <- dyads[dyads$perpA != dyads$perpB, ]

# Group by year
results <- dyads %>%
  group_by(year) %>%
  group_split() %>%
  lapply(function(year_data) {
    # Create an edge list
    edges <- year_data[, c("perpA", "perpB")]
    
    # Create a graph object
    g <- graph_from_data_frame(edges, directed = F)
    
    # Calculate outdegree
    outdegree <- degree(g, mode = "out")
    
    # Perform Louvain community detection
    louvain <- cluster_louvain(g)
    
    # Prepare the result for this year
    result <- data.frame(
      name = V(g)$name,
      det_id = sapply(V(g)$name, function(x) {
        match_row <- year_data[year_data$perpA == x | year_data$perpB == x, ]
        if (nrow(match_row) > 0) return(match_row$det_id[1]) else return(NA)
      }),
      year = unique(year_data$year),
      outdegree = outdegree,
      louvain = membership(louvain)
    ) |> as_tibble()


  })

# Combine results for all years
final_results <- do.call(rbind, results)

link_data |> writexl::write_xlsx("Data/sp_analysis_data/edgelist.xlsx")

# View the final results
final_results |> writexl::write_xlsx("Data/sp_analysis_data/nodelist.xlsx")

final_results |>
  dplyr::select(name, det_id, year) |> 
  left_join(
    torture_expanded, by = c("name" = "perp_id", "det_id", "year")
  ) |> 
  writexl::write_xlsx("Data/sp_analysis_data/toture_perpCV_full.xlsx")


# Load necessary libraries
library(tidyverse)
library(igraph)
library(ggraph)

# Iterate over each year to create network plots
unique_years <- unique(final_results$year)

for (yr in unique_years) {
  # Filter data for the current year
  year_data <- final_results %>% filter(year == yr)
  
  # Create edge list
  edges <- year_data %>%
    select(from = name, to = det_id, outdegree, louvain)
  
  # Create igraph object
  g <- graph_from_data_frame(edges, directed = FALSE)
  
  # Add edge and node attributes
  E(g)$weight <- edges$outdegree  # Edge thickness
  E(g)$louvain <- as.factor(edges$louvain)  # Edge Louvain
  V(g)$outdegree <- degree(g, mode = "out")  # Node degree
  V(g)$louvain <- as.factor(E(g)$louvain[match(V(g)$name, edges$from)])
  
  # Network plot for the current year
  plot <- ggraph(g, layout = "fr") +  # Fruchterman-Reingold layout
    geom_edge_link(aes(width = weight, color = louvain), alpha = 0.7,
                   show.legend = F) +
    geom_node_point(aes(size = outdegree, color = louvain), shape = 21, 
                    fill = "white", show.legend = F) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3, show.legend = F) +
    scale_edge_width(range = c(0.5, 3), guide = "none") +  # Adjust edge thickness range
    scale_edge_color_brewer(palette = "Set2") +  # Color edges by Louvain groups
    theme_minimal() +
    ggtitle(paste("Network Plot for Year", yr)) +
    theme_void() + ggpubr::theme_transparent()
  
  # Save the plot
  ggsave(paste0("Output/", yr, ".png"), plot, width = 10, height = 6, dpi = 400)
}


det_prevalence <- final_results %>%
  group_by(det_id, year) %>%
  summarize(
    total_outdegree = sum(outdegree, na.rm = TRUE),
    avg_outdegree = mean(outdegree, na.rm = TRUE),
    num_perps = n_distinct(name),
    .groups = "drop"
  )

det_prevalence_summary <- det_prevalence %>%
  group_by(det_id) %>%
  summarize(
    total_outdegree_overall = sum(total_outdegree, na.rm = TRUE),
    avg_outdegree_overall = mean(avg_outdegree, na.rm = TRUE),
    total_perps = sum(num_perps, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_outdegree_overall))

yearly_network_metrics <- final_results %>%
  group_by(year) %>%
  group_split() %>%
  lapply(function(data) {
    # Create graph
    g <- graph_from_data_frame(
      d = data,
      directed = FALSE
    )
    
    # Calculate centrality measures
    node_metrics <- data.frame(
      name = V(g)$name,
      degree = degree(g),
      closeness = closeness(g, normalized = TRUE),
      betweenness = betweenness(g, normalized = TRUE),
      year = unique(data$year)
    )
    
    return(node_metrics)
  }) %>%
  bind_rows()

persistent_nodes <- yearly_network_metrics %>%
  group_by(name) %>%
  summarize(
    avg_degree = mean(degree, na.rm = TRUE),
    avg_closeness = mean(closeness, na.rm = TRUE),
    avg_betweenness = mean(betweenness, na.rm = TRUE),
    active_years = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_degree)) %>%
  filter(active_years > 1)  # Only nodes active in multiple years

community_analysis <- final_results %>%
  group_by(year) %>%
  group_split() %>%
  lapply(function(data) {
    g <- graph_from_data_frame(data, directed = FALSE)
    louvain <- cluster_louvain(g)
    community_sizes <- sizes(louvain)
    
    # Summarize community data
    data.frame(
      year = unique(data$year),
      community_id = seq_along(community_sizes),
      size = community_sizes
    )
  }) %>%
  bind_rows()

library(ggplot2)

ggplot(det_prevalence, aes(x = year, y = total_outdegree, color = det_id)) +
  geom_line(show.legend = F) +
  labs(
    title = "Outdegree Over Time",
    x = "Year",
    y = "Total Outdegree"
  ) +
  theme_minimal()

network_summary <- final_results %>%
  group_by(year) %>%
  summarize(
    avg_outdegree = mean(outdegree, na.rm = TRUE),
    total_outdegree = sum(outdegree, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(network_summary, aes(x = year)) +
  geom_line(aes(y = avg_outdegree, color = "Average Outdegree")) +
  geom_line(aes(y = log(total_outdegree), color = "Ln(Total Outdegree)")) +
  labs(
    title = "Network Outdegree Trends Over Time",
    x = "Year",
    y = "Outdegree"
  ) +
  theme_minimal()
