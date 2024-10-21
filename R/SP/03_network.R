length(unique(NKPD_detainees$Title))
length(unique(detainees_perpetrator_viol_wide$det_id))
length(unique(detainees_perpetrator_viol_wide$perp_id))
names(perpetrators_cl)
library(tidyverse)
detainees_perpetrator_viol_wide |> 
  mutate(det_start_re = lubridate::dmy(det_start),
         det_end_re = lubridate::dmy(det_end)) |> 
  dplyr::select(det_start_re, det_end_re, everything()) |> 
  mutate(diff = det_end_re - det_start_re)
detainees_perpetrator_viol_wide |> 
  dplyr::select(det_id, perp_id, det_start, det_end, det_related_penal_facilities, perp_related_penal_facility) ->
  test

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

perp_A |> full_join(perp_B,
                    by = "facility") |> 
  rowwise() |> 
  dplyr::filter(!perp_id.x %in% perp_id.y) |> 
  ungroup() |> 
  dplyr::filter(perp_id.x > perp_id.y) |> 
  rename(perpA_id = perp_id.x, perpB_id = perp_id.y) ->
  dyads_perp_institute

dyads_perp_institute |> dplyr::select(perpA_id, perpB_id, facility) |> 
  arrange(perpA_id, perpB_id)

# Data preparation
net_perp <- dyads_perp_institute |> dplyr::select(-facility)
net_perp |> distinct() -> net_perp

# Check the number of unique nodes
num_nodes <- length(unique(c(net_perp$perpA_id, net_perp$perpB_id)))

# Create a network object
net <- network(net_perp, directed = FALSE)

# Ensure the facility column is correctly matched with the nodes in the network
# If 'facility' refers to the nodes, ensure each node has one facility assigned

# Assuming dyads_perp_institute still has the facility column:
facility_data <- dyads_perp_institute |> 
  dplyr::filter(perpA_id %in% unique(c(net_perp$perpA_id, net_perp$perpB_id))) |> 
  dplyr::select(facility) |> distinct()

# If the facility data doesn't directly map to the nodes, you might need to match it explicitly.
# Ensure `facility_data` corresponds to the vertices of `net`.

# Assign the 'facility' attribute to nodes
set.vertex.attribute(net, "facility", facility_data$facility)

# Plotting the network with ggnet2
ggnet2(net, 
       color = "facility"),        # Color nodes by facility
       palette = "Set1",          # Color palette for distinct groups
       alpha = 0.3,
       #edge.color = c("color", "grey50"), # Edge color, adjust as necessary
       size = 3,                  # Node size
       #label = TRUE,              # Add labels (if needed)
       #label.size = 3,            # Label size
       #label.color = "black",     # Label color
       edge.size = 0.5)           # Edge thickness
