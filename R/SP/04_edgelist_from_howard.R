
perp <- read_csv("Data/NKPD_perpetrators.csv")
det <- read_csv("Data/NKPD_detainees.csv")
detainees_perpetrator_viol_wide <- readRDS("Data/sp_analysis_data/detainees_perpetrator_viol_wide.rds")

## Load packages in use
if (!require("pacman")) install.packages("pacman")
pacman::p_load(janitor, lubridate, sf, gridExtra, tools, 
               viridis, patchwork, scales, knitr, igraph, tidyverse)
detainees_perpetrator_viol_wide |> 
  mutate(date = paste0(det_start_year, "-", det_end_year)) ->detainees_perpetrator_viol_wide
detainees_perpetrator_viol_wide |> 
  dplyr::select(perp_id, date, 20, 21, facility = 22, 27:37) -> perp_cv

### Perp_cv
perp_cv |> 
  separate(date, into = c("start_year", "end_year"), sep = "-") |>
  mutate(
    start_year = as.integer(start_year),
    end_year = as.integer(end_year)
  ) |>
  rowwise() %>%
  mutate(year = list(start_year:end_year)) %>% # Create a list column of years
  unnest(year) %>% # Expand the list column into rows
  select(-start_year, -end_year) |> 
  dplyr::select(perp_id, year, everything()) |> 
  group_by(perp_id, year, facility) |>
  summarise(
    outdegree = n()
  ) -> summary_cv

### Edgelist ------------
perp_cv |> dplyr::select(perp_Aid = perp_id, date, facility) -> perp_A
perp_cv |> dplyr::select(perp_Bid = perp_id, date, facility) -> perp_B

perp_A |> 
  tidyr::expand(perp_Aid, perp_Aid, date) -> full_perp
names(full_perp) <- c("perp_Aid", "perp_Bid", "date")
full_perp |> dplyr::filter(!perp_Aid == perp_Bid) -> full_perp

perp_A |> full_join(perp_B, by = c("date", "facility")) |> mutate(same = 1L) |> 
  dplyr::filter(!perp_Aid == perp_Bid) -> same_date_facility_perp

# Transform the data
expanded_perp <- same_date_facility_perp |>
  separate(date, into = c("start_year", "end_year"), sep = "-") %>%
  mutate(
    start_year = as.integer(start_year),
    end_year = as.integer(end_year)
  ) |>
  rowwise() %>%
  mutate(year = list(start_year:end_year)) %>% # Create a list column of years
  unnest(year) %>% # Expand the list column into rows
  select(-start_year, -end_year) # Drop unnecessary columns

expanded_perp |> dplyr::select(-facility) |> distinct() |> 
  dplyr::select(perp_Aid, perp_Bid, year, link = same) -> edge_list

# Expand the edge list
expanded_edge_list <- edge_list |>
  # Group by perp_Aid and perp_Bid to handle each pair separately
  group_by(perp_Aid, perp_Bid) |>
  # Complete the year sequence for each group
  complete(year = full_seq(year, 1)) |>
  # Replace NA in 'link' with 0 for the newly created years
  mutate(link = 1) |>
  # Ungroup to return to a regular data frame
  ungroup()

### node_list

node_list <- expanded_edge_list |>
  # Combine all unique perp_Aid and perp_Bid into one column
  dplyr::select(perp_Aid, perp_Bid) |>
  pivot_longer(cols = everything(), names_to = "type", values_to = "perp_id") |>
  distinct(perp_id) |>
  # Join with perp_cv to get additional information
  left_join(perp_cv, by = c("perp_id" = "perp_id")) |>
  separate(date, into = c("start_year", "end_year"), sep = "-") %>%
  mutate(
    start_year = as.integer(start_year),
    end_year = as.integer(end_year)
  ) |>
  rowwise() %>%
  mutate(year = list(start_year:end_year)) %>% # Create a list column of years
  unnest(year) %>% # Expand the list column into rows
  dplyr::select(-c(start_year, end_year)) |> 
  dplyr::select(perp_id, year, everything()) |> distinct()
  mutate(
    outdegree = sapply(perp_id, function(id) sum(edge_list$perp_Aid == id | edge_list$perp_Bid == id))) |>
  dplyr::select(name = perp_id, facility, date, outdegree)

  node_list <- expanded_edge_list |>
    group_split(year) |>
    map_df(function(df) {
      # 연도 정보
      current_year <- unique(df$year)
      
      # 노드 리스트 생성
      df |>
        dplyr::select(perp_Aid, perp_Bid) |>
        pivot_longer(cols = everything(), names_to = "type", values_to = "perp_id") |>
        distinct(perp_id) |>
        # Join with perp_cv to get additional information
        left_join(perp_cv, by = c("perp_id" = "perp_id")) |>
        separate(date, into = c("start_year", "end_year"), sep = "-") %>%
        mutate(
          start_year = as.integer(start_year),
          end_year = as.integer(end_year)
        ) |>
        rowwise() %>%
        mutate(year = list(start_year:end_year)) %>% # Create a list column of years
        unnest(year) %>% # Expand the list column into rows
        filter(year == current_year) |> # 현재 연도에 해당하는 노드만 필터링
        dplyr::select(-c(start_year, end_year)) |> 
        dplyr::select(perp_id, year, everything()) |> 
        distinct() |> 
        mutate(
          outdegree = vapply(perp_id, function(id) sum(df$perp_Aid == id | df$perp_Bid == id), numeric(1))
        ) |> 
        dplyr::select(name = perp_id, year, facility, outdegree)
    })

node_list  
  
node_list |> separate(date, into = c("start_year", "end_year"), sep = "-") %>%
  mutate(
    start_year = as.integer(start_year),
    end_year = as.integer(end_year)
  ) |>
  rowwise() %>%
  mutate(year = list(start_year:end_year)) %>% # Create a list column of years
  unnest(year) %>% # Expand the list column into rows
  select(-start_year, -end_year) |> # Drop unnecessary columns |> 
dplyr::select(name, facility, year, outdegree) |> distinct(name, facility, year, outdegree) ->
  node_list_cl

node_list_cl |> left_join(
  perp_cv_full |> dplyr::select(name, facility, year, louvain)
) -> node_list_cl

### CV full ---- need to merge louvain.

node_list_table |> drop_na(name, date, facility) |> 
  left_join(perp_cv |> drop_na(perp_id, date, facility),
            by = c("name" = "perp_id",
                   "date", "facility")) ->
  perp_check

perp_check |> 
  separate(date, into = c("start_year", "end_year"), sep = "-") %>%
  mutate(
    start_year = as.integer(start_year),
    end_year = as.integer(end_year)
  ) |>
  rowwise() %>%
  mutate(year = list(start_year:end_year)) %>% # Create a list column of years
  unnest(year) %>% # Expand the list column into rows
  select(-start_year, -end_year) |> 
  distinct(name, facility, year, outdegree, louvain, perp_age_range,
           perp_rank, `Deprivation of Liberty`, `Sexual Violence`,
           Torture, `Freedom of Expression`, Health, `Fair Trial`,
           `Conscience and Religion`, `Forced Labor`, Children, Life, 
           Disability ) ->
  perp_cv_full


perp_cv_full  |> writexl::write_xlsx("Data/sp_analysis_data/perp_cv.xlsx")
expanded_edge_list |> writexl::write_xlsx("Data/sp_analysis_data/edgelist.xlsx")
node_list_cl |> writexl::write_xlsx("Data/sp_analysis_data/nodelist.xlsx")
# Step 2: Create a Louvain-like grouping (if network analysis is required)
# Optional: If you need Louvain clusters, build an igraph network
library(igraph)
list <- same_date_facility_perp %>%
  select(perp_Aid, perp_Bid, date)
g <- graph_from_data_frame(list, directed = F)
louvain_clusters <- cluster_louvain(g)
node_list_table <- node_list %>%
  mutate(louvain = membership(louvain_clusters)[name])

# Step 3: Save or display the transformed data
writexl::write_xlsx(node_list_table, "applelist.xlsx")

# Step 1: Create an edgelist
edge_list <- perp_cv %>%
  select(actorA = perp_id, actorB = det_id, date) %>% 
  mutate(
    year = sub("-.*", "", date), # Extract the starting year from the date range
    link = 1 # Set a constant value for the link (if it's always 1)
  ) %>%
  select(actorA, actorB, year, link)

# Step 2: Save the edge list as a CSV file
write.csv(edge_list, "edge_list.csv", row.names = FALSE)




head(perp_cv)
perp_cv |> dplyr::select(perp_id, date)

det
perp_B <- perp_A

perp_A <- perp_A |> rename(perpA_id = perp_id)
perp_B <- perp_B |> rename(perpB_id = perp_id)

merged_data <- perp_A %>%
  inner_join(perp_B, by = "facility") %>%
  filter(perpA_id != perpB_id)

setwd("/Users/howardliu/Documents/NAVCO/NAVCO3.0/navco3-0countrysheets/")

# pkg
source('/Users/howardliu/Dropbox/Dissertation/Rcode/loadPkg.R')

pacman::p_load('foreign','dplyr', 'reshape2', 'readr','readxl', 'lubridate', # for date
          'ggplot2', 'magrittr',
          'stringr','stringi',
          'statnet','igraph',
          'amen', 'eigenmodel', 'latentnet')

  
  ### same facility: 
  # same title, 2) violence type
  
  ## 1. only 

  # anti-gov only
  perp_A$facility%>% table %>% .[order(., decreasing = T)]
  perp_A$perp_id %>% table %>% .[order(., decreasing = T)]
  navco$cameo_actor_3 %>% table %>% .[order(., decreasing = T)]
  navco$target_3 %>% table %>% .[order(., decreasing = T)]
  domActors = c("COP", "GOV", "JUD", "SPY", "MIL",
                "OPP", "ACT", "UNS")#,"LLY" )  REB       # not sure I want to include Mubarak supporters??
  domActors = c(domActors, "EGY")
  
  data = navco %>% filter(. , cameo_actor_3 %in% domActors & target_3 %in% domActors) # only dom vs dom events
  data2 = data %>% filter(. , is.na(st_posture) !=TRUE )
  #### 1. create event_id ####
  # may want to have "same location" as a criteria
  
  
  # date format: ymd or mdy ?  data$year = data$date %>% mdy() %>% year(.)
  data$year = data$date %>% ymd() %>% year(.)
  data$month =data$date %>% ymd() %>% month(.) ; data$month = str_pad(data$month, 2, pad = "0") # paste a leading zero if one digit
  data$day = data$date %>% ymd() %>% day(.) ; data$day = str_pad(data$day, 2, pad = "0")
  minYr = data$year %>% unique %>% min
  maxYr = data$year %>% unique %>% max
  
  data$time_id = paste0(data$year,data$month,data$day)
  
  data$event_id <- data %>% group_indices(source_title) ## group by "source_title" !!
  
  event_id = data$event_id %>% unique()
  event_id %>% length # total 1656 events (1991-2012)
  
  data$seq_id = NA
  #ii =1
  for(ii in 1: length(event_id)){
    # count event_id in each group
    seq = data[data$event_id == event_id[ii],]$event_id %>% table %>% as.numeric()
    data$seq_id[data$event_id == event_id[ii]] = 1:seq
  }
  # done
  
  #### 2. create actor-actor verb List ####
  
  actorList = NULL
  ii = 5
  
  for(ii in 1: length(event_id)){
    tmp = data[data$event_id == event_id[ii],]
    actorL = tmp %>% select(. , time_id, event_id, seq_id ,actor_id, cameo_actor_3, target_3, verb_10, verb_100)
    actorList = rbind(actorList,actorL)
    # cat("\r",ii)
    # flush.console()
  }
  
  ## convert to sideDiss -- sideGov
  govActors = c("EGY","COP", "GOV", "JUD", "MIL", "SPY")
  dissActors= c("OPP", "ACT", "UNS")
  ii = 1
  
  actorList$source = NA
  for(ii in 1: nrow(actorList)){
    if ( length(str_match(actorList$cameo_actor_3[ii], govActors) %>% .[!is.na(.)]) >0 ){
      actorList$source[ii] = "G"
    }else{
      actorList$source[ii] = "D"
    }
  }
  
  actorList$target = NA
  for(ii in 1: nrow(actorList)){
    if ( length(str_match(actorList$target_3[ii], govActors) %>% .[!is.na(.)]) >0 ){
      actorList$target[ii] = "G"
    }else{
      actorList$target[ii] = "D"
    }
  }
  
  actorList$action = NA; actorList$action = paste0(actorList$source,"_", actorList$verb_10) # verb_10
  
  ##
  actorList$same_actors = NA
  actorList$same_actors[actorList$source == actorList$target] = "same" # count %
  actorList$same_actors[actorList$source != actorList$target] = "Nsame" # count %
  same_lgth = actorList$same_actors[actorList$same_actors == "same"] %>% .[!is.na(.)] %>% length
  same_lgth/nrow(actorList)
  
  actorList$action %>% table # nodes
  
  # remove same actors
  actorList1 = actorList %>% filter(. , same_actors == "Nsame") # remove 548 rows
  actorList <- actorList1
  
  
  
  ##### 3. Convert to action list: action-reaction edgelist #####
  # Create adj matrix (directed) for each event [m,m] m = c(14,20)
  ## based on events: ties are built within the same event
  
  # --> email Chenoweth my solution: providing event_ID: duration, tactical exchanges
  ii = 8
  event_id %>% length
  actorList[actorList$event_id == event_id[ii], ]
  
  ## how to build direction is very important!!!
  
  ### year ????
  actionList = NULL
  
  for(ii in 1: length(event_id)){
    tmp = actorList[actorList$event_id == event_id[ii], ]
    action = tmp$action %>% .[!duplicated(.)]
    action %>% substring(., 1,1)
    action
    length(unique(action %>% substring(., 1,1)))
    
    if( length(unique(action %>% substring(., 1,1))) >1 ){
      
      D = action[which(!is.na(str_match(action, "D")))] %>% as.list
      G = action[which(!is.na(str_match(action, "G")))] %>% as.list
      actionL = cbind(D,G) %>% as.data.frame()
      
      actionList = rbind(actionList,actionL)
    }
    
    # time sequence (ignore time for now due to bipartite network, no direction) --> hidden markov chain
    # assume there are enough similarity
    cat("\r",ii)
    flush.console()
  }
  
  names(actionList) = c("side_D", "side_G") # no direction, can go either way
  ## Consider to use the full 173, 175 code
  ## in the same day:row沒有按照事情先後
  
  # turn to words
  actionList$side_D = str_replace_all(actionList$side_D, "D_01", "D_statement") %>%
    str_replace_all(., "D_02", "D_frendly appeal") %>%
    str_replace_all(., "D_03", "D_intent to coop") %>%
    str_replace_all(., "D_04", "D_visit") %>%
    str_replace_all(., "D_05", "D_pol coop") %>%
    str_replace_all(., "D_06", "D_mat coop") %>%
    str_replace_all(., "D_07", "D_aid") %>%
    str_replace_all(., "D_08", "D_concede") %>%
    str_replace_all(., "D_09", "D_investigate") %>%
    str_replace_all(., "D_10", "D_demand") %>%
    str_replace_all(., "D_11", "D_accuse") %>%
    str_replace_all(., "D_12", "D_reject") %>%
    str_replace_all(., "D_13", "D_threat") %>%
    str_replace_all(., "D_14", "D_protest") %>%
    str_replace_all(., "D_15", "D_mob force") %>%
    str_replace_all(., "D_16", "D_reduce relation") %>%
    str_replace_all(., "D_17", "D_seize") %>% # civilian only seize property 171
    str_replace_all(., "D_18", "D_assault") %>%
    str_replace_all(., "D_19", "D_fight") %>%
    str_replace_all(., "D_20", "D_mass vio") %>%
    str_replace_all(., "D_21", "D_defect")
  
  actionList$side_G = str_replace_all(actionList$side_G, "G_01", "G_statement") %>%
    str_replace_all(., "G_02", "G_frendly appeal") %>%
    str_replace_all(., "G_03", "G_intent to coop") %>%
    str_replace_all(., "G_04", "G_visit") %>%
    str_replace_all(., "G_05", "G_pol coop") %>%
    str_replace_all(., "G_06", "G_mat coop") %>%
    str_replace_all(., "G_07", "G_aid") %>%
    str_replace_all(., "G_08", "G_concede") %>%
    str_replace_all(., "G_09", "G_investigate") %>%
    str_replace_all(., "G_10", "G_demand") %>%
    str_replace_all(., "G_11", "G_accuse") %>%
    str_replace_all(., "G_12", "G_reject") %>%
    str_replace_all(., "G_13", "G_threat") %>%
    str_replace_all(., "G_14", "G_protest") %>%
    str_replace_all(., "G_15", "G_mob force") %>%
    str_replace_all(., "G_16", "G_reduce relation") %>%
    str_replace_all(., "G_17", "G_coerce") %>%
    str_replace_all(., "G_18", "G_assault") %>%
    str_replace_all(., "G_19", "G_fight") %>%
    str_replace_all(., "G_20", "G_mass vio") %>%
    str_replace_all(., "G_21", "G_defect")
  
  
  ## comm detection (1990-2012)
  # aggregate time: past 6 days or months or years
  actionList$value = 1
  el = actionList
  #el
  el[,1]=as.character(el[,1])
  el[,2]=as.character(el[,2])
  el=as.matrix(el)
  el[,3] =as.numeric(el[,3])
  #el
  
  # 2. convert to a graph object
  g=graph.edgelist(el[,1:2], directed = F)
  E(g)$weight=as.numeric(el[,3])
  #E(g)$weight
  #g = simplify(g)
  #plot(g,  edge.width= E(g)$weight  ) ## weight: how many times in 1991-2012
  
  # L =layout_as_bipartite(g)
  # ?layout_as_bipartite
  # tmp = cluster_louvain(g, weights = E(g)$weight)
  # tmp
  # # store in data frame
  # comms = data.frame(strategies = names(membership(tmp)), # country names
  #                    comm = as.numeric(membership(tmp)), # comm. membership
  #                    algor = algorithm(tmp), # alogirthm used
  #                    #year = timeRange[i], # year of data
  #                    modul = modularity(tmp),
  #                    outDegree = igraph::degree(g, mode = "out"),
  #                    inDegree = igraph::degree(g, mode = "in"))
  #
  # plot(tmp, g, main= "Comm Detection (Louvain)", vertex.size =5, layout = layout_with_fr(g))
  
  
  
  Community_k <- cluster_louvain(g, weights = E(g)$weight)
  
  sameComm = data.frame(tactics = names(membership(Community_k)), # country names
                        comm = as.numeric(membership(Community_k)), # comm. membership
                        algor = algorithm(Community_k), # alogirthm used
                        #year = timeRange[i], # year of data
                        modul = modularity(Community_k),
                        Global_outDegree = igraph::degree(g, mode = "out"),
                        Global_inDegree = igraph::degree(g, mode = "in")
  )
  sameComm$comm %>% max
  #outDegree = degree(yearlyNetwork[[i]], mode = "out")) # modularity
  
  # color it: how many communities
  library(RColorBrewer)
  
  prettyColors <- brewer.pal(max(sameComm$comm), "Dark2")
  #  rainbow(max(sameComm$comm),s = 0.5)
  #  c("turquoise4", "azure4", "olivedrab","deeppink4","tan3",
  #                  "orchid3", "skyblue3")
  communityColors_k <- prettyColors[membership(Community_k)]
  
  # I use the original layout as a base for the new one
  #karate
  #karateLayout
  
  layout_with_fr(g)
  g = simplify(g)
  karateLayout_k <- layout.fruchterman.reingold(g, layout.fruchterman.reingold(g))
  # the graph with the nodes grouped
  
  pdf(paste0("graph/",country,"_net.pdf"),width=10,height=10,paper='special')
  
  plot(x = Community_k, y = g, edge.width = 1, vertex.size = 10,
       mark.groups = NULL, layout = karateLayout_k, col = communityColors_k,
       c("darkgrey","tomato2")[crossing(Community_k, g) + 1], #, vertex.label = NA
       main = paste0("[",country,"]" ," Communities in protest and repression network") )
  mtext(paste0("comms: ", max(sameComm$comm)))
  
  dev.off()
  
}
# -> Kyle: label the groups: 1) protest-arrest-mob-seize group 2) fight-fight group 3) G concede moves 4) D concede moves 5) visit
# give a meaningful classification + something we didn't know before



# write.csv(comms, "/Users/howardliu/Dropbox/Dissertation/data/comms_navco3_verb100.csv")
# centrality

# density



########## stage 2 ###########
## 1) prepare dyad-country-year data. Big dataframe
# need to country se and dyad se

# predictor: sameComm, sameComm*connectivity

## 2) dyad(oneside fixed)-country-year data [like Fariss 4 tables] |解釋：Given D14抗議出現, G會不會越暴力
# need country se

## 3) dyad(oneside fixed)-country(fixed, EGY)-year data [time-series data] <- country specific story (x)

## monadic data frame
# 1. pick the best G as a predictor

# 1. prepare scad_like event data: actual (one) vs predicted (one), if 2 actions 2 rows
