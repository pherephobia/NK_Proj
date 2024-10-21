## Project: NK Prison Database Analysis
## Log:
##    - 2024-10-02 Make dyads of detainees and peerpetrators
##    - 2024-10-08 Make wide to long for other variables (e.g., violation type)

## Load packages in use
if (!require("pacman")) install.packages("pacman")
pacman::p_load(janitor, lubridate, sf, gridExtra, tools, RcolorBrewer,
               viridis, patchwork, scales, knitr, tidyverse)

### Draw the map
detainees_perpetrator_viol_wide <- readRDS("Data/sp_analysis_data/detainees_perpetrator_viol_wide.rds")
city <- readxl::read_xlsx("Data/sp_analysis_data/facilities_city.xlsx")

detainees_perpetrator_viol_wide |> 
  mutate(det_related_penal_facilities_split = str_split(det_related_penal_facilities, "\\|")) |>
  unnest_wider(det_related_penal_facilities_split, names_sep = "_") |> 
  pivot_longer(
    cols = c(det_related_penal_facilities_split_1:det_related_penal_facilities_split_8),
    names_to = "det_related_penal_facilities_time",
    values_to = "det_related_penal_facilities_type"
  ) |> drop_na(det_related_penal_facilities_type) ->
  detainees_perpetrator_viol_facility

detainees_perpetrator_viol_facility |> 
  left_join(city, 
            by = c("det_related_penal_facilities_type" = "facilities")) ->
  detainees_perpetrator_viol_facility_city

library(codebookr)
library(dplyr, warn.conflicts = FALSE)

detainees_perpetrator_viol_facility_city_cl <- 
  detainees_perpetrator_viol_facility_city |> dplyr::select(-c(det_related_penal_facilities,
                                                          det_province_penal_facility,
                                                          det_penal_facility_affiliation,
                                                          det_type_of_penal_facility,
                                                          det_event,
                                                          det_violation,
                                                          detainee_number,
                                                          det_violation_cl,
                                                          det_related_penal_facilities_time))

detainees_perpetrator_viol_facility_city_codebook <- codebook(detainees_perpetrator_viol_facility_city)
print(detainees_perpetrator_viol_facility_city_codebook, "Data/codebook/city_codebook.docx")

# North korea shape file
north_korea <- st_read("Data/PRK_adm2.shp")

north_korea <- north_korea %>%
  mutate(
    city_kr = gsub("[^[:alnum:] ]", "", NAME_2)
  )

nkjoin <- north_korea %>% full_join(detainees_perpetrator_viol_facility_city, by = c("city_kr"))

nksv <- nkjoin %>%
  group_by(city_kr) %>%
  summarise(n = sum(`Sexual Violence`, na.rm = TRUE), .groups = "drop") %>%
  mutate(n = replace_na(n, 0),) %>%
  mutate(n_agg = case_when(
    n <  10 ~ 1L,
    n >= 10 & n < 25 ~ 2L,
    n >= 25 & n < 50 ~ 3L,
    n >= 50 & n < 75 ~ 4L,
    n >= 75 & n < 100 ~ 5L,
    n >= 100 & n < 125 ~ 6L,
    n >= 125 & n < 150 ~ 7L,
    n >= 150 & n < 175 ~ 8L,
    n >= 175 & n < 200 ~ 9L,
    n >= 200 ~ 10L,
    T ~ NA_integer_
  ))

nkfl <- nkjoin %>%
  group_by(city_kr) %>%
  summarise(n = sum(`Forced Labor`, na.rm = TRUE), .groups = "drop") %>%
  mutate(n = replace_na(n, 0),) %>%
  mutate(n_agg = case_when(
    n <  10 ~ 1L,
    n >= 10 & n < 25 ~ 2L,
    n >= 25 & n < 50 ~ 3L,
    n >= 50 & n < 75 ~ 4L,
    n >= 75 & n < 100 ~ 5L,
    n >= 100 & n < 125 ~ 6L,
    n >= 125 & n < 150 ~ 7L,
    n >= 150 & n < 175 ~ 8L,
    n >= 175 & n < 200 ~ 9L,
    n >= 200 ~ 10L,
    T ~ NA_integer_
  ))

nkt <- nkjoin %>%
  group_by(city_kr) %>%
  summarise(n = sum(Torture, na.rm = TRUE), .groups = "drop") %>%
  mutate(n = replace_na(n, 0),) %>%
  mutate(n_agg = case_when(
    n <  10 ~ 1L,
    n >= 10 & n < 25 ~ 2L,
    n >= 25 & n < 50 ~ 3L,
    n >= 50 & n < 75 ~ 4L,
    n >= 75 & n < 100 ~ 5L,
    n >= 100 & n < 125 ~ 6L,
    n >= 125 & n < 150 ~ 7L,
    n >= 150 & n < 175 ~ 8L,
    n >= 175 & n < 200 ~ 9L,
    n >= 200 ~ 10L,
    T ~ NA_integer_
  ))

north_korea |>
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf(
    data = nksv,
    aes(geometry = geometry, fill = n_agg),
    alpha = 0.9, show.legend = TRUE, stat = "sf_coordinates") +
  scale_fill_viridis(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    limits = c(1, 10),
    labels = c("Under 10", "10-25", "25-50",
               "50-75", "75-100" ,"100-125", "125-150", "150-175", 
               "175-200", "Over 200"),
    option = "mako",
    direction = -1) +
  ggrepel::geom_label_repel(size = 3,
                            data = nksv |> dplyr::filter(n > 1),
                            aes(geometry = geometry, label = n),
                            max.overlaps=Inf,
                            show.legend = F,
                            stat = "sf_coordinates") +
  labs(subtitle = "Sexual Violence") +
  theme_void() + theme(legend.title = element_blank(), plot.subtitle = element_text(size = 15))

ggsave("Output/figures/fig_sexual_violence_map.pdf", width = 10, height = 6, dpi = 1200)

north_korea |>
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf(
    data = nkfl,
    aes(geometry = geometry, fill = n_agg),
    alpha = 0.9, show.legend = TRUE, stat = "sf_coordinates") +
  scale_fill_viridis(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    limits = c(1, 10),
    labels = c("Under 10", "10-25", "25-50",
               "50-75", "75-100" ,"100-125", "125-150", "150-175", 
               "175-200", "Over 200"),
    option = "mako",
    direction = -1) +
  ggrepel::geom_label_repel(size = 3,
                            data = nkfl |> dplyr::filter(n > 1),
                            aes(geometry = geometry, label = n),
                            max.overlaps=Inf,
                            show.legend = F,
                            stat = "sf_coordinates") +
  labs(subtitle = "Forced Labor") +
  theme_void() + theme(legend.title = element_blank(), plot.subtitle = element_text(size = 15))

ggsave("Output/figures/fig_forced_labor_map.pdf", width = 10, height = 6, dpi = 1200)


north_korea |>
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf(
    data = nkt,
    aes(geometry = geometry, fill = n_agg),
    alpha = 0.9, show.legend = TRUE, stat = "sf_coordinates") +
  scale_fill_viridis(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    limits = c(1, 10),
    labels = c("Under 10", "10-25", "25-50",
               "50-75", "75-100" ,"100-125", "125-150", "150-175", 
               "175-200", "Over 200"),
    option = "mako",
    direction = -1) +
  ggrepel::geom_label_repel(size = 3,
                            data = nkt |> dplyr::filter(n > 1),
                            aes(geometry = geometry, label = n),
                            max.overlaps=Inf,
                            show.legend = F,
                            stat = "sf_coordinates") +
  labs(subtitle = "Torture") +
  theme_void() + theme(legend.title = element_blank(), plot.subtitle = element_text(size = 15))

ggsave("Output/figures/fig_torture_map.pdf", width = 10, height = 6, dpi = 1200)