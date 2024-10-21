rm(list = ls())      
library(tidyverse);library(sf);library(gridExtra);library(tools);library(RColorBrewer);library(viridis);library(patchwork);library(scales);library(knitr)
library(dplyr, warn.conflicts = FALSE);library(codebookr)

# The data Sanghoon made (detainees_perpetrator_viol_wide)
nkdyad <- read_csv("NKdyad.csv")

# City of penal facilities data 
locations <- readxl::read_xlsx("facilities_city.xlsx")

# North korea shape file
north_korea <- st_read("PRK_adm2.shp")

nkdyad1 <- nkdyad %>%
  separate_rows(
    det_related_penal_facilities,
    det_province_penal_facility,
    det_penal_facility_affiliation,
    det_type_of_penal_facility,
    sep = "\\|"
  )

nkdyad1 <- nkdyad1 %>%
  mutate(det_related_penal_facilities = str_trim(det_related_penal_facilities))

nkdyadcom <- nkdyad1 %>%
  left_join(locations, by= c("det_related_penal_facilities" = "facilities")) 

north_korea <- north_korea %>%
  mutate(
    city_kr = gsub("[^[:alnum:] ]", "", NAME_2)
  )

nkjoin <- north_korea %>% full_join(nkdyadcom, by = c("city_kr"))


nkjoin <- nkjoin %>%
  dplyr::select(-ID_0, -NAME_0, -ISO, -NAME_2, -HASC_2, -CCN_2, -CCA_2, -NL_NAME_2, -VARNAME_2, -"...1", -Province) 

nkjoin <- nkjoin %>%
  dplyr::select(ID_1, NAME_1, ID_2, city_kr, everything())

################
#codebook

library(labelled)

var_label(nkjoin) <- list(
  ID_1 = "Numeric variable indicating the province ID.", 
  NAME_1 = "Name of provinces.",
  ID_2 = "Numeric variable indicating the city/county ID.",
  city_kr = "Name of the city/county.",
  TYPE_2 = "Indicates if the location is a city or county (in Korean).",
  ENGTYPE_2 = "Indicates if the location is a city or county (in English).",
  det_id = "Detainee identification number.",
  perp_id = "Perpetrator identification number.",
  det_start = "Start date of the human rights violation (day/month/year).",
  det_end = "End date of the human rights violation (day/month/year).",
  det_start_year = "Start year of the human rights violation.",
  det_end_year = "End year of the human rights violation.",
  det_vulnerable_population = "Character variable providing information on whether the detainee belongs to a vulnerable population.",
  det_age_range = "Information about the age range of the detainee.",
  det_gender = "Information about the gender of the detainee.",
  det_status = "Information about the detainee's status.",
  det_violation = "Type of human rights violation.",
  det_related_penal_facilities = "Penal facilities where the violation occurred.",
  det_province_penal_facility = "The province of the penal facilities where the violation occurred.",
  det_penal_facility_affiliation = "The affiliation of the penal facilities where the violation occurred.",
  det_type_of_penal_facility = "The type of penal facilities where the violation occurred.",
  det_event = "Indicates the number of violation events the individual detainee experienced.",
  detainee_number = "The number of detainees related to the individual perpetrator in the row.",
  perp_nationality = "The nationality of the perpetrators.",
  perp_organisational_affiliation = "The affiliation of the perpetrator.",
  perp_age_range = "Information about the age range of the perpetrator.",
  perp_rank = "Information about the rank of the perpetrator.",
  perp_related_penal_facility = "The penal facility related to the individual perpetrator.",
  perp_affiliated_province = "The province related to the individual perpetrator.",
  perp_type_of_penal_facility = "The type of penal facility related to the individual perpetrator.",
  det_vulnerable_population_bi = "A dummy variable indicating whether the detainee belongs to a vulnerable population.",
  det_violation_cl = "The types of violations that an individual detainee experienced.",
  'Deprivation of Liberty' = "A dummy variable indicating whether the detainee experienced deprivation of liberty.",
  'Sexual Violence' = "A dummy variable indicating whether the detainee experienced sexual violence.",
  Torture = "A dummy variable indicating whether the detainee experienced torture.",
  'Freedom of Expression' = "A dummy variable indicating whether the detainee experienced a violation of freedom of expression.",
  Health = "A dummy variable indicating whether the detainee experienced a violation related to health.",
  'Fair Trial' = "A dummy variable indicating whether the detainee experienced a violation of fair trial rights.",
  'Conscience and Religion' = "A dummy variable indicating whether the detainee experienced a violation of the right to freedom of conscience, thought, and religion.",
  'Forced Labor' = "A dummy variable indicating whether the detainee experienced forced labor.",
  'Children' = "A dummy variable indicating whether the detainee experienced a violation of children's rights.",
  'Life' = "A dummy variable indicating whether the detainee experienced a violation of the right to life.",
  'Disability' = "A dummy variable indicating whether the detainee experienced a violation of the rights of detainees with disabilities.",
  geometry = "Geometric polygons of cities."
)

nkjoin1 <- nkjoin %>%
  st_set_geometry(NULL) 
nkcodebook <- codebook(nkjoin1)
print(nkcodebook, "nk_codebook.docx")

################

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
                            data = nksv,
                            aes(geometry = geometry, label = n),
                            max.overlaps=Inf,
                            show.legend = F,
                            stat = "sf_coordinates") +
  labs(subtitle = "Sexual Violence") +
  theme_void() + theme(legend.title = element_blank(), plot.subtitle = element_text(size = 15))


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
                            data = nkfl,
                            aes(geometry = geometry, label = n),
                            max.overlaps=Inf,
                            show.legend = F,
                            stat = "sf_coordinates") +
  labs(subtitle = "Forced Labor") +
  theme_void() + theme(legend.title = element_blank(), plot.subtitle = element_text(size = 15))

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
                            data = nkt,
                            aes(geometry = geometry, label = n),
                            max.overlaps=Inf,
                            show.legend = F,
                            stat = "sf_coordinates") +
  labs(subtitle = "Torture") +
  theme_void() + theme(legend.title = element_blank(), plot.subtitle = element_text(size = 15))
