## Project: NK Prison Database Analysis
## Log:
##    - 2024-10-02 Make dyads of detainees and peerpetrators
##    - 2024-10-08 Make wide to long for other variables (e.g., violation type)

## Load packages in use
if (!require("pacman")) install.packages("pacman")
pacman::p_load(janitor, lubridate, tidyverse)

here::here() |> setwd()
getwd()

## Load data to use ---------------
### deetainees data ---------------
detainees <- read_csv("Data/NKPD_detainees.csv") |> janitor::clean_names()
perpetrators <- read_csv("Data/NKPD_perpetrators.csv") |> janitor::clean_names()
hr <- read_csv("Data/NKPD_HR.csv") |> janitor::clean_names()
facilities <- read_csv("Data/NKPD_penal_facilities.csv") |> janitor::clean_names()

detainees_cl <- 
  detainees |> 
  # select necessary variables
  dplyr::select(
    title, det_vulnerable_population = vulnerable_population, 
    det_age_range = age_range, 
    det_gender = gender, 
    det_status = status, 
    contains("event_"), 
    det_related_penal_facilities = related_penal_facilities, 
    det_province_penal_facility = province_where_the_penal_facility_is_located, 
    det_penal_facility_affiliation = penal_facility_affiliation,
    det_type_of_penal_facility = related_type_of_penal_facility
  ) |> 
  mutate(
    det_id = paste0("det-", title)) |> 
  separate(event_1_date_range, into = c("event_1_start", "event_1_end"), sep = "~", remove = FALSE) |>
  separate(event_2_date_range, into = c("event_2_start", "event_2_end"), sep = "~", remove = FALSE) |> 
  separate(event_3_date_range, into = c("event_3_start", "event_3_end"), sep = "~", remove = FALSE) |> 
  dplyr::select(-c(title, event_1_date_range, event_2_date_range, event_3_date_range))

detainees_event_1 <- detainees_cl |> dplyr::select(-c(contains("_2_"), contains("_3_"))) |> 
  drop_na(event_1_start, event_1_end) |> 
  rename(det_start = event_1_start,
         det_end = event_1_end,
         det_violation = event_1_violations) |> mutate(det_event = 1)

detainees_event_2 <- detainees_cl |> dplyr::select(-c(contains("_1_"), contains("_3_"))) |> 
  drop_na(event_2_start, event_2_end) |> 
  rename(det_start = event_2_start,
         det_end = event_2_end,
         det_violation = event_2_violations) |> mutate(det_event = 2)

detainees_event_3 <- detainees_cl |> dplyr::select(-c(contains("_1_"), contains("_2_"))) |> 
  drop_na(event_3_start, event_3_end) |> 
  rename(det_start = event_3_start,
         det_end = event_3_end,
         det_violation = event_3_violation) |> mutate(det_event = 3)

detainees_long <- bind_rows(
  detainees_event_1, detainees_event_2, detainees_event_3
)

detainees_long |>
  mutate(
    det_start_year = str_extract(det_start, pattern = "\\d{4}"),
    det_end_year = str_extract(det_end, pattern = "\\d{4}")
  ) |> 
  mutate(det_start_year = as.numeric(det_start_year),
         det_end_year = as.numeric(det_end_year)) |> 
  dplyr::select(det_id, det_start_year, det_end_year, everything()) -> 
  detainees_long_cl

### perpetrator data ---------------
perpetrators_cl <- perpetrators |> 
  # select necessary variables
  dplyr::select(
    title, perp_nationality = nationality, 
    related_detainee, perp_organisational_affiliation = organisational_affiliation,
    perp_age_range = event_1_age_range, 
    perp_rank = event_1_perpetrator_rank,
    perp_related_penal_facility = related_penal_facility,
    perp_affiliated_province = affiliated_province,
    perp_type_of_penal_facility = related_penal_facility_type) |> 
  mutate(
    perp_id = paste0("perp-", title)) |> 
  mutate(related_detainee_split = str_split(related_detainee, "\\|")) |> 
  unnest_wider(related_detainee_split, names_sep = "_") |> 
  dplyr::select(-c(title, related_detainee)) |> 
  dplyr::select(perp_id, contains("related_detainee"), everything()) |> 
  tidyr::pivot_longer(
    cols = c(related_detainee_split_1:related_detainee_split_14),
    names_to = "detainee_number",
    values_to = "related_detainee"
  ) |> 
  drop_na(related_detainee) |> 
  mutate(det_id = paste0("det-", related_detainee)) |> 
  dplyr::select(perp_id, det_id, contains("related_detainee"), detainee_number, everything()) |> 
  mutate(detainee_number = str_remove(detainee_number, "related_detainee_split_"),
         detainee_number = as.numeric(detainee_number))

## Data manipulation -----------------------------------------------------------
### join detainees and perpetrators -------------------------------------------
detainees_long_cl |> drop_na(det_id) |> 
  left_join(
    perpetrators_cl |> drop_na(det_id, perp_id) |> 
      dplyr::select(-related_detainee), by = c("det_id" = "det_id")) |> 
  dplyr::select(det_id, perp_id, det_start, det_end, det_start_year, det_end_year, everything()) |> 
  drop_na(perp_id) -> detainees_perpetrator

#### See some nested variables ---
names(detainees_perpetrator)
glimpse(detainees_perpetrator)

#### 1. vulnerable population ---
#### I just made a binary variable to show whether the detainees belong to vulnerable pop or not.
detainees_perpetrator |> 
  mutate(det_vulnerable_population_bi = if_else(det_vulnerable_population %in% "N/A", 0L, 1L)) ->
  detainees_perpetrator

#### 2. violation type ---
#### I did one-hot encoding for violation type. Observations with no violation 
#### type (non-identified) were dropped.
detainees_perpetrator |> 
  mutate(det_violation_cl = gsub("_A[0-9]{4}", "", det_violation)) |> 
  mutate(det_violation_cl = gsub("_[0-9]{1}", "", det_violation_cl)) ->
  detainees_perpetrator_viol

detainees_perpetrator_viol |> 
  mutate(det_violation_cl_split = str_split(det_violation_cl, "\\|")) |>
  unnest_wider(det_violation_cl_split, names_sep = "_") |> 
  pivot_longer(
    cols = c(det_violation_cl_split_1:det_violation_cl_split_37),
    names_to = "det_violation_time",
    values_to = "det_violation_type"
  ) |> mutate(
    det_violation_type = case_when(
      det_violation_type %in% 
        c("Arbitrary Deprivation of Liberty", "Arbitrary Deprivation of Liberty\n",
          "Arbitrary Deprivation of Liberty", "Arbitrary Deprivation of Liberty301",
          "Arbitrary Deprivation of Liberty ") ~ 
        "Deprivation of Liberty",
      det_violation_type %in% 
        c("Forced Labour", "Forced Labour", "Forced Labour ") ~ 
        "Forced Labor",
      det_violation_type %in% 
        c("Rape & Other Forms of Sexual Violence", "Rape & Other Forms of Sexual Violence\t\t",
          "Rape & Other Forms of Sexual Violence\n", "Rape & Other Forms of Sexual Violence0",
          "Sexual and Gender-based Violence") ~ 
        "Sexual Violence",
      det_violation_type %in% 
        c("Torture & CID", "Torture & CID ", "Torture & CID0", "Torture & CID1", 
          "Torture & CID2", "Torture & CID268", "Torture & CID3") ~ 
        "Torture",
      det_violation_type %in% 
        c("Violation of Freedom of Expression", "Violation of Freedom of Expression\n", 
          "Violation of Freedom of Expression", "Violation of Freedom of Expression0", 
          "Violation of Freedom of Expression1", "Violation of Freedom of Expression2", 
          "Violation of Freedom of Expression3", "Violation of Freedom of Expression ") ~ 
        "Freedom of Expression",
      det_violation_type %in% 
        c("Violation of Right to health", "Violation of Right to Health", 
          "Violation of Right to Health\n", "Violation of Right to Health", 
          "Violation of Right to Health0", "Violation of Right to Health1",
          "Violation of Right to Health1", "Violation of Right to Health2",
          "Violation of Right to Health3", "Violation of Right to Health ") ~ 
        "Health",
      det_violation_type %in% "Violation of the Right to a Fair Trial" ~ "Fair Trial",
      det_violation_type %in% 
        c("Violation of the Right to Freedom of Conscience, Thought and Religion", 
          "Violation of the Right to Freedom of Conscience, Thought and Religion\t\t", 
          "Violation of the Right to Freedom of Conscience, Thought and Religion",
          "Violation of the Right to Freedom of Conscience, Thought and Religion0",
          "Violation of the Right to Freedom of Conscience, Thought and Religion1",
          "Violation of the Right to Freedom of Conscience, Thought and Religion2",
          "Violation of the Right to Freedom of Conscience, Thought and Religion3",
          "Violation of the Right to Freedom of Conscience, Thought, and Religion",
          "Violation of the Right to Freedom of Conscience, Through and Religion", 
          "Violation of the Right to Freedom of Conscience, Thought and Religion ") ~ 
        "Conscience and Religion",
      det_violation_type %in% 
        c("Violation of the Right to Life", 
          "Violation of the Right to Life",
          " Violation of the Right to Life ",
          "Violation of the Right to Life ") ~ "Life",
      det_violation_type %in% 
        c("Violation of the Rights of Children") ~ "Children",
      det_violation_type %in% 
        c("Violation of the Rights of Detainees with Disabilities",
          "Violation of the Rights of Detainees with Disability",
          "Violation of the Rights of Detainees with DIsability") ~ "Disability",
      T ~ det_violation_type)
  ) |> dplyr::select(-det_violation_time) -> detainees_perpetrator_viol

detainees_perpetrator_viol_wide <- detainees_perpetrator_viol |> 
  drop_na(det_violation_type) |> 
  mutate(n = 1) |> 
  distinct() |>  
  pivot_wider(names_from = det_violation_type,
              values_from = n) %>% 
  mutate_at(vars(matches(
  "Children|Conscience and Religion|Deprivation of Liberty|Disability|Fair Trial|Forced Labor|Freedom of Expression|Health|Life|Sexual Violence|Torture")), replace_na, 0)

saveRDS(detainees_perpetrator_viol_wide, "Data/sp_analysis_data/detainees_perpetrator_viol_wide.rds")

#### See which violation type is the most prevalent in NK
showtext::showtext_auto(T)
detainees_perpetrator_viol_wide |> 
  dplyr::select(det_gender, 27:37) |> 
  pivot_longer(
    cols = `Deprivation of Liberty`:Disability,
    names_to = "violation type",
    values_to = "event"
  ) |> group_by(`violation type`, event, det_gender) |> count() |> 
  group_by(`violation type`, det_gender) |> 
  mutate(total = sum(n), ratio = n/total) |> 
  dplyr::filter(event == 1L) |> 
  ggplot(aes(x = reorder(`violation type`, ratio), y = ratio, label = paste0(round(ratio*100, 2), "%"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(expand =  c(Inf,Inf)) +
  geom_col(color = "black", fill = "white") + 
  geom_text(aes(label = paste0(round(ratio*100, 2), "%")),
            position = position_stack(0.8), color = "black") +
  # ggrepel::geom_text_repel(
  #   position = position_stack(vjust = 0.5),
  #   size = 8,
  #   direction = "y") +
  labs(x = "Violation Type\n", y = "\nProportion") +
  facet_grid(~det_gender) +
  ggpubr::theme_pubr()
  #coord_flip()

ggsave("Output/figures/fig_distribution_violation_type.pdf", width = 8, height = 4, dpi = 900)  

#### 3. det_related_penal_facilities ---
#### I did one-hot encoding for det_related_penal_facilities. 

table(detainees_perpetrator_viol$det_related_penal_facilities)
detainees_perpetrator_viol |> 
  mutate(det_related_penal_facilities_split = str_split(det_related_penal_facilities, "\\|")) |>
  unnest_wider(det_related_penal_facilities_split, names_sep = "_") |> 
  pivot_longer(
    cols = c(det_related_penal_facilities_split_1:det_related_penal_facilities_split_8),
    names_to = "det_related_penal_facilities_time",
    values_to = "det_related_penal_facilities_type"
  ) |> pull(det_related_penal_facilities_type) |> table() |>
  as.data.frame() |> as_tibble() |> 
  writexl::write_xlsx("Output/shp/list_related_penal_facilities.xlsx")
  
detainees_perpetrator_viol |> 
  mutate(det_province_penal_facility_split = str_split(det_province_penal_facility, "\\|")) |>
  unnest_wider(det_province_penal_facility_split, names_sep = "_") |> 
  pivot_longer(
    cols = c(det_province_penal_facility_split_1:det_province_penal_facility_split_8),
    names_to = "det_province_penal_facility_time",
    values_to = "det_province_penal_facility_type"
  ) |> pull(det_province_penal_facility_type) |> table() |>
  as.data.frame() |> as_tibble() |> 
  writexl::write_xlsx("Output/shp/list_province_penal_facility.xlsx")

detainees_perpetrator_viol |> 
  mutate(det_penal_facility_affiliation_split = str_split(det_penal_facility_affiliation, "\\|")) |>
  unnest_wider(det_penal_facility_affiliation_split, names_sep = "_") |> 
  pivot_longer(
    cols = c(det_penal_facility_affiliation_split_1:det_penal_facility_affiliation_split_8),
    names_to = "det_penal_facility_affiliation_time",
    values_to = "det_penal_facility_affiliation_type"
  ) |> pull(det_penal_facility_affiliation_type) |> table() |>
  as.data.frame() |> as_tibble() |> 
  writexl::write_xlsx("Output/shp/list_penal_facility_affiliation.xlsx")

detainees_perpetrator_viol |> 
  mutate(det_type_of_penal_facility_split = str_split(det_type_of_penal_facility, "\\|")) |>
  unnest_wider(det_type_of_penal_facility_split, names_sep = "_") |> 
  pivot_longer(
    cols = c(det_type_of_penal_facility_split_1:det_type_of_penal_facility_split_8),
    names_to = "det_type_of_penal_facility_time",
    values_to = "det_type_of_penal_facility_type"
  ) |> pull(det_type_of_penal_facility_type) |> table() |>
  as.data.frame() |> as_tibble() |> 
  writexl::write_xlsx("Output/shp/list_type_of_penal_facility_type.xlsx")

table(detainees_perpetrator_viol$perp_organisational_affiliation)

### Codebook?
library(codebookr)
library(dplyr, warn.conflicts = FALSE)

detainees_perpetrator_codebook <- codebook(detainees_perpetrator_viol_wide)
print(detainees_perpetrator_codebook, "Data/codebook/basic_codebook.docx")
