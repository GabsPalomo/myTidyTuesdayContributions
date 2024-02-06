## My contribution to Tidy Tuesday using the groundhogs dataset 
## 2024/01/30

## Packages -------------------------------------------------------------------------
library(tidyverse)
library(here)
library(fs)
library(httr2)

## Download data --------------------------------------------------------------------
working_dir <- here::here('2024_01_30_groundhogs', 'data')

groundhogs_all <- httr2::request("https://groundhog-day.com/api/v1/groundhogs/") |> 
  httr2::req_perform() |> 
  httr2::resp_body_json() |> 
  getElement("groundhogs") |>
  # _$groundhogs |> ## For newer versions of R 
  tibble::tibble(data = _) |> 
  tidyr::unnest_wider(data)

groundhogs <- groundhogs_all |> 
  dplyr::select(
    -predictions,
    -successor, # Only 1 groundhog has a successor, we'll mention it in the post.
    -contact
  ) |> 
  janitor::clean_names() |> 
  dplyr::mutate(
    # At least one has a stray comma at the end
    coordinates = stringr::str_remove(coordinates, ",$")
  ) |> 
  tidyr::separate_wider_delim(
    coordinates,
    ",",
    names = c("latitude", "longitude")
  ) |> 
  dplyr::mutate(
    # Correct types
    latitude = as.double(latitude),
    longitude = as.double(longitude),
    is_groundhog = as.logical(is_groundhog),
    active = as.logical(active)
  )

predictions <- groundhogs_all |> 
  dplyr::select(id, predictions) |> 
  tidyr::unnest_longer(predictions) |> 
  tidyr::unnest_wider(predictions) |> 
  dplyr::mutate(shadow = as.logical(shadow))

readr::write_csv(
  groundhogs,
  fs::path(working_dir, "groundhogs.csv")
)
readr::write_csv(
  predictions,
  fs::path(working_dir, "predictions.csv")
)

## Clean data to plot -----------------------------------------------------------
## We only want information from TRUE groundhogs and we want to organize it by year
## and the consesus of the majority of groundhogs on whether or not they saw their shadow
## for a particular year 

groundhogs_true <- groundhogs |> 
  dplyr::filter(type == 'Groundhog')

predictions_groundhogs <- dplyr::left_join(groundhogs_true, 
                                           predictions,
                                           by = join_by('id')) 

yearly_pred_true <- predictions_groundhogs |> 
  group_by(year, shadow) |> 
  summarise(n = n()) |> 
  filter(shadow == TRUE) |> 
  ungroup()

yearly_pred_false <- predictions_groundhogs |> 
  group_by(year, shadow) |> 
  summarise(n = n()) |> 
  filter(shadow == FALSE) |> 
  ungroup()

yearly_pred <- predictions_groundhogs |> 
  group_by(year, shadow) |> 
  summarise(n = n()) |>
  ungroup() |> 
  pivot_wider(names_from = shadow, 
              values_from = n) |> 
  rename('shadow_false' = 'FALSE', 
         'shadow_true' = 'TRUE') |> 
  filter_at(vars('shadow_true', 'shadow_false'),all_vars(!is.na(.))) |> 
  mutate(result = case_when(shadow_true > shadow_false ~ 'long winter', 
                            shadow_false > shadow_true ~ 'early spring', 
                            shadow_true == shadow_false ~ 'tie')) |> 
  select(!'NA')

yearly_pred |> 
  filter(year > 2020) |> 
ggplot()+
  geom_segment(aes(x = shadow_false, y = year, 
                   xend = shadow_true, yend = year))+
  geom_point(aes(x = shadow_false, 
                 y = year, 
                 color = result), 
             size = 4, 
             show.legend = TRUE)+
  theme_classic()
  