library(dplyr)
library(lubridate)
library(mice)
library(ggplot2)
library(naniar)

prepare_data <- function(raw_data) {
  casualties <- raw_data$casualties
  accidents <- raw_data$accidents
  vehicles <- raw_data$vehicles

  data <- casualties %>%
    filter(casualty_class == "Pedestrian") %>%
    left_join(accidents, by = "accident_index") %>%
    left_join(vehicles, by = c("accident_index", "vehicle_reference"), relationship = "many-to-many") %>%
    mutate(across(everything(), ~ ifelse(. == -1, NA, .))) %>%
    mutate(across(
      where(~ is.character(.) | is.factor(.)),
      ~ na_if(trimws(as.character(.)), "Unknown")
    )) %>%
    mutate(across(
      where(~ is.character(.) | is.factor(.)),
      ~ na_if(., "Not known")
    )) %>%
    select(
      casualty_severity,
      obs_date,
      light_conditions,
      weather_conditions,
      road_surface_conditions,
      special_conditions_at_site,
      carriageway_hazards,
      speed_limit_mph,
      road_type,
      junction_detail,
      junction_control,
      pedestrian_crossing_human_control,
      pedestrian_crossing_physical_facilities,
      pedestrian_location,
      pedestrian_movement,
      sex_of_casualty,
      age_of_casualty,
      casualty_home_area_type,
      sex_of_driver,
      age_of_driver,
      driver_home_area_type,
      vehicle_type,
      vehicle_manoeuvre
    ) %>%
    select(where(~ mean(is.na(.)) < 0.5)) %>% # drop columns with >50% missing
    mutate(across(where(is.character), as.factor))

  imputed <- mice(data, method = "cart", m = 5, maxit = 5, printFlag = FALSE)
  completed_data <- complete(imputed)

  # Feature engineering
  completed_data %>%
    mutate(
      obs_date_parsed = as_datetime(as.numeric(obs_date), tz = "UTC"),
      weekday = wday(obs_date_parsed, label = TRUE),
      hour = hour(obs_date_parsed),
      month = month(obs_date_parsed, label = TRUE),
      year = year(obs_date_parsed),
      light_simple = case_when(
        light_conditions == "Daylight" ~ "Daylight",
        light_conditions == "Darkness - lights lit" ~ "Lit Darkness",
        TRUE ~ "Unlit Darkness"
      ),
      weather_simple = case_when(
        grepl("^Fine", weather_conditions) ~ "Clear",
        grepl("^Rain", weather_conditions) | grepl("Fog", weather_conditions) ~ "Rain",
        grepl("^Snow", weather_conditions) ~ "Snow",
        weather_conditions %in% c("Other", "Unknown") ~ "Other",
        TRUE ~ "Unknown"
      ),
      road_surface_simple = case_when(
        road_surface_conditions == "Dry" ~ "Dry",
        road_surface_conditions == "Wet or damp" ~ "Wet",
        road_surface_conditions %in% c("Frost or ice", "Snow", "Flood over 3cm. deep") ~ "Slippery",
        TRUE ~ "Unknown"
      ),
      carriageway_hazard_simple = case_when(
        carriageway_hazards == "None" ~ "None",
        carriageway_hazards == "Pedestrian in carriageway - not injured" ~ "Pedestrian Hazard",
        TRUE ~ "Object Hazard"
      ),
      speed_category = case_when(
        speed_limit_mph <= 30 ~ "Low",
        speed_limit_mph <= 50 ~ "Medium",
        speed_limit_mph > 50 ~ "High"
      ),
      casualty_age_group = case_when(
        age_of_casualty <= 15 ~ "Child",
        age_of_casualty <= 24 ~ "Youth",
        age_of_casualty <= 44 ~ "Adult",
        age_of_casualty <= 64 ~ "Middle-aged",
        age_of_casualty >= 65 ~ "Senior"
      ),
      home_area_binary = case_when(
        casualty_home_area_type == "Urban area" ~ "Urban",
        casualty_home_area_type %in% c("Small town", "Rural") ~ "Non-urban"
      ),
      driver_age_group = case_when(
        age_of_driver <= 24 ~ "Young",
        age_of_driver <= 39 ~ "Adult",
        age_of_driver <= 59 ~ "Middle-aged",
        age_of_driver <= 74 ~ "Senior",
        age_of_driver >= 75 ~ "Elderly"
      )
    ) %>%
    select(-c(
      obs_date_parsed, obs_date, light_conditions, weather_conditions,
      road_surface_conditions, carriageway_hazards, speed_limit_mph,
      age_of_casualty, casualty_home_area_type, age_of_driver
    )) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(speed_category = factor(speed_category, levels = c("Low", "Medium", "High")))
}
