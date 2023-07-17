library(tidyverse)
rm(list = ls())

obj_etc_data <- read_csv("./data/object_legislation_outcome_summary_stats_2019-2021.csv")

stop_data <- read_csv("./data/2023-06-02 - summarised_stops_2019-2021.csv")

# load population data, make it longer to be able to merge with stops data, and
# fix inconsistent names that should have "something of" before them
population_data <-  read_csv("data/census_2021_pop_ests_collapsed.csv") %>%
  rename(
    la_name = LAD,
    la_code = LAD_code
  ) %>%
  select(la_name,
    la_code, Black, White) %>%
  pivot_longer(cols = c(Black, White), names_to = "ethnicity", values_to = "value") %>%
  mutate(
    metric = "population",
    value_type = "frequency"
  ) %>%
  mutate(
    la_name = if_else(la_name == "Rhondda Cynon Taf", "Rhondda Cynon Taff", la_name),
    la_name = if_else(la_name == "Bristol", "Bristol, City of", la_name),
    la_name = if_else(la_name == "Herefordshire", "Herefordshire, County of", la_name),
    la_name = if_else(la_name == "Kingston upon Hull", "Kingston upon Hull, City of", la_name)
  )


# make stop data longer by creating separate "metric" and "value" columns
stop_data_long <- stop_data %>%
  pivot_longer(cols = c(Black_stop_rate,
                        White_stop_rate,
                        rr, rr_ci_low,
                        rr_ci_upp,
                        Black_stopped,
                        White_stopped,
                        or,
                        or_ci_low,
                        or_ci_upp),
               names_to = "metric",
               values_to = "value") %>%
  # create metric_category variable to differentiate variables under same metric.
  # e.g. confidence intervals corresponding to ratios
  mutate(
    metric_category = case_when(
      str_detect(metric, "or") ~ metric,
      str_detect(metric, "rr") ~ metric,
      TRUE ~ NA
    ),
    # assign ethnicity variable based on presence of ethnicity string
    ethnicity = case_when(
      str_detect(metric, "stop_rate") ~ if_else(str_detect(metric, "Black"), "Black", "White"),
      str_detect(metric, "stopped") ~ if_else(str_detect(metric, "Black"), "Black", "White"),
      TRUE ~ NA
    ),
    # create better names for variable types
    value_type = case_when(
      str_detect(metric, "stop_rate") ~ "percentage",
      str_detect(metric, "stopped") ~ "frequency",
      str_detect(metric, "or|rr") ~ "ratio",
      TRUE ~ NA
    ),
    # drop redundant parts of names
  metric = str_remove(metric, "Black_|White_"),
  metric = str_remove(metric, "_ci_low|_ci_upp"),
  metric = case_when(
    str_detect(metric, "stopped") ~ "number_stopped",
    TRUE ~ metric
  )
  # move variables to more appropriate locations
    ) %>%
  relocate(
    metric_category, .before = value
  ) %>%
  relocate(
    value_type, .before = value
  ) %>%
  relocate(
    ethnicity, .before = value
  ) %>%
  rename(
    la_name = la,
  ) %>%
  # adjust spelling so that it is consistent with other data sources
  mutate(
    la_name = if_else(la_name == "Rhondda Cynon Taf", "Rhondda Cynon Taff", la_name)
  )

# make reason, legislation, and outcome data longer
obj_etc_data_long <- obj_etc_data %>%
  pivot_longer(cols = c(frequency, percentage),
               names_to = "value_type",
               values_to = "value") %>%
  rename(
    date = year
  ) %>%
  mutate(
    la_name = if_else(la_name == "Rhondda Cynon Taf", "Rhondda Cynon Taff", la_name)
  )

# create a lookup data frame from stop data to get correspondence between
# las and forces
lookup <- stop_data_long %>%
  select(la_name, la_code, county, region, country, force, force_code) %>%
  distinct() %>%
  arrange(la_name)

# add lookup data to population data
population_data <- merge(population_data,
                         lookup[c("la_code",
                                  "county",
                                  "region",
                                  "country",
                                  "force",
                                  "force_code")],
                         by = "la_code")

# combine reasons etc., stop, and population data into one frame
all_data <- bind_rows(obj_etc_data_long,
                      stop_data_long[, -c(which(str_detect(colnames(stop_data_long), "Black_|White")))],
                      population_data) %>%
  relocate(
    ethnicity, .before = value
  )


# Need to add in missing la_codes in stop data
## get la codes
la_codes <- stop_data_long %>%
  select(la_name, la_code) %>%
  distinct() %>%
  arrange(la_name)

# Need to add in missing force_codes in stop data
## get force codes
force_codes <- stop_data_long %>%
  select(force, force_code) %>%
  distinct() %>%
  arrange(force)

## function for filling missing la codes
### if la code is missing from the df, find it in the la_codes frame by searching
### its name
fill_missing_lacode <- function(df) {
  df %>%
    mutate(la_code = if_else(is.na(la_code), la_codes$la_code[match(la_name, la_codes$la_name)], la_code))
}

# Call the function to fill in missing la_code values
all_data_filled <- fill_missing_lacode(all_data)

## function for filling missing force codes
### if force code is missing from the df, find it in the force_codes frame by
### searching its name
fill_missing_force_code <- function(df) {
  df %>%
    mutate(force_code = if_else(is.na(force_code), force_codes$force_code[match(force, force_codes$force)], force_code))
}


# Call the function to fill missing force codes
all_data_filled <- fill_missing_force_code(all_data_filled)

# move force to better location
all_data_filled <- all_data_filled %>%
  relocate(
    force_code, .after = force
  )


# Check that all la_codes have been added correctly
all_data_check <- all_data_filled %>%
  select(la_name, la_code) %>%
  distinct() %>%
  arrange(la_name)

## should be 331 (nrow(la_codes))
sum(all_data_check$la_name == la_codes$la_name & all_data_check$la_code == la_codes$la_code)


# Check that all force_codes have been added correctly
all_data_check <- all_data_filled %>%
  select(force, force_code) %>%
  distinct() %>%
  arrange(force)

# should be 43 (nrow(force_codes)) - both frames need to be in same order!
sum(all_data_check$force == force_codes$force & all_data_check$force_code == force_codes$force_code)

write_csv(all_data_filled, file = paste0("./data/", Sys.Date(), " - stop_search_all_metrics_2019-2021.csv"))
