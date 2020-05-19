library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(tidytext)
library(hrbrthemes)

theme_set(theme_bw())

#data from
#https://www.census.gov/data/tables/time-series/demo/geographic-mobility/state-to-state-migration.html

clean_census_migration_data <- function(data){
  
  message(str_c("Reading in:", data, sep = " "))
  
  df <- suppressMessages(read_xls(data, range = "A7:DZ78", na = "N/A")) %>%
    rename(state_from = 1,
           state_population_same_one_year_plus = 2,
           state_population_same_house_one_year = 4,
           state_population_same_state_one_year = 6,
           state_population_different_state_residence_one_year = 8
    ) %>%
    select(-contains("..")) %>%
    clean_names() %>%
    filter_all(any_vars(!str_detect(., "Estimate"))) %>%
    filter(!str_detect(state_from, "residence"))

  message(str_c("Rows:", nrow(df), sep = " "))
  message(str_c("Columns:", ncol(df), sep = " "))
          

  df <- df %>%
    mutate_at(vars(2:ncol(df)), as.numeric) %>%
    filter(!str_detect(state_from, "United States")) %>%
    pivot_longer(cols = 6:ncol(df), names_to = "state_to", values_to = "migration") %>%
    group_by(state_from) %>%
    mutate(total_migration_population = sum(migration, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pct_migrated = migration / total_migration_population,
           state_to = str_replace_all(state_to, "_", " "),
           state_to = str_to_title(state_to),
           state_to = str_remove_all(state_to, "[[:digit:]]"),
           state_to = str_replace(state_to, " Of ", " of "))

  return(df)
}


df_migration <- list.files("data", full.names = TRUE) %>% 
  #keep(str_detect(., "2017")) %>% 
  set_names() %>% 
  map_dfr(clean_census_migration_data, .id = "file_name") %>% 
  mutate(year = str_extract(file_name, "[[:digit:]]{4}")) %>% 
  select(year, everything(), -file_name)

df_migration

df_migration %>% 
  distinct(state_from) %>% 
  View()

df_migration %>% 
  distinct(state_to) %>% 
  View()

df_migration %>% 
  write_csv("output/migration_data_cleaned.csv")

