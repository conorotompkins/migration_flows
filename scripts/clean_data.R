library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(tidytext)
library(hrbrthemes)

theme_set(theme_bw())

df_raw <- list.files("data", full.names = TRUE) %>% 
  set_names() %>% 
  map_dfr(read_xls, range = "A7:DZ78", na = "N/A")

df <- df_raw %>%
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

dim(df)
glimpse(df)


df <- df %>% 
  mutate_at(vars(2:ncol(df)), as.numeric)


df <- df %>% 
  filter(!str_detect(state_from, "United States")) %>% 
  pivot_longer(cols = 6:ncol(df), names_to = "state_to", values_to = "migration") %>% 
  group_by(state_from) %>% 
  mutate(total_migration_population = sum(migration, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(pct_migrated = migration / total_migration_population,
         state_to = str_replace_all(state_to, "_", " "),
         state_to = str_to_title(state_to))



