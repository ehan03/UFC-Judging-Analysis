# Load libraries
library(tidyverse)

# Raw data downloaded from [https://github.com/Greco1899/scrape_ufc_stats]
# Latest refresh: October 21, 2024
# Scraped data is dirty and poorly/inefficiently structured

# Read in raw scraped data
event_details <- read.csv("./data/UFC Stats/ufc_event_details.csv")
fight_details <- read.csv("./data/UFC Stats/ufc_fight_details.csv")
fight_results <- read.csv("./data/UFC Stats/ufc_fight_results.csv")
fight_stats <- read.csv("./data/UFC Stats/ufc_fight_stats.csv")
fighter_details <- read.csv("./data/UFC Stats/ufc_fighter_details.csv")
fighter_tott <- read.csv("./data/UFC Stats/ufc_fighter_tott.csv")

# Clean event details
events <- event_details %>%
  mutate(id = sapply(strsplit(URL, "/"), tail, 1)) %>%
  mutate(date = as.Date(DATE, format = "%B %d, %Y")) %>%
  mutate(city = sapply(lapply(strsplit(LOCATION, ", "), head, -1), paste, 
                       collapse = ", ")) %>%
  mutate(country = sapply(lapply(strsplit(LOCATION, ", "), tail, 1), paste,
                          collapse = ", ")) %>%
  rename(name = EVENT) %>%
  select(id, name, date, city, country) %>%
  arrange(date)

# Clean and combine fighter data
# Function to convert height in x' y" format to inches
convert_height <- function(height_str) {
  if (is.na(height_str)) {return(NA)}
  
  matches <- regmatches(height_str, 
                        regexec("(\\d+)'\\s*(\\d*\\.?\\d*)", height_str))
  feet <- as.numeric(matches[[1]][2])
  inches <- as.numeric(matches[[1]][3])
  total_inches <- feet * 12 + inches
  
  return(total_inches)
}

# There are several invalid fighter URLs that are historical artifacts as a
# result of appending data rather than overwriting data
invalid_urls <- fighter_tott %>%
  filter(!URL %in% fighter_details$URL) %>%
  pull(URL)

fighter_tott_clean <- fighter_tott %>%
  filter(!URL %in% invalid_urls) %>%
  mutate(id = sapply(strsplit(URL, "/"), tail, 1)) %>%
  mutate(HEIGHT = na_if(HEIGHT, "--")) %>%
  mutate(REACH = na_if(REACH, "--")) %>%
  mutate(STANCE = na_if(STANCE, "")) %>%
  mutate(DOB = na_if(DOB, "--")) %>%
  mutate(height_inches = sapply(HEIGHT, convert_height)) %>%
  mutate(reach_inches = if_else(!is.na(REACH), 
                                as.numeric(gsub("\"", "", REACH)), NA)) %>%
  mutate(date_of_birth = as.Date(DOB, format = "%B %d, %Y")) %>%
  rename(name = FIGHTER, stance = STANCE) %>%
  select(id, name, height_inches, reach_inches, stance, date_of_birth)

fighter_details_clean <- fighter_details %>%
  mutate(id = sapply(strsplit(URL, "/"), tail, 1)) %>%
  mutate(nickname = str_trim(na_if(NICKNAME, ""))) %>%
  select(id, nickname)

fighters <- fighter_tott_clean %>%
  left_join(fighter_details_clean, by = "id", suffix = c("", "")) %>%
  select(id, name, nickname, height_inches, reach_inches, stance, date_of_birth)

# Clean and combine bout data


# Save all dataframes to disk
