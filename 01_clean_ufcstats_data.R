# Load libraries
library(tidyverse)

# Raw data downloaded from [https://github.com/Greco1899/scrape_ufc_stats]
# Latest refresh: October 21, 2024
# Scraped data is dirty and poorly/inefficiently structured

# Read in raw scraped data
event_details <- read.csv("./data/UFC Stats/raw/ufc_event_details.csv")
fight_results <- read.csv("./data/UFC Stats/raw/ufc_fight_results.csv")
fight_stats <- read.csv("./data/UFC Stats/raw/ufc_fight_stats.csv")
fighter_details <- read.csv("./data/UFC Stats/raw/ufc_fighter_details.csv")
fighter_tott <- read.csv("./data/UFC Stats/raw/ufc_fighter_tott.csv")

# Read in data I manually inputted to address missing/poorly managed data
edge_case_fighters_map <- read.csv("./data/UFC Stats/hotfixes/edge_case_fighters_map.csv")
missing_bouts <- read.csv("./data/UFC Stats/hotfixes/missing_bouts.csv")
missing_bouts_stats_by_round <- read.csv("./data/UFC Stats/hotfixes/missing_bouts_stats_by_round.csv")


# Clean event details
events <- event_details %>%
  # Create unique event id from URL
  mutate(id = sapply(strsplit(URL, "/"), tail, 1)) %>%
  
  # Convert to date object
  mutate(date = as.Date(DATE, format = "%B %d, %Y")) %>%
  
  # Derive city and country from location
  mutate(city = sapply(lapply(strsplit(LOCATION, ", "), head, -1), paste, 
                       collapse = ", ")) %>%
  mutate(country = sapply(lapply(strsplit(LOCATION, ", "), tail, 1), paste,
                          collapse = ", ")) %>%
  
  # Create final format, order from oldest to newest event
  rename(name = EVENT) %>%
  select(id, name, date, city, country) %>%
  arrange(desc(row_number()))


# Clean and combine fighter data
# Function to convert height in x' y" format to inches
convert_height <- function(height_str) {
  if (is.na(height_str)) { 
    return(NA) 
  }
  
  # Split up height strings to separate out feet and inches parts
  matches <- regmatches(height_str, 
                        regexec("(\\d+)'\\s*(\\d*\\.?\\d*)", height_str))
  
  # Calculate height in inches as numeric variable
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

# Clean fighter characteristics
fighter_tott_clean <- fighter_tott %>%
  filter(!URL %in% invalid_urls) %>%
  
  # Create unique fighter id from URL
  mutate(id = sapply(strsplit(URL, "/"), tail, 1)) %>%
  
  # Handle missing values and convert to NA
  mutate(HEIGHT = na_if(HEIGHT, "--")) %>%
  mutate(REACH = na_if(REACH, "--")) %>%
  mutate(STANCE = na_if(STANCE, "")) %>%
  mutate(DOB = na_if(DOB, "--")) %>%
  
  # Extract height in inches, reach in inches, and convert DOB to date object
  mutate(height_inches = sapply(HEIGHT, convert_height)) %>%
  mutate(reach_inches = if_else(!is.na(REACH), 
                                as.numeric(gsub("\"", "", REACH)), NA)) %>%
  mutate(date_of_birth = as.Date(DOB, format = "%B %d, %Y")) %>%
  
  # Create final format
  rename(name = FIGHTER, stance = STANCE) %>%
  select(id, name, height_inches, reach_inches, stance, date_of_birth)

# Prepare fighter details to join with above dataframe to include nicknames
fighter_details_clean <- fighter_details %>%
  mutate(id = sapply(strsplit(URL, "/"), tail, 1)) %>%
  mutate(nickname = str_trim(na_if(NICKNAME, ""))) %>%
  select(id, nickname)

# Join the two processed dataframes to create finalized fighters data
fighters <- fighter_tott_clean %>%
  left_join(fighter_details_clean, by = join_by(id), suffix = c("", "")) %>%
  select(id, name, nickname, height_inches, reach_inches, stance, date_of_birth)


# Clean bout data
fight_results_clean <- fight_results %>%
  mutate(id = sapply(strsplit(URL, "/"), tail, 1)) %>%
  mutate(EVENT = str_trim(EVENT)) %>%
  
  # Join on events to get corresponding event id
  left_join(events %>% 
              rename(event_id = id) %>% 
              select(event_id, name), 
            by = join_by(EVENT == name), suffix = c("", "")) %>%
  rename(event_name = EVENT) %>%
  mutate(BOUT = gsub("  ", " ", str_trim(BOUT))) %>%
  mutate(bout_name = BOUT) %>%
  
  # Extract fighters' names in red and blue corners
  separate(BOUT, into = c("red_fighter_name", "blue_fighter_name"), 
           sep = " vs. ") %>%
  
  # Join on fighters dataframe to get fighter ids
  # Restrict to unique names to avoid faulty many-to-many relationships
  # We also consider some edge cases where fighter names changed over time
  left_join(fighters %>% 
              group_by(name) %>%
              filter(n() == 1) %>%
              ungroup() %>%
              rename(red_fighter_id = id) %>%
              select(red_fighter_id, name) %>%
              bind_rows(edge_case_fighters_map %>%
                        rename(red_fighter_id = id)),
            by = join_by(red_fighter_name == name), suffix = c("", "")) %>%
  left_join(fighters %>% 
              group_by(name) %>%
              filter(n() == 1) %>%
              ungroup() %>%
              rename(blue_fighter_id = id) %>%
              select(blue_fighter_id, name) %>%
              bind_rows(edge_case_fighters_map %>%
                        rename(blue_fighter_id = id)),
            by = join_by(blue_fighter_name == name), suffix = c("", "")) %>%
  
  # Handle the two fighters named Bruno Silva
  mutate(red_fighter_id = ifelse((red_fighter_name == "Bruno Silva") & 
                                 (id %in% c("94541dd979e0b4bd", 
                                            "c3af2b6ab1262fef",
                                            "b381dc272b32ce03",
                                            "55bf7f19a132e0fc",
                                            "dbb5d682a2d7f584",
                                            "1a2d13834696c4ab",
                                            "57ff0eb2351979c4")),
                                 "294aa73dbf37d281", red_fighter_id)) %>%
  mutate(blue_fighter_id = ifelse((blue_fighter_name == "Bruno Silva") & 
                                  (id %in% c("94541dd979e0b4bd", 
                                             "c3af2b6ab1262fef",
                                             "b381dc272b32ce03",
                                             "55bf7f19a132e0fc",
                                             "dbb5d682a2d7f584",
                                             "1a2d13834696c4ab",
                                             "57ff0eb2351979c4")),
                                  "294aa73dbf37d281", blue_fighter_id)) %>%
  mutate(red_fighter_id = ifelse((red_fighter_name == "Bruno Silva") &
                                 (is.na(red_fighter_id)),
                                 "12ebd7d157e91701", red_fighter_id)) %>%
  mutate(blue_fighter_id = ifelse((blue_fighter_name == "Bruno Silva") &
                                  (is.na(blue_fighter_id)),
                                  "12ebd7d157e91701", blue_fighter_id)) %>%
  
  # Rename rest of columns for consistency
  rename(bout_type = WEIGHTCLASS) %>%
  mutate(red_outcome = sapply(strsplit(OUTCOME, "/"), head, 1)) %>%
  rename(outcome_method = METHOD) %>%
  rename(outcome_method_details = DETAILS) %>%
  rename(end_round = ROUND) %>%
  rename(end_round_time = TIME) %>%
  rename(round_time_format = TIME.FORMAT) %>%
  rename(referee = REFEREE) %>%
  select(id, event_id, event_name, red_fighter_id, blue_fighter_id, bout_name, 
         red_fighter_name, blue_fighter_name, bout_type, red_outcome, 
         outcome_method, outcome_method_details, end_round, end_round_time, 
         round_time_format, referee)


# Function to convert strings like "3:20" into seconds (numeric)
convert_time <- function(time_str) {
  if (is.na(time_str)) { 
    return(NA) 
  }
  
  # Split the time string into minutes and seconds
  parts <- strsplit(time_str, ":")[[1]]
  
  # Convert minutes and seconds to numeric and calculate total seconds
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  
  # Calculate total seconds
  total_seconds <- minutes * 60 + seconds
  
  return(total_seconds)
}

# Clean fight statistics by round
fight_stats_clean <- fight_stats %>%
  filter(ROUND != "") %>%
  
  # Join with cleaned fight results to extract fighter ids
  left_join(fight_results_clean %>%
              select(id, event_name, red_fighter_id, red_fighter_name, 
                     bout_name) %>%
              rename(fighter_id = red_fighter_id) %>%
              rename(fighter_name = red_fighter_name) %>%
              bind_rows(fight_results_clean %>%
                          select(id, event_name, blue_fighter_id, 
                                 blue_fighter_name, bout_name) %>%
                          rename(fighter_id = blue_fighter_id) %>%
                          rename(fighter_name = blue_fighter_name)) %>%
              filter(!id %in% c("2750ac5854e8b28b", "ec1bda9a4c2aab42")),
            by = join_by(EVENT == event_name, BOUT == bout_name, 
                         FIGHTER == fighter_name),
            suffix = c("", "")) %>%
  
  # Handle edge case where both fighters fought each other twice in one night
  mutate(fighter_id = ifelse((is.na(fighter_id)) & 
                             (FIGHTER == "Kazushi Sakuraba"),
                             "9b5b5a75523728f3", fighter_id)) %>%
  mutate(fighter_id = ifelse((is.na(fighter_id)) & 
                             (FIGHTER == "Marcus Silveira"),
                             "f717b6002486f73f", fighter_id)) %>%
  mutate(id = ifelse((is.na(id)) & (SIG.STR. == "1 of 2"), 
                     "ec1bda9a4c2aab42", id)) %>%
  mutate(id = ifelse(is.na(id), "2750ac5854e8b28b", id)) %>%
  
  # Clean and rename columns for consistency
  # Bulk of this is separating different strikes and takedowns landed/attempted
  # into separate columns
  mutate(round = as.numeric(gsub("Round ", "", ROUND))) %>%
  rename(knockdowns_scored = KD) %>%
  separate(SIG.STR., into = c("sig_strikes_landed", "sig_strikes_attempted"), 
           sep = " of ") %>%
  separate(TOTAL.STR., into = c("total_strikes_landed", 
                                "total_strikes_attempted"), sep = " of ") %>%
  separate(TD, into = c("takedowns_landed", "takedowns_attempted"), 
           sep = " of ") %>%
  rename(submissions_attempted = SUB.ATT) %>%
  rename(reversals_scored = REV.) %>%
  mutate(CTRL = na_if(CTRL, "--")) %>%
  mutate(control_time_seconds = sapply(CTRL, convert_time)) %>%
  separate(HEAD, into = c("sig_strikes_head_landed",
                          "sig_strikes_head_attempted"), sep = " of ") %>%
  separate(BODY, into = c("sig_strikes_body_landed",
                          "sig_strikes_body_attempted"), sep = " of ") %>%
  separate(LEG, into = c("sig_strikes_leg_landed", "sig_strikes_leg_attempted"),
           sep = " of ") %>%
  separate(DISTANCE, into = c("sig_strikes_distance_landed",
                              "sig_strikes_distance_attempted"),
           sep = " of ") %>%
  separate(CLINCH, into = c("sig_strikes_clinch_landed",
                            "sig_strikes_clinch_attempted"), sep = " of ") %>%
  separate(GROUND, into = c("sig_strikes_ground_landed",
                            "sig_strikes_ground_attempted"), sep = " of ") %>%
  
  # Make sure derived landed/attempted columns are numeric
  mutate(across(c(sig_strikes_landed, sig_strikes_attempted,
                  total_strikes_landed, total_strikes_attempted, 
                  takedowns_landed, takedowns_attempted, 
                  sig_strikes_head_landed, sig_strikes_head_attempted,
                  sig_strikes_body_landed, sig_strikes_body_attempted,
                  sig_strikes_leg_landed, sig_strikes_leg_attempted,
                  sig_strikes_distance_landed, sig_strikes_distance_attempted,
                  sig_strikes_clinch_landed, sig_strikes_clinch_attempted,
                  sig_strikes_ground_landed, sig_strikes_ground_attempted), 
                as.numeric)) %>%
  select(id, round, fighter_id, knockdowns_scored, total_strikes_landed,
         total_strikes_attempted, sig_strikes_landed, sig_strikes_attempted, 
         sig_strikes_head_landed, sig_strikes_head_attempted, 
         sig_strikes_body_landed, sig_strikes_body_attempted, 
         sig_strikes_leg_landed, sig_strikes_leg_attempted, 
         sig_strikes_distance_landed, sig_strikes_distance_attempted, 
         sig_strikes_clinch_landed, sig_strikes_clinch_attempted, 
         sig_strikes_ground_landed, sig_strikes_ground_attempted, 
         takedowns_landed, takedowns_attempted, submissions_attempted, 
         reversals_scored, control_time_seconds)

# Confirm that missing bout ids correspond to bouts where per-round stats are
# unavailable on UFC Stats website
round_stats_unavailable <- fight_results_clean %>% 
  filter(!id %in% fight_stats_clean$id) %>%
  pull(id)
round_stats_unavailable

# Sanity check that we don't have any missing rounds
rounds_comparison <- fight_results_clean %>%
  filter(!id %in% round_stats_unavailable) %>%
  select(id, end_round) %>%
  full_join(fight_stats_clean %>%
              count(id) %>%
              mutate(num_rounds = n / 2) %>%
              select(id, num_rounds),
            by = join_by(id), suffix = c("", ""))
sum(is.na(rounds_comparison))
all(rounds_comparison$end_round == rounds_comparison$num_rounds)

# Results and stats data is missing two events entirely for some reason
# We will need to manually record and append this data
events %>%
  filter(!id %in% fight_results_clean$event_id)

# Create final bouts dataframe combining `fight_results_clean` with manually
# collected data for missing fights
bouts <- fight_results_clean %>%
  select(-c(event_name, bout_name, red_fighter_name, blue_fighter_name)) %>%
  bind_rows(missing_bouts) %>%
  group_by(event_id) %>%
  mutate(bout_order = row_number()) %>%
  ungroup() %>%
  as.data.frame() %>%
  left_join(events %>%
              mutate(event_order = row_number()) %>%
              select(id, event_order),
            by = join_by(event_id == id), suffix = c("", "")) %>%
  arrange(event_order, desc(bout_order)) %>%
  select(-c(event_order, bout_order))

# Create final bouts stats dataframe combining `fight_stats_clean` with manually
# collected data for missing fights
bouts_stats_by_round <- fight_stats_clean %>%
  bind_rows(missing_bouts_stats_by_round) %>%
  left_join(bouts %>%
              mutate(bout_order = row_number()) %>%
              select(id, bout_order),
            by = join_by(id), suffix = c("", "")) %>%
  arrange(bout_order) %>%
  select(-bout_order)


# Save all dataframes to disk
saveRDS(events, "./data/UFC Stats/events.rds")
saveRDS(fighters, "./data/UFC Stats/fighters.rds")
saveRDS(bouts, "./data/UFC Stats/bouts.rds")
saveRDS(bouts_stats_by_round, "./data/UFC Stats/bouts_stats_by_round.rds")
