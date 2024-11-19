# Load libraries
library(tidyverse)

# Load raw scraped data from MMA Decisions
raw_events <- readRDS("./data/MMA Decisions/raw/events.rds")
raw_fighters <- readRDS("./data/MMA Decisions/raw/fighters.rds")
raw_judges <- readRDS("./data/MMA Decisions/raw/judges.rds")
raw_bouts <- readRDS("./data/MMA Decisions/raw/bouts.rds")
raw_bouts_scores <- readRDS("./data/MMA Decisions/raw/bouts_scores.rds")


# Clean events data
# Check first few rows
head(raw_events)

# Check for rows with NA
raw_events[which(apply(is.na(raw_events), 1, any)), ]

# Table of country names, city not too important for our uses
table(raw_events$country)

# Sanity check and make sure event names are unique, will use in addition
# to dates to match with UFC Stats data
nrow(raw_events) == length(unique(raw_events$name))

# Seems fine, can save as is
clean_events <- raw_events
saveRDS(clean_events, "./data/MMA Decisions/mmadecisions_events.rds")


# Clean fighters data
# Check first few rows
head(raw_fighters)

# Handle `born_info` column, need to separate DOB and birth location
# Find number of pieces of info
raw_fighters <- raw_fighters %>%
  mutate(born_info_count = str_count(born_info, "\\[SEP\\]"))
table(raw_fighters$born_info_count, useNA = "always")

table(raw_fighters[which(raw_fighters$born_info_count == 0), "born_info"])

month_pattern <- "^(January|February|March|April|May|June|July|August|September|October|November|December)"
clean_fighters <- raw_fighters %>%
  # Replace entries "No data available" with NA
  mutate(born_info = na_if(born_info, "No data available")) %>%
  
  # Prepend and append [SEP] delimiters
  mutate(born_info = ifelse(grepl(month_pattern, born_info) & 
                            !is.na(born_info) & 
                            born_info_count == 0,
                            paste0(born_info, "[SEP]"), born_info)) %>%
  mutate(born_info = ifelse(!grepl(month_pattern, born_info) & 
                            !is.na(born_info) & 
                            born_info_count == 0,
                            paste0("[SEP]", born_info), born_info)) %>%
  
  # Separate into DOB and birth location
  separate(born_info, into = c("date_of_birth", "birth_location"), 
           sep = "\\[SEP\\]") %>%
  mutate(date_of_birth = na_if(date_of_birth, "")) %>%
  mutate(birth_location = na_if(birth_location, "")) %>%
  
  # Clean up strange whitespace characters in names
  mutate(name = gsub("\u00A0", " ", name, fixed = TRUE)) %>%
  
  # Clean DOB
  mutate(date_of_birth = as.Date(gsub(" \\(.*\\)", "", date_of_birth),
                                 format = "%B %d, %Y")) %>%
  
  # Clean reach
  mutate(reach_inches = as.numeric(na_if(gsub("\"", "", reach), "n/a"))) %>%
  
  # Clean height and convert to inches
  mutate(height = na_if(height, "n/a")) %>%
  mutate(height_inches = str_extract(height, "^[0-9]+") %>% as.numeric() * 12 +
           str_extract(height, "[0-9]+\\.?[0-9]*(?=\")") %>% 
           as.numeric()) %>%
  
  # Nicknames
  mutate(nicknames = gsub("\\[SEP\\]", "; ", nickname_info)) %>%
  
  # Select relevant columns
  select(id, name, nicknames, height_inches, reach_inches, date_of_birth, 
         birth_location)

# There might be some duplicate fighters
clean_fighters[duplicated(clean_fighters$name) | 
               duplicated(clean_fighters$name, fromLast = TRUE), ]

# Manually checking, Nick Fiore and Zha Yi are duplicated
# Remove fighter entry and replace in bouts and scores data
clean_fighters <- clean_fighters %>%
  filter(id != 4934) %>%
  filter(id != 6748)
raw_bouts <- raw_bouts %>%
  mutate(fighter1_id = ifelse(fighter1_id == 4934, 6373, fighter1_id)) %>%
  mutate(fighter2_id = ifelse(fighter2_id == 4934, 6373, fighter2_id)) %>%
  mutate(fighter1_id = ifelse(fighter1_id == 6748, 6394, fighter1_id)) %>%
  mutate(fighter2_id = ifelse(fighter2_id == 6748, 6394, fighter2_id))
raw_bouts_scores <- raw_bouts_scores %>%
  mutate(fighter_id = ifelse(fighter_id == 4934, 6373, fighter_id)) %>%
  mutate(fighter_id = ifelse(fighter_id == 6748, 6394, fighter_id))

# Save cleaned data
saveRDS(clean_fighters, "./data/MMA Decisions/mmadecisions_fighters.rds")


# Clean judges data
# Check first few rows
head(raw_judges)

# Check for duplicate names
nrow(raw_judges) == length(unique(raw_judges$name))

# Seems fine, can save as is
clean_judges <- raw_judges
saveRDS(clean_judges, "./data/MMA Decisions/mmadecisions_judges.rds")


# Clean bouts data
# Check first few rows
head(raw_bouts)

# Seems fine, can save as is
clean_bouts <- raw_bouts
saveRDS(clean_bouts, "./data/MMA Decisions/mmadecisions_bouts.rds")


# Clean bouts scores data
# Check first few rows
head(raw_bouts_scores)

# Seems fine, can save as is
clean_bouts_scores <- raw_bouts_scores
saveRDS(clean_bouts_scores, "./data/MMA Decisions/mmadecisions_bouts_scores.rds")
