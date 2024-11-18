# Load libraries
library(tidyverse)
library(stringi)

# Load UFC Stats dataframes
ufcstats_events <- readRDS("./data/UFC Stats/ufcstats_events.rds")
ufcstats_fighters <- readRDS("./data/UFC Stats/ufcstats_fighters.rds")
ufcstats_bouts <- readRDS("./data/UFC Stats/ufcstats_bouts.rds")

# Load MMA Decisions dataframes
mmadecisions_events <- readRDS("./data/MMA Decisions/mmadecisions_events.rds")
mmadecisions_fighters <- readRDS("./data/MMA Decisions/mmadecisions_fighters.rds")
mmadecisions_bouts <- readRDS("./data/MMA Decisions/mmadecisions_bouts.rds")


# Merge events
event_ids_decisions <- ufcstats_bouts %>%
  filter(grepl("Decision", outcome_method)) %>%
  pull(event_id) %>%
  unique()
ufcstats_events_filtered <- ufcstats_events %>%
  filter(id %in% event_ids_decisions)
mmadecisions_events <- mmadecisions_events %>%
  arrange(date)

# Try just doing one-to-one mapping in order
event_id_map <- data.frame(ufcstats_event_id = ufcstats_events_filtered$id,
                           mmadecisions_event_id = mmadecisions_events$id)

# Get number of decisions by event id
ufcstats_decision_counts <- ufcstats_bouts %>%
  filter(grepl("Decision", outcome_method) | 
         grepl("[0-9]", outcome_method_details)) %>%
  group_by(event_id) %>%
  summarise(ufcstats_count = n(), .groups = "drop") %>%
  as.data.frame()
mmadecisions_decision_counts <- mmadecisions_bouts %>%
  group_by(event_id) %>%
  summarise(mmadecisions_count = n(), .groups = "drop") %>%
  as.data.frame()

decision_count_comparison <- event_id_map %>%
  left_join(ufcstats_decision_counts, 
            by = join_by(ufcstats_event_id == event_id),
            suffix = c("", "")) %>%
  left_join(mmadecisions_decision_counts,
            by = join_by(mmadecisions_event_id == event_id),
            suffix = c("", ""))

# Sanity check, seems like MMA Decisions omitted a few fights that were later
# overturned
# Otherwise, event id matching is straightforward
decision_count_comparison %>%
  filter(ufcstats_count != mmadecisions_count)


# Merge fighters
decision_fighter_ids <- ufcstats_bouts %>%
  filter(grepl("Decision", outcome_method) | 
         grepl("[0-9]", outcome_method_details)) %>%
  select(red_fighter_id, blue_fighter_id) %>%
  unlist() %>%
  as.vector() %>%
  unique()
decision_fighters <- ufcstats_fighters %>%
  filter(id %in% decision_fighter_ids)

mmadecisions_unique_name <- mmadecisions_fighters %>%
  group_by(name) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  as.data.frame()
ufcstats_unique_name <- decision_fighters %>%
  group_by(name) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  as.data.frame()


fighter_matches1 <- mmadecisions_unique_name %>%
  mutate(name_latin = stri_trans_general(name, "Latin-ASCII")) %>%
  inner_join(ufcstats_unique_name, by = join_by(name_latin == name)) %>%
  rename(ufcstats_fighter_id = id.y) %>%
  rename(mmadecisions_fighter_id = id.x) %>%
  select(ufcstats_fighter_id, mmadecisions_fighter_id)

fighter_matches2 <- mmadecisions_unique_name %>%
  filter(!id %in% fighter_matches1$mmadecisions_fighter_id) %>%
  group_by(date_of_birth) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  as.data.frame() %>%
  inner_join(ufcstats_unique_name %>%
               filter(!id %in% fighter_matches1$ufcstats_fighter_id) %>%
               group_by(date_of_birth) %>%
               filter(n() == 1) %>%
               ungroup() %>%
               as.data.frame(),
             by = join_by(date_of_birth)) %>%
  rename(ufcstats_fighter_id = id.y) %>%
  rename(mmadecisions_fighter_id = id.x) %>%
  select(ufcstats_fighter_id, mmadecisions_fighter_id)

fighter_matches3 <- mmadecisions_unique_name %>%
  filter(!id %in% fighter_matches1$mmadecisions_fighter_id) %>%
  filter(!id %in% fighter_matches2$mmadecisions_fighter_id) %>%
  filter(!is.na(nicknames)) %>%
  inner_join(ufcstats_unique_name %>%
               filter(!id %in% fighter_matches1$ufcstats_fighter_id) %>%
               filter(!id %in% fighter_matches2$ufcstats_fighter_id) %>%
               filter(!is.na(nickname)),
             by = join_by(nicknames == nickname)) %>%
  rename(ufcstats_fighter_id = id.y) %>%
  rename(mmadecisions_fighter_id = id.x) %>%
  select(ufcstats_fighter_id, mmadecisions_fighter_id)

fighter_matches4 <- data.frame(ufcstats_fighter_id = c("294aa73dbf37d281",
                                                       "12ebd7d157e91701",
                                                       "6cbb7661c3258617",
                                                       "c4b81cdecd5d6abe",
                                                       "8b5f9ea38184ded3",
                                                       "8ce87f7e3a9baed2",
                                                       "3ec1e4ba98c9c85a",
                                                       "edd02825c29028fe",
                                                       "6fd953151d981979",
                                                       "ca43de99b07b6b40",
                                                       "31bbd39c0a075d4e",
                                                       "fa07345cc9db5cc9",
                                                       "8c1ca54b5089d199",
                                                       "25f9a5f3e8a52618"),
                               mmadecisions_fighter_id = c(5433, 5898, 34, 740,
                                                           2685, 3062, 3160,
                                                           3643, 4077, 4515,
                                                           4851, 5091, 6242, 
                                                           6686))

fighter_id_map <- bind_rows(fighter_matches1, fighter_matches2, 
                            fighter_matches3, fighter_matches4)


# Merge bouts

