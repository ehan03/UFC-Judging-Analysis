# Load libraries
library(tidyverse)
library(stringi)

# Load UFC Stats dataframes
ufcstats_events <- readRDS("./data/UFC Stats/ufcstats_events.rds")
ufcstats_fighters <- readRDS("./data/UFC Stats/ufcstats_fighters.rds")
ufcstats_bouts <- readRDS("./data/UFC Stats/ufcstats_bouts.rds")
ufcstats_bouts_stats <- readRDS("./data/UFC Stats/ufcstats_bouts_stats.rds")

# Load MMA Decisions dataframes
mmadecisions_events <- readRDS("./data/MMA Decisions/mmadecisions_events.rds")
mmadecisions_fighters <- readRDS("./data/MMA Decisions/mmadecisions_fighters.rds")
mmadecisions_bouts <- readRDS("./data/MMA Decisions/mmadecisions_bouts.rds")
mmadecisions_bouts_scores <- readRDS("./data/MMA Decisions/mmadecisions_bouts_scores.rds")


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
# Filter for fighters that fought in bouts that went to judges' scorecards
# Some fights were overturned later, but originally went to decisions
decision_fighter_ids <- ufcstats_bouts %>%
  filter(grepl("Decision", outcome_method) | 
         grepl("[0-9]", outcome_method_details)) %>%
  select(red_fighter_id, blue_fighter_id) %>%
  unlist() %>%
  as.vector() %>%
  unique()
decision_fighters <- ufcstats_fighters %>%
  filter(id %in% decision_fighter_ids)

# Isolate fighters with unique names
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

# First, match for identical names (accents removed)
fighter_matches1 <- mmadecisions_unique_name %>%
  mutate(name_latin = stri_trans_general(name, "Latin-ASCII")) %>%
  inner_join(ufcstats_unique_name, by = join_by(name_latin == name)) %>%
  rename(ufcstats_fighter_id = id.y) %>%
  rename(mmadecisions_fighter_id = id.x) %>%
  select(ufcstats_fighter_id, mmadecisions_fighter_id)

# Next, match by date of birth if unique
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

# Match by nickname if unique
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

# Manually match remaining fighters, including ones with duplicate names
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
bout_match_primer <- mmadecisions_bouts %>%
  left_join(event_id_map, 
            by = join_by(event_id == mmadecisions_event_id),
            suffix = c("", "")) %>%
  left_join(fighter_id_map %>%
              rename(fighter1_ufcstats_id = ufcstats_fighter_id),
            by = join_by(fighter1_id == mmadecisions_fighter_id),
            suffix = c("", "")) %>%
  left_join(fighter_id_map %>%
              rename(fighter2_ufcstats_id = ufcstats_fighter_id),
            by = join_by(fighter2_id == mmadecisions_fighter_id),
            suffix = c("", "")) %>%
  rename(mmadecisions_bout_id = id) %>%
  rename(mmadecisions_event_id = event_id)

bout_id_map <- bout_match_primer %>%
  inner_join(ufcstats_bouts %>%
              rename(ufcstats_bout_id = id) %>%
              filter(grepl("Decision", outcome_method) | 
                     grepl("[0-9]", outcome_method_details)),
            by = join_by(ufcstats_event_id == event_id,
                         fighter1_ufcstats_id == red_fighter_id,
                         fighter2_ufcstats_id == blue_fighter_id),
            suffix = c("", "")) %>%
  bind_rows(bout_match_primer %>%
              inner_join(ufcstats_bouts %>%
                           rename(ufcstats_bout_id = id) %>%
                           filter(grepl("Decision", outcome_method) | 
                                    grepl("[0-9]", outcome_method_details)),
                         by = join_by(ufcstats_event_id == event_id,
                                      fighter1_ufcstats_id == blue_fighter_id,
                                      fighter2_ufcstats_id == red_fighter_id),
                         suffix = c("", ""))) %>%
  select(ufcstats_bout_id, mmadecisions_bout_id)

dim(bout_id_map)
length(unique(bout_id_map$mmadecisions_bout_id))
length(unique(bout_id_map$ufcstats_bout_id))


# Create final combined dataframe that has all minimal relevant information
# including fight stats and judge scores by round
stats_merge <- bout_id_map %>%
  left_join(ufcstats_bouts %>%
              select(id, event_id, red_fighter_id, blue_fighter_id) %>%
              rename(ufcstats_event_id = event_id,
                     red_ufcstats_fighter_id = red_fighter_id,
                     blue_ufcstats_fighter_id = blue_fighter_id),
            by = join_by(ufcstats_bout_id == id),
            suffix = c("", "")) %>%
  left_join(mmadecisions_bouts %>%
              select(id, event_id) %>%
              rename(mmadecisions_event_id = event_id),
            by = join_by(mmadecisions_bout_id == id),
            suffix = c("", "")) %>%
  left_join(fighter_id_map %>%
              rename(red_mmadecisions_fighter_id = mmadecisions_fighter_id),
            by = join_by(red_ufcstats_fighter_id == ufcstats_fighter_id),
            suffix = c("", "")) %>%
  left_join(fighter_id_map %>%
              rename(blue_mmadecisions_fighter_id = mmadecisions_fighter_id),
            by = join_by(blue_ufcstats_fighter_id == ufcstats_fighter_id),
            suffix = c("", "")) %>%
  
  # Whoever sees the following code, I'm so sorry
  left_join(ufcstats_bouts_stats %>%
              rename(red_knockdowns_scored = knockdowns_scored,
                     red_total_strikes_landed = total_strikes_landed,
                     red_total_strikes_attempted = total_strikes_attempted,
                     red_sig_strikes_landed = sig_strikes_landed,
                     red_sig_strikes_attempted = sig_strikes_attempted,
                     red_sig_strikes_head_landed = sig_strikes_head_landed,
                     red_sig_strikes_head_attempted = sig_strikes_head_attempted,
                     red_sig_strikes_body_landed = sig_strikes_body_landed,
                     red_sig_strikes_body_attempted = sig_strikes_body_attempted,
                     red_sig_strikes_leg_landed = sig_strikes_leg_landed,
                     red_sig_strikes_leg_attempted = sig_strikes_leg_attempted,
                     red_sig_strikes_distance_landed = sig_strikes_distance_landed,
                     red_sig_strikes_distance_attempted = sig_strikes_distance_attempted,
                     red_sig_strikes_clinch_landed = sig_strikes_clinch_landed,
                     red_sig_strikes_clinch_attempted = sig_strikes_clinch_attempted,
                     red_sig_strikes_ground_landed = sig_strikes_ground_landed,
                     red_sig_strikes_ground_attempted = sig_strikes_ground_attempted,
                     red_takedowns_landed = takedowns_landed,
                     red_takedowns_attempted = takedowns_attempted,
                     red_submissions_attempted = submissions_attempted,
                     red_reversals_scored = reversals_scored,
                     red_control_time_seconds = control_time_seconds),
            by = join_by(ufcstats_bout_id == id,
                         red_ufcstats_fighter_id == fighter_id),
            suffix = c("", "")) %>%
  left_join(ufcstats_bouts_stats %>%
              rename(blue_knockdowns_scored = knockdowns_scored,
                     blue_total_strikes_landed = total_strikes_landed,
                     blue_total_strikes_attempted = total_strikes_attempted,
                     blue_sig_strikes_landed = sig_strikes_landed,
                     blue_sig_strikes_attempted = sig_strikes_attempted,
                     blue_sig_strikes_head_landed = sig_strikes_head_landed,
                     blue_sig_strikes_head_attempted = sig_strikes_head_attempted,
                     blue_sig_strikes_body_landed = sig_strikes_body_landed,
                     blue_sig_strikes_body_attempted = sig_strikes_body_attempted,
                     blue_sig_strikes_leg_landed = sig_strikes_leg_landed,
                     blue_sig_strikes_leg_attempted = sig_strikes_leg_attempted,
                     blue_sig_strikes_distance_landed = sig_strikes_distance_landed,
                     blue_sig_strikes_distance_attempted = sig_strikes_distance_attempted,
                     blue_sig_strikes_clinch_landed = sig_strikes_clinch_landed,
                     blue_sig_strikes_clinch_attempted = sig_strikes_clinch_attempted,
                     blue_sig_strikes_ground_landed = sig_strikes_ground_landed,
                     blue_sig_strikes_ground_attempted = sig_strikes_ground_attempted,
                     blue_takedowns_landed = takedowns_landed,
                     blue_takedowns_attempted = takedowns_attempted,
                     blue_submissions_attempted = submissions_attempted,
                     blue_reversals_scored = reversals_scored,
                     blue_control_time_seconds = control_time_seconds),
            by = join_by(ufcstats_bout_id == id,
                         round == round,
                         blue_ufcstats_fighter_id == fighter_id),
            suffix = c("", "")) %>%
  
  # Filter for fights where we have stats by round
  filter(!is.na(round))
  
# Filter for scores on 10-point scale and are known
standard_scores <- mmadecisions_bouts_scores %>%
  filter(score != 1,
         !is.na(score))

# Join with scoring data
final_df <- stats_merge %>%
  
  # Finally, join with scoring data
  left_join(standard_scores %>%
              rename(red_score = score),
            by = join_by(mmadecisions_bout_id == id,
                         round == round,
                         red_mmadecisions_fighter_id == fighter_id),
            suffix = c("", "")) %>%
  left_join(standard_scores %>%
              rename(blue_score = score),
            by = join_by(mmadecisions_bout_id == id,
                         round == round,
                         judge_num == judge_num,
                         judge_id == judge_id,
                         blue_mmadecisions_fighter_id == fighter_id),
            suffix = c("", "")) %>%
  
  # Filter out edge cases
  filter(!is.na(red_score)) %>%
  
  # Sort
  arrange(mmadecisions_bout_id, round, judge_num)

head(final_df)

# Handle instances where there were point deductions
deductions <- final_df %>%
  filter(red_score < 10,
         blue_score < 10)
ufcstats_bouts %>%
  filter(id %in% deductions$ufcstats_bout_id) %>%
  select(id, outcome_method_details)

points_fix <- read.csv("./data/point_deductions.csv")
final_df_fix <- final_df %>%
  left_join(points_fix,
            by = join_by(ufcstats_bout_id, round)) %>%
  mutate(red_score = ifelse((!is.na(red_deduction)) & (red_deduction == 1), 
                            red_score + points, red_score)) %>%
  mutate(blue_score = ifelse((!is.na(red_deduction)) & (red_deduction == 0), 
                             blue_score + points, blue_score)) %>%
  select(-c(red_deduction, points))

# Save final combined dataframe
saveRDS(final_df_fix, "./data/final_merged.rds")
