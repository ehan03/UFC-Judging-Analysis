# Load libraries
library(tidyverse)

# Load data
x <- readRDS("./data/final_merged.rds")
head(x)

y <- readRDS("./data/MMA Decisions/mmadecisions_judges.rds")
head(y)


# Quick data exploration
dim(x)

length(unique(x$ufcstats_bout_id))

length(unique(x$judge_id))
table(x$judge_id, useNA = "always")

# Join with judge names for better readability
xx <- x %>%
  left_join(y %>%
              rename(judge_name = name),
            by = join_by(judge_id == id),
            suffix = c("", ""))
sort(table(xx$judge_name, useNA = "always"), decreasing = TRUE)
length(unique(xx$judge_name)) == length(unique(xx$judge_id))

# Create exhaustive set of features for convenience, not all will be used
xf <- xx %>%
  mutate(knockdowns_scored_diff = red_knockdowns_scored - 
           blue_knockdowns_scored) %>%
  mutate(total_strikes_landed_diff = red_total_strikes_landed - 
           blue_total_strikes_landed) %>%
  mutate(total_strikes_attempted_diff = red_total_strikes_attempted - 
           blue_total_strikes_attempted) %>%
  mutate(sig_strikes_landed_diff = red_sig_strikes_landed - 
           blue_sig_strikes_landed) %>%
  mutate(sig_strikes_attempted_diff = red_sig_strikes_attempted - 
           blue_sig_strikes_attempted) %>%
  mutate(sig_strikes_head_landed_diff = red_sig_strikes_head_landed - 
           blue_sig_strikes_head_landed) %>%
  mutate(sig_strikes_head_attempted_diff = red_sig_strikes_head_attempted - 
           blue_sig_strikes_head_attempted) %>%
  mutate(sig_strikes_body_landed_diff = red_sig_strikes_body_landed - 
           blue_sig_strikes_body_landed) %>%
  mutate(sig_strikes_body_attempted_diff = red_sig_strikes_body_attempted - 
           blue_sig_strikes_body_attempted) %>%
  mutate(sig_strikes_leg_landed_diff = red_sig_strikes_leg_landed -
           blue_sig_strikes_leg_landed) %>%
  mutate(sig_strikes_leg_attempted_diff = red_sig_strikes_leg_attempted - 
           blue_sig_strikes_leg_attempted) %>%
  mutate(sig_strikes_distance_landed_diff = red_sig_strikes_distance_landed - 
           blue_sig_strikes_distance_landed) %>%
  mutate(sig_strikes_distance_attempted_diff = red_sig_strikes_distance_attempted - 
           blue_sig_strikes_distance_attempted) %>%
  mutate(sig_strikes_clinch_landed_diff = red_sig_strikes_clinch_landed - 
           blue_sig_strikes_clinch_landed) %>%
  mutate(sig_strikes_clinch_attempted_diff = red_sig_strikes_clinch_attempted - 
           blue_sig_strikes_clinch_attempted) %>%
  mutate(sig_strikes_ground_landed_diff = red_sig_strikes_ground_landed - 
           blue_sig_strikes_ground_landed) %>%
  mutate(sig_strikes_ground_attempted_diff = red_sig_strikes_ground_attempted - 
           blue_sig_strikes_ground_attempted) %>%
  mutate(takedowns_landed_diff = red_takedowns_landed - 
           blue_takedowns_landed) %>%
  mutate(takedowns_attempted_diff = red_takedowns_attempted - 
           blue_takedowns_attempted) %>%
  mutate(submissions_attempted_diff = red_submissions_attempted - 
           blue_submissions_attempted) %>%
  mutate(reversals_scored_diff = red_reversals_scored - 
           blue_reversals_scored) %>%
  mutate(control_time_seconds_diff = red_control_time_seconds - 
           blue_control_time_seconds) %>%
  mutate(score_diff = red_score - blue_score) %>%
  mutate(judge_name = as.factor(judge_name)) %>%
  select(knockdowns_scored_diff, total_strikes_landed_diff, 
         total_strikes_attempted_diff, sig_strikes_landed_diff, 
         sig_strikes_attempted_diff, sig_strikes_head_landed_diff,
         sig_strikes_head_attempted_diff, sig_strikes_body_landed_diff,
         sig_strikes_body_attempted_diff, sig_strikes_leg_landed_diff,
         sig_strikes_leg_attempted_diff, sig_strikes_distance_landed_diff,
         sig_strikes_distance_attempted_diff, sig_strikes_clinch_landed_diff,
         sig_strikes_clinch_attempted_diff, sig_strikes_ground_landed_diff,
         sig_strikes_ground_attempted_diff, takedowns_landed_diff, 
         takedowns_attempted_diff, submissions_attempted_diff, 
         reversals_scored_diff, control_time_seconds_diff, judge_name, 
         score_diff)
  
xff <- xf %>%
  select(knockdowns_scored_diff, total_strikes_landed_diff, 
         sig_strikes_head_landed_diff, sig_strikes_body_landed_diff,
         sig_strikes_leg_landed_diff, takedowns_landed_diff, 
         submissions_attempted_diff, reversals_scored_diff, 
         control_time_seconds_diff, judge_name, score_diff)
