# Load libraries
library(tidyverse)
library(GGally)
library(ggfortify)
library(broom)

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

# Create preliminary set of features
xf <- xx %>%
  mutate(knockdowns_scored_diff = red_knockdowns_scored - 
           blue_knockdowns_scored) %>%
  mutate(total_strikes_landed_diff = red_total_strikes_landed - 
           blue_total_strikes_landed) %>%
  mutate(sig_strikes_head_landed_diff = red_sig_strikes_head_landed - 
           blue_sig_strikes_head_landed) %>%
  mutate(sig_strikes_body_landed_diff = red_sig_strikes_body_landed - 
           blue_sig_strikes_body_landed) %>%
  mutate(sig_strikes_leg_landed_diff = red_sig_strikes_leg_landed -
           blue_sig_strikes_leg_landed) %>%
  mutate(takedowns_landed_diff = red_takedowns_landed - 
           blue_takedowns_landed) %>%
  mutate(submissions_attempted_diff = red_submissions_attempted - 
           blue_submissions_attempted) %>%
  mutate(reversals_scored_diff = red_reversals_scored - 
           blue_reversals_scored) %>%
  mutate(control_time_seconds_diff = red_control_time_seconds - 
           blue_control_time_seconds) %>%
  mutate(score_diff = red_score - blue_score) %>%
  mutate(score_string = factor(paste0(red_score, "-", blue_score),
                               levels = c("8-10", "9-10", "10-10", "10-9", 
                                          "10-8"))) %>%
  mutate(judge_name = as.factor(judge_name)) %>%
  select(knockdowns_scored_diff, total_strikes_landed_diff, 
         sig_strikes_head_landed_diff, sig_strikes_body_landed_diff,
         sig_strikes_leg_landed_diff, takedowns_landed_diff, 
         submissions_attempted_diff, reversals_scored_diff, 
         control_time_seconds_diff, judge_name, score_diff, score_string)

# Save for ease of use
saveRDS(xf, "./data/final_merged_features.rds")

# Distribution of scores
table(xf$score_diff, useNA = "always")
table(xf$score_string, useNA = "always")

# Number of judges
length(unique(xf$judge_name))
judge_counts <- table(xf$judge_name)
summary(as.numeric(judge_counts))

xf %>%
  group_by(judge_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  as.data.frame() %>%
  head(10)

# Visualizations
set.seed(123)
xf_diff_long <- xf %>%
  sample_n(1000) %>%
  select(-c(judge_name, score_string)) %>%
  pivot_longer(cols = -score_diff, names_to = "Predictor", values_to = "Value")

ggplot(xf_diff_long, aes(x = Value, y = score_diff)) + 
  geom_point() + 
  geom_jitter(alpha = 0.6, width = 0.2, height = 0.4) +
  facet_wrap(~ Predictor, ncol = 3, scales = "free_x") + 
  theme_minimal() + 
  labs(title = "Score Difference vs. Predictors", x = "Predictor Values", 
       y = "Score Difference")



xf_diff_long_full <- xf %>%
  select(-c(judge_name, score_diff)) %>%
  pivot_longer(cols = -score_string, names_to = "Predictor", 
               values_to = "Value")

ggplot(xf_diff_long_full, aes(x = Value, y = score_string)) + 
  geom_boxplot() +  
  facet_wrap(~ Predictor, ncol = 3, scales = "free_x") + 
  theme_minimal() + 
  labs(title = "Score Category vs. Numeric Predictors", 
       x = "Predictor Values", y = "Score Category")


ggpairs(xf %>% sample_n(1000) %>% select(-c(judge_name, score_diff, 
                                            score_string)),
        upper = list(continuous = wrap("cor", size = 2)),
        lower = list(continuous = wrap("points", size = 0.5))) + 
  theme(axis.text = element_text(size = 3),
        strip.text = element_text(size = 2))


# Linear regression model
levels(xf$judge_name)[1]

m1 <- lm(score_diff ~ knockdowns_scored_diff + total_strikes_landed_diff + 
         sig_strikes_head_landed_diff + sig_strikes_body_landed_diff +
         sig_strikes_leg_landed_diff + takedowns_landed_diff +
         submissions_attempted_diff + reversals_scored_diff +
         control_time_seconds_diff + judge_name, data = xf)
summary(m1)
autoplot(m1)


# Linear regression + interactions for judges
top_20_freq <- xf %>%
  group_by(judge_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  as.data.frame() %>%
  head(20)
top_20_freq

xf_sub <- xf %>%
  filter(judge_name %in% top_20_freq$judge_name) %>%
  mutate(judge_name = factor(judge_name))

m2 <- lm(score_diff ~ judge_name / knockdowns_scored_diff + 
           judge_name / total_strikes_landed_diff + 
           judge_name / sig_strikes_head_landed_diff + 
           judge_name / sig_strikes_body_landed_diff + 
           judge_name / sig_strikes_leg_landed_diff + 
           judge_name / takedowns_landed_diff + 
           judge_name / submissions_attempted_diff + 
           judge_name / reversals_scored_diff + 
           judge_name / control_time_seconds_diff, data = xf_sub)
summary(m2)
autoplot(m2)


# Get coefficients and confidence intervals
model_results <- tidy(m2, conf.int = TRUE)

# Filter for relevant rows
plot_data <- model_results %>%
  filter(grepl(":", term)) %>%
  mutate(Variable = sub(".*:", "", term)) %>%
  mutate(Judge = sub("^judge_name(.*):.*", "\\1", term)) %>%
  select(Judge, Variable, estimate, conf.low, conf.high)

# Create plot
ggplot(plot_data, aes(x = estimate, y = Judge, color = Judge)) +
  geom_point(size = 1.2) +  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +  
  facet_wrap(~ Variable, nrow = 3, scales = "free_x") + 
  theme_minimal() +
  scale_color_hue(h = c(180, 300)) + 
  labs(title = "Marginal Effects of Fight Variables by Judge with 95% CIs",
       x = "Marginal Effect (Coefficient)",
       y = "Judge") +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 8),
        legend.position = "none")


