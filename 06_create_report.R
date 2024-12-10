#' ---
#' title: "S&DS 625 Final Project"
#' date: "`r Sys.Date()`"
#' author: "Eugene Han"
#' abstract: "This is a dummy paragraph to test knitted abstract output."
#' output: 
#'   pdf_document:
#'     number_sections: true
#' bibliography: misc/biblio.bib
#' nocite: "@*"
#' ---
#' 

#+ include = FALSE

library(knitr)
library(tidyverse)
library(GGally)
library(ggfortify)

#-

#' 
#' # Introduction
#' 
#' Judging in mixed martial arts (MMA), especially in high-profile promotions 
#' like the Ultimate Fighting Championship (UFC), can be controversial. Although 
#' the Association of Boxing Commissions (ABC) MMA committee has established a 
#' set of criteria for scoring fights, notably "effective striking/grappling," 
#' "effective aggressiveness," and "fighting area control" in order of 
#' decreasing priority, the application of these guidelines is often
#' subjective and open to judges' interpretations. By analyzing round-level 
#' scoring data coupled with corresponding fight performance statistics, this 
#' project aims to explore how different variables, such as the difference in 
#' strikes landed, influence scoring and how that impact may vary across judges.
#' 
#' The dataset used for this investigation is a combination of two separate 
#' datasets sourced from the websites MMA Decisions (scoring data) and UFC Stats
#' (fight performance data). Prior work, such as Nate Latshaw's "Judge AI"
#' project [@Latshaw_2021], has taken a similar approach in terms of combining 
#' data from these same two websites but has primarily focused on predictive 
#' modeling applications to predict a "consensus" judge score by round. Our work 
#' differs in the sense that we are interested in inferential questions and 
#' scoring at the individual judge level, rather than a proxy "ground truth" 
#' score.
#' 
#' The report will be structured as follows: Section 2 will provide an overview
#' of the data used in this project, including the process of obtaining,
#' cleaning, and merging the two sources; Section 3 presents the results of
#' data exploration and our analyses; and Section 4 will summarize our findings,
#' limitations, and future scopes.
#' 
#' \newpage
#' # Data Overview
#' 
#' A significant part of this project was spent obtaining the final dataset due
#' to challenges in scraping, cleaning, and merging the data. Before describing
#' this process in detail, we will first briefly introduce the final dataset
#' with descriptions and sample rows/columns. Below is one such sample for the
#' most recent fight in our data, Rob Font vs. Kyler Phillips on October 19, 
#' 2024:
#' 

#+ echo = FALSE

x <- readRDS("./data/final_merged.rds")
x %>%
  select(round, red_sig_strikes_landed, blue_sig_strikes_landed, judge_id, 
         red_score, blue_score) %>%
  tail(9) %>%
  kable(row.names = FALSE)

#-

#' 
#' In this example, the fight was 3 rounds long and we have scoring data
#' from all 3 judges, giving a total of 9 rows. Fighters either come from the
#' "red" or "blue" corners of the arena, so fighter-specific column names come
#' with a corresponding "red" or "blue" prefix. For example,
#' `red_sig_strikes_landed` represents the number of significant strikes the
#' fighter in the red corner landed on their opponent (fighter in the blue
#' corner) in that round, while `blue_score` represents the score given to the
#' fighter in the blue corner by the judges. All judge scores use a 10-point
#' scale described in the judging criteria/scoring document from the ABC. The
#' `judge_id` column refers to a unique identifier associated with each judge
#' that references the data scraped from MMA Decisions; here are the
#' corresponding rows from the relevant dataframe:
#' 

#+ echo = FALSE

y <- readRDS("./data/MMA Decisions/mmadecisions_judges.rds")
y %>%
  filter(id %in% c(357, 318, 127)) %>%
  kable(row.names = FALSE)

#-

#' 
#' It's important to note that we only displayed a small subset of the columns
#' in the final dataset. The following is a more comprehensive list:
#' 
#' * `[ufcstats/mmadecisions]_bout_id` - Unique identifier for each fight
#' * `[ufcstats/mmadecisions]_event_id` - Unique identifier for the event that
#' the fight belongs to
#' * `[red/blue]_[ufcstats/mmadecisions]_fighter_id` - Unique identifier for
#' each fighter
#' * `round` - Round number, either 1 through 3 or 1 through 5
#' * `[red/blue]_knockdowns_scored` - Number of knockdowns by red/blue corner
#' fighter on opponent due to strikes
#' * `[red/blue]_total_strikes_[landed/attempted]` - Number of total strikes
#' landed/attempted by red/blue corner fighter
#' * `[red/blue]_sig_strikes_[landed/attempted]` - Number of significant strikes
#' landed/attempted in total across all body regions and positions by red/blue 
#' corner fighter
#' * `[red/blue]_sig_strikes_[head/body/leg]_[landed/attempted]` - Number of 
#' significant strikes landed/attempted by red/blue corner fighter by opponent's
#' body region
#' * `[red/blue]_sig_strikes_[distance/clinch/ground]_[landed_attempted]` - 
#' Number of significant strikes landed/attempted by red/blue corner fighter
#' by position
#' * `[red/blue]_takedowns_[landed/attempted]` - Number of takedowns
#' landed/attempted by red/blue corner fighter
#' * `[red/blue]_submissions_attempted` - Number of submission attempts by
#' red/blue corner fighter
#' * `[red/blue]_reversals_scored` - Number of times red/blue corner fighter 
#' reversed their position in grappling or in the clinch from a disadvantageous
#' position to a more advantageous one (e.g. bottom to top control)
#' * `[red/blue]_control_time_seconds` - Number of seconds red/blue corner
#' fighter maintained and controlled an advantageous position in grappling
#' 
#' 
#' ## Acquisition
#' 
#' To respect the UFC's Terms of Service, we did not directly scrape fight
#' data from the UFC Stats website and instead elected to use the data
#' collected and aggregated in a public GitHub repository by user Greco1899 
#' (https://github.com/Greco1899/scrape_ufc_stats). The data used for this 
#' report was downloaded on October 21, 2024 to ensure a fixed cutoff date since 
#' the repository updates on a weekly basis.
#' 
#' To obtain data from the MMA Decisions website, we utilized `rvest` for
#' extracting relevant information from the HTML structure and `polite` for
#' session management and crawling web pages respectfully, ensuring a 5 second
#' delay between requests. The general strategy we took was to grab all
#' necessary URLs and saving them to disk before extracting data and creating
#' the final dataframes so that we avoid any excessive page revisits.
#' 
#' 
#' ## Cleaning
#' 
#' Cleaning the UFC Stats data mainly revolved around converting strings into
#' numeric values and standardizing the data to rely on unique IDs over
#' potentially non-unique names/strings. A particularly relevant example of
#' converting strings to numbers were statistics involving successes and
#' attempts. For instance, the scraped data might list "1 of 2" for takedowns
#' which needs to be parsed into 1 takedown landed and 2 takedowns attempted.
#' In terms of unique IDs, one of the biggest problems in the scraped data was
#' how fight-level data was stored. While each fight could easily be traced back 
#' to a unique bout ID, the IDs of the two fighters were not stored by fight and
#' instead a fight "name" was stored in the form "Red Fighter Name vs. Blue 
#' Fighter Name." This was especially problematic because some fighters have
#' identical first and last names. Moreover, the GitHub user had wrote their
#' scripts such that fighter data would only append new entries while all other
#' data would be rewritten every week; any fighters whose names had been updated
#' over time made it so that a simple join would not handle all cases. As a 
#' result, a nontrivial amount of the matching had to be done manually by
#' cross-referencing the UFC Stats website. The motivation behind all of this
#' was that if we could successfully create a one-to-one mapping between the
#' event IDs and fighter IDs from UFC Stats and those of MMA Decisions, the 
#' fight IDs could then be matched trivially so that all fight statistics had 
#' corresponding judge scores.
#' 
#' Cleaning the MMA Decisions data was significantly easier as a lot of the
#' preprocessing logic had been baked directly into the code for scraping the
#' information from the website. Only the fighter data needed attention to
#' handle duplicate fighters and parsing out fields like dates of birth and
#' nicknames from free-form text which was an artifact of the poor HTML 
#' structure of fighter pages.
#' 
#' 
#' ## Merging
#' 
#' We first tried to match the events between the two data sources. We selected
#' only the event IDs from UFC Stats for which at least one fight in the
#' corresponding event ended in a decision based on the outcome method. We then
#' made the assumption that both data sources had been scraped in such a way
#' that respected their chronological order, making the matching a simple
#' concatenation of event ID columns. To sanity check this, we computed the
#' number of fights that ended in decision by event per data source and
#' manually inspected any discrepancies. All discrepancies were confirmed to be
#' edge cases where the fights had originally ended in decisions but were
#' later overturned to no contests (due to reasons such as failed drug tests)
#' according to UFC Stats, but were not listed on the MMA Decisions website.
#' 
#' Next, we matched the fighters. We first filtered for fighter IDs that were
#' present only in fights that originally ended in decisions according to UFC
#' Stats, narrowing the number of candidates and reducing the chance for false
#' one-to-many relationships. We then filtered for fighters from both data
#' sources that had unique names and temporarily saved the exact name matches. 
#' With the remaining fighters, we repeated this process for exact matches
#' for unique dates of birth and nicknames and finally manually matching the
#' remaining unmatched fighters.
#' 
#' To match bout IDs, we took the bout data for MMA Decisions, which had the
#' bout ID, event ID, and both fighter IDs with respect to the MMA Decisions
#' website, joined it with the event and fighter ID mappings mentioned above, 
#' and finally joined this result with the bout data for UFC Stats, which also 
#' contains the bout ID, event ID, and both fighter IDs with respect to the UFC 
#' Stats website. The last step was to join this with the fighting statistics 
#' from UFC Stats and judge scoring data from MMA Decisions, making sure the
#' round numbers lined up.
#' 
#' 
#' ## Final Preprocessing
#' 
#' This merged dataset still had a few issues to address. First, judges can
#' deduct one or more points in any round for certain infractions such as
#' eye pokes or egregious fence holding. This can create situations where the
#' fight statistics clearly point to a "10-9" round in favor of a fighter, but
#' a point deduction results in a "9-9" round. Since these deductions are
#' shared across all judges and completely independent of the striking and
#' grappling statistics, we are more interested in investigating the "what-if"
#' scores without these deductions. We filtered for fights with these strange
#' scores where both fighters' scores were less than 10 or corresponded to
#' outcome method text information from UFC Stats that suggested point
#' deductions. We then cross-referenced MMA Decisions to find out the exact
#' round, number of points deducted, and the corresponding fighter for each of
#' these cases and manually created a CSV file that could be joined with our
#' merged dataset from above to add those points back.
#' 
#' Next, we filtered out any remaining rows where judges scored a "10-7" round
#' for a fighter (equivalently, where fighters' scores differed by more than 2).
#' Not only are these incredibly rare (only 3 cases remained), but judges are
#' generally discouraged from scoring rounds as "10-7" per the ABC's guidelines.
#' Moreover, these guidelines' description of a "10-7" round heavily overlaps
#' with that of a "10-8" round.
#' 
#' Additionally, MMA Decisions has recorded some judge scores where the judge's
#' identity was unknown, leading to a missing judge ID. Lastly, we include rows 
#' where judges have scored at least a full fight, a minimum of 3 rounds, 
#' resulting in a final dataframe with 28690 rows.
#' 
#' 
#' # Results
#' 
#' We now present the results of our study, which includes data exploration
#' and analysis. In the exploration section, we derive candidates for response
#' variables as well as engineer a basic set of features based on domain
#' knowledge that we believe would fundamentally influence judge scoring. We
#' then visualize these variables and their relationships between each other
#' and the response(s). In the analysis section, we **TODO**
#' 
#' 
#' ## Exploration
#' 
#' One way to define a response variable for analysis purposes is to calculate
#' the difference between `red_score` and `blue_score`, resulting in 
#' `score_diff` which is a discrete, numeric variable. Inspecting the
#' distribution of these score differences, we see that judges rarely give
#' a tie score (equivalently a "10-10") with the bulk of scores resulting in
#' a fighter having a one point advantage. We also see that the fighter in the
#' red corner tends to win rounds more often in both the one and two point 
#' advantage scenarios.
#' 

#+ echo = FALSE

xf <- readRDS("./data/final_merged_features.rds")
table(xf$score_diff, useNA = "always")

#-

#' 
#' Alternatively, we can create a factor by concatenating the scores into a
#' string in the form "{`red_score`}-{`blue_score`}" and specify the order of
#' the levels, resulting in an ordinal variable. This not only respects the 
#' natural ordering of `score_diff` but also accounts for the fact that the 
#' "gaps" between a "10-10" and a "10-9" versus between a "10-9" and "10-8" are 
#' not the same nor well-defined, which isn't captured by a discrete numeric
#' scale. We can see that this preserves the distribution seen above.
#' 

#+ echo = FALSE

table(xf$score_string, useNA = "always")

#-

#' 
#' In total, there are 394 unique judges represented in this dataset, with
#' significant variability in the number of scored rounds by judge, ranging
#' from just 3 rounds to 2935 rounds.
#' 

#+ echo = FALSE

judge_counts <- table(xf$judge_name)
summary(as.numeric(judge_counts))

#-

#' 
#' If we join with the judges data to use judge names over their IDs, we can
#' obtain the top 10 judges with the largest number of rounds scored:
#' 

#+ echo = FALSE

xf %>%
  group_by(judge_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  as.data.frame() %>%
  head(10)

#-

#' 
#' Next, we define 9 engineered features that encode comparative information
#' that directly pit the two fighters' performance against each other.
#' 
#' * `knockdowns_scored_diff` - Defined as (`red_knockdowns_scored` - 
#' `blue_knockdowns_scored`). If a fighter scores a knockdown, this typically
#' means that they dealt a substantial amount of damage to their opponent in a 
#' strike, so we should expect a positive difference to be associated with a
#' larger score discrepancy in favor of the fighter in the red corner.
#' 
#' * `sig_strikes_[head/body/leg]_landed_diff` - Defined as 
#' (`red_sig_strikes_[head/body/leg]_landed` - 
#' `blue_sig_strikes_[head/body/leg]_landed`). We break this feature down by
#' the targeted body region since we should expect significant strikes to the
#' head and body will be more impactful and damaging than those to the leg.
#' We don't include the features by position since there would be some
#' redundancy and there isn't a strong justification for why strikes from a 
#' certain position would contribute more to scoring than other positions.
#' 
#' * `total_strikes_landed_diff` - Defined as (`red_total_strikes_landed` - 
#' `blue_total_strikes_landed`). Captures any striking advantaage holistically,
#' including non-significant strikes.
#' 
#' * `takedowns_landed_diff` - Defined as (`red_takedowns_landed` - 
#' `blue_takedowns_landed`). A large difference in the number of takedowns
#' landed would suggest more aggressiveness and a dominance on the ground from
#' a fighter.
#' 
#' * `submissions_attempted_diff` - Defined as (`red_submissions_attempted` - 
#' `blue_submissions_attempted`). A large difference in this variable would
#' represent a higher skill in grappling and putting the opponent into
#' disadvantageous situations.
#' 
#' * `reversals_scored_diff` - Defined as (`red_reversals_scored` - 
#' `blue_reversals_scored`). Similar justification as above.
#' 
#' * `control_time_seconds_diff` - Defined as (`red_control_time_seconds` - 
#' `blue_control_time_seconds`). Similar justification as above.
#' 
#' 
#' To visualize these features' relationship with the score difference, we plot
#' scatterplots for each feature with `score_diff` for now. We sample 2500 rows
#' for computational efficiency and to make the plot a bit cleaner since we are
#' just interested in seeing if there are any general trends. Since there's
#' quite a bit of overlapping points, we also jitter the points a little bit.
#' 

#+ echo = FALSE

set.seed(123)
xf_diff_long <- xf %>%
  sample_n(2500) %>%
  select(-c(judge_name, score_string)) %>%
  pivot_longer(cols = -score_diff, names_to = "Predictor", values_to = "Value")

ggplot(xf_diff_long, aes(x = Value, y = score_diff)) + 
  geom_point() + 
  geom_jitter(alpha = 0.4, width = 0.2, height = 0.4) +
  facet_wrap(~ Predictor, ncol = 3, scales = "free_x") + 
  theme_minimal() + 
  labs(title = "Score Difference vs. Numeric Predictors", 
       x = "Predictor Values", y = "Score Difference")

#-

#' 
#' In general, we see approximately linear positive relationships between our
#' engineered features and `score_diff` as hypothesized, although this isn't
#' as clear for `reversals_scored_diff` and `submissions_attempted_diff`. We 
#' can also repeat these with boxplots and `score_string` using the full
#' dataset.
#' 

#+ echo = FALSE

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

#-

#' 
#' One interesting observation we get here is that for 
#' `control_time_seconds_diff`, in order to score a "10-8"/"8-10" a fighter
#' needs to have a substantial median control time advantage when compared to 
#' scoring a "10-9"/"9-10". The change between the median control time 
#' difference when going from a "10-10" score to "10-9"/"9-10" is much smaller
#' than when going from "10-9"/"9-10" to "10-8"/"8-10".
#' 
#' Lastly, we make a pair plot of all the numeric predictors we constructed to
#' investigate if there is any substantial multicollinearity. We only sampled
#' 1000 rows to reduce the computational load of plotting. For the majority
#' of the pairwise comparisons, our variables have little to no correlation
#' ranging between around -0.3 to 0.3. With that being said, there are two pairs 
#' that stand out: `total_strikes_landed_diff` with 
#' `sig_strikes_head_landed_diff` and `takedowns_landed_diff` with 
#' `control_time_seconds_diff`. These pairs of variables have somewhat 
#' moderately strong correlations of 0.676 and 0.638, respectively.
#' 
#' This isn't very surprising since we expect fighters to aim more for the head
#' to inflict more damage, so strikes to the head would naturally make up a
#' larger portion of total strikes landed and scale accordingly. Moreover, if
#' a fighter lands more takedowns, we should expect them to spend more time on
#' the ground in a dominant position. Although this multicollinearity may cause
#' issues with respect to having less reliable coefficient estimates, we
#' believe it's still important to include these features since they capture
#' different facets of performance that aren't necessarily redundant. For
#' instance, if two fighters are neck and neck on significant strikes, any
#' differences in overall total strikes may be a deciding factor. Also,
#' takedowns landed measure a fighter's ability to get their opponent to the
#' ground, but the control time quantifies how effective that fighter is on
#' the ground and how well they take advantage of that positioning.
#' 

#+ echo = FALSE

ggpairs(xf %>% sample_n(1000) %>% select(-c(judge_name, score_diff, 
                                            score_string)),
        upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", size = 0.5))) + 
  theme(axis.text = element_text(size = 4),
        strip.text = element_text(size = 3))

#-

#' 
#' ## Analysis
#' 
#' ### Baseline Linear Regression
#' 
#' As a preliminary analysis, we fit a linear regression model with `score_diff`
#' as our response variable, loosely treating it as a continuous variable, with
#' our 9 engineered features as numeric predictors and `judge_name` as a
#' categorical predictor. Since `judge_name` is a factor that hasn't been
#' releveled, the default (alphabetical) ordering is used such that the judge
#' "Aaron Chatfield" is our reference level.
#' 

#+ echo = FALSE

m1 <- lm(score_diff ~ knockdowns_scored_diff + total_strikes_landed_diff + 
           sig_strikes_head_landed_diff + sig_strikes_body_landed_diff +
           sig_strikes_leg_landed_diff + takedowns_landed_diff +
           submissions_attempted_diff + reversals_scored_diff +
           control_time_seconds_diff + judge_name, data = xf)
options(max.print = 70)
summary(m1)

#-

#' 
#' Based on our model summary, we see that all of our engineered numeric
#' predictors are statistically significant, although this is partially an
#' artifact of having such a large sample size such that even small deviations
#' from zero can be flagged as significant. Still, this model yields some
#' useful information to us. We can see that the signs and magnitudes of the
#' coefficients for each of these features align with our expectations that
#' 
#' With that being said, it's important to point out the obvious limitations of
#' this model.
#' 

#+ echo = FALSE

autoplot(m1)

#-

#' 
#' **Discuss diagnostic plots**
#' 
#' 
#' 
#' ### Linear Regression with Interaction Terms
#' 
#' 
#' 
#' # Conclusion
#' 
#' \newpage
#' # References