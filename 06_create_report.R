#' ---
#' title: "S&DS 625 Final Project"
#' date: "`r Sys.Date()`"
#' author: "Eugene Han"
#' abstract: "This is a dummy paragraph to test knitted abstract output."
#' output: 
#'   pdf_document:
#'     number_sections: true
#' bibliography: deliverables/biblio.bib
#' nocite: "@*"
#' ---
#' 

#+ include = FALSE

library(knitr)
library(tidyverse)

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
#' 
#' 
#' 
#' ## Final Preprocessing
#' 
#' # Results
#' 
#' ## Exploration
#' 
#' ## Analysis
#' 
#' # Conclusion
#' 
#' \newpage
#' # References