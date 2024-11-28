# Load libraries
library(rvest)
library(polite)
library(svMisc)
library(tidyverse)

# Initialize polite session with home URL
base_session <- bow(url = "https://mmadecisions.com/", user_agent = "eugene",
                    force = TRUE, delay = 5)

# Define all functions
# Grab event URLs corresponding to UFC events
get_event_urls <- function(year, bow = base_session) {
  session <- nod(bow = bow, path = paste0("decisions-by-event/", year, "/"))
  scraped_page <- scrape(session)
  
  # Get relative links, names, and dates of events
  event_urls <- scraped_page %>%
    html_nodes("tr.decision > td.list > a") %>%
    html_attr("href")
  event_names <- scraped_page %>%
    html_nodes("tr.decision > td.list > a") %>%
    html_text()
  event_dates <- scraped_page %>%
    html_nodes("tr.decision > td.list-center") %>%
    html_text() %>%
    str_trim()
  
  stopifnot(length(event_dates) %% 2 == 0)
  event_dates <- as.Date(event_dates[seq_along(event_dates) %% 2 > 0],
                         format = "%B %d, %Y")
  
  # Filter for only UFC events up to October 21, 2024 (our data cutoff date)
  # Ignore UFC 5 and 7, these don't actually have decision info
  ufc_urls <- event_urls[which(grepl("UFC|UU|UFN|TUF|Ortiz/Shamrock 3", 
                                     event_names) &
                               (event_dates <= "2024-10-21") &
                               (!event_names %in% c("UFC 5: The Return of the Beast",
                                                    "UFC 7: The Brawl in Buffalo")))]
  
  return(ufc_urls)
}

# Get event metadata to be used to map to UFC Stats data
get_event_details <- function(event_url, scraped_event_page) {
  event_info <- scraped_event_page %>%
    html_node("tr.top-row > td") %>%
    html_text(trim = TRUE) %>%
    str_split("\n") %>%
    unlist() %>%
    str_trim()

  event_id <- as.numeric(unlist(str_split(event_url, "/"))[2])
  
  # Handle a few edge cases from poor data quality
  if(event_id == 1003) {
    event_info[3] <- "Montevideo, Uruguay"
  } else if(event_id == 1112) {
    event_info[2] <- "UFC Apex"
    event_info[3] <- "Las Vegas, Nevada, USA"
  }
  
  event_name <- event_info[1]
  venue <- event_info[2]
  city <- paste(head(str_split(event_info[3], ", ")[[1]], -1), 
                collapse = ", ")
  country <- tail(str_split(event_info[3], ", ")[[1]], 1)
  date <- as.Date(scraped_event_page %>% 
                    html_node("tr.bottom-row > td.decision-bottom2") %>% 
                    html_text(trim = TRUE), 
                  format = "%B %d, %Y")
  
  event_df <- data.frame(id = event_id, name = event_name, date = date,
                         venue = venue, city = city, country = country)
  
  return(event_df)
}

# Get bout metadata
get_bout_details <- function(bout_url, scraped_bout_page) {
  bout_id <- as.numeric(unlist(str_split(bout_url, "/"))[2])
  
  # Get event id corresponding to the bout
  event_url <- scraped_bout_page %>% 
    html_node("tr.top-row > td.decision-top2 > b > a") %>% 
    html_attr("href")
  event_id <- as.numeric(unlist(str_split(event_url, "/"))[2])
  
  # Get fighter URLs and ids
  fighter1_url <- scraped_bout_page %>% 
    html_node("tr.top-row > td.decision-top > a") %>% 
    html_attr("href")
  fighter2_url <- scraped_bout_page %>% 
    html_node("tr.bottom-row > td.decision-bottom > a") %>% 
    html_attr("href")
  fighter1_id <- as.numeric(unlist(str_split(fighter1_url, "/"))[2])
  fighter2_id <- as.numeric(unlist(str_split(fighter2_url, "/"))[2])
  
  bout_df <- data.frame(id = bout_id, event_id = event_id, 
                        fighter1_id = fighter1_id, fighter2_id = fighter2_id)
  
  return(bout_df)
}

# Get bout scores by round
get_bout_scores <- function(bout_url, scraped_bout_page) {
  bout_id <- as.numeric(unlist(str_split(bout_url, "/"))[2])
  
  judge_urls <- scraped_bout_page %>%
    html_nodes("tr.top-row > td.judge > a") %>%
    html_attr("href")
  judge_ids <- vapply(str_split(judge_urls, "/"), function(x) as.numeric(x[2]),
                      numeric(1))
  
  if(length(judge_ids) < 3) {
    judge_ids <- c(judge_ids, rep(NA, 3 - length(judge_ids)))
  }
  stopifnot(length(judge_ids) == 3)
  
  fighter1_url <- scraped_bout_page %>% 
    html_node("tr.top-row > td.decision-top > a") %>% 
    html_attr("href")
  fighter2_url <- scraped_bout_page %>% 
    html_node("tr.bottom-row > td.decision-bottom > a") %>% 
    html_attr("href")
  fighter1_id <- as.numeric(unlist(str_split(fighter1_url, "/"))[2])
  fighter2_id <- as.numeric(unlist(str_split(fighter2_url, "/"))[2])
  
  score_info <- scraped_bout_page %>% 
    html_nodes("table > tr > td > table > tr.decision > td.list") %>% 
    html_text(trim = TRUE)
  stopifnot(length(score_info) %% 9 == 0)
  
  score_info <- na_if(score_info, "-")
  num_rounds <- length(score_info) / 9
  rounds <- as.numeric(score_info[seq_along(score_info) %% 3 == 1])
  fighter1_scores <- as.numeric(score_info[seq_along(score_info) %% 3 == 2])
  fighter2_scores <- as.numeric(score_info[seq_along(score_info) %% 3 == 0])
  
  # Organize dataframe such that each row is one score from one judge for
  # one of the fighters in a bout round
  bout_scores_df <- data.frame(id = bout_id, round = rep(rounds, 2),
                               fighter_id = c(rep(fighter1_id, num_rounds * 3), 
                                              rep(fighter2_id, num_rounds * 3)),
                               judge_num = rep(c(rep(1, num_rounds),
                                                 rep(2, num_rounds),
                                                 rep(3, num_rounds)),
                                               2),
                               judge_id = rep(c(rep(judge_ids[1], num_rounds),
                                                rep(judge_ids[2], num_rounds),
                                                rep(judge_ids[3], num_rounds)), 
                                              2),
                               score = c(fighter1_scores, fighter2_scores))
  stopifnot(ncol(bout_scores_df) == 6)
  
  return(bout_scores_df)
}

# Get judge info
get_judge_details <- function(scraped_bout_page) {
  judge_urls <- scraped_bout_page %>%
    html_nodes("tr.top-row > td.judge > a") %>%
    html_attr("href")
  judge_names <- scraped_bout_page %>%
    html_nodes("tr.top-row > td.judge > a") %>%
    html_text(trim = TRUE)
  
  judge_ids <- sapply(str_split(judge_urls, "/"), function(x) as.numeric(x[2]))
  judge_df <- data.frame(id = judge_ids, name = judge_names)
  
  return(judge_df)
}

# Get fighter details
get_fighter_details <- function(fighter_url, bow = base_session) {
  session <- nod(bow = bow, path = paste0(fighter_url, "/"))
  scraped_fighter_page <- scrape(session)
  
  fighter_id <- as.numeric(unlist(str_split(fighter_url, "/"))[2])
  fighter_name <- scraped_fighter_page %>% 
    html_node("tr.top-row > td.judge2") %>% 
    html_text(trim = TRUE)
  
  tables <- scraped_fighter_page %>% 
    html_nodes("table > tr > td > table")
  tds <- tables[1] %>% 
    html_nodes("tr > td.top-cell, tr > td.list")
  
  born_info <- NA
  height_info <- NA
  reach_info <- NA
  nickname_info <- NA
  fought_out_of_info <- NA
  
  current_category <- NA
  category_data <- c()
  
  # Extract out data for relevant fields
  # This code is necessary due to the site's poor HTML design and the fact that
  # some headers like "BORN" can have multiple separate pieces of info that
  # fall under it
  for(i in seq_along(tds)) {
    node <- tds[i]
    class <- html_attr(node, "class")
    
    if(class == "top-cell") {
      if(!is.na(current_category) && length(category_data) > 0) {
        combined <- paste(category_data, collapse = "[SEP]")
        
        if(current_category == "BORN") {
          born_info <- combined
        } else if(current_category == "HEIGHT") {
          height_info <- combined
        } else if(current_category == "REACH") {
          reach_info <- combined
        } else if(current_category == "NICKNAME(S)") {
          nickname_info <- combined
        } else if(current_category == "HAS FOUGHT OUT OF") {
          fought_out_of_info <- combined
        }
        
        category_data <- c()
      }
      
      current_category <- node %>%
        html_text(trim = TRUE)
    } else if(class == "list" && !is.na(current_category)) {
      value <- node %>%
        html_text(trim = TRUE) %>%
        gsub("\\r|\\n|\\t", "", .)
      category_data <- c(category_data, value)
    }
  }
  
  if(!is.na(current_category) && length(category_data) > 0) {
    combined <- paste(category_data, collapse = "[SEP]")
    
    if(current_category == "BORN") {
      born_info <- combined
    } else if(current_category == "HEIGHT") {
      height_info <- combined
    } else if(current_category == "REACH") {
      reach_info <- combined
    } else if(current_category == "NICKNAME(S)") {
      nickname_info <- combined
    } else if(current_category == "HAS FOUGHT OUT OF") {
      fought_out_of_info <- combined
    }
  }
  
  fighter_df <- data.frame(id = fighter_id, name = fighter_name, 
                           born_info = born_info, height = height_info,
                           reach = reach_info, nickname_info = nickname_info,
                           fought_out_of_info = fought_out_of_info)
  
  return(fighter_df)
}


# Main code
# Define dataframe paths
events_df_path <- "./data/MMA Decisions/raw/events.rds"
bouts_df_path <- "./data/MMA Decisions/raw/bouts.rds"
bouts_scores_df_path <- "./data/MMA Decisions/raw/bouts_scores.rds"
judges_df_path <- "./data/MMA Decisions/raw/judges.rds"
fighters_df_path <- "./data/MMA Decisions/raw/fighters.rds"

# Define checkpoint paths
bout_urls_path <- "./data/MMA Decisions/checkpoints/bout_urls.rds"
fighter_urls_path <- "./data/MMA Decisions/checkpoints/fighter_urls.rds"

if(!file.exists(events_df_path) | !file.exists(bout_urls_path)) {
  # Get URLs of all UFC events with decisions
  ufc_urls_all <- unlist(map(2024:1995, ~get_event_urls(.x)))
  
  # Loop over event URLs to construct events dataframe and get bout URLs
  event_df_list <- vector(mode = "list", length = length(ufc_urls_all))
  bout_urls_list <- vector(mode = "list", length = length(ufc_urls_all))
  
  for (i in 1:length(ufc_urls_all)) {
    progress(i, length(ufc_urls_all))
    session <- nod(bow = base_session, path = paste0(ufc_urls_all[i], "/"))
    scraped_event_page <- scrape(session)
    
    event_df_list[[i]] <- get_event_details(ufc_urls_all[i], scraped_event_page)
    bout_urls_list[[i]] <- scraped_event_page %>%
      html_nodes("td.list2 > b > a") %>%
      html_attr("href") %>%
      str_trim()
  }
  
  events <- do.call(rbind, event_df_list) %>%
    arrange(id)
  bout_urls_all <- rev(unlist(bout_urls_list))
  
  # Save events dataframe
  saveRDS(events, events_df_path)
  
  # Save bout URLs to avoid having to scrape again
  saveRDS(bout_urls_all, bout_urls_path)
}

if(!file.exists(bouts_df_path) | !file.exists(bouts_scores_df_path) |
   !file.exists(judges_df_path) | !file.exists(fighter_urls_path)) {
  # Load in bout URLs
  bout_urls_all <- readRDS(bout_urls_path)
  
  # Loop over bout URLs to get bout info, scoring, fighter URLs, and referees
  bout_df_list <- vector(mode = "list", length = length(bout_urls_all))
  bout_scores_df_list <- vector(mode = "list", length = length(bout_urls_all))
  judge_df_list <- vector(mode = "list", length = length(bout_urls_all))
  fighter_urls_list <- vector(mode = "list", length = length(bout_urls_all))
  
  for (i in 1:length(bout_urls_all)) {
    progress(i, length(bout_urls_all))
    session <- nod(bow = base_session, path = paste0(bout_urls_all[i], "/"))
    scraped_bout_page <- scrape(session)
    
    bout_df_list[[i]] <- get_bout_details(bout_urls_all[i], scraped_bout_page)
    bout_scores_df_list[[i]] <- get_bout_scores(bout_urls_all[i], 
                                                scraped_bout_page)
    judge_df_list[[i]] <- get_judge_details(scraped_bout_page)
    
    fighter1_url <- scraped_bout_page %>% 
      html_node("tr.top-row > td.decision-top > a") %>% 
      html_attr("href")
    fighter2_url <- scraped_bout_page %>% 
      html_node("tr.bottom-row > td.decision-bottom > a") %>% 
      html_attr("href")
    fighter_urls_list[[i]] <- c(fighter1_url, fighter2_url)
  }
  
  bouts <- do.call(rbind, bout_df_list)
  bouts_scores <- do.call(rbind, bout_scores_df_list)
  judges <- do.call(rbind, judge_df_list) %>%
    distinct(id, .keep_all = TRUE) %>%
    arrange(id)
  fighter_urls_all <- unique(unlist(fighter_urls_list))
  
  # Save bouts, bouts scores, and judges dataframes
  saveRDS(bouts, bouts_df_path)
  saveRDS(bouts_scores, bouts_scores_df_path)
  saveRDS(judges, judges_df_path)
  
  # Save fighter URLs to avoid having to scrape again
  saveRDS(fighter_urls_all, fighter_urls_path)
}

if(!file.exists(fighters_df_path)) {
  # Load in fighter URLs
  fighter_urls_all <- readRDS(fighter_urls_path)
  fighters <- map_df(fighter_urls_all, get_fighter_details) %>%
    arrange(id)
  
  # Save fighters dataframe
  saveRDS(fighters, fighters_df_path)
}