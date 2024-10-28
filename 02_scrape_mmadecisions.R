# Load libraries
library(rvest)
library(polite)
library(tidyverse)

# Initialize polite session with home URL
base_session <- bow(url = "https://mmadecisions.com/", user_agent = "eugene",
                    force = TRUE, delay = 1)

# Define all functions
# Grab event URLs corresponding to UFC events
get_event_urls <- function(year, bow = base_session) {
  session <- nod(bow = bow, path = paste0("decisions-by-event/", year, "/"))
  scraped_page <- scrape(session)
  
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

# Grab bout URLs based on event URL
get_bout_urls <- function(event_url, bow = base_session) {
  session <- nod(bow = bow, path = event_url)
}

# Grab fighter URLs based on bout URL
get_fighter_urls <- function(bout_url, bow = base_session) {
  session <- nod(bow = bow, path = bout_url)
}

# Get event metadata to be used to map to UFC Stats data
get_event_details <- function(event_url, bow = base_session) {
  session <- nod(bow = bow, path = event_url)
  scraped_page <- scrape(session)
  
  event_details <- scraped_page %>%
    html_node("tr.top-row > td") %>%
    html_text(trim = TRUE) %>%
    str_split("\n") %>%
    unlist() %>%
    str_trim()
  
  event_id <- as.numeric(unlist(str_split(event_url, "/"))[2])
  event_name <- event_details[1]
  venue <- event_details[2]
  city <- paste(head(strsplit(event_details[3], ", ")[[1]], -1), 
                collapse = ", ")
  country <- tail(strsplit(event_details[3], ", ")[[1]], 1)
  date <- as.Date(scraped_page %>% 
                    html_node("tr.bottom-row > td.decision-bottom2") %>% 
                    html_text(trim = TRUE), 
                  format = "%B %d, %Y")
  
  return(data.frame(id = event_id, name = event_name, date = date, 
                    venue = venue, city = city, country = country))
}

# Get bout metadata

# Main code
ufc_urls_all <- map(2024:2023, ~get_event_urls(.x)) %>% unlist()
events <- map_df(ufc_urls_all, get_event_details)