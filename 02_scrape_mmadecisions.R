# Load libraries
library(rvest)
library(polite)
library(tidyverse)

base_session <- bow(url = "https://mmadecisions.com/", user_agent = "eugene",
                    force = TRUE, delay = 1)

get_event_urls <- function(year, bow = base_session) {
  session <- nod(bow = bow, path = paste0("decisions-by-event/", year, "/"))
  scraped_page <- scrape(session)
  
  event_urls_all <- scraped_page %>%
    html_nodes("tr.decision > td.list > a") %>%
    html_attr("href")
  event_names_all <- scraped_page %>%
    html_nodes("tr.decision > td.list > a") %>%
    html_text()
  
  ufc_urls <- event_urls_all[which(grepl("UFC|UU|UFN|TUF|Ortiz/Shamrock 3", 
                                         event_names_all))]
  
  return(ufc_urls)
}

ufc_urls_all <- map(2024:1995, ~get_event_urls(.x)) %>% unlist()
