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
