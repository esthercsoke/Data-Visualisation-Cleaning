#upload regular used libraries
library(DataExplorer)
library(tidyverse)
library(lubridate)
library(ggtext)
library(ggdist)
library(glue)
library(patchwork)
library(showtext)
library(bbplot)
library(htmltools)
library(psych)


# read TMDB csv file
tmdb_q1 <- read_csv("tmdb.csv")


# filter by stats = released
# release date
# vote_average
# genres
# https://dplyr.tidyverse.org/reference/filter.html




# year year annd month
tmdb_q1$month <- month(tmdb_q1$release_date)

# group by year and month
# average the rating by film genre
# Group and summarising
tmdb_q1 <- tmdb_q1 %>%
  group_by(release_year, month, genre) %>%
  summarise(
    average_rating = mean(vote_average, na.rm = TRUE),  
    average_revenue = mean(revenue, na.rm = TRUE),      
    .groups = 'drop'  
  )



# round  average rating to 2 dec point
tmdb_q1$average_rating <- round(tmdb_q1$average_rating, 2)

tmdb_q1$average_revenue <- round(tmdb_q1$average_revenue, 2)


# Check for NA values in year or month
final_q1 <- tmdb_q1 %>%
  filter(!is.na(year) & !is.na(month) & !is.na(average_revenue))


# Convert year and month to a date (first day of each month)
tmdb_q1$date_ts <- make_date(final_q1$release_year, final_q1$month, 1)


# descriptive stats

str(tmdb_q1)

plot_histogram(tmdb_q1)

plot_intro(tmdb_q1)


# write to csv
write_csv(tmdb_q1,"question1.csv")




