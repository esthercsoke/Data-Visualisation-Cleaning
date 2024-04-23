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
tmdb <- read_csv("TMDB_movie_dataset_v11.csv")

# select columns for Q1
tmdb_q1 <- tmdb %>%
  select(title, vote_average, release_date, revenue, status, genres, vote_count)


# set release_date as date type
tmdb_q1$release_date <- as.Date(tmdb_q1$release_date, format="%Y-%m-%d")


# filter by stats = released
# release date
# vote_average
# genres
# https://dplyr.tidyverse.org/reference/filter.html

tmdb_q1 <- tmdb_q1 %>%
  filter(status == "Released", ignore.case = TRUE, na.rm = TRUE) %>%
  filter(revenue != '0', na.rm = TRUE ) %>%
  drop_na(release_date) %>%
  drop_na(genres) %>%
  filter(vote_count > 1000) 


# Convert to millions and rounding to two decimal places
tmdb_q1$revenue <- round(tmdb_q1$revenue / 1e6, 2)


# sep columns for genres
tmdb_q1 <- tmdb_q1 %>% separate(genres, c("genre1"), ",")


# check distribution
# first look showed a lot of 0s for revenue and vote_average
movies <- tmdb_q1 %>% 
  mutate(year = as.integer(substr(release_date, 1, 4)))


# check how many duplicates are in the 
duplicate_counts <- movies %>%
  group_by(title, year) %>%
  summarize(count = n(), .groups = 'drop') %>%
  filter(count > 1) %>%
  select(title, count)


# year year annd month
tmdb_q1$year <- year(tmdb_q1$release_date)
tmdb_q1$month <- month(tmdb_q1$release_date)

# group by year and month
# average the rating by film genre

# Group and summarising
final_q1 <- tmdb_q1 %>%
  group_by(year, month, genre1) %>%
  summarise(average_rating = mean(vote_average), .groups = 'drop')


# convert months to char (names)
final_q1$month <- month.name[final_q1$month]


# round  average rating to 2 dec point
final_q1$average_rating <- round(final_q1$average_rating, 2)



# descriptive stats

str(tmdb_q1)

plot_histogram(tmdb_q1)

describe(tmdb_q1)




