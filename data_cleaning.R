#upload regular used libraries
library(DataExplorer)
library(tidyverse)
library(lubridate)


# read TMDB csv file
tmdb_clean <- read_csv("TMDB_movie_dataset_v11.csv")
# read imdb
imdb_clean <- read_csv("IMDbMovies-Clean.csv")

######## IMPORTANT FEATURES

####### TMDB
#‘release_date’,  ‘genres’,  ‘revenue’  ‘vote_average’, status (for knowing if released),
# title, release year, budget
# columns to check for cleaning, status, vote count

####### IMDB
# main genres, release year, rating, title, gross colummns
# for cleaning, rating number, rating 


# Remove un-needed data columns
# deselect columns for tmdb and assignment to new variable
tmdb_clean <- tmdb_clean %>%
  select(-id,  -backdrop_path, -homepage, -imdb_id, -overview, -poster_path, -tagline, -spoken_languages, -production_countries)


# deselect columns for imdb and assignment to new variable
imdb_clean <- imdb_clean %>%
  select(-Summary, -Director, -Writer)



# Standardise Column Names to snake case
#  imdb
imdb_clean <- imdb_clean %>%
  rename(
    title = Title,
    genres = `Main Genres`,
    release_year = `Release Year`,
    vote_average = `Rating (Out of 10)`,
    vote_count_th = `Number of Ratings (in thousands)`,
    budget = `Budget (in millions)`,
    revenue_north_america = `Gross in US & Canada (in millions)`,
    revenue_worldwide = `Gross worldwide (in millions)`,
    revenue_opening_weekend = `Gross Opening Weekend (in millions)`,
    classification_rating = `Motion Picture Rating`,
    runtime = `Runtime (Minutes)`,
    release_year_US = `Opening Weekend in US & Canada`)



# # Filter imdb by 2020 - 2024
imdb_clean <- imdb_clean %>% 
  filter(release_year >= '2000' & release_year <= '2024')

# create a release_year column to filter
tmdb_clean$release_year <- year(tmdb_clean$release_date) 
# tmdb_clean$release_month <- month(tmdb_clean$release_date)
# tmdb_clean$timeseries_date <- make_date(final_q1$year, final_q1$month, 1)

# Filter tmdb by 2020 - 2024
tmdb_clean <- tmdb_clean %>%
  filter(release_year >= '2000' & release_year <= '2024')



# Convert revenue to millions
tmdb_clean$revenue <- tmdb_clean$revenue / 1e6

# Convert budget to millions
tmdb_clean$budget <- tmdb_clean$budget / 1e6


##############################
# Filter Revenue > 1mil
##############################


tmdb_clean <- tmdb_clean %>%
  filter(revenue > 100, na.rm = TRUE)

imdb_clean <- imdb_clean %>%
  filter(revenue_worldwide > 100, na.rm = TRUE)



############### IMDB, convert 3 dec to 2 decimal points


imdb_clean$revenue_opening_weekend <- round(imdb_clean$revenue_opening_weekend, 2)
imdb_clean$revenue_worldwide <- round(imdb_clean$revenue_worldwide, 2)
imdb_clean$revenue_north_america <- round(imdb_clean$revenue_north_america, 2)
imdb_clean$budget <- round(imdb_clean$budget, 2)


tmdb_clean$vote_count <- round(tmdb_clean$vote_count, 1)
tmdb_clean$vote_average <- round(tmdb_clean$vote_average, 1)
tmdb_clean$budget <- round(tmdb_clean$budget, 2)
tmdb_clean$revenue <- round(tmdb_clean$revenue, 2)
tmdb_clean$popularity <- round(tmdb_clean$popularity, 2)


# filter by english og language
tmdb_clean <- tmdb_clean %>%
  filter(original_language == 'en')


# Modifying tmdb_clean to separate genres, convert to lowercase, and trim whitespace
tmdb_clean <- tmdb_clean %>%
  separate(genres, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "genre_6"), sep = ",", extra = "drop", fill = "right") %>%
  mutate(across(starts_with("genre"), ~ trimws(tolower(.))))

# remove whitespace
imdb_clean <- imdb_clean %>%
  separate(genres, c("genre_1", "genre_2", "genre_3"), sep = ",", extra = "drop", fill = "right") %>%
  mutate(across(starts_with("genre"), ~ trimws(tolower(.))))

## replace sci fi text with science fiction
imdb_clean <- imdb_clean %>%
  mutate(across(starts_with("genre"), ~ str_replace_all(., "sci-fi", "science fiction")))

# # drop NA for genres
# tmdb_clean <- tmdb_clean %>%
#   filter(!is.na(genres))





# Modifying tmdb_clean to separate production companies, convert to lowercase, and trim whitespace
tmdb_clean <- tmdb_clean %>%
  separate(production_companies, c("prod_1", "prod_2", "prod_3", "prod_4", "prod_5", "prod_6"), sep = ",", extra = "drop", fill = "right") %>%
  mutate(across(starts_with("prod"), ~ trimws(tolower(.))))




# remove tmdb columns
tmdb_clean <- tmdb_clean %>%
  select(-prod_2, -prod_3, -prod_4, -prod_5, -prod_6,  -genre_2, -genre_3, -genre_4, -genre_5, -genre_6,-status,-original_language, -original_title)

#rename tmdb columns
tmdb_clean <- tmdb_clean %>%
  rename(
    prod_company = prod_1,
    genre = genre_1
  )


# remove imdb genre columns
imdb_clean <- imdb_clean %>%
  select( -genre_2, -genre_3)





# Fix Dates in IMDB, as the release_year should be the year for US
imdb_clean$release_year_US <- as.Date(imdb_clean$release_year_US, format="%m.%d.%Y")
imdb_clean$release_year <- year(imdb_clean$release_year_US) 


# Check Missing Values of data
plot_missing(tmdb_clean)
plot_missing(imdb_clean)



# Remove 0 values for vote_average in tmdb
tmdb_clean <- tmdb_clean %>%
  filter(vote_average > 0)

# Remove 0 values for vote_count in tmd
tmdb_clean <- tmdb_clean %>%
  filter(vote_count > 0)




# count how many duplicates by title and year
duplicate_counts <- imdb_clean %>%
  group_by(title, release_year) %>%
  summarize(count = n(), .groups = 'drop') %>%
  filter(count > 1) %>%
  select(title, count)






# Check distribution
plot_histogram(tmdb_clean)
plot_histogram(imdb_clean)



# Full join of tmdb and imdb datasets
imdb_tmdb <- full_join(imdb_clean, tmdb_clean, by = c("release_year", "title"))

result_df <- imdb_tmdb %>%
  mutate(genre_match = genre_1.y == genre_1.x)
number_of_matches <- sum(result_df$genre_match, na.rm = TRUE)









