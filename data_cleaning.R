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


# Cleaning Datasets



# upload datasets


# read TMDB csv file
tmdb <- read_csv("TMDB_movie_dataset_v11.csv")
# read imdb
imdb <- read_csv("IMDbMovies-Clean.csv")
# read rt dataset
rotten_tomatoes <- read_csv("rotten_tomatoes_movies.csv")


######## IMPORTANT FEATURES

####### TMDB
#‘release_date’,  ‘genres’,  ‘revenue’  ‘vote_average’, status (for knowing if released),
# title, release year, budget
# columns to check for cleaning, status, vote count

####### IMDB
# main genres, release year, rating, title, gross colummns
# for cleaning, rating number, rating 

####### ROTTEN TOMATOES
# release date streaming, release data theaters, box office



##############################
# Remove un-needed data columns
##############################




##############################
# Standardise Data Types
##############################


##############################
# Standardise Column Names
##############################




##############################
# Standardise Numerical Values
##############################



##############################
# Clean Date Columns
##############################





##############################
# Validate data
##############################


##############################
# Check Missing Values with DX
##############################








