## Task 1: Average Stars using member_since Date
# Upload Libraries
library(dplyr) # For Manipulating Data
library(ggplot2) # Used For Plotting Data
library(lubridate) # Used For The Date
library(knitr)
library(kableExtra)

# Read Users Data Csv File
users <- read.csv("~/Downloads/R Project/users.csv", stringsAsFactors = FALSE)

# We are converting member_since data to a data format, and then filtering this data
cleanUsers <- users %>%
  mutate(member_since = as.Date(member_since)) %>%
  # Filtering out rows with missing values
  filter(!is.na(member_since), !is.na(review_count), !is.na(average_stars)) %>%