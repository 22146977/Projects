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
  
  # Setting member since groups names and conditions
  mutate(user_group = case_when(
    year(member_since) < 2017 ~ "Veteran", # Before 2017 Veteran
    year(member_since) >= 2017 & year(member_since) <= 2022 ~ "Intermediate", # between 2017 and 2022, Intermediate
    year(member_since) > 2022 ~ "New" # After 2022 New
  ))

# Testing new total rows after NA values were removed
cat("New Total Row Amount:", nrow(cleanUsers))

# Testing if Headings were assigned correctly 
# Testing if table display groups and dates correctly
head(cleanUsers %>% select(user_id, member_since, user_group))

SumUser <- cleanUsers %>%
  group_by(user_group) %>%
  summarise(
    numUsers = n(), 
    avgStars = round(mean(average_stars, na.rm = TRUE), 2),
    avgReview = round(mean(review_count, na.rm = TRUE), 2)
  )
  
  # Setting order the values are shown in
  SumUser$user_group <- factor(SumUser$user_group,
                               levels = c("Veteran", "Intermediate", "New"))

# Checking if the table is displayed properly
print(SumUser)


# Plotting Values, and Styling them in a green color
ggplot(SumUser, aes(x = user_group, y = avgStars, fill = user_group)) +
  geom_col( width = 0.8, color = "black") +
  theme_classic() +
  # Choosing color to green
  scale_fill_manual(
    values = c("Veteran" = 'darkgreen',
               "Intermediate" = 'darkolivegreen',
               "New" = 'darkseagreen')) +

# Labeling the Graph
labs(
  title = "Average Review Stars by User Age Group",
  x = "User Groups",
  y = "Average Amount of Stars"
) +

  theme(legend.position = "bottom") # The legend is at the bottom of the page

# Task 2: Average star reviews per state in the US

# Import Libraries
library(dplyr) # For Manipulating Data
library(ggplot2) # Used For Plotting Data
library(lubridate) # Used For The Date
library(knitr)
library(kableExtra)

# Upload Files
users <- read.csv("~/Downloads/R Project/users.csv", stringsAsFactors = FALSE)
businesses <- read.csv("~/Downloads/R Project/businesses.csv", stringsAsFactors = FALSE)
reviews <- read.csv("~/Downloads/R Project/reviews.csv", stringsAsFactors = FALSE)

# Merging data from CSV files
cleanData <- reviews %>%
  inner_join(businesses %>% select(business_id, state), by = "business_id") %>% # State
  inner_join(users %>% select(user_id), by = "user_id") %>% # Keeps actual users only
  filter(!is.na(state), !is.na(stars), !is.na(user_id)) # Filters NAs

# Display Table
showSum <- cleanData %>%
  group_by(state) %>%


