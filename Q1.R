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
  # Table headings and calculations
  summarise (
    Average_Stars = round(mean(stars), 2),
    Total_Reviews = n(),
    Unique_Users = n_distinct(user_id)
  ) %>% 
  arrange(desc(Average_Stars)) # Setting Average_stars to a descending order

# Print Table
showSum %>%
  kbl(caption="The Average Review Stars by State") %>% # Title
  kable_styling(bootstrap_options = 
                  c( "striped", "hover", "condensed", 
                     full_width = FALSE
                  )
  )

# Visualise Data
# Plot Data
ggplot(showSum, aes(x = reorder(state, Average_Stars), y = Average_Stars, fill = state)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_classic() +
  labs(
    title = "The Average Review Stars by State",
    x = "State",
    y = "Average Stars Per State"
  )

# Task 3: Top 10 users

# Upload Library
library(dplyr) # For Manipulating Data
library(ggplot2) # Used For Plotting Data
library(lubridate) # Used For The Date
library(knitr)
library(kableExtra)

# Upload Data Set
# Read Csv Files
users <- read.csv("~/Downloads/R Project/users.csv", stringsAsFactors = FALSE) # Users.csv
businesses <- read.csv("~/Downloads/R Project/businesses.csv", stringsAsFactors = FALSE) # Businesses.csv
reviews <- read.csv("~/Downloads/R Project/reviews.csv", stringsAsFactors = FALSE) # Reviews.csv

# Merging data from CSV files
cleanData <- reviews %>%
  inner_join(businesses %>% select(business_id, state), by = "business_id") %>% # State
  inner_join(users %>% select(user_id), by = "user_id") %>% # Keeps actual users only
  filter(!is.na(state), !is.na(stars), !is.na(user_id)) # Filters NAs

# Checks to see total users present in reviews
cat("Total Amount of Rows in Reviews:", nrow(reviews))

topUsers <- users %>%
  filter(!is.na(review_count), !is.na(user_id)) %>% # Filtering Data
  arrange(desc(review_count)) %>% # Arranging order Descending
  slice_head(n = 10)

topReviews <- reviews %>%
  filter(!is.na(user_id), !is.na(stars)) %>% # Filtering Data
  inner_join(topUsers, by = "user_id")

UserSum <- topReviews %>%
  group_by(user_id) %>%
  summarise(
    TotReviews = n(),
    avgStars = round(mean(stars, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(TotReviews)) 


# Show Summary Table
UserSum %>%
  kbl(caption=" The Average Review Stars of The Top 10 Users") %>% # Title
  kable_styling(bootstrap_options = c( "striped", "hover", "condensed", 
                                       full_width = FALSE)
  )

# Visualise Data
# Plot Data
ggplot(topReviews, aes(x = reorder(user_id, stars, FUN = median), y = stars, fill = user_id)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  theme_classic() +
  labs(
    title = "The Average Review Stars of The Top 10 Users",
    x = "User ID",
    y = "Star Rating"
  ) 

