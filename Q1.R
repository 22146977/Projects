## Task 1: Average Stars using member_since Date
# Upload Libraries
library(dplyr) # For Manipulating Data
library(ggplot2) # Used For Plotting Data
library(lubridate) # Used For The Date
library(knitr)
library(kableExtra)

# Read Users Data Csv File
users <- read.csv("~/Downloads/R Project/users.csv", stringsAsFactors = FALSE)