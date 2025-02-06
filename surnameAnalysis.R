## Surname Analysis and Data Merging Script

### Preliminary Environment Setup
# Clear the environment
rm(list = ls())

# Set working directory (adjust the path as needed)
setwd("/Users/jasirrahman/Desktop/SOPA Capstone/R/Code")

# Load required libraries
library(tidyr)
library(zoo)
library(sp)
library(ggpubr)
library(tidyverse)
library(lubridate)
library(wru)
library(tidycensus)

### Load Data
# Load ZIP-to-ZCTA mapping
# (Assumes a CSV file "zipcode_zcta.csv" with at least columns: ZIP, ZCTA, and county_fips)
zipcode_zcta <- read.csv("zipcode_zcta.csv", stringsAsFactors = FALSE)

# Load Weekly Criminal History data
# (Assumes a CSV file "weekly_criminal_history.csv" with a column "ZIP")
criminal_history <- read.csv("weekly_criminal_history.csv", stringsAsFactors = FALSE)

# Optionally, load bail data for surname analysis (if available)
# (Assumes a CSV file "bail.csv" exists with the necessary columns for the wru package)
bail <- read.csv("bail.csv", stringsAsFactors = FALSE)

### Census API Setup
# Set your Census API key
census_api_key("d7f7cd3ab0204387684215b28c87070649056422", install = TRUE)

### Step 1: Download ACS Data
# 1a. Get ACS data for ZCTAs (median income)
zcta_income <- get_acs(
  geography = "zcta",
  variables = c(medincome = "B19013_001"),
  year = 2021
)
# Note: In the returned data, the column GEOID holds the ZCTA identifier.

# 1b. Get ACS county-level median income data for AMI classification
county_income <- get_acs(
  geography = "county",
  variables = c(medincome = "B19013_001"),
  year = 2021
)
# In this dataset, GEOID is the county FIPS code and NAME gives the county name.

### Step 2: Merge Data
# 2a. Merge ZIP code mapping with ACS ZCTA income data.
# Here we assume zipcode_zcta has a column named "ZCTA" that matches zcta_income$GEOID.
zip_income <- merge(
  zipcode_zcta, 
  zcta_income, 
  by.x = "ZCTA", 
  by.y = "GEOID", 
  all.x = TRUE
)

# 2b. Merge county-level income data.
# We assume the ZIP mapping file has a column "county_fips" that can be merged with county_income$GEOID.
zip_income <- merge(
  zip_income, 
  county_income, 
  by.x = "county_fips", 
  by.y = "GEOID", 
  suffixes = c("_zcta", "_county"), 
  all.x = TRUE
)

# 2c. Merge with Weekly Criminal History data.
# We assume the criminal_history data has a column "ZIP" that matches the ZIP column in zip_income.
final_data <- merge(
  zip_income, 
  criminal_history, 
  by = "ZIP", 
  all.x = TRUE
)

### Step 3: Create AMI Income Categories
# Define AMI categories based on county-level median income.
# Here we create five income categories using quantiles and add an "Unhoused" category for missing county income.
# First, compute quantiles (20th, 40th, 60th, 80th percentiles)
income_quantiles <- quantile(final_data$estimate_county, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Create a new variable "AMI_category" based on the county median income
final_data$AMI_category <- cut(
  final_data$estimate_county, 
  breaks = c(-Inf, income_quantiles[-length(income_quantiles)], Inf),
  labels = c("Very Low", "Low", "Moderate", "High", "Very High")
)

# Assign the "Unhoused" category for ZIP codes with missing county income data 
final_data$AMI_category[is.na(final_data$estimate_county)] <- "Unhoused"

### Step 4: Conduct Surname Analysis
# Use the wru package to predict race based on surnames from the bail dataset.
race_predictions <- predict_race(voter.file = bail, surname.only = TRUE)

### (Optional) Save the Results
# Save the final merged dataset and race predictions to CSV files.
write.csv(final_data, "final_merged_data.csv", row.names = FALSE)
write.csv(race_predictions, "race_predictions.csv", row.names = FALSE)
