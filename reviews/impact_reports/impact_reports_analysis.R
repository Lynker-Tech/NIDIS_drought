# Script for evaluating drought impacts from the US Drought Impact Reporter

# Data Source: https://droughtreporter.unl.edu/advancedsearch/impacts.aspx

# Search settings: 
# Time: 2000-01-01 to 2020-12-03
# States: AZ, CO, NM, UT, and WY individually, and combo of all 5 ("Multi-state")
# All categories
# All sources

# Keith Jennings
# kjennings@lynkertech.com
# 2020-12-03

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(lubridate)

################################################################################
# Import data

# Identify the directory with impact data
data_directory = "data_raw/impact_reports/"

# List the files
files <- paste0(data_directory, list.files(path = data_directory,
                                           pattern = ".csv$"))

# Name the columns
cnames <- c("id", "title", "date_post", "date_start", "date_end",
               "description", "categories", "places")

# Import multiple files into list and bind into dataframe
impacts <- lapply(files, read_csv, col_names = cnames, skip = 1) %>% 
  plyr::ldply(., bind_rows)

################################################################################
# Clean data
# Data come provided with multiple subcategories within the category column

# Compute number of impact reports
n_reports = length(impacts$id)

# Split entries with multiple categories into multiple rows
impacts <- impacts %>% 
  separate_rows(categories, sep = ",") %>% # function requires col be a character
  mutate(categories = str_trim(categories, side = "both"))

# Add state classification
impacts <- impacts %>% 
  mutate(state = case_when(str_detect(places, "Ariz") ~ "AZ",
                           str_detect(places, "Colo") ~ "CO",
                           str_detect(places, "Mexi") ~ "NM",
                           str_detect(places, "Utah") ~ "UT",
                           str_detect(places, "Wyom") ~ "WY",
                           TRUE ~ places))

# Add date information
# Note: don't use date_post because there are many retrospective impacts
impacts <- impacts %>% 
  mutate(date = case_when(state == "IMW" ~ as.Date(date_start, format = "%m/%d/%y"),
                          TRUE ~ as.Date(date_start, format = "%m/%d/%Y")),
         year = year(date))

################################################################################
# Summarize data

# Summarize impacts by year
impacts_by_year <- impacts %>% 
  group_by(year) %>% 
  summarise(n_impacts = n())

# Summarize impacts by category
impacts_by_cat <- impacts %>% 
  group_by(categories) %>% 
  summarise(n_impacts = n(),
         pct_impacts = n_impacts / n_reports * 100)

# Summarize impacts by year and category
impacts_by_cat_year <- impacts %>% 
  group_by(categories, year) %>% 
  summarise(n_impacts = n())

################################################################################
# Plot

# By year and category
ggplot(filter(impacts_by_cat_year, year >= 2000), 
       aes(year, n_impacts, fill = categories)) + 
  geom_bar(stat = "identity")
