# Script for analyzing monthly gridded PDSI values in IMW
# Data come from: https://wrcc.dri.edu/wwdt/batchdownload.php

# Abatzoglou, J. T., McEvoy, D. J., & Redmond, K. T. (2017). 
# The West Wide Drought Tracker: drought monitoring at fine spatial scales. 
# Bulletin of the American Meteorological Society, 98(9), 1815-1820.

# Data accessed 2020-12-30
# Script: wwdt_wget-1.sh

# Keith Jennings
# kjennings@lynkertech.com
# 2020-12-30

# Load packages
library(ncdf4)
library(raster)
library(sp)
library(rgdal)
library(tidyverse)

# Add data directory and kist PDSI files
# Large files are kept on local machine in ../data folder
# Each is about 0.5GB
data_dir = "../data/climate/pdsi/"
files = list.files(data_dir, pattern = ".nc$")

# Import IMW shapefile
imw <- readOGR(dsn = "../data/geospatial/", layer = "imw_states_4326")

# Make dummy data frame to store drought data
drought <- data.frame()

# Loop through PDSI files, extracting data
for(i in 1:length(files)){
  # Name the temporary file
  tmp_file = paste0(data_dir ,files[i])
  
  # Import PDSI data into raster stack
  pdsi_stack <- stack(tmp_file, varname = "data")
  
  # Extract IMW data into dataframe
  pdsi_extract <- raster::extract(x = pdsi_stack,
                                  y = imw,
                                  df = T)
  
  # Rename columns
  oldnames <- colnames(pdsi_extract)
  newnames <- c("id", 1895:2020)
  pdsi_extract <- pdsi_extract %>% 
    rename_at(vars(oldnames), ~ newnames) %>% 
    select(-id)

  # Melt the data into long format
  pdsi_all <- pdsi_extract %>% 
    pivot_longer(cols = everything(), 
                 names_to = "year", 
                 values_to = "pdsi")
  
  # Summarize the data by drought category
  pdsi_summary <- pdsi_all %>% 
    group_by(year) %>% 
    summarise(d4 = sum(pdsi <= -5),
              d3 = sum(pdsi > -5 & pdsi <= -4),
              d2 = sum(pdsi > -4 & pdsi <= -3),
              d1 = sum(pdsi > -3 & pdsi <= -2),
              d0 = sum(pdsi > -2 & pdsi <= -1),
              d_ = sum(pdsi > -1)) %>% 
    mutate(year = as.numeric(year)) %>% 
    pivot_longer(cols = !year,
                 values_to = "count", names_to = "category") %>% 
    group_by(year) %>% 
    mutate(pct = count/sum(count) * 100, 
           date = as.Date(paste(year, i, "15", sep = "-"), format = "%Y-%m-%d"))
  
  # Bind data to previous drought data
  drought <- bind_rows(drought, pdsi_summary)
  
}

# Arrange drought data by data
drought <- drought %>% 
  arrange(date)

# Plot
drought %>% 
  filter(year > 1925) %>% 
  ggplot() + 
  geom_area(aes(date, pct, fill = category)) + 
  scale_fill_manual(values = c("#ffffff", "#ffff00", "#fcd37f", 
                               "#ffaa00","#e60000", "#730000"), 
                    labels = c("No Drought","Abnormally Dry", 
                               "Moderate Drought", "Severe Drought", 
                               "Extreme Drought" , "Exceptional Drought"),
                    name = "") 

# Export the drought data
saveRDS(object = drought,
        file = "data_clean/pdsi/imw_drought_cat_by_pdsi_1895_2020.RDS")
saveRDS(object = pdsi_all,
        file = "data_clean/pdsi/imw_drought_pdsi_all_values_1895_2020.RDS")
