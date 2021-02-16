# Analyze CO River streamflow by year
# This uses the Lee's Ferry gage point

# Natural flow data from:
# https://www.usbr.gov/lc/region/g4000/NaturalFlow/current.html
# Data accessed 2020-12-31

# Keith Jennings
# kjennings@lynkertech.com
# 2020-12-30

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())

# Import natural flow for Lee's Ferry
flow <- read.csv("data_raw/streamflow/co_river_lees_ferry_annual_natural_flow.csv")

# Add 5 year moving average
flow <- flow %>% 
  mutate(flow_ac_ft_5y = zoo::rollmean(flow_ac_ft, k = 5, fill = NA))

# Plot by year
ggplot(flow, aes(year, flow_ac_ft/1000000)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  geom_line(data = flow,
            aes(year, flow_ac_ft_5y/1000000),
            lwd = 1, color = "purple") +
  geom_hline(aes(yintercept = mean(flow_ac_ft)/1000000), lty = "dashed", lwd = 1) +
  labs(x = "Year",
       y = "Annual Flow (million ac. ft.)")
