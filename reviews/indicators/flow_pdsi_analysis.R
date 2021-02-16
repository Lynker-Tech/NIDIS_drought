# Script for combining PDSI and flow analysis
# uses data from pdsi_analysis.R and flow_analysis.R

# Keith Jennings
# kjennings@lynkertech.com
# 2020-12-31

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())

# Import data
drought <- readRDS("data_clean/pdsi/imw_drought_cat_by_pdsi_1895_2020.RDS")
flow <- read.csv("data_raw/streamflow/co_river_lees_ferry_annual_natural_flow.csv")

# For drought, compute the % area in category 3 or 4 
# And grab annual maximum
drought_severe <- drought %>% 
  filter(category %in% c("d4", "d3")) %>% 
  group_by(date, year) %>% 
  summarise(severe_pct = sum(pct)) %>% 
  group_by(year) %>% 
  summarise(severe_pct_max = max(severe_pct))

# Compute a 5-year moving average
drought_severe <- drought_severe %>% ungroup() %>% 
  mutate(severe_pct_5y = zoo::rollmean(severe_pct_max, k = 5, fill = NA))

# Compute 5 year moving average for flow
flow <- flow %>% 
  mutate(flow_ac_ft_5y = zoo::rollmean(flow_ac_ft, k = 5, fill = NA))


# Plot
cat_flow_plot <-
  plot_grid(
    drought_severe %>% 
      filter(year %in% min(flow$year):max(flow$year)) %>% 
      ggplot() + 
      geom_bar(aes(year, severe_pct_max), 
               stat = "identity", fill = "#e60000") + 
      geom_line(aes(year, severe_pct_5y), lwd = 1) +
      annotate(geom = "rect", 
               xmin = 1931.5, xmax = 1937.5, 
               ymin = 0, ymax = 80, 
               fill = "red", alpha = 0.1) +
      annotate(geom = "rect", 
               xmin = 1952.5, xmax = 1957.5, 
               ymin = 0, ymax = 80, 
               fill = "red", alpha = 0.1) +
      annotate(geom = "rect", 
               xmin = 1999.5, xmax = 2004.5,
               ymin = 0, ymax = 80, 
               fill = "red", alpha = 0.1) +
      annotate(geom = "rect", 
               xmin = 2010.5, xmax = 2014.5, 
               ymin = 0, ymax = 80, 
               fill = "red", alpha = 0.1) +
      annotate(geom = "text", x = 1934.5, y = 78, 
               label = "1930s Dust Bowl") +
      annotate(geom = "text", x = 1955, y = 65, 
               label = "1950s Drought", color = "black") +
      annotate(geom = "text", x = 2002, y = 78, 
               label = "2002 Drought", color = "black") +
      annotate(geom = "text", x = 2012, y = 65, 
               label = "2012 Drought", color = "black") +
      labs(y = "Maximum Extent of Extreme\n& Exceptional Drought (%)") +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank()),
    ggplot(flow, aes(year, flow_ac_ft/1000000)) +
      geom_bar(stat = "identity", fill = "skyblue3") +
      geom_line(data = flow,
                aes(year, flow_ac_ft_5y/1000000),
                lwd = 1, color = "purple") +
      annotate(geom = "text", x = 1970, y = 23, 
               label = paste0("Avg. annual flow\n", 
                              round(mean(flow$flow_ac_ft)/1000000, digits = 1), 
                              " million ac. ft.")) +
      geom_hline(aes(yintercept = mean(flow_ac_ft)/1000000), lty = "dashed", lwd = 1) +
      geom_segment(aes(x = 1970, y = 21, 
                       xend = 1963, yend = mean(flow$flow_ac_ft)/1000000 + 0.1),
                   arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
      labs(x = "Year",
           y = "Annual Colorado River\nStreamflow (million ac. ft.)"),
    ncol = 1, labels = "auto"
  )

# Export plot
save_plot(plot = cat_flow_plot,
          filename = "output/flow_pdsi/drought_category_flow_timeseries.png",
          base_height = 6, base_width = 8)
