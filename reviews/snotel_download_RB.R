#from Keith snotel_download.R


# Load packages
library(tidyverse)


###############################################################################
# Download daily SNOTEL data

# Make data folder for downloads


# Identify SNOTEL sites of interest
#Stillwater creek = 793; High Lonesome = 1187
sites <- 793 #just choose 793 because it has more data

# Create vector of water years to download (Oct. 1 through Sept. 30)
wyears <- 1981:2020

# Download strings
dl1 = "https://wcc.sc.egov.usda.gov/nwcc/view?intervalType=Historic+&report=STAND&timeseries=Daily&format=copy&sitenum="
dl2 = "&year="
dl3 = "&month=WY"


# Make empty list for data
snotel <- data.frame()

# Loop by site and wyear
for(i in 1:length(sites)){
  
  # Make temporary list for downloaded data
  tmp.l <- list()
  
  # Loop through the water years
  for(j in 1:length(wyears)){
    
    # Download data into list
    tmp.l[[j]] <-
      read.csv(url(paste0(dl1,
                          sites[i],
                          dl2,
                          wyears[j],
                          dl3)),
               stringsAsFactors = F,
               skip = 4)
  }
  
  # Bind data from temporary list into a single data frame
  tmp.df <- plyr::ldply(tmp.l, bind_rows)
  
  # Bind temporary data frame to snotel data frame
  snotel <- bind_rows(snotel, tmp.df)
  
  # Remove tmp files
  rm(tmp.l, tmp.df)
  
}

# Rename select columns
# Convert depth to mm
depth_conv_factor = 25.4 # for inches to millimeters

snotel <- snotel %>% 
  select(site_id = Site.Id,
         date = Date,
         swe_in = WTEQ.I.1..in.,
         ppt_in = PREC.I.1..in.,
         tobs_c = TOBS.I.1..degC.,
         tmax_c = TMAX.D.1..degC.,
         tmin_c = TMIN.D.1..degC.,
         tavg_c = TAVG.D.1..degC.,
         sno_dpth_in = SNWD.I.1..in.) %>% 
  transmute(site_id, 
            date = as.Date(date),
            tobs_c, tmax_c, tmin_c, tavg_c,
            swe_mm = swe_in * depth_conv_factor,
            ppt_mm = ppt_in * depth_conv_factor,
            sno_dpth_mm = sno_dpth_in * depth_conv_factor)

# Save as RDS file
saveRDS(object = snotel,
        file = paste0("./data_raw/snotel/snotel_",
                      sites, "_",
                      min(wyears), "_", 
                      max(wyears), ".RDS"))


