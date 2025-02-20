##
## 02.3-prism.R 
##
## Using PRISM data: extract daily precipitation for each site on its specific sampling date
##
## Jasmine Williamson
## Date Created: 01-29-2025
##
#### settings -------------------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(prism)
    library(terra)
    library(dplyr)
    library(raster)
    library(zoo)
    
    #set directory to new "prism" folder in data folder - this is where the PRISM data will download to in later step
    prism_set_dl_dir("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/prism")
 
#### load data ------------------------------------------------------------------------------------------------------
    
    #site-level data
    dat <- read.csv("sites_with_aspect.csv") # has lat/long
    site <- read.csv("site.complete.csv") # has date mdy
    merged <- merge(dat, site, by="site_id") # combine the two
    
    site_sub <- merged[,c("site_id","date_mdy","lat","long")] # subset needed cols
    
    site_sub$date_mdy <- as.Date(site_sub$date_mdy) # convert to date
 
    
#### download and extract prism -----------------------------------------------------------------------------------------
    
    
    dates_needed23 <- seq(as.Date("2023-02-10"), as.Date("2023-06-10"), by = "day") #vector with all dates from mar 10 - jun 10 2023
    dates_needed24 <- seq(as.Date("2024-02-10"), as.Date("2024-06-10"), by = "day")
    dates_needed <- c(dates_needed23,dates_needed24)
    
    get_prism_dailys(type = "ppt", dates = dates_needed, keepZip = FALSE) # download daily precip data from PRISM
    
    # store files
    raster_files <- list.files("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/prism", pattern = "*.bil", full.names = TRUE)
    
    # Get list of subdirectories that were stored (each day folder)
    prism_folders <- list.dirs("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/prism", full.names = TRUE, recursive = FALSE)
    
    # Get list of only .bil files within each folder
    bil_files <- lapply(prism_folders, function(folder) {
      list.files(folder, pattern = "\\.bil$", full.names = TRUE)
    })
    
    
#### connect prism data to site locations -----------------------------------------------------------------------------
    
    # Format dates as YYYYMMDD strings for matching
    site_sub$survey_date_str <- format(site_sub$date_mdy, "%Y%m%d")
    
    # data frame to store precip data
    precip_data <- data.frame(site_id = site_sub$site_id, survey_date = site_sub$date_mdy, precip_mm = NA)           
    
    # Initialize with NA to ensure all 127 rows are filled
    precip_data$precip_mm <- rep(NA, nrow(site_sub))
    
    
    # sequence each row in site_sub and attach the corresponding precip value by location and date
    for (i in seq_len(nrow(site_sub))) {  
      date_str <- site_sub$survey_date_str[i]
      
      if (!is.na(date_str)) {  # Check for NA 
        matching_file <- unlist(lapply(bil_files, function(x) grep(date_str, x, value = TRUE)))
        
        if (length(matching_file) > 0) {
          r <- terra::rast(matching_file)
          precip_value <- terra::extract(r, site_sub[i, c("long", "lat")], ID = TRUE) # extract locations
          
          if (!is.null(precip_value) && length(precip_value) > 0) {
            precip_data$precip_mm[i] <- precip_value[, 2]  # Extract precip column
          }
        }
      }
    }
    
    # Check that all rows are filled
    table(is.na(precip_data$precip_mm))  # yes - 127 total rows
    
   
  
#### daily precip data for every date march-june -------------------------------------------------------------------------------------
    
    # Get list of all PRISM .bil files (daily precipitation)
    bil_files_all <- unlist(lapply(prism_folders, function(folder) {
      list.files(folder, pattern = "\\.bil$", full.names = TRUE)
    }))
    
    # Extract date from filenames (containing YYYYMMDD)
    bil_files <- unlist(bil_files) #unlist to turn into a vector of .bil file paths
    bil_dates <- as.Date(sub(".*_(\\d{8})_bil\\.bil$", "\\1", bil_files), format = "%Y%m%d")
    
    # Initialize empty dataframe for daily precipitation per site
    daily_precip_data <- data.frame()
    
    for (i in seq_along(bil_files)) {
      r <- rast(bil_files[i])  # Load raster for a specific date
      
      # Extract precip values for site locations
      precip_values <- extract(r, site_sub[, c("long", "lat")])
      
      # Store results
      daily_precip_data <- rbind(daily_precip_data, 
                                 data.frame(site_id = site_sub$site_id, 
                                            date = bil_dates[i], 
                                            precip_mm = precip_values[, 2]))  # Adjust column index if needed
    }
    
    
    daily_precip_data$precip_mm <- round(daily_precip_data$precip_mm,2) # round precip to 2 decimal places
    
    # Save 
    #write.csv(daily_precip_data, "daily_precip_data.csv", row.names = FALSE)
    
    
#### merge with site location data -------------------------------------------------------------------------------------
    
    # dont need this
    
    #daily_precip_data <- read.csv("daily_precip_data.csv")
  
    # Rename date column for merging
    colnames(daily_precip_data)[colnames(daily_precip_data) == "date"] <- "date_mdy"
    
    daily_precip_data$date_mdy <- as.Date(daily_precip_data$date_mdy)
    
    # Merge daily precip data with site data
    merged_data <- full_join(site_sub, daily_precip_data, by = c("site_id", "date_mdy"))
    
    
#### calculate last rain date column  -----------------------------------------------------------------------------------------
    
    
    rain_df <- daily_precip_data
    
    # create rain event column - did it rain that day?
    rain_df$rain_event <- ifelse(rain_df$precip_mm >= 1, 1, 0) # rain 1mm or more = 1 (yes), <1mm = 0 (no)
    
    # Ensure data is sorted by site_id and date
    rain_df <- rain_df[order(rain_df$site_id, rain_df$date_mdy), ]
    
    # Initialize the new column with NA
    rain_df$last_rain_date <- NA
    
    # Loop through each row
    for (i in seq_len(nrow(rain_df))) {
      # Only fill last_rain_date for non-rain days
      if (rain_df$rain_event[i] == 0) {
        # Find the last rain event within the same site
        past_rain_dates <- rain_df$date_mdy[rain_df$site_id == rain_df$site_id[i] & 
                                               rain_df$rain_event == 1 & 
                                               rain_df$date_mdy < rain_df$date_mdy[i]]
        
        # Assign the most recent rain date (if any)
        if (length(past_rain_dates) > 0) {
          rain_df$last_rain_date[i] <- max(past_rain_dates)
        }
      }
    }
    
    
    # convert to YYYY-MM-DD date
    rain_df$last_rain_date <- as.Date(rain_df$last_rain_date, origin = "1970-01-01")
    

    # Ensure both are Date objects
    rain_df$date_mdy <- as.Date(rain_df$date_mdy)
    rain_df$last_rain_date <- as.Date(rain_df$last_rain_date)
    
    # Calculate days_since_rain
    rain_df$days_since_rain <- ifelse(!is.na(rain_df$last_rain_date), 
                                       as.numeric(difftime(rain_df$date_mdy, rain_df$last_rain_date, units = "days")),
                                       NA)
    
    # make NA = 0 days since rain
    rain_df$days_since_rain[is.na(rain_df$days_since_rain)] <- 0
    
    # merge with my site level data to only keep data for my sites/dates
    merged_df <- left_join(site_sub, rain_df, by = c("site_id", "date_mdy"))
    
    # Save entire rain data frame
    write.csv(merged_df, "site_rain_data.csv", row.names = FALSE)
    
    
#### merge with larger site level data frame for later use -------------------------------------------------------------------------
    
    # remove unnecessary cols from merged df above and then join with dat
    new_merged_df <- merged_df[c("site_id", "date_mdy", "precip_mm","days_since_rain")]
    
    # combine
    all_merged_df <- full_join(dat,new_merged_df, by = c("site_id"))
    
    # save
    write.csv(all_merged_df, "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/site_aspect_precip_all_vars.csv") 

    
    
    
    
    
    
        