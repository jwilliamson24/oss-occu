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
    
    #set directory to new "prism" folder in data folder
    prism_set_dl_dir("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/prism")
 
#### load data ------------------------------------------------------------------------------------------------------
    
    #site-level data
    dat <- read.csv("sites_with_aspect.csv") # has lat/long
    site <- read.csv("site.complete.csv") # has date mdy
    merged <- merge(dat, site, by="site_id") # combine the two
    
    site_sub <- merged[,c("site_id","date_mdy","lat","long")] # subset needed cols
    
    site_sub$date_mdy <- as.Date(site_sub$date_mdy) # convert to date
 
    
#### download and extract prism -----------------------------------------------------------------------------------------
    
    
    dates_needed <- seq(as.Date("2023-03-10"), as.Date("2023-06-10"), by = "day") #vector with all dates from mar 10 - jun 10 2023
    #dates_needed <-unique(site_sub$date_mdy) # make list of survey dates that we need data for
    get_prism_dailys(type = "ppt", dates = dates_needed, keepZip = FALSE) # get daily precip
    
    # store files
    raster_files <- list.files("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/prism", pattern = "*.bil", full.names = TRUE)
    
    # Get list of subdirectories that were stored (each day folder)
    prism_folders <- list.dirs("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/prism", full.names = TRUE, recursive = FALSE)
    
    # Get list of only .bil files within each folder
    bil_files <- lapply(prism_folders, function(folder) {
      list.files(folder, pattern = "\\.bil$", full.names = TRUE)
    })
    
    head(unlist(bil_files))
    
    
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
    
   
     
#### merge and save -------------------------------------------------------------------------------------
    
    # add precip and survey date to the large site level df
    merged <- cbind(dat,precip_data)
    merged <- merged[,-29]    
    
    write.csv(merged, "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/site_data_aspect_precip")
    
    
  
#### precip data for every date march-june -------------------------------------------------------------------------------------
    
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
    
    # Save daily precipitation data for future use
    write.csv(daily_precip_data, "daily_precip_data.csv", row.names = FALSE)
    
    
#### merge with site location data -------------------------------------------------------------------------------------
    
    #daily_precip_data <- read.csv("daily_precip_data.csv")
  
    
    # Rename date column for merging
    daily_precip_data <- daily_precip_data %>%
      rename(date_mdy = date)
    
    daily_precip_data$date_mdy <- as.Date(daily_precip_data$date_mdy)
    
    # Merge daily precip data with site data
    merged_data <- full_join(site_sub, daily_precip_data, by = c("site_id", "date_mdy"))
    
    
#### calculate last rain date column  -----------------------------------------------------------------------------------------   ### code works above here
    
    
    
    # Step 1: Mark rain event days (precip_mm > 0)
    merged_data <- merged_data %>%
      group_by(site_id) %>%
      mutate(
        rain_event = ifelse(precip_mm > 0, as.Date(merged_data$date_mdy), NA)  # Mark rain event dates
      ) %>%
      ungroup()
    
    # Step 2: Propagate the last rain date using na.locf
    merged_data <- merged_data %>%
      group_by(site_id) %>%
      mutate(
        # Fill missing rain event dates with the previous non-NA value (i.e., last rain date)
        last_rain_date = zoo::na.locf(rain_event, na.rm = FALSE, fromLast = FALSE)
      ) %>%
      ungroup()
    
    # Step 3: Calculate the days since the last rain (if last_rain_date is not NA)
    merged_data <- merged_data %>%
      mutate(
        days_since_rain = ifelse(!is.na(last_rain_date), as.numeric(difftime(date_mdy, last_rain_date, units = "days")), NA)
      )
    
    # Check the result
    head(merged_data)
    
    
    
    ### try calculating time since rain from the daily precip df that only has site id, date, and precip amount
    ## then merge back with site with the new addt cols for time since precip
    
    
    
    
    
