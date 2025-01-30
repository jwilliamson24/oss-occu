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
    
    #dates_needed <- seq(as.Date("2023-03-10"), as.Date("2023-06-10"), by = "day") #vector with all dates from mar 10 - jun 10 2023
    dates_needed <-unique(site_sub$date_mdy)
    get_prism_dailys(type = "ppt", dates = dates_needed, keepZip = FALSE)
    
    raster_files <- list.files("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/prism", pattern = "*.bil", full.names = TRUE)
    
#### download and extract prism -----------------------------------------------------------------------------------------
    
    # # Loop through dates and extract values ------------------------------------------none of this is verified, just trials
    # 
    # results <- data.frame(site_id = site_sub$site_id)
    # 
    # for (date in dates_needed) {
    #   raster_file <- paste0("PRISM_ppt_stable_4kmD2_", gsub("-", "", date), "_bil.bil")
    #   r <- rast(raster_file)
    #   values <- extract(r, site_sub[, c("long", "lat")])
    #   results[date] <- values[, 2]  # Extracted precipitation values
    # }
    # 
    # # Save as Excel-friendly format
    # write.csv(results, "precipitation_data.csv", row.names = FALSE)
    # 
    # 
    # 
    # # Create an empty data frame to store results
    # precip_data <- data.frame(site_id = site_sub$site_id, survey_date = site_sub$date_mdy, precip_mm = NA)
    # 
    # # Loop through each site and extract precipitation
    # for (i in 1:nrow(site_sub)) {
    #   # Find the matching raster for the survey date (adjust date formatting)
    #   date_str <- gsub("-", "", as.character(site_sub$date_mdy[i]))  # Convert Date to YYYYMMDD format
    #   raster_file <- raster_files[grepl(date_str, raster_files)]  # Match raster file by date
    #   
    #   if (length(raster_file) > 0) {
    #     # Load the raster file
    #     r <- rast(raster_file)
    #     
    #     # Extract precipitation at the site coordinates (lat, long)
    #     precip_value <- extract(r, sites[i, c("long", "lat")])
    #     
    #     # Store the precipitation value in the results data frame
    #     precip_data$precip_mm[i] <- precip_value
    #   } else {
    #     warning(paste("No raster file found for date:", site_sub$date_mdy[i]))
    #   }
    # }
    # 
    # # View results
    # head(precip_data)
    # 
    # # Save to CSV or Excel
    # write.csv(precip_data, "site_precip_data.csv", row.names = FALSE)
    # 
    # 
    # 
    # r <- raster("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/prism/PRISM_ppt_stable_4kmD2_20230314_bil/PRISM_ppt_stable_4kmD2_20230314_bil.bil")
    # plot(r)  # See if it loads and plots correctly
    

#-------
    
    # Get list of subdirectories (i.e., each day folder)
    prism_folders <- list.dirs("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/prism", full.names = TRUE, recursive = FALSE)
    
    # Get list of only .bil files within each folder
    bil_files <- lapply(prism_folders, function(folder) {
      list.files(folder, pattern = "\\.bil$", full.names = TRUE)
    })
    
    # View file paths for debugging
    head(unlist(bil_files))
    
    
    # Format site survey dates as YYYYMMDD strings for matching
    site_sub$survey_date_str <- format(site_sub$date_mdy, "%Y%m%d")
    
    # Initialize a data frame to store precipitation data
    precip_data <- data.frame(site_id = site_sub$site_id, survey_date = site_sub$date_mdy, precip_mm = NA) ############### works to here
    
    # Loop through sites and match their survey date with the corresponding .bil file
    for (i in 1:nrow(site_sub)) {
      # Find the correct .bil file for each survey date
      date_str <- site_sub$survey_date_str[i]
      
      # Look for .bil file matching the survey date
      matching_file <- unlist(lapply(bil_files, function(x) grep(date_str, x, value = TRUE)))
      
      if (length(matching_file) > 0) {
        # Load the raster from the matched file
        r <- terra::rast(matching_file)
        
        # Extract precipitation value for the site location (lat, lon)
        precip_value <- terra::extract(r, site_sub[i, c("long", "lat")])
        
        # Store the precipitation value
        precip_data$precip_mm[i] <- precip_value
      } else {
        warning(paste("No .bil file found for date:", site_sub$date_mdy[i]))
      }
    }
    
    # View results
    head(precip_data)
    

  #  -------
    
    # Loop through sites and match their survey date with the corresponding .bil file
    for (i in 1:nrow(site_sub)) {
      # Find the correct .bil file for each survey date
      date_str <- site_sub$survey_date_str[i]
      
      # Look for .bil file matching the survey date
      matching_file <- unlist(lapply(bil_files, function(x) grep(date_str, x, value = TRUE)))
      
      if (length(matching_file) > 0) {
        # Load the raster from the matched file
        r <- terra::rast(matching_file)
        
        # Extract precipitation value for the site location (lat, lon)
        precip_value <- terra::extract(r, site_sub[i, c("long", "lat")], ID = TRUE)
        
        # Ensure that only one value is assigned (e.g., extract the first value)
        precip_data$precip_mm[i] <- precip_value[1, "value"]  # Extract the first value from the result
      } else {
        warning(paste("No .bil file found for date:", site_sub$date_mdy[i]))
      }
    }
    
    # View results
    head(precip_data)
    