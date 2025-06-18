## =================================================
##
## Title: oss-gen-samples-info
## Author: Jasmine Williamson
## Date Created: 06/06/2025
##
## Description: Info for Chris on OSS genetic samples to determine how many 
## should be analyzed. 
##
## =================================================

## settings 


## load data 

  sals <- read.csv("data/sals.complete.csv")
  sals$sample_id[sals$sample_id == ""] <- NA
  
  plots <- read.csv("data/subplot.complete.csv")
  
  
  # sample_id was only recorded for animals with a sample (OSS)
  # sample_id = observer initials, # animal in that site
  # sample_id is not unique to each animal without site code info
  # sal_id is unique to each animal (all spp)
  # sal_id = site_site rep_year_# animal in that site
  
  
## data summaries ---------------------------------------------------------------
  
  # keep only rows with a sample_id
  df_samples <- sals[!is.na(sals$sample_id), ]
  
  # 138 total samples
  length(df_samples$sal_id)
  
  # samples per stand 
  samples <- table(df_samples$stand)
  min(samples)
  max(samples)
  mean(samples)    
  
  
  # samples per site ID
  # site and stand are not the same. there can be two sites in one stand if they are >100m apart
  samples_per_site <- table(df_samples$site_id)
  # but rest of summarty stats are almost identical to samples_per_stand
  
  
  # stands with 1-3 samples
  sum(samples >= 1) # 44 stands have at least one sample
  sum(samples >= 2) # 30 stands have at least 2 samples 
  sum(samples >= 3) # 20 stands have at least 2 samples
  sum(samples[samples == 1]) # 14 have only one sample

  
  # Option 1:
  # Analyze 44 samples
  # one sample per stand
  
  # Option 2:
  # Analyze 74 samples
  # Use the 14 stands that have only one sample
  # Run 2 samples on the 30 stands that have >=2 samples
  
  
  
# get lat/long for each sample  
  
  plot.loc <- plots[,c(1,9:11)]
  
  merge1 <- merge(df_samples, plot.loc, by = c("site_id", "subplot"), all.x = TRUE)
  
  write.csv(merge1, "/Users/jasminewilliamson/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/occupancy/oss-samples-location.csv", 
            row.names = FALSE)
  
  
  
  
  
  
  