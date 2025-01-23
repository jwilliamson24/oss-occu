##
## 02.1-dwd-exploration.R 
##
## Efforts to organize, explore, and summarize dwd data
##
## Jasmine Williamson
## Date Created: 10-18-2024
##
## -------------------------------------------------------------------------------------------------------

    rm(list=ls())
    #setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(ggpubr)

#### Load data ---------------------------------------------------------------------------------------
#     site <- read.csv("site.complete.csv")
#     subplot <- read.csv("subplot.complete.csv")
#     sals <- read.csv("sals.complete.csv", 
#                  colClasses = c(landowner="factor", stand="character", trt="factor",
#                                 obs="factor", subplot="factor", recap="factor",
#                                 pass="factor", spp="factor", cover_obj="factor", 
#                                 substrate="factor", age_class="factor"))
    
    dat <- readRDS("site_level_matrix.RDS")  
    #row.names(dat) <- dat[,1]
    dwd <- dat[,c(1,5,19:25)]
    
    dwd_all <- read.csv("dwd.complete.csv")
    

#### Add density per m^2 to site matrix --------------------------------------------------------------

## add log/stump dens
    dwd$dwd_dens <- round(dwd$dwd_count/567,2) # total 567m^2 per site
    dwd$log_dens <- round(dwd$logs/567,2)
    dwd$stump_dens <- round(dwd$stump/567,2)

## visualize    
    p1 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = dwd_count), fill = "#FF5050", alpha=0.8)
    p2 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = stumps), fill = "#FF5050", alpha=0.8)
    p3 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = logs), fill = "#FF5050", alpha=0.8)
    p4 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = size_cl), fill = "#FF5050", alpha=0.8)
    p5 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = decay_cl), fill = "#FF5050", alpha=0.8)
    p6 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = char_cl), fill = "#FF5050", alpha=0.8)
    p7 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = length_cl), fill = "#FF5050", alpha=0.8)
    p8 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = dwd_dens), fill = "#FF5050", alpha=0.8)
    p9 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = log_dens), fill = "#FF5050", alpha=0.8)
    p10 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = stump_dens), fill = "#FF5050", alpha=0.8)
    ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, ncol=3, nrow=4)    
    
 

#10/18/24
# need to use literature to determine a metric for volume or area, the count of logs is not
# robust because there were larger diameter and longer logs in some plots (unharvested).

# could do a volume of each piece of wood on the original dwd df, then avg vol per m^2.
# could also calculate proportion of ground surface area occupied by logs by calculating
# cross-sectional area and est how much of the subplot is covered by wood.
    
# there is a paper (Woodall 2008) that uses a complex formula based on some data that I
# dont have...seems pretty robust. also has a decay factor. dont know how to make this
# useful for my data.
    
    
#### Calculate volume --------------------------------------------------------------------------------  

# Log Volume = pi * radius^2 * length
# Stump Volume = pi * radius^2 * height
        
# convert size class to radius
# size class 1 (25-50cm), choosing midpoint of 37.5 diameter = radius 18.75cm
# size class 2 (>50cm), choosing 60 diameter = radius 30cm
    
# convert length class to length value
# class 1 (<1m), choosing 0.5m midpoint
# class 2 (1-3m), choosing 2m midpoint
# class 3 (>3m), choosing 4m value
    
# choose stump height
# size class 1, choosing 25cm 
# size class 2, choosing 50cm
# based these measurements on visual inspection of study site photos

    
    # assign radius for each item in meters
    dwd_all <- dwd_all %>%
      mutate(
        radius = case_when(
          size_cl == 1 ~ .1875,  # radius for size class 1 (25-50 cm)
          size_cl == 2 ~ .30,    # radius for size class 2 (>50 cm)
          TRUE ~ NA_real_
        )
      )
    
    
    # assign length for each log in meters
    dwd_all <- dwd_all %>%
      mutate(
        length_value = case_when(
          length_cl == 1 ~ .5,  # Midpoint for class 1 (<1m)
          length_cl == 2 ~ 2,    # Midpoint for class 2 (1-3m)
          length_cl == 3 ~ 4,    # Midpoint for class 3 (>3m)
          TRUE ~ NA_real_
        )
      )
    
    
    # assign height for each stump in meters
    dwd_all <- dwd_all %>%
      mutate(
        stump_height = case_when(
          dwd_type == "S" & size_cl == 1 ~ .25,
          dwd_type == "S" & size_cl == 2 ~ .50,
          TRUE ~ NA_real_
        ))
    
    
    # calculate volume for stumps and logs in m^3
    dwd_all <- dwd_all %>%
      mutate(
        volume = case_when(
          dwd_type == "L" ~ pi * radius^2 * length_value,  # Log volume
          dwd_type == "S" ~ pi * radius^2 * stump_height   # Stump volume 
          )
        )
    
    
    # Summarize by site and subplot, summing all log and stump volumes 
    dwd_volume <- dwd_all %>%
        group_by(site_id, subplot) %>%
        summarise(
          total_volume = sum(volume, na.rm = TRUE),
          ) %>%
        group_by(site_id) %>% # aggregate and average volume per site 
           summarise(
             avg_volume = mean(total_volume, na.rm = TRUE)
           )

     dwd_volume <- as.matrix(dwd_volume)
    
    
    # add volume to site matrix
     dwd_merge <- merge(dwd, dwd_volume, by = "site_id")

write.csv(dwd_merge, "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/dwd.extra.metrics.csv",
          row.names = FALSE)    
    
#### reclassify log size categories as one continuous variable ---------------------------------    

    
    
    
    
    
    
    
    
    
    
    
        
    







