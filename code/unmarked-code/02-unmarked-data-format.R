## -------------------------------------------------------------------------------------------------------
##
## 02-unmarked-data-format.R 
##
## Rerun the occupancy models using package Unmarked from 2023 analysis
##
## Jasmine Williamson
## Date Created: 07-09-2024
##

## goals
# using the unmarked code that i started last august, rework to use new covariate matrix

## insights
# 2/27/25 tried formatting for use of csvToUMF but couldnt get function to work, gave up on it 
# ran models with just dwd covs to figure out which one is best, getting negative relationships with dwd & occu
# det covs (rain, temp, soil moist) all showing non significant - shouldnt be the case for rain


## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    #setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    
    library(unmarked)
    library(ggplot2)
    library(stats)
    library(MASS)
    library(tidyverse)

    

## load data----------------------------------------------------------------------------------------------

    site <- read.csv("data/covariate matrices/env_subset_corr.csv", row.names = 1)
    trt <- read.csv("data/site.complete.csv", row.names = 1)
    subplot <- read.csv("data/subplot.complete.csv")
    
    oss.dets <- read.csv("data/occupancy/oss.occu.wide.csv")
    enes.dets <- read.csv("data/occupancy/enes.occu.wide.csv")

    
#load saved csv from data manipulatio steps below
    df_oss <- read.csv("data/occupancy/oss_forUMF.csv")
    

## Detection covs  ------------------------------------------------------------------------------------------

#format subplot/det covs from long to wide dataframes

# soil moisture 

    #subset df with only cols i need (cant use site id here bc its a character)
    df <- subset(subplot[,c('subplot','soil_moist_avg')])
    
    #change to long format
    df_long <- df %>%
      pivot_longer(cols = -subplot, names_to = "variable", values_to = "value")
    #add site id back in
    df_long <- cbind(df_long, subplot$site_id)
    
    # Reshape the data back to wide format, flipping the dimensions
    df_wide <- df_long %>%
      pivot_wider(names_from = subplot, values_from = value)
    
    df_wide <- df_wide[,-1]
    df_wide <- as.data.frame(df_wide)
    rownames(df_wide) <- df_wide[,1]
    soil_moist <- df_wide[,-1]
    soil_moist <- round(soil_moist, 2)
    names(soil_moist) <- c("soilmoist-1","soilmoist-2","soilmoist-3","soilmoist-4","soilmoist-5","soilmoist-6","soilmoist-7")
    
    scaled_soilmoist <- as.data.frame(scale(soil_moist))
    scaled_soilmoist <- round(scaled_soilmoist, 2)
    
# time since rain
    
    df <- site[,"days_since_rain", drop=FALSE]
    rain <- cbind(df, rep(df, each = 6))
    names(rain) <- c("rain-1","rain-2","rain-3","rain-4","rain-5","rain-6","rain-7")
    scaled_rain <- as.data.frame(scale(rain))

# temp
    
    df <- site[,"temp", drop=FALSE]
    temp <- cbind(df, rep(df, each = 6))
    names(temp) <- c("temp-1","temp-2","temp-3","temp-4","temp-5","temp-6","temp-7")
    scaled_temp <- as.data.frame(scale(temp))

    
## Site/occu covs ----------------------------------------------------------------------------------
    
    site_subset <- subset(site[,c('veg_cov','dwd_cov','fwd_cov','dwd_count','decay_cl','avg_volume')])
    site_subset$trt <- trt$trt
    site_subset$trt <- factor(trt$trt, 
                                  levels = c("UU", "BU", "HB", "HU", "BS"))
    # scale
    scaled_sitecovs <- as.data.frame(scale(site_subset))
    
    # add treatment to site covs matrix
    scaled_sitecovs$trt <- trt$trt
    scaled_sitecovs$trt <- factor(trt$trt, 
                       levels = c("UU", "BU", "HB", "HU", "BS"))
    
    
## merge and format --------------------------------------------------------------------------------------
 
  
    # this chunk was originally for csvToUMF; merging to single df is not required for model function
    #ex <- read.csv(system.file("csv","widewt.csv", package="unmarked"))

# scaled     
    scaled_sitecovs$site_id <- 1:nrow(scaled_sitecovs)
    
    # merge into one df
    df1 <- cbind(scaled_sitecovs, oss.dets)
    df1 <- cbind(df1, scaled_soilmoist)
    df1 <- cbind(df1, scaled_temp)
    df1 <- cbind(df1, scaled_rain)
    
    #reorder
    df_oss <- df1[,c(8:15, 1:7, 16:36)]
    
    write.csv(df_oss, "data/occupancy/oss_forUMF_scaled.csv", row.names = FALSE)
    
    
# unscaled    
    site_subset$site_id <- 1:nrow(site_subset)
    
    # merge into one df
    df2 <- cbind(site_subset, oss.dets)
    df2 <- cbind(df2, soil_moist)
    df2 <- cbind(df2, temp)
    df2 <- cbind(df2, rain)
    
    #reorder
    df_oss2 <- df2[,c(8:15, 1:7, 16:36)]
    
    write.csv(df_oss2, "data/occupancy/oss_forUMF_unscaled.csv", row.names = FALSE)
    
## convert using csvToUMF ------------------------------------------------------------------------------------
    
   # cant get this to work, gave up 
    #  dat1 <- csvToUMF("data/occupancy/oss_csvToUMF.csv", long = FALSE, type = "unmarkedFrameOccu")
    
    

    



