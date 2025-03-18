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
# 02/27/25 tried formatting for use of csvToUMF but couldnt get function to work, gave up on it 
# 03/18/25 updated matrix for UMF using all variables for global model


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
    
    
# dwd cover
    
    df <- subset(subplot[,c('subplot','dwd_cov')])
    
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
    dwd_cov <- df_wide[,-1]
    names(dwd_cov) <- c("dwdcov-1","dwdcov-2","dwdcov-3","dwdcov-4","dwdcov-5","dwdcov-6","dwdcov-7")
    
# time since rain
    
    df <- site[,"days_since_rain", drop=FALSE]
    rain <- cbind(df, rep(df, each = 6))
    names(rain) <- c("rain-1","rain-2","rain-3","rain-4","rain-5","rain-6","rain-7")

# temp
    
    df <- site[,"temp", drop=FALSE]
    temp <- cbind(df, rep(df, each = 6))
    names(temp) <- c("temp-1","temp-2","temp-3","temp-4","temp-5","temp-6","temp-7")
    
# treatment
    
    df <- trt[,"trt", drop=FALSE]
    treatment <- cbind(df, rep(df, each = 6))
    names(treatment) <- c("trt-1","trt-2","trt-3","trt-4","trt-5","trt-6","trt-7")
    

    
## Site/occu covs ----------------------------------------------------------------------------------
    
    site_subset <- subset(site[,c('veg_cov', 'canopy_cov', 'soil_moist', 'fwd_cov', 'dwd_count', 'avg_volume')])
    site_subset$trt <- trt$trt
    site_subset$trt <- factor(trt$trt, 
                                  levels = c("UU", "BU", "HB", "HU", "BS"))

    
    # for this method, scale in the formula, not in the input data
    
    
## merge and format --------------------------------------------------------------------------------------
 
    
    site_subset$site_id <- 1:nrow(site_subset)
    
    # merge into one df
    df2 <- cbind(site_subset, oss.dets)
    df2 <- cbind(df2, soil_moist)
    df2 <- cbind(df2, temp)
    df2 <- cbind(df2, rain)
    df2 <- cbind(df2, dwd_cov)
    df2 <- cbind(df2, treatment)
    
    #reorder
    df_oss2 <- df2[,c(8:15, 1:7, 16:50)]
    
    write.csv(df_oss2, "data/occupancy/oss-for-UMF-1.csv", row.names = FALSE)
    

    
    
    

    



