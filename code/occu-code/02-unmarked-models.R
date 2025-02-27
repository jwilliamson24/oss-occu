## -------------------------------------------------------------------------------------------------------
##
## 02-unmarked-models-trt.R 
##
## Rerun the occupancy models using package Unmarked from 2023 analysis
##
## Jasmine Williamson
## Date Created: 07-09-2024
##
## 
## goals
## using the unmarked code that i started last august, rework to use new covariate matrix

## insights
## 2/27/25 tried formatting for use of csvToUMF but couldnt get function to work, gave up on it 
# 
# 

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
    sals <- read.csv("data/sals.complete.csv", 
                     colClasses = c(landowner="factor", stand="character", trt="factor",
                                    obs="factor", subplot="factor", recap="factor",
                                    pass="factor", spp="factor", cover_obj="factor", 
                                    substrate="factor", age_class="factor"))
    
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
    names(soil_moist)[names(soil_moist) == "1"] <- "soil_moist"
    
    scaled_soilmoist <- as.data.frame(scale(soil_moist))
    names(scaled_soilmoist) <- c("soilmoist-1","soilmoist-2","soilmoist-3","soilmoist-4","soilmoist-5","soilmoist-6","soilmoist-7")

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
    # scale
    scaled_sitecovs <- as.data.frame(scale(site_subset))
    
    # add treatment to site covs matrix
    scaled_sitecovs$trt <- trt$trt
    scaled_sitecovs$trt <- factor(trt$trt, 
                       levels = c("UU", "BU", "HB", "HU", "BS"))
    
    
## merge and format --------------------------------------------------------------------------------------
    
    # this chunk was originally for csvToUMF; merging to single df is not required for model function
    #ex <- read.csv(system.file("csv","widewt.csv", package="unmarked"))
    
    scaled_sitecovs$site_id <- 1:nrow(scaled_sitecovs)
    
    # merge into one df
    df4 <- cbind(scaled_sitecovs, oss.dets)
    df4 <- cbind(df4, scaled_soilmoist)
    df4 <- cbind(df4, scaled_temp)
    df4 <- cbind(df4, scaled_rain)
    
    #reorder
    df_oss <- df4[,c(8:15, 1:7, 16:36)]
    
    write.csv(df_oss, "data/occupancy/oss_forUMF.csv", row.names = FALSE)
    
## convert using csvToUMF ------------------------------------------------------------------------------------
    
   # cant get this to work, gave up 
    #  dat1 <- csvToUMF("data/occupancy/oss_csvToUMF.csv", long = FALSE, type = "unmarkedFrameOccu")
    
    
## name and scale covs for unmarked object ----------------------------------------------------------

#data frames with site rows and subplots columns
    # hum <- sitecovs$hum
    # temp <- sitecovs$temp
    # weather <- weather
    # date <- site$date_mdy
    # soilmoist <- soil_moist
    # date <-  site$date_mdy
    # 
    # #scale det dovs
    # scaled_hum <- as.data.frame(scale(hum))
    # scaled_temp <- as.data.frame(scale(temp))
    # #scaled_date <- as.data.frame(scale(date))
    # scaled_soilmoist <- as.data.frame(scale(soilmoist))



## OSS Top Model Exploration-------------------------------------------------------------------------------
    
    # Build OSS unmarkedFrameOccu Object
    
    UMF.oss.1 <- unmarkedFrameOccu(
      y = df_oss[, grep("^X", names(df_oss))],  # selects cols starting with X
      siteCovs = df_oss[, c("veg_cov","dwd_cov","fwd_cov","dwd_count","decay_cl","avg_volume","trt")],
      obsCovs = list(
        temp = df_oss[, grep("temp-", names(df_oss))], # selects cols including temp-
        soilmoist = df_oss[, grep("soilmoist-", names(df_oss))],
        rain = df_oss[, grep("rain-", names(df_oss))]
      )
    )
    
    

    # UMF.oss.scaled <- unmarkedFrameOccu(
    #   y = oss.dets, 
    #   obsCovs = list(weather = weather, soilmoist = scaled_soilmoist), 
    #   siteCovs = scaled_sitecovs)

    
# Run several models to determine Psi and P covariates
    # occu(~det covs ~occu covs, data=, se=TRUE)

    #1: psi(.) p(.)
    m1 <- occu(~1 ~1, data=UMF.oss.1)
    #AIC: 
    
    #2: psi(treatment) p(.)
    m2 <- occu(~1 ~trt, data=UMF.oss.1)
    #AIC: 
    
    #3: psi(.) p(days since rain)                 
    m3 <- occu(~rain ~1, data=UMF.oss.1)
    #AIC: 
    
    #4: psi(treatment) p(days since rain) 
    m4 <- occu(~rain ~trt, data=UMF.oss.1)
    #AIC: 821.3062 
    
    #5: psi(treatment) p(temp + days since rain) 
    m5 <- occu(~temp + rain ~trt, data=UMF.oss.1)
    #AIC: 
    
    #6: psi(treatment) p(soil moist + days since rain)
    m6 <- occu(~soilmoist + rain ~trt, data=UMF.oss.1)
    #AIC: 
    
    #7: psi(treatment) p(soil moist + veg cov + days since rain)
    m7 <- occu(~soilmoist + veg_cov + rain ~trt, data=UMF.oss.1)
    #AIC: 
    
    #8: psi(treatment) p(temp + days since rain) 
    m8 <- occu(~ + rain ~trt + decay_cl + dwd_cov, data=UMF.oss.1)
    #AIC:
    
    #9: psi(treatment) p(temp + days since rain) 
    m9 <- occu(~temp + rain ~trt, data=UMF.oss.1)
    #AIC:
    
    #10: psi(treatment) p(temp + days since rain) 
    m10 <- occu(~temp + rain ~trt, data=UMF.oss.1)
    #AIC:
    
    
  
## Extract treatment occupancy predictions and plot ------------------------------------------------------------

# Extract psi predictions from model
    preds <- predict(m1, type="state")
    sites_trt <- sitecovs[, c("site_id","trt")]
    site_preds <- cbind(sites_trt, preds)
    
    
# Barplot of predictions for each treatment
    ggplot(site_preds, aes(x = trt, y = Predicted, ymin = lower, ymax = upper, fill = trt)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.25, position = position_dodge(width = 0.7), linewidth = 1) +
      labs(title = "OSS: Treatment vs. Predicted Occupancy",
           x = "Treatment", y = "Predicted Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1")
    
    #ggsave("unmarked_occu_barplot_oss.png", 
         #  path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/figures/Unmarked_occu_TWS_2023")
    
    
# Boxplot of preds with confidence intervals
    ggplot(site_preds, aes(x = trt, y = Predicted, fill = trt)) +
      geom_boxplot() +
      geom_point(position = position_dodge(width = 0.75), size = 3) +
      geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.25, position = position_dodge(width = 0.75), size = 1) +
      labs(title = "OSS: Treatment vs. Predicted Occupancy",
           x = "Treatment", y = "Predicted Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1")
    
    #ggsave("unmarked_occu_pt_est_oss.png", 
        # path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/figures/Unmarked_occu_TWS_2023")


