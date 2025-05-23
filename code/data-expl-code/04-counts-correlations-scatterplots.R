# -------------------------------------------------------------------------------------------------------
##
## 04-counts-correlations-scatterplots.R 
##
## Basic correlations using code from Jim's class.
##
## Jasmine Williamson
## Date Created: 07-23-2024
##
## 
## goals --------------------------------------------------------------------------------------------------

# i want to calculate correlations between sal data and habitat variables:
# subplot level: canopy, canopy, dwd,veg, fwd, soil moist
# site level: elev, temp, hum

## insights --------------------------------------------------------------------------------------------------

# none of the correlations are above 0.7, so there are not any easily apparent linear relationships between
# variables and salamander counts according to pearson's coefficient
# the results were identical with both scaled and unscaled covs

# next steps: explore nonlinear relationships using scatter plots
# categorical (cover metrics) seeming very weird, maybe wont work with this method
# soil moisture looks weird. not great with detection data
# nothing exciting here. move on to more robust methods.

## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(dplyr)
    library(tidyr)
    library(tidyverse)
    library(corrplot)
    

## load data ----------------------------------------------------------------------------------------------

    site <- read.csv("site.complete.csv")
    # dwd <- read.csv("dwd.complete.csv")
    subplot <- read.csv("subplot.complete.csv")
    sals <- read.csv("sals.complete.csv", 
                     colClasses = c(landowner="factor", stand="character", trt="factor",
                                    obs="factor", subplot="factor", recap="factor",
                                    pass="factor", spp="factor", cover_obj="factor", 
                                    substrate="factor", age_class="factor"))
    
    
    covs <- read.csv("covariate matrices/env_subset_corr.csv")
    covs <- rename(covs, site_id = X)
    

## captures by site -----------------------------------------------------------------------------------

    # create df with counts and non-detections
    sals$detect <- 1
    sals.agg <- sals %>%
      group_by(site_id,spp) %>%
      summarise(counts = sum(detect))
    
    sites <- unique(site$site_id)
    sites <- data.frame(site_id=sites)
    
    
    #add non-detections and turn NAs to 0
    sals.all.sites <- merge(sites,sals.agg,by="site_id",all.x=TRUE)
    sals.all.sites$counts[is.na(sals.all.sites$counts)] <- 0
    # total counts of all spp
    all.counts <- sals.all.sites %>%
      group_by(site_id) %>%
      summarise(counts = sum(counts))
    
    
    # all oss captures
    oss.counts <- subset(sals.all.sites, spp=="OSS")
    #add non-detections and turn NAs to 0
    oss.counts <- merge(sites,oss.counts,by="site_id",all.x=TRUE)
    oss.counts$counts[is.na(oss.counts$counts)] <- 0
    oss.counts <- oss.counts[,-2]


    # all enes captures
    enes.counts <- subset(sals.all.sites, spp=="ENES")
    #add non-detections and turn NAs to 0
    enes.counts <- merge(sites,enes.counts,by="site_id",all.x=TRUE)
    enes.counts$counts[is.na(enes.counts$counts)] <- 0
    enes.counts <- enes.counts[,-2]


## captures by subplot -----------------------------------------------------------------------------

    # create df with counts and non-detections
    sals$detect <- 1
    sals.agg <- sals %>%
      group_by(site_id,subplot,spp) %>%
      summarise(counts = sum(detect))
    
    subplots <- subplot[,c(1,9)]
    
    
    #add non-detections and turn NAs to 0
    sals.all.sub <- merge(subplots,sals.agg,by=c("site_id","subplot"),all.x=TRUE)
    sals.all.sub$counts[is.na(sals.all.sub$counts)] <- 0
    # total counts of all spp
    all.counts.sub <- sals.all.sub %>%
      group_by(site_id,subplot) %>%
      summarise(counts = sum(counts))
    
    
    # all oss captures
    oss.counts.sub <- subset(sals.all.sub, spp=="OSS")
    #add non-detections and turn NAs to 0
    oss.counts.sub <- merge(subplots,oss.counts.sub,by=c("site_id","subplot"),all.x=TRUE)
    oss.counts.sub$counts[is.na(oss.counts.sub$counts)] <- 0
    oss.counts.sub <- oss.counts.sub[,-3]
    
    #write.csv(oss.counts.sub, "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/oss_counts_subplot.csv",
     #         row.names = FALSE)
    
    
    # all enes captures
    enes.counts.sub <- subset(sals.all.sub, spp=="ENES")
    #add non-detections and turn NAs to 0
    enes.counts.sub <- merge(subplots,enes.counts.sub,by=c("site_id","subplot"),all.x=TRUE)
    enes.counts.sub$counts[is.na(enes.counts.sub$counts)] <- 0
    enes.counts.sub <- enes.counts.sub[,-3]
    
    #write.csv(enes.counts.sub, "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/enes_counts_subplot.csv",
     #         row.names = FALSE)

## correlations at site level ------------------------------------------------------------------
# no correlations > 0.7

# site level: elev, temp, hum
    
    # scale site covs, but keep site id col
    scaled_sitecovs <- data.frame(
      site_id = covs$site_id,
      scale(covs[, sapply(covs, is.numeric)])
    )
    
    # all spp
    correl1 <- merge(all.counts,scaled_sitecovs, by="site_id")
    cor1 <- cor(correl1$counts,correl1[,3:16])
    round(cor1,4)

    
    
    # oss
    correl2 <- merge(oss.counts,scaled_sitecovs,all=TRUE)
    cor2 <- cor(correl2$counts,correl2[,3:16])
    round(cor2,4)

    barplot(cor2,
            main = "Correlations with Counts",
            xlab = "Environmental Variables",
            ylab = "Correlation Coefficient",
            col = "skyblue",
            border = "black",
            las = 2)
    
    
    # enes
    correl <- merge(enes.counts,scaled_sitecovs,all=TRUE)
    cor(correl$counts,correl[,3:5])
    #         elev        temp        hum
    # [1,] -0.06664847 0.009123333 0.05129161


## correlations at subplot level -----------------------------------------------------------------
# no correlations > 0.7


# subplot level: canopy, canopy, dwd, veg, fwd, soil moist

    # all spp
    correl <- merge(all.counts.sub,subplot,all=TRUE)
    cor(correl$counts,correl[,15:19])
    #     canopy_cov   dwd_cov   veg_cov    fwd_cov soil_moist_avg
    # [1,]  0.2117299 0.1037624 0.0281595 0.02207587     0.02463708
    
    
    # oss
    correl <- merge(oss.counts.sub,subplot,all=TRUE)
    cor(correl$counts,correl[,15:19])
    #     canopy_cov    dwd_cov      veg_cov    fwd_cov soil_moist_avg
    # [1,]  0.1404988 0.08180737 0.0004098591 0.01666842   -0.003715001
    
    
    # enes
    correl <- merge(enes.counts.sub,subplot,all=TRUE)
    cor(correl$counts,correl[,15:19])
    #     canopy_cov    dwd_cov   veg_cov     fwd_cov soil_moist_avg
    # [1,]  0.1916478 0.07399331 0.0526096 0.006406893     0.04975393


## scatter plots: site level -------------------------------------------------------------------------


    correl1 <- merge(all.counts,scaled_sitecovs,all=TRUE)
    correl2 <- merge(all.counts,site,all=TRUE)
    correl3 <- merge(oss.counts,scaled_sitecovs,all=TRUE)
    correl4 <- merge(oss.counts,site,all=TRUE)
    correl5 <- merge(enes.counts,scaled_sitecovs,all=TRUE)
    correl6 <- merge(enes.counts,site,all=TRUE)
    
    
    ggplot(correl3, aes(x=temp, y=counts)) +
      geom_point() +  # Plot points
      geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add smooth line
      labs(title = "Capture Counts vs. Temperature", x = "Temperature F", y = "Count") +
      theme_minimal()
    
    ggplot(correl4, aes(x=hum, y=counts)) +
      geom_point() +  # Plot points
      geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add smooth line
      labs(title = "Capture Counts vs. Humidity", x = "% Humidity", y = "Count") +
      theme_minimal()
    
    ggplot(correl5, aes(x=temp, y=counts)) +
      geom_point() +  # Plot points
      geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add smooth line
      labs(title = "Capture Counts vs. Temp", x = "Temperature F", y = "Count") +
      theme_minimal()


## scatter plots: subplot level ---------------------------------------------------------------------

    correl7 <- merge(all.counts.sub,subplot,all=TRUE)
    correl8 <- merge(oss.counts.sub,subplot,all=TRUE)
    correl9 <- merge(enes.counts.sub,subplot,all=TRUE)
    
    
    ggplot(correl7, aes(x=soil_moist_avg, y=counts)) +
      geom_point() +  # Plot points
      geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add smooth line
      labs(title = "Capture Counts vs. Soil Moisture", x = "Soil Moisture", y = "Count") +
      theme_minimal()



    
    
    

    ggplot(correl2, aes(x = dwd_count, y = counts)) +
      geom_point(position = position_jitter(height = 0.1)) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      labs(title = "Relationship between DWD Counts and Salamander Presence",
           x = "DWD Count",
           y = "Salamander Present") +
      theme_classic()



























