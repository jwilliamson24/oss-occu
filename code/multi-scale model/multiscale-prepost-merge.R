## =================================================
##
## Title: multiscale-prepost-merge
## Author: Jasmine Williamson
## Date Created: 5/29/2025
##
## Description: Create single matrix with: pre-fire and post-fire, detection and covariate data
## Use rows at the subplot level and columns at the repeat survey (1-3) level
##
## =================================================


## settings -----------------------------------------------------------------------------------------------

  #rm(list=ls())
  #setwd("/Users/jasminewilliamson/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
  
  library(unmarked)
  library(ggplot2)
  library(stats)
  library(MASS)
  library(tidyverse)
  library(dplyr)


## load data ------------------------------------------------------------------------

# 2023-2024 covariate data
  # site <- read.csv("data/site.complete.csv")
  # subplot <- read.csv("data/subplot.complete.csv")
  # 
  # site.lvl <- read.csv("data/covariate matrices/site_level_matrix.csv") # jul date
  # subplot.lvl <- read.csv("data/covariate matrices/habitat.occu.complete.csv") # temp
  # dwd.count <- read.csv("data/covariate matrices/avg-dwd-subplot-matrix.csv") # dwd count

# 2023-2024 covariate data
  df1 <- read.csv("data/covariate matrices/env_subset_corr2.csv", row.names = 1) # relevant env
  df2 <- read.csv("data/covariate matrices/site_aspect_precip_all_vars.csv", row.names = 1) # includes trt, landowner  

# pre-fire lat/long/elev data
  geo.data.pre <- read.csv("data/prefire-shp-attributes/geo-data-all-plots-prefire.csv")
  
# from pre-fire-matrices.R
  xo <- read.csv("data/occupancy/dets.o.pre.csv") # pre-fire oss
  xe <- read.csv("data/occupancy/dets.e.pre.csv") # pre-fire enes
  
# from occu-matrices-postfire.R
  dets.o <- read.csv("data/occupancy/dets.o.post.csv") # post-fire oss
  dets.e <- read.csv("data/occupancy/dets.e.post.csv") # post-fire enes

  
## format data -----------------------------------------------------------------------
  
# sal counts to detections
  xo$V1 <- ifelse(xo$V1 > 0, 1, 0)
  xo$V2 <- ifelse(xo$V2 > 0, 1, 0)
  xo$V3 <- ifelse(xo$V3 > 0, 1, 0)
  
  xe$V1 <- ifelse(xe$V1 > 0, 1, 0)
  xe$V2 <- ifelse(xe$V2 > 0, 1, 0)
  xe$V3 <- ifelse(xe$V3 > 0, 1, 0)
  
# merge and subset
  covs <- merge(df1, df2[, c("site_id","trt", "landowner")], by = "site_id", all.x = TRUE)
  covs <- subset(covs, select = -c(dwd_cov, size_cl, char_cl, aspect, avg_volume))
  
# landowner to management type column
  covs <- covs %>%
    mutate(mgmt_type = case_when(
      landowner == "PB" ~ "0", # private
      landowner == "WY" ~ "0",  # private
      landowner == "BLM" ~ "1",  # public
      landowner == "ODF" ~ "1",  # public
      TRUE ~ "default_value"  
    ))  
  
# add treatment columns
  treatments <- unique(covs$trt)
  
  for (trt in treatments) {
    covs[[trt]] <- as.numeric(covs$trt == trt)
  }  
  
# add covariates to occupancy matrices 
  
  # post-fire
  occu.o <- dets.o %>%
    left_join(covs, by = "site_id")

  occu.e <- dets.e %>%
    left_join(covs, by = "site_id")
  

## pre fire matrices ------------------------------------------------------------------
  
# subset
  xo2 <- xo[,c(1:3,5:8,10:12,16)]
  colnames(xo2) <- c("stand","year","owner","subplot","V1","V2","V3","DW","jul_date","temp","trt")

  xe2 <- xe[,c(1:3,5:8,10:12,16)]
  colnames(xe2) <- c("stand","year","owner","subplot","V1","V2","V3","DW","jul_date","temp","trt")
  
# change treatment designation names
  xo2$trt <- as.character(xo2$trt)
  xo2$trt[xo2$trt == "Control"] <- "UU"
  xo2$trt[xo2$trt == "PreTrt"] <- "UU"
  xo2$trt[xo2$trt == "PostTrt"] <- "HU"
  xo2$trt <- as.factor(xo2$trt)
  
  xe2$trt <- as.character(xe2$trt)
  xe2$trt[xe2$trt == "Control"] <- "UU"
  xe2$trt[xe2$trt == "PreTrt"] <- "UU"
  xe2$trt[xe2$trt == "PostTrt"] <- "HU"
  xe2$trt <- as.factor(xe2$trt)
  
# create site_id col to match post-fire data
  xo2$site_id <- paste(xo2$stand, 1, xo2$year, sep = " _ ")
  xe2$site_id <- paste(xe2$stand, 1, xe2$year, sep = " _ ")
  
# landowner to management type column
  xe2 <- xe2 %>%
    mutate(mgmt_type = case_when(
      owner == "PBTF" ~ "0", # private
      owner == "WY" ~ "0",  # private
      owner == "BLM" ~ "1",  # public
      owner == "ODF" ~ "1",  # public
      TRUE ~ "default_value"  
    ))
  
  xo2 <- xo2 %>%
    mutate(mgmt_type = case_when(
      owner == "PBTF" ~ "0", # private
      owner == "WY" ~ "0",  # private
      owner == "BLM" ~ "1",  # public
      owner == "ODF" ~ "1",  # public
      TRUE ~ "default_value"  
    ))

# add treatment columns
  treatments <- unique(xo2$trt)
  for (trt in treatments) {
    xo2[[trt]] <- as.numeric(xo2$trt == trt)
  } 
  
  treatments <- unique(xe2$trt)
  for (trt in treatments) {
    xe2[[trt]] <- as.numeric(xe2$trt == trt)
  } 
  
# add lat, long, elev
  xe3 <- merge(xe2, geo.data.pre, by = c("stand", "subplot"))
  
  
# merge -----------------------------------------------------------------------------
  
# oss
  # subset
  occu.o <- occu.o[,c(1:3,4,5:13,18,23,24)]
  colnames(occu.o)
  colnames(occu.o) <- c("site_id","stand","trt","year","subplot","V1","V2","V3",
                        "jul_date","lat","long","elev","temp","DW","owner","mgmt_type")

  # gotta add to xo2 and xe2 lat long elev and then merge
  colnames(xo2)
  xo2 <- xo2[, c("site_id","subplot","stand","trt","year","V1","V2","V3","jul_date","temp","DW","owner","mgmt_type")]
  
  oss.full <- rbind(occu.o, xo2)
  
  write.csv(oss.full, "data/occupancy/oss.prepost.multiscale.occu.csv", row.names = FALSE)
    
  
# enes
  occu.e <- occu.e[,c(1:3,4,5:13,18,23,24)]
  colnames(occu.e)
  colnames(occu.e) <- c("site_id","stand","trt","year","subplot","V1","V2","V3",
                        "jul_date","lat","long","elev","temp","DW","owner","mgmt_type")
  
  colnames(xo2)
  xe2 <- xe2[, c("site_id","subplot","stand","trt","year","V1","V2","V3","jul_date","temp","DW","owner","mgmt_type")]
  
  enes.full <- rbind(dets.e, xe2)
   
  write.csv(enes.full, "data/occupancy/enes.prepost.multiscale.occu.csv", row.names = FALSE)
  
  
  
  
