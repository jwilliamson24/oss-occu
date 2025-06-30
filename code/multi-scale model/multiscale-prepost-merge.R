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
  site <- read.csv("data/site.complete.csv")
  subplot <- read.csv("data/subplot.complete.csv")
  
  site.lvl <- read.csv("data/covariate matrices/site_level_matrix.csv") # jul date
  subplot.lvl <- read.csv("data/covariate matrices/habitat.occu.complete.csv") # temp
  dwd.count <- read.csv("data/covariate matrices/avg-dwd-subplot-matrix.csv") # dwd count
  
# from pre-fire-matrices.R
  xo <- read.csv("data/occupancy/dets.o.pre.csv") # pre-fire oss
  xe <- read.csv("data/occupancy/dets.e.pre.csv") # pre-fire enes
  
# from occu-matrices-postfire.R
  dets.o <- read.csv("data/occupancy/dets.o.post.csv") # post-fire oss
  dets.e <- read.csv("data/occupancy/dets.e.post.csv") # post-fire enes

  
## format data -----------------------------------------------------------------------
  
# dwd long format
  dwd.long <- dwd.count %>%
    pivot_longer(cols = starts_with("X"),
                 names_to = "subplot",
                 names_prefix = "X",
                 values_to = "DW") %>%
    mutate(subplot = as.integer(subplot))
  
  
# temp from F to C
  #enes$temp_C <- (enes$temp - 32) * 5/9
  subplot.lvl$temp_C <- round((subplot.lvl$temp - 32) * 5/9, 1)
  
  
# sal counts to detections
  xo$V1 <- ifelse(xo$V1 > 0, 1, 0)
  xo$V2 <- ifelse(xo$V2 > 0, 1, 0)
  xo$V3 <- ifelse(xo$V3 > 0, 1, 0)
  
  xe$V1 <- ifelse(xe$V1 > 0, 1, 0)
  xe$V2 <- ifelse(xe$V2 > 0, 1, 0)
  xe$V3 <- ifelse(xe$V3 > 0, 1, 0)
  
  
## post fire matrix ---------------------------------------------------------------
  
# add covariates: DW, JulianDate, AirTemp, Owner to post-fire matrix to match pre-fire matrix

# oss  
  
  dets.o <- merge(dets.o, site.lvl[, c("site_id", "jul_date")], by = "site_id", all.x = TRUE) # jul date

  dets.o <- merge(dets.o, subplot.lvl[, c("site_id", "subplot", "temp_C")], by = c("site_id", "subplot"), all.x = TRUE) # temp

  dets.o <- merge(dets.o, dwd.long[, c("site_id", "subplot", "DW")], by = c("site_id", "subplot"), all.x = TRUE) # dwd
  
  dets.o <- merge(dets.o, site.lvl[, c("site_id", "landowner")], by = c("site_id"), all.x = TRUE) # owner
  
  # dets.o <- merge(dets.o, subplot.lvl[, c("site_id", "subplot", "lat")], by = c("site_id", "subplot"), all.x = TRUE) # lat
  # 
  # dets.o <- merge(dets.o, subplot.lvl[, c("site_id", "subplot", "long")], by = c("site_id", "subplot"), all.x = TRUE) # long
  # 
  # dets.o <- merge(dets.o, subplot.lvl[, c("site_id", "subplot", "elev")], by = c("site_id", "subplot"), all.x = TRUE) # elev
  
########## when i get lat/long/elev for pre fire data, i can just run the commented code here to add these to the post fire matrix 
  
    
# enes
  
  dets.e <- merge(dets.e, site.lvl[, c("site_id", "jul_date")], by = "site_id", all.x = TRUE) # jul date
  
  dets.e <- merge(dets.e, subplot.lvl[, c("site_id", "subplot", "temp_C")], by = c("site_id", "subplot"), all.x = TRUE) # temp
  
  dets.e <- merge(dets.e, dwd.long[, c("site_id", "subplot", "DW")], by = c("site_id", "subplot"), all.x = TRUE) # dwd

  dets.e <- merge(dets.e, site.lvl[, c("site_id", "landowner")], by = c("site_id"), all.x = TRUE) # owner
  
  # dets.e <- merge(dets.e, subplot.lvl[, c("site_id", "subplot", "lat")], by = c("site_id", "subplot"), all.x = TRUE) # lat
  # 
  # dets.e <- merge(dets.e, subplot.lvl[, c("site_id", "subplot", "long")], by = c("site_id", "subplot"), all.x = TRUE) # long
  # 
  # dets.e <- merge(dets.e, subplot.lvl[, c("site_id", "subplot", "elev")], by = c("site_id", "subplot"), all.x = TRUE) # elev  

  
 # landowner to management type column
  
  dets.e <- dets.e %>%
    mutate(mgmt_type = case_when(
      landowner == "PB" ~ "0", # private
      landowner == "WY" ~ "0",  # private
      landowner == "BLM" ~ "1",  # public
      landowner == "ODF" ~ "1",  # public
      TRUE ~ "default_value"  
    ))
  
  dets.o <- dets.o %>%
    mutate(mgmt_type = case_when(
      landowner == "PB" ~ "0", # private
      landowner == "WY" ~ "0",  # private
      landowner == "BLM" ~ "1",  # public
      landowner == "ODF" ~ "1",  # public
      TRUE ~ "default_value"  
    ))
  
  
## pre fire matrix ------------------------------------------------------------------
  
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
  

  
# merge -----------------------------------------------------------------------------
  
# oss
  
  colnames(dets.o)[10] <- "temp"
  colnames(dets.o)[12] <- "owner"
  colnames(dets.o)
  colnames(xo2)
  xo2 <- xo2[, c("site_id","subplot","stand","trt","year","V1","V2","V3","jul_date","temp","DW","owner","mgmt_type")]
  
  oss.full <- rbind(dets.o, xo2)
  
  write.csv(oss.full, "data/occupancy/oss.prepost.multiscale.occu.csv", row.names = FALSE)
    
  
# enes
  
  colnames(dets.e)[10] <- "temp"
  colnames(dets.e)[12] <- "owner"
  colnames(dets.e)
  xe2 <- xe2[, c("site_id","subplot","stand","trt","year","V1","V2","V3","jul_date","temp","DW","owner","mgmt_type")]
  
  enes.full <- rbind(dets.e, xe2)
   
  write.csv(enes.full, "data/occupancy/enes.prepost.multiscale.occu.csv", row.names = FALSE)
  
  
  
  
