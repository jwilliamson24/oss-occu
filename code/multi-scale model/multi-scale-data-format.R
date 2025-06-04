## =================================================
##
## Title: multi-scale-data-format
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


## load data ----------------------------------------------------------------------------------------------

# 2023-2024 data
  site <- read.csv("data/site.complete.csv")
  subplot <- read.csv("data/subplot.complete.csv")
  
  site.lvl <- read.csv("data/covariate matrices/site_level_matrix.csv") # jul date
  subplot.lvl <- read.csv("data/covariate matrices/habitat.occu.complete.csv") # temp
  dwd.count <- read.csv("data/covariate matrices/avg-dwd-subplot-matrix.csv") # dwd count
  
# run code pre-fire-matrices.R
  head(xo) # pre-fire oss
  head(xe) # pre-fire enes
  
# run code occu-matrices-updated052025.R
  head(dets.o) # post-fire oss
  head(dets.e) # post-fire enes

# dwd long format
  dwd.long <- dwd.count %>%
    pivot_longer(cols = starts_with("X"),
                 names_to = "subplot",
                 names_prefix = "X",
                 values_to = "DW") %>%
    mutate(subplot = as.integer(subplot))
  
# temp from F to C
  enes$temp_C <- (enes$temp - 32) * 5/9
  subplot.lvl$temp_C <- round((subplot.lvl$temp - 32) * 5/9, 1)
  

## post fire matrix -------------------------------------------------------------------------------------------
  
# add covariates: DW, JulianDate, AirTemp to post-fire matrix to match pre-fire matrix

# oss  
  
  dets.o <- merge(dets.o, site.lvl[, c("site_id", "jul_date")], by = "site_id", all.x = TRUE) # jul date

  dets.o <- merge(dets.o, subplot.lvl[, c("site_id", "subplot", "temp_C")], by = c("site_id", "subplot"), all.x = TRUE) # temp

  dets.o <- merge(dets.o, dwd.long[, c("site_id", "subplot", "DW")], by = c("site_id", "subplot"), all.x = TRUE) # dwd
  
# enes
  
  dets.e <- merge(dets.e, site.lvl[, c("site_id", "jul_date")], by = "site_id", all.x = TRUE) # jul date
  
  dets.e <- merge(dets.e, subplot.lvl[, c("site_id", "subplot", "temp_C")], by = c("site_id", "subplot"), all.x = TRUE) # temp
  
  dets.e <- merge(dets.e, dwd.long[, c("site_id", "subplot", "DW")], by = c("site_id", "subplot"), all.x = TRUE) # dwd


## pre fire matrix -------------------------------------------------------------------------------------------
  
# subset
  
  xo2 <- xo[,c(1,2,5:8,10:12,16)]
  colnames(xo2) <- c("stand","year","subplot","V1","V2","V3","DW","jul_date","temp","trt")

  xe2 <- xe[,c(1,2,5:8,10:12,16)]
  colnames(xe2) <- c("stand","year","subplot","V1","V2","V3","DW","jul_date","temp","trt")
  
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

  
# merge -------------------------------------------------------------------------------------------------------
  
# oss
  
  colnames(dets.o)[10] <- "temp"
  colnames(dets.o)
  xo2 <- xo2[, c("site_id","subplot","stand","trt","year","V1","V2","V3","jul_date","temp","DW")]
  
  oss.full <- rbind(dets.o, xo2)
  
  write.csv(oss.full, "data/occupancy/oss.prepost.multiscale.occu.csv", row.names = FALSE)
    
  
# enes
  
  colnames(dets.e)[10] <- "temp"
  colnames(dets.e)
  xe2 <- xe2[, c("site_id","subplot","stand","trt","year","V1","V2","V3","jul_date","temp","DW")]
  
  enes.full <- rbind(dets.e, xe2)
   
  write.csv(enes.full, "data/occupancy/enes.prepost.multiscale.occu.csv", row.names = FALSE)
  
  
  
  
