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

  rm(list=ls())
  #setwd("/Users/jasminewilliamson/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")


## load data ------------------------------------------------------------------------

# 2023-2024 covariate data
  site.lvl <- read.csv("data/covariate matrices/site_level_matrix.csv") # site level covs
  dwd.count <- read.csv("data/covariate matrices/avg-dwd-subplot-matrix.csv") # dwd count by plot
  subplot.lvl <- read.csv("data/covariate matrices/habitat.occu.complete.csv") # covariates at subplot lvl
  
# pre-fire lat/long/elev data
  geo.data.pre <- read.csv("data/prefire-shp-attributes/geo-data-all-plots-prefire.csv", row.names = 1)
  
# from pre-fire-matrices.R
  xo <- read.csv("data/occupancy/dets.o.pre.csv") # pre-fire oss
  xe <- read.csv("data/occupancy/dets.e.pre.csv") # pre-fire enes
  
# from occu-matrices-postfire.R
  dets.o <- read.csv("data/occupancy/dets.o.post.csv") # post-fire oss
  dets.e <- read.csv("data/occupancy/dets.e.post.csv") # post-fire enes

  
## format data -----------------------------------------------------------------------
  
# site covariates 
  
  # landowner to management type column
  site.lvl <- site.lvl %>%
    mutate(mgmt_type = case_when(
      landowner == "PB" ~ "0", # private
      landowner == "WY" ~ "0",  # private
      landowner == "BLM" ~ "1",  # public
      landowner == "ODF" ~ "1",  # public
      TRUE ~ "default_value"  
    ))  
  
  # add treatment columns
  treatments <- unique(site.lvl$trt)
  for (trt in treatments) {
    site.lvl[[trt]] <- as.numeric(site.lvl$trt == trt)
  }  
  
  # subset site, date, owner, mgmt, treatments
  site.info <- site.lvl %>%
    select(site_id, jul_date, landowner, mgmt_type, HB,BU,BS,HU,UU)
  
  
# subplot covariates
  
  # dwd
  dwd.long <- dwd.count %>%
    pivot_longer(cols = starts_with("X"),
                 names_to = "subplot",
                 names_prefix = "X",
                 values_to = "DW") %>%
    mutate(subplot = as.integer(subplot))
  
  # temp F to C
  subplot.lvl$tempC <- (subplot.lvl$temp - 32) * 5/9
  
  # site, subplot, temp, lat, long, elev
  subplot.info <- subplot.lvl %>%
    select(site_id, subplot, tempC, lat, long, elev)
  
  
# add to detection matrices
  
  # oss  
  dets.o <- dets.o %>%
    left_join(site.info, by = "site_id") %>%
    left_join(subplot.info, by = c("site_id", "subplot")) %>%
    left_join(dwd.long %>% select(site_id, subplot, DW), by = c("site_id", "subplot"))
  
  # enes
  dets.e <- dets.e %>%
    left_join(site.info, by = "site_id") %>%
    left_join(subplot.info, by = c("site_id", "subplot")) %>%
    left_join(dwd.long %>% select(site_id, subplot, DW), by = c("site_id", "subplot"))
  
  
  

## pre fire matrices ------------------------------------------------------------------

# sal counts to detections
  xo$V1 <- ifelse(xo$V1 > 0, 1, 0)
  xo$V2 <- ifelse(xo$V2 > 0, 1, 0)
  xo$V3 <- ifelse(xo$V3 > 0, 1, 0)
  
  xe$V1 <- ifelse(xe$V1 > 0, 1, 0)
  xe$V2 <- ifelse(xe$V2 > 0, 1, 0)
  xe$V3 <- ifelse(xe$V3 > 0, 1, 0)
  
  
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
  xe3 <- merge(xe2, geo.data.pre, by = c("stand", "subplot", "year"))
  xo3 <- merge(xo2, geo.data.pre, by = c("stand", "subplot", "year"))
  
  
  
# merge -----------------------------------------------------------------------------
  
# oss
  # subset
  colnames(dets.o)
  colnames(dets.o) <- c("site_id","stand","trt","year","subplot","V1","V2","V3",
                        "jul_date","owner","mgmt_type", "HB", "BU", "BS", "HU", "UU", 
                        "temp", "lat", "long", "elev", "DW")

  colnames(xo3)
  xo3$BS <- 0
  xo3$HB <- 0
  xo3$BU <- 0

  oss.full <- rbind(dets.o, xo3)
  summary(oss.full)
  
  write.csv(oss.full, "data/occupancy/oss.prepost.multiscale.occu.csv", row.names = FALSE)
    
  
# enes
  colnames(dets.e)
  colnames(dets.e) <- c("site_id","stand","trt","year","subplot","V1","V2","V3",
                        "jul_date","owner","mgmt_type", "HB", "BU", "BS", "HU", "UU", 
                        "temp", "lat", "long", "elev", "DW")
  
  colnames(xe3)
  xe3$BS <- 0
  xe3$HB <- 0
  xe3$BU <- 0
  
  enes.full <- rbind(dets.e, xe3)
  summary(enes.full)
  
  write.csv(enes.full, "data/occupancy/enes.prepost.multiscale.occu.csv", row.names = FALSE)
  
  
