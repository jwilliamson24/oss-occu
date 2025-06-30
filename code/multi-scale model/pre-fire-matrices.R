## =================================================
##
## Title: pre-fire-matrices
## Author: Jasmine Williamson
## Date Created: 5/28/2025
##
## Description: Create occu matrix for all years of pre-fire data for oss and enes;
## used code from Jay Jones CreateBACIModelingDatasets.r 
##
## =================================================


## settings -----------------------------------------------------------------------------------------------

#  rm(list=ls())
  library(tidyr)
  library(dplyr)
  library(ggplot2)


## pre-fire data ----------------------------------------------------------------------------------------------

# OSS

  xo13 <- read.csv("data/pre-fire data/OSS detections and covariates 2013 - stacked.csv")
  xo14 <- read.csv("data/pre-fire data/OSS detections and covariates 2014 - stacked.csv")
  xo15 <- read.csv("data/pre-fire data/OSS detections and covariates 2015 - stacked.csv")
  xo16 <- read.csv("data/pre-fire data/OSS detections and covariates 2016 - stacked.csv")
  xo17 <- read.csv("data/pre-fire data/OSS detections and covariates 2017 - stacked.csv")
  xo18 <- read.csv("data/pre-fire data/OSS detections and covariates 2018 - stacked.csv")
  xo19 <- read.csv("data/pre-fire data/OSS detections and covariates 2019 - stacked.csv")
  
  xo13$Year <- rep("2013", nrow(xo13))
  xo14$Year <- rep("2014", nrow(xo14))
  xo15$Year <- rep("2015", nrow(xo15))
  xo16$Year <- rep("2016", nrow(xo16))
  xo17$Year <- rep("2017", nrow(xo17))
  xo18$Year <- rep("2018", nrow(xo18))
  xo19$Year <- rep("2019", nrow(xo18))

# ENES

  xe13 <- read.csv("data/pre-fire data/ENES detections and covariates 2013 - stacked.csv")
  xe14 <- read.csv("data/pre-fire data/ENES detections and covariates 2014 - stacked.csv")
  xe15 <- read.csv("data/pre-fire data/ENES detections and covariates 2015 - stacked.csv")
  xe16 <- read.csv("data/pre-fire data/ENES detections and covariates 2016 - stacked.csv")
  xe17 <- read.csv("data/pre-fire data/ENES detections and covariates 2017 - stacked.csv")
  xe18 <- read.csv("data/pre-fire data/ENES detections and covariates 2018 - stacked.csv")
  xe19 <- read.csv("data/pre-fire data/ENES detections and covariates 2019 - stacked.csv")
  
  xe13$Year <- rep("2013", nrow(xe13))
  xe14$Year <- rep("2014", nrow(xe14))
  xe15$Year <- rep("2015", nrow(xe15))
  xe16$Year <- rep("2016", nrow(xe16))
  xe17$Year <- rep("2017", nrow(xe17))
  xe18$Year <- rep("2018", nrow(xe18))
  xe19$Year <- rep("2019", nrow(xe18))


# Trt
  
  trt.wide <- read.csv("data/pre-fire data/Harvest state 2013-2019.csv", na.strings="*")
  trt.wide$TreeFarm <- as.factor(ifelse(as.character(trt.wide$TreeFarm)=="SP ", "SP", as.character(trt.wide$TreeFarm)))
  
  # remove rows that are NA for years 2013-2016
  trt.wide <- trt.wide[!(is.na(trt.wide$X2013) & is.na(trt.wide$X2014) & is.na(trt.wide$X2015) & is.na(trt.wide$X2016)),]
  
  # stack
  trt <- gather(trt.wide, "Year", "HarvestState", 4:10)
  trt$StandID <- as.numeric(trt$StandNo)
  trt$Trt <- as.factor(trt$HarvestState)
  trt$YearN <- as.numeric(as.factor(trt$Year))
  
  trt.grp <- trt %>% group_by(StandNo) %>% summarize(Group=min(as.numeric(Trt), na.rm=T)) # 1 = unit was cut; 2 = unit was never cut
  trt <- merge(trt, trt.grp)
  trt$TrtGrp <- as.factor(ifelse(trt$Group==1 & trt$Trt=="Cut", "PostTrt", # if site is trt group=1 and trt=cut, label it post-trt
                                 ifelse(trt$Group==1 & trt$Trt=="Timbered", "PreTrt", "Control"))) 
                                 # if site is group=1 and trt=timbered, label it pre-trt
                                 # otherwise, it is control (group=2 which means it was never cut)
  

## merge  ----------------------------------------------------------------------------------------------
  
  
  xo <- rbind(xo13, xo14, xo15, xo16, xo17, xo18, xo19)
  xo$StandID <- as.numeric(as.factor(xo$StandNo))
  xo$YearN <- as.numeric(as.factor(xo$Year))
  xo$Owner <- factor(xo$Owner, levels=c("BLM", "ODF", "PBTF", "WY"))
  xo <- xo[!is.na(xo$DW) & !is.na(xo$JulianDate) & !is.na(xo$AirTemp),]
  
  
  xe <- rbind(xe13, xe14, xe15, xe16, xe17, xe18, xe19)
  xe$Owner <- as.factor(ifelse(as.character(xe$Owner)=="PB", "PBTF", as.character(xe$Owner)))
  xe$StandID <- as.numeric(as.factor(xe$StandNo))
  xe$YearN <- as.numeric(as.factor(xe$Year))
  xe <- xe[!is.na(xe$DW) & !is.na(xe$JulianDate) & !is.na(xe$AirTemp),]
  
  
  trt <- trt[trt$StandNo %in% unique(xo$StandNo),]
  trt$StandNo <- as.character(trt$StandNo)
  trt$Year <- gsub("^X", "", trt$Year)
  
  xo <- merge(xo, trt[, c("StandNo", "HarvestState", "TrtGrp", "Year")])
  xe <- merge(xe, trt[, c("StandNo", "HarvestState", "TrtGrp", "Year")])
  
  write.csv(xo, "data/occupancy/dets.o.pre.csv", row.names = FALSE)
  write.csv(xe, "data/occupancy/dets.e.pre.csv", row.names = FALSE)
  
  
  
  
  
  