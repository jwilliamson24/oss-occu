##
## 02-data-exploration.R 
##
## Initial efforts to organize, explore, and summarize 2023-2024 data
##
## Jasmine Williamson
## Date Created: 06-10-2024
##
## -------------------------------------------------------------------------------------------------------

rm(list=ls())
#setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")
setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")

library(dplyr)
library(tidyr)

#### Load data -----------------------------------------------------------------
site <- read.csv("site.complete.csv")
dwd <- read.csv("dwd.complete.csv")
subplot <- read.csv("subplot.complete.csv")
sals <- read.csv("sals.complete.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", subplot="factor", recap="factor",
                                pass="factor", spp="factor", cover_obj="factor", 
                                substrate="factor", age_class="factor"))

#sals_2023 <- read.csv("sals.2023.csv")
#sals_2024 <- read.csv("sals.2024.csv")



#subsetting so i can look at the years separately
site <- read.csv("site.complete.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor"))
summary(site)
site23 <- subset(site, year==2023)
site24 <- subset(site, year==2024)
summary(site23)
summary(site24)

#### Summarizing basic numeric data ------------------------------------------------

#Site: temp, humidity, elevation
#goal: calc mean, sd, min, max and bind those rows together in summ table
head(site)
site_num <- site[sapply(site, is.numeric)]
site.names <- names(site_num) #save col names
site.sum.table <- c() #create empty summary table to fill

for (i in seq_along(site_num)) { #for cols 2-4 in site (the numerical ones)
  site.sum.table <- rbind(site.sum.table, c(mean(site_num[,i]), sd(site_num[,i]), 
                                            min(site_num[,i]), max(site_num[,i])))
} #calc mean, sd, min, max and bind those rows together in the summ table

#cbind site names and then add column names
site.sum.table <- cbind(as.data.frame(site.names),site.sum.table)
colnames(site.sum.table) <- c("variable","mean","sd","min","max")


#Subplot: soil moisture
head(subplot)
soil.moist <- c(mean(subplot$soil_moist_avg), sd(subplot$soil_moist_avg),
                min(subplot$soil_moist_avg), max(subplot$soil_moist_avg))

#avg subplot soil moisture by treatment
by.trt.mean <- tapply(subplot$soil_moist_avg, subplot$trt, mean)
as.data.frame(by.trt.mean)

#soil moist by landowner and trt
aggregate(subplot$soil_moist_avg, list(trt=subplot$trt, owner=subplot$landowner), mean)



#### Summarizing salamander count data ------------------------------------------------

head(sals)

# add detection column to the entire sals data frame
sals$detect = 1 
length(sals$detect) # = 409

#total subplot level detections by species
detections <- aggregate(sals[c(21)], by=sals[c(14)], sum)
detections 

# spp detect
# 1 AMGR      5
# 2 ANFE      1
# 3 ENES    138
# 4  OSS    257
# 5 PLDU      5
# 6 TAGR      3

sals_2024 <- subset(sals, year == 2024)
detections24 <- aggregate(sals_2024[c(21)], by=sals_2024[c(14)], sum)
detections24


#detections just per treatment group
trt.detect <- aggregate(sals[c(21)], by=sals[c(5)], sum)
trt.detect

# trt detect
# 1  BS     35
# 2  BU    130
# 3  HB     70
# 4  HU     47
# 5  UU    127



#### troubleshooting sals data frames - done ------------------------------------------------


# stands are repeated throughout data; stand names are not unique identifier
# site id = unique identifier for each survey instance, 
# including site_rep number noting sites that were surveyed a second time

# sal id = unique identifier for each individual animal captured
# use sal id to get total count data
# use site id and detections to get 0/1 occupancy data

  
  # all sals

  length(sals$detect) # = 409

  #aggregate detections by site id
  stand.detect <- aggregate(sals[c(21)], by=sals[c(1)], sum)
  stand.detect
  length(stand.detect$detect) # = 92

  #unique number of sites in detection data frame
  site.detect1 <- unique(sals[,c(1)])
  length(site.detect1) # = 92
  
  #unique instances of site and treatment
  site.detect <- unique(sals[,c(1,5)])
  length(site.detect$site_id) # = 92 
  
  #unique instances of site, treatment, and year
  site.detect.2 <- unique(sals[,c(1,5,6)])
  length(site.detect.2$site_id) # = 92
  
  #unique instances of site, trt, year, subplot
  #gives single subplot detection as 1, not including repeats in a single subplot
  site.detect.3 <- unique(sals[,c(1,5,6,12)])
  length(site.detect.3$site_id) # = 243
  
  #unique instances of site and sal id
  #gives all detections, including repeats
  site.detect.4 <- unique(sals[,c(1,9)])
  length(site.detect.4$site_id) #409
  
  
  # 2023 sals
  # 408178 was surveyed twice
  
  sals_2023$detect = 1
  length(sals_2023$detect) # = 180
  length(unique(sals_2023$stand)) # = 42
  length(unique(sals_2023$site_id)) # = 43
  
  #aggregate detections by site
  stand.detect <- aggregate(sals_2023[c(18)], by=sals_2023[c(1)], sum)
  stand.detect
  length(stand.detect$detect) # = 43
  
  #unique number of sites in detection data frame
  site.detect1 <- unique(sals_2023[,c(1)])
  length(site.detect1) # = 43
  
  #unique instances of stand and treatment
  #bc both 408178 stands are different treatments
  site.detect <- unique(sals_2023[,c(3,5)])
  length(site.detect$stand) # = 43 
  
  #unique site id and trt
  site.detect <- unique(sals_2023[,c(1,5)])
  length(site.detect$site_id) # = 43 
  
  #unique instances of stand, trt, year, subplot
  site.detect.3 <- unique(sals_2023[,c(3,5,6,12)])
  length(site.detect.3$stand) # = 106
  
  
  
  
  # 2024 sals
  #many were surveyed twice, some have two in same stand and trt combo
  
  sals_2024$detect = 1
  length(sals_2024$detect) # = 229
  length(unique(sals_2024$stand)) # = 47
  length(unique(sals_2024$site_id)) # = 49
  
  #unique instances of stand and treatment
  site.detect <- unique(sals_2024[,c(3,5)])
  length(site.detect$stand) # = 49
  
  #unique instances of stand, treatment, site rep and year
  site.detect.2 <- unique(sals_2024[,c(3,4,5,6)])
  length(site.detect.2$stand) # = 49
  
  #unique instances of stand, trt, year, subplot
  site.detect.3 <- unique(sals_2024[,c(3,5,6,12)])
  length(site.detect.3$stand) # = 137
  
  
#### matrix of oss occupied sites with coords for ODFW 1/9/24 -----------------------------------------
  
  oss <- subset(sals, spp=="OSS")
  oss_subset <- oss[, c("site_id","subplot","spp")]

  subplot_subset <- subplot[, c("site_id","subplot","lat","long")]

  merged <- merge(oss_subset, subplot_subset, by = c("site_id","subplot"))
  
  merged$state <- "OR"
  merged <- merged[,-2]
  
  merged <- merged[, c("spp","site_id","state","lat","long")]
  
  # Remove duplicate rows based on the "site" column
  merged_unique <- merged %>%
    distinct(site_id, .keep_all = TRUE)
  
  
write.csv(merged_unique, "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/oss_points.csv")

#### end --------------------------------------------------------------------------