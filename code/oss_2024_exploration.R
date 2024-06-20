#### oss_2024_exploration.R ####
##
## Initial efforts to organize, explore, and summarize 2024 data
##
## Jasmine Williamson
## Date Created: 06-10-2024
##
##

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-spatial-occu/data")

library(dplyr)
library(tidyr)

#### Load data -----------------------------------------------------------------

# Site-level data - done
site <- read.csv("oss_2024_site.csv", 
                 colClasses = c(landowner="factor", stand="character", 
                                trt="factor", obs="factor"))     
site$date_mdy <- as.Date(site$date, format = "%m/%d/%y") #add new formatted date column
summary(site)


# Subplot-level data - done
subplot <- read.csv("oss_2024_subplot.csv", 
                    colClasses = c(landowner="factor", stand="character", 
                                   trt="factor", subplot="factor", 
                                   obs="factor", weather="factor"))
subplot$date_mdy <- as.Date(subplot$date, format = "%m/%d/%Y")
subplot$time_24 <- gsub(":", "", subplot$time) #add new time column without colon
subplot$time_24 <- as.numeric(subplot$time_24) #make new time col numeric
subplot <- subplot[-c(17:19)] #delete the three single soil moisture columns
summary(subplot)
length(unique(subplot$stand)) #60 stands


# Downed wood data - done
# notes: type/class columns are 0 if no wood was found in the plot
# length column is blank for all stumps, values only entered for logs
dwd <- read.csv("oss_2024_dwd.csv", 
                colClasses = c(landowner="factor", stand="character", trt="factor", 
                               subplot="factor", dwd_type="factor", size_cl="factor",
                               decay_cl="factor", char_cl="factor", length_cl="factor"))
dwd$date_mdy <- as.Date(dwd$date, format = "%m/%d/%y")
summary(dwd)
length(unique(dwd$stand)) #60 stands


# Salamander capture data - almost done, need to check data sheets
# 2 missing age classes - obs and sal id person dont match?
# notes: this data frame includes all captures. unoccupied sites not included.
# SLV has NA's for animals that we didn't measure (non-OSS animals and OSS recaps)
sals <- read.csv("oss_2024_sals.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", subplot="factor", pass="factor", spp="factor", 
                                cover_obj="factor", substrate="factor", age_class="factor"))
sals$date_mdy <- as.Date(sals$date, format = "%m/%d/%y")
summary(sals)
length(unique(sals$stand)) #59 stands
length(unique(sals$spp)) #7 spp


# is there a way i can save these data frames for later use?
# 6/18/24 all four data sheets have been read in/formatted to the best of my knowledge


#### Summary Stats -------------------------------------------------------------

# Summarizing numeric data
head(site)
site.names <- names(site) #save col names
site.sum.table <- c() #create empty summary table to fill

for (i in 7:9) { #for cols 7-9 in site (the numerical ones)
  site.sum.table <- rbind(site.sum.table, c(mean(site[,i]), sd(site[,i]), 
                                            min(site[,i]), max(site[,i])))
} #calc mean, sd, min, max and bind those rows together in the summ table

#cbind site names and then add column names
site.sum.table <- cbind(as.data.frame(site.names[7:9]),site.sum.table)
colnames(site.sum.table) <- c("variable","mean","sd","min","max")


head(subplot)
#only one numerical variable to summarize in subplot data
soil.moist <- c(mean(subplot$soil_moist_avg), sd(subplot$soil_moist_avg),
                min(subplot$soil_moist_avg), max(subplot$soil_moist_avg))

#avg subplot soil moisture by treatment
by.trt.mean <- tapply(subplot$soil_moist_avg, subplot$trt, mean)
as.data.frame(by.trt.mean)

#soil moist by landowner and trt
aggregate(subplot$soil_moist_avg, list(trt=subplot$trt, owner=subplot$landowner), mean)



# Summarizing categorical data
head(subplot) 
head(dwd)
head(sals)

#total species counts
spp.tot.counts <- table(sals$spp)
spp.tot.counts <- as.data.frame(spp.tot.counts)
colnames(spp.tot.counts) <- c("species","count")
spp.tot.counts

#create sal detection data frame using unique combinations of landowner, stand, trt,
#subplot, and species 
spp.detect <- unique(sals[,c(1:3,7,10)])
spp.detect$detect = 1 #create new column, detection=1 for all rows indicating captures

#total subplot level detections by species
detections <- aggregate(spp.detect[c(6)], by=spp.detect[c(5)], sum)
#detections just per treatment group
trt.detect <- aggregate(spp.detect[c(6)], by=spp.detect[c(3)], sum)

#stand level detections
stand.detect <- aggregate(spp.detect[c(6)], by=spp.detect[c(2)], sum)
#stand level detections by treatment
sal.richness <- aggregate(spp.detect[c(6)], by=spp.detect[c(2:3)], sum)


#merge sal richness and site data into one data frame
#this will add NA's in detect column for sites without detections
#will be used to look at correllations between detections and site variables
sal.correl <- merge(sal.richness,site, all=TRUE)
#change NA's to zeros in detect column
sal.correl$detect <- ifelse(is.na(sal.correl$detect),0,sal.correl$detect)

#look at the correlation between detections and temp
cor(sal.correl$detect,sal.correl$temp)
# -0.4 is a low negative correllation, as temp inc detect decreases



####  --------------------------------------------------------------------------