#### 02-data-exploration.R ####
##
## Initial efforts to organize, explore, and summarize 2023-2024 data
##
## Jasmine Williamson
## Date Created: 06-10-2024
##
##

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

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


#### Summarizing basic numeric data ------------------------------------------------

#Site: temp, humidity, elevation
#goal: calc mean, sd, min, max and bind those rows together in summ table
head(site)
site.names <- names(site) #save col names
site.sum.table <- c() #create empty summary table to fill

for (i in 2:4) { #for cols 2-4 in site (the numerical ones)
  site.sum.table <- rbind(site.sum.table, c(mean(site[,i]), sd(site[,i]), 
                                            min(site[,i]), max(site[,i])))
} #calc mean, sd, min, max and bind those rows together in the summ table

#cbind site names and then add column names
site.sum.table <- cbind(as.data.frame(site.names[2:4]),site.sum.table)
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

#counting the number of each species found over both seasons
spp.tot.counts <- table(sals$spp) #tally of spp counts
spp.tot.counts <- as.data.frame(spp.tot.counts) #make data frame
colnames(spp.tot.counts) <- c("spp","count") #format col names
spp.tot.counts
sum(spp.tot.counts$count)

#counting the number of salamanders observed in each treatment
sal.trt.counts <- table(sals$trt)
sal.trt.counts <- as.data.frame(sal.trt.counts)
colnames(sal.trt.counts) <- c("trt","count")
sal.trt.counts
sum(sal.trt.counts$count)


# add detection column to the entire sals data frame
sals$detect = 1 
length(sals$detect) # = 409
#409 detections total, including repeats per stand
#this works because the sals df only includes detections;
#there are no "empty" or "zero" rows for subplots that did not have any sals found


#using unique combinations of stand, trt
#this is only detections per stand, not showing plot repeats
site.detect <- unique(sals[,c(2,3)])
length(site.detect$stand) # = 64 site level detections including all spp

#using unique combinations of stand, trt, subplot
#this is only detection data = it does not include multiple counts of same spp within a subplot
subplot.detect <- unique(sals[,c(2,3,7)])
length(subplot.detect$stand) # =205 subplot level detections including all spp




#better/quicker way to do the sal count data frames:

#total subplot level detections by species
detections <- aggregate(sals[c(17)], by=sals[c(11)], sum)
detections


#detections just per treatment group
trt.detect <- aggregate(sals[c(17)], by=sals[c(3)], sum)
trt.detect





      # why do all these sections of code give me different answers? 
  #stand level detections
  stand.detect <- aggregate(sals[c(17)], by=sals[c(2)], sum)
  stand.detect
  length(stand.detect$detect) # = 62

  #detections per stand, not showing plot repeats
  site.detect1 <- unique(sals[,c(2)])
  length(site.detect1) # = 62
  
  #using unique combinations of stand, trt
  #detections per stand, not showing plot repeats
  site.detect <- unique(sals[,c(2,3)])
  length(site.detect$stand) # = 64 
  
  #using unique combinations of stand, trt, year
  site.detect.2 <- unique(sals[,c(2,3,4)])
  length(site.detect.2$stand) # = 92
  
  #using unique combinations of stand, trt, year, subplot
  site.detect.3 <- unique(sals[,c(2,3,4,7)])
  length(site.detect.3$stand) # = 243
  
  
  # i did not repeat any sites within 2023 season
  # in 2024, i repeated many sites from 2023 once - 
    # stand names stayed identical
  # in 2024, i repeated some sites twice within the season -
    # stand names became stand#.1 and stand#.2
  
  
  
  
  
  
  
  
  
  

#stand level detections by treatment
sal.richness <- aggregate(spp.detect[c(6)], by=spp.detect[c(2:3)], sum)

#### correl efforts from peterson code example-----------------------------------------------

#merge sal richness and site data into one data frame
#this will add NA's in detect column for sites without detections
#will be used to look at correllations between detections and site variables
sal.correl <- merge(sal.richness,site, all=TRUE)
#change NA's to zeros in detect column
sal.correl$detect <- ifelse(is.na(sal.correl$detect),0,sal.correl$detect)

#look at the correlation between detections and temp
cor(sal.correl$detect,sal.correl$temp)
# -0.4 is a low negative correllation, as temp inc detect decreases



#### end --------------------------------------------------------------------------