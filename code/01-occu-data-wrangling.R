#### 01-occu-data-wrangling.R ####
##
## Creates working csv's for all four data sheets/types:
## downed wood data = dwd.complete.csv
## site data = site.complete.csv
## subplot data =
## salamander data =
##
## Jasmine Williamson
## Date Created: 06-20-2024
##
##

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)

## Notes on data
#changed 10185 from HB to BS in all three original 2023 csv sheets
#need to verify 16804 as BU or BS using maps (and maybe field verification??)
#its listed as BU in 2023 data sheet, but listed as BS on my original detailed stand info google sheet
#i have two extra HU sites. i used the wrong sheet to tally numbers for 2024 season plans, so i went over


#### Downed Wood Data -----------------------------------------------------------------
# downed wood data - done
# length class col = 0 if row is a stump

#2023 data
dwd_2023 <- read.csv("oss_2023_dwd.csv")
dwd_2023 <- dwd_2023[, -10] #delete random extra column

#identify rows with NA in decay class column
na_rows <- which(is.na(dwd_2023$decay_cl))
dwd_2023 <- dwd_2023[-na_rows, ] #Remove these rows from the data frame

#separate subplot code column into three columns by underscore
dwd_2023 <- dwd_2023 %>%
      separate(subplot_code, into = c("stands", "trt", "subplot"), sep = "_")

#separate "stands" column into two columns by letters and numbers
dwd_2023 <- dwd_2023 %>%
  mutate(landowner = str_extract(stands, "^([A-Za-z]+)"),
         stand = str_extract(stands, "(?<=\\D)(\\d+)")) %>% #call new column "stand"
  select(-stands) #remove original "stands" column 
dwd_2023 <- dwd_2023[, -5] #delete "id" column
dwd_2023$date_mdy <- as.Date(dwd_2023$date, format = "%m/%d/%Y") #format date, add column
dwd_2023$subplot <- as.numeric(gsub("^0+", "", dwd_2023$subplot)) #remove leading 0 so both years match

#make columns factors to check for data entry errors
dwd_2023$trt <- as.factor(dwd_2023$trt)
dwd_2023$subplot <- as.factor(dwd_2023$subplot)
dwd_2023$dwd_type <- as.factor(dwd_2023$dwd_type)
dwd_2023$size_cl <- as.factor(dwd_2023$size_cl)
dwd_2023$decay_cl <- as.factor(dwd_2023$decay_cl)
dwd_2023$char_cl <- as.factor(dwd_2023$char_cl)
dwd_2023$landowner <- as.factor(dwd_2023$landowner)



#2024 data
dwd_2024 <- read.csv("oss_2024_dwd.csv", 
                colClasses = c(landowner="factor", stand="character", trt="factor", 
                               subplot="factor", dwd_type="factor", size_cl="factor",
                               decay_cl="factor", char_cl="factor"))
dwd_2024$date_mdy <- as.Date(dwd_2024$date, format = "%m/%d/%Y")



#combine both years of downed wood
dwd.complete <- rbind(dwd_2023, dwd_2024)

#changing blank NA cells to zeros in the length class column
dwd.complete$length_cl[dwd.complete$length_cl == "" | is.na(dwd.complete$length_cl)] <- 0
dwd.complete$length_cl <- as.factor(dwd.complete$length_cl) #making it a factor

summary(dwd.complete)

#unique number of stands by year
unique_stands_by_year <- dwd.complete %>%
       group_by(year) %>%
       summarize(unique_stands = n_distinct(stand))


# save as csv
write.csv(dwd.complete, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/dwd.complete.csv", 
          row.names = FALSE)




#### Salamander Data -----------------------------------------------------------------
# 2 missing age classes - obs and sal id person dont match?
# notes: this data frame includes all captures. unoccupied sites not included.
# SLV has NA's for animals that we didn't measure (non-OSS animals and OSS recaps)
# ID has NA's for 2023 animals, blank for 2024 animals that are not OSS clips

#load and format 2023 data
sals_2023 <- read.csv("oss_2023_sals.csv", 
                      colClasses = c(obs="factor", pass="factor", 
                                     spp="factor", cover_obj="factor", 
                                     substrate="factor", age_class="factor"))

#separate subplot code column
sals_2023 <- sals_2023 %>%
  separate(subplot_code, into = c("stands", "trt", "subplot"), sep = "_")

sals_2023 <- sals_2023 %>%
  mutate(landowner = str_extract(stands, "^([A-Za-z]+)"),
         stand = str_extract(stands, "(?<=\\D)(\\d+)")) %>%
  select(-stands) 

#delete unnecessary col, add year, format date, delete leading zero
sals_2023 <- sals_2023[, -7] #delete "under" column
sals_2023$year <- 2023
sals_2023$date_mdy <- as.Date(sals_2023$date, format = "%m/%d/%Y")
sals_2023$subplot <- as.numeric(gsub("^0+", "", sals_2023$subplot))


#load and format 2024 data
sals_2024 <- read.csv("oss_2024_sals.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", pass="factor", spp="factor", 
                                cover_obj="factor", substrate="factor"))

sals_2024$date_mdy <- as.Date(sals_2024$date, format = "%m/%d/%Y")

#data frame with both years of sal data
sals <- bind_rows(sals_2024,sals_2023)

#change columns to factor to check
sals$landowner <- as.factor(sals$landowner)
sals$trt <- as.factor(sals$trt)
sals$subplot <- as.factor(sals$subplot)

sals$age_class <- as.character(sals$age_class)
sals$age_class[sals$age_class == "" | is.na(sals$age_class)] <- "U"
sals$age_class <- as.factor(sals$age_class)

#### Site Data -----------------------------------------------------------------

site_2023 <- read.csv("oss_2023_sitesubplot.csv")
site_2023$year <- 2023 #add year column to 2023 data
site_2023$date_mdy <- as.Date(site_2023$date, format = "%m/%d/%Y") #format date
summary(site_2023)

site_2024 <- read.csv("oss_2024_site.csv")
site_2024$date_mdy <- as.Date(site_2024$date, format = "%m/%d/%y") #format date
summary(site_2024)

#subset 2023/2024 data so that merged will only include cols they both have in common
site_2023_subset <- names(site_2023) %in% names(site_2024) #vector of 2023 col names that match 2024 col names
site_2024_subset <- names(site_2024) %in% names(site_2023) #vector of 2024 col names that match 2023 col  names

#rbind them to include only those cols in common
site_2023_2024 <- bind_rows(site_2023[site_2023_subset],site_2024[site_2024_subset])

#2023 has data for all subplots, so we need to work on that
#subset new data frame to inlcude only cols we care about for 2023
new_site_2023_subset <- subset(site_2023_2024, subset = year==2023)

#i want to average elev, temp, hum for all rows that have the same stand number
#this code creates new data frame with that info
site_2023_avg <- aggregate(cbind(temp,hum,elev)~stand,new_site_2023_subset,mean)

#prepare to merge site_2023_avg cols with other site cols
new_site_2023_subset[, c(5:7)] <- list(NULL) #remove old temp,hum,elev cols
new_site_2023_subset <- unique(new_site_2023_subset) #keep only one of each stand, remove duplicates
site_2023_joined <- inner_join(site_2023_avg,new_site_2023_subset,by="stand") #merge
summary(site_2023_joined)

#combine by subsetted 2023 data with the 2024 site data and format
site.complete <- bind_rows(site_2023_joined,subset(site_2023_2024,subset=year==2024))
site.complete$landowner <- as.factor(site.complete$landowner)
site.complete$trt <- as.factor(site.complete$trt)

summary(site.complete)

# save as csv
write.csv(site.complete, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/site.complete.csv", 
          row.names = FALSE)



#### Subplot Data -----------------------------------------------------------------

site_2023 <- read.csv("oss_2023_sitesubplot.csv")
site_2023$year <- 2023 #add year column to 2023 data
site_2023$date_mdy <- as.Date(site_2023$date, format = "%m/%d/%Y") #format date

subplot_24 <- read.csv("oss_2024_subplot.csv", 
                colClasses = c(landowner="factor", stand="character", trt="factor"))
subplot_24$date_mdy <- as.Date(subplot_24$date, format="%m/%d/%Y") #format date
subplot_24$stand <- as.integer(subplot_24$stand) #change stand so matches 2023 for merging

#subset 2023/2024 data so that merged will only include cols they both have in common
subplot_23_subset <- names(site_2023) %in% names(subplot_24)
subplot_24_subset <- names(subplot_24) %in% names(site_2023)
#combine
subplot_23_24 <- bind_rows(site_2023[subplot_23_subset],subplot_24[subplot_24_subset])

#format
subplot_23_24$landowner <- as.factor(subplot_23_24$landowner)
subplot_23_24$trt <- as.factor(subplot_23_24$trt)
subplot_23_24$subplot <- as.factor(subplot_23_24$subplot)
subplot_23_24$weather <- as.factor(subplot_23_24$weather)
summary(subplot_23_24)


#subsetting so i can look at the years separately
site <- read.csv("site.complete.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor"))
summary(site)
site23 <- subset(site, year==2023)
site24 <- subset(site, year==2024)
summary(site23)
summary(site24)




