#### oss_allyears_exploration.R ####
##
## Initial efforts to organize, explore, and summarize 2023 and 2024 data together
##
## Jasmine Williamson
## Date Created: 06-20-2024
##
##

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

library(dplyr)
library(tidyr)
library(tidyverse)

#### Downed Wood Data -----------------------------------------------------------------


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
dwd <- rbind(dwd_2023, dwd_2024)

#changing blank NA cells to zeros in the length class column
dwd$length_cl[dwd$length_cl == "" | is.na(dwd$length_cl)] <- 0
dwd$length_cl <- as.factor(dwd$length_cl) #making it a factor

summary(dwd)

# downed wood data - done
# length class col = 0 if row is a stump




#### Salamander Data -----------------------------------------------------------------
# 2 missing age classes - obs and sal id person dont match?
# notes: this data frame includes all captures. unoccupied sites not included.
# SLV has NA's for animals that we didn't measure (non-OSS animals and OSS recaps)

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
                                cover_obj="factor", substrate="factor", age_class="factor"))

sals_2024$date_mdy <- as.Date(sals_2024$date, format = "%m/%d/%Y")

#data frame with both years of sal data
sals <- bind_rows(sals_2024,sals_2023)


#### Site Data -----------------------------------------------------------------
#want to do the same with rest of data






