#### oss_allyears_exploration.R ####
##
## Initial efforts to organize, explore, and summarize 2023 and 2024 data together
##
## Jasmine Williamson
## Date Created: 06-20-2024
##
##

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-spatial-occu/data")

library(dplyr)
library(tidyr)
library(tidyverse)

#### Downed Wood Data -----------------------------------------------------------------

dwd_2023 <- read.csv("oss_2023_dwd.csv", 
                     colClasses = c(landowner="factor", trt="factor", 
                                    subplot="factor", dwd_type="factor", size_cl="factor",
                                    decay_cl="factor", char_cl="factor", length_cl="factor"))
dwd_2023 <- dwd_2023[, -10] #delete random extra column


#separate subplot code column into three columns by underscore
dwd_2023 <- dwd_2023 %>%
      separate(subplot_code, into = c("stands", "trt", "subplot"), sep = "_")

#separate stand column into two columns by letters and numbers
dwd_2023 <- dwd_2023 %>%
  mutate(landowner = str_extract(stands, "^([A-Za-z]+)"),
         stand = str_extract(stands, "(?<=\\D)(\\d+)")) %>%
  select(-stands) #remove original column 
dwd_2023 <- dwd_2023[, -5] #delete id column
dwd_2023$date_mdy <- as.Date(dwd_2023$date, format = "%m/%d/%Y")
dwd_2023$subplot <- as.numeric(gsub("^0+", "", dwd_2023$subplot)) #remove leading 0 so both years match



# Downed wood data - done
# notes: type/class columns are 0 if no wood was found in the plot
# length column is blank for all stumps, values only entered for logs
dwd_2024 <- read.csv("oss_2024_dwd.csv", 
                colClasses = c(landowner="factor", stand="character", trt="factor", 
                               subplot="factor", dwd_type="factor", size_cl="factor",
                               decay_cl="factor", char_cl="factor", length_cl="factor"))
dwd_2024$date_mdy <- as.Date(dwd_2024$date, format = "%m/%d/%Y")


#combine both years of downed wood
dwd <- rbind(dwd_2023, dwd_2024)


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






