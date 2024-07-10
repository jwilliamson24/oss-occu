## -------------------------------------------------------------------------------------------------------
##
## 04-occupancy-matrices.R 
##
## Create occupancy matrix data frames for all spp, oss, enes
##
## Jasmine Williamson
## Date Created: 07-02-2024
##
## 

## settings -----------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

library(unmarked)
library(ggplot2)
library(stats)
library(MASS)
library(tidyverse)

## load data----------------------------------------------------------------------------------------------

site <- read.csv("site.complete.csv")
subplot <- read.csv("subplot.complete.csv")
sals <- read.csv("sals.complete.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", subplot="factor", recap="factor",
                                pass="factor", spp="factor", cover_obj="factor", 
                                substrate="factor", age_class="factor"))



## Create occu data frames for oss -----------------------------------------------------------------

#subset of sals with site, subplot, spp
sals.new <- sals[,c(1,12,14)] 

#creating occupancy df with non-detections
df.new <- subplot[,c(1,9)] #new df with all subplots to merge sals df with
df.new$subplot <- as.factor(df.new$subplot)


## OSS --------------------------------------------------------------------------------------------

#oss detection df
sals.oss <- subset(sals.new, spp=="OSS")
sals.oss <- sals.oss[,-3]
sals.oss <- sals.oss %>%
  distinct(site_id, subplot) #only keeping single detection per subplot
sals.oss$detect <- 1

#oss occupancy df
df.merge.oss <- full_join(df.new,sals.oss,by=c("site_id","subplot")) #merge by site and subplot
df.merge.oss$detect <- ifelse(is.na(df.merge.oss$detect), 0, df.merge.oss$detect) #make NA's = 0

#make into wide format occu df
df.wide.oss <- df.merge.oss %>%
  pivot_wider(names_from = subplot, values_from = detect)
df.wide.oss <- as.data.frame(df.wide.oss)
rownames(df.wide.oss) <- df.wide.oss[,1]
df.wide.oss <- df.wide.oss[,-1]

#save
write.csv(df.wide.oss, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/oss.occu.df.csv", 
          row.names = FALSE)



## ENES --------------------------------------------------------------------------------------------

#enes detection df
sals.enes <- subset(sals.new, spp=="ENES")
sals.enes <- sals.enes[,-3]
sals.enes <- sals.enes %>%
  distinct(site_id, subplot)
sals.enes$detect <- 1

#enes occupancy df
df.merge.enes <- full_join(df.new,sals.enes,by=c("site_id","subplot")) #merge by site and subplot
df.merge.enes$detect <- ifelse(is.na(df.merge.enes$detect), 0, df.merge.enes$detect) #make NA's = 0

#make into wide format occu df
df.wide.enes <- df.merge.enes %>%
  pivot_wider(names_from = subplot, values_from = detect)
df.wide.enes <- as.data.frame(df.wide.enes)
rownames(df.wide.enes) <- df.wide.enes[,1]
df.wide.enes <- df.wide.enes[,-1]

#save
write.csv(df.wide.enes, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/enes.occu.df.csv", 
          row.names = FALSE)


