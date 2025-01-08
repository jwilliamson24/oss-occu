##
## 04-repeat-occu-model-nimble.R 
##
## Rerunning the bayesian occupancy code from 2023 with all data
##
## Jasmine Williamson
## Date Created: 07-03-2024
##
#### settings ---------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

library(nimble)
library(ggplot2)
library(data.table)
library(tidyverse)
library(mcmcplots)
library(MCMCvis)
library(boot)
source('attach.nimble_v2.R')


#### load data --------------------------------------------------------------------------------------------

site <- read.csv("site.complete.csv")
subplot <- read.csv("subplot.complete.csv")
sals <- read.csv("sals.complete.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", subplot="factor", recap="factor",
                                pass="factor", spp="factor", cover_obj="factor", 
                                substrate="factor", age_class="factor"))


#### format data -----------------------------------------------------------------------------------------

# site = df with all sites and treatments
# spp_count = df with sal counts, enes.obs, and oss.obs
# 
        ###### struggling to get the occupancy df created in the right way____________________________________________________________

## goal:
## df with rows for every site and subplot, cols with temp, hum, soil moist, enes obs, oss obs, all obs

# add detection col
sals$detect <- 1

# new df group by site
df_count <- sals %>%
  group_by(site_id, date_mdy, subplot, spp, detect) %>%
  summarize(n = n())

# Reshape the data to have one row per site and a count column for each species
spp_count <- df_count %>%
  pivot_wider(names_from = spp, values_from = detect, values_fill = 0)

# delete spp i dont care about
spp_count <- spp_count[,-8]
spp_count <- spp_count[,-8]
spp_count <- spp_count[,-8]
spp_count <- spp_count[,-5]
spp_count <- as.data.frame(spp_count)
names(spp_count) <- c("site","date","subplot","count","enes.obs","oss.obs")
head(spp_count)



#### boxplot trt effect size ---------------------------------------------------------









#### boxplot trt occu prob ---------------------------------------------------------








