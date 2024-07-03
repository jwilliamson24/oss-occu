##
## 04-repeat-occu-figures.R 
##
## New figures using the bayesian occupancy code from 2023 
##
## Jasmine Williamson
## Date Created: 07-03-2024
##
#### settings ---------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

library(ggplot2)
library(unmarked)
library(RColorBrewer)
library(tidyverse)
library(ggpattern)
library(dplyr)
library(ggthemes)


#### load data --------------------------------------------------------------------------------------------

site <- read.csv("site.complete.csv")
dwd <- read.csv("dwd.complete.csv")
subplot <- read.csv("subplot.complete.csv")
sals <- read.csv("sals.complete.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", subplot="factor", recap="factor",
                                pass="factor", spp="factor", cover_obj="factor", 
                                substrate="factor", age_class="factor"))

sals_2023 <- read.csv("sals.2023.csv")
sals_2024 <- read.csv("sals.2024.csv")

#### format data -----------------------------------------------------------------------------------------

# add detection col
sals$detect <- 1



#### boxplot trt effect size ---------------------------------------------------------









#### boxplot trt occu prob ---------------------------------------------------------








