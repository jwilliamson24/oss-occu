## -------------------------------------------------------------------------------------------------------
##
## Script name: Unmarked_occu_TWS_2023.R 
##
## Purpose of script: Occupancy Analysis using package Unmarked for TWS 2023 Louisville Presentation
##
## Jasmine Williamson
## Date Created: 2023-11-02
##
## 


# where i left off: 7/4/24
# i figured out how to format the detection covariated from long format into wide format.
# the code below works for soil moisture and weather
# i also created a site covs data frame that includes many of the covs i will want
# 
# what i still need to do:
# i want to create another detection df for downed wood count per subplot
# i want to add more covs to the site data frame



## settings -----------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

library(unmarked)
library(ggplot2)
library(stats)
library(MASS)
library(tidyverse)

## load data----------------------------------------------------------------------------------------------

dwd <- read.csv("dwd.complete.csv")
site <- read.csv("site.complete.csv")
df <- read.csv("subplot.complete.csv")
sals <- read.csv("sals.complete.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", subplot="factor", recap="factor",
                                pass="factor", spp="factor", cover_obj="factor", 
                                substrate="factor", age_class="factor"))



## format and scale: subplot/det covs from long to wide dataframes ----------------------------------------

# soil moisture 

#subset df with only cols i need (cant use site id here bc its a character)
df <- subset(subplot[,c('subplot','soil_moist_avg')])

#change to long format
df_long <- df %>%
  pivot_longer(cols = -subplot, names_to = "variable", values_to = "value")
#add site id back in
df_long <- cbind(df_long, subplot$site_id)

# Reshape the data back to wide format, flipping the dimensions
df_wide <- df_long %>%
  pivot_wider(names_from = subplot, values_from = value)

soil_moist_wide <- df_wide[,-1]
soil_moist_wide <- as.data.frame(soil_moist_wide)
rownames(soil_moist_wide) <- soil_moist_wide[,1]
soil_moist <- soil_moist_wide[,-1]

#scale
scaled_soilmoist <- as.data.frame(scale(soilmoist))



# weather 

#subset df with only cols i need (cant use site id here bc its a character)
df <- subset(subplot[,c('subplot','weather')])

#change to long format
df_long <- df %>%
  pivot_longer(cols = -subplot, names_to = "variable", values_to = "value")
#add site id back in
df_long <- cbind(df_long, subplot$site_id)

# Reshape the data back to wide format, flipping the dimensions
df_wide <- df_long %>%
  pivot_wider(names_from = subplot, values_from = value)

weather_wide <- df_wide[,-1]
weather_wide <- as.data.frame(weather_wide)
rownames(weather_wide) <- weather_wide[,1]
weather <- weather_wide[,-1]



# dwd

#i want a df with the avg number of pieces of downed wood in each subplot




## Format site/occu cov ----------------------------------------------------------------------------------

site_new <- subset(site[,c('site_id','trt','date_mdy','elev','temp','hum')])






# all the code below this is from the 2023 document. the code above this has been updated
# as of 7/4/2024, with the goal of repeating this unmarked code with all data

## Format Data  ------------------------------------------------------------------------------------------

# Detection Covariates - sites rows, subplots columns
hum <- read.csv("detcov_humidity.csv", row.names = 1)
temp <- read.csv("detcov_temp.csv", row.names = 1 )
weather <- read.csv("detcov_weather.csv", row.names = 1 )
date <- read.csv("detcov_juliandate.csv", row.names = 1 )
soilmoist <- read.csv("detcov_soilmoist.csv", row.names = 1 )

# Scale Detection Covariates
scaled_hum <- as.data.frame(scale(hum))
scaled_temp <- as.data.frame(scale(temp))
scaled_date <- as.data.frame(scale(date))
scaled_soilmoist <- as.data.frame(scale(soilmoist))

# Occupancy Covariates
sitecovs <- read.csv("sitecovs.csv")
# Reorder treatments
sitecovs$treatment <- factor(sitecovs$treatment, 
                             levels = c("UU", "BU", "HB", "HU", "BS"))
# Scale Occupancy Covariates
sitecovs$site <- as.character(sitecovs$site)
# Identify numeric sitecovs
numeric_sitecovs <- sitecovs[sapply(sitecovs, is.numeric)]
# Scale the numeric sitecovs
scaled_numeric_sitecovs <- as.data.frame(scale(numeric_sitecovs))
# Combine the scaled numeric sitecovs with non-numeric sitecovs
scaled_sitecovs <- cbind(scaled_numeric_sitecovs, sitecovs[setdiff(names(sitecovs), names(numeric_sitecovs))])


## Species data
oss.dat <- read.csv("OSS_occ_7.csv")
oss.dets <- oss.dat[,-1]
enes.dat <- read.csv("ENES_occ_7.csv")
enes.dets <- enes.dat[,-1]


## OSS Top Model Exploration-------------------------------------------------------------------------------
# Build OSS unmarkedFrameOccu Object
UMF.oss.scaled <- unmarkedFrameOccu(
  y = oss.dets, 
  obsCovs = list(humidity = scaled_hum, temp = scaled_temp, weather = weather, date = scaled_date, 
                 soilmoist = scaled_soilmoist), 
  siteCovs = scaled_sitecovs)

# Run four models to determine Psi and P covariates
# 1: psi(.) p(.)
# m1 <- occu(~1 ~1, data=UMF.oss.scaled)
#AIC: 373.7835

# 2: psi(treatment) p(.)
# m2 <- occu(~1 ~treatment, data=UMF.oss.scaled)
#AIC: 376.8198 

# 3: psi(.) p(x) , x = date, treatment, or soil moisture                *************************
# m3 <- occu(~temp ~1, data=UMF.oss.scaled)
#AIC: 373.4518

# 4: psi(treatment) p(x) , x = date, treatment, or soil moisture
m4 <- occu(~temp ~treatment, data=UMF.oss.scaled)
#AIC: 376.5377

## Extract treatment occupancy predictions and plot --------------------------------------------------------
# Extract psi predictions from model 4
preds4 <- predict(m4, type="state")
sites_trt <- sitecovs[, c("site","treatment")]
site_preds4 <- cbind(sites_trt, preds4)


# Barplot of predictions for each treatment
ggplot(site_preds4, aes(x = treatment, y = Predicted, ymin = lower, ymax = upper, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.25, position = position_dodge(width = 0.7), linewidth = 1) +
  labs(title = "OSS: Treatment vs. Predicted Occupancy",
       x = "Treatment", y = "Predicted Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
ggsave("unmarked_occu_barplot_oss.png", 
       path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/figures/Unmarked_occu_TWS_2023")


# Boxplot of preds with confidence intervals
ggplot(site_preds4, aes(x = treatment, y = Predicted, fill = treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.25, position = position_dodge(width = 0.75), size = 1) +
  labs(title = "OSS: Treatment vs. Predicted Occupancy",
       x = "Treatment", y = "Predicted Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
ggsave("unmarked_occu_pt_est_oss.png", 
      path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/figures/Unmarked_occu_TWS_2023")


## ENES Top Model Exploration-------------------------------------------------------------------------------
# Build ENES unmarkedFrameOccu Object
UMF.enes.scaled <- unmarkedFrameOccu(
  y = enes.dets, 
  obsCovs = list(humidity = scaled_hum, temp = scaled_temp, weather = weather, date = scaled_date, 
                 soilmoist = scaled_soilmoist), 
  siteCovs = scaled_sitecovs)
summary(UMF.enes.scaled)

# Run four models to determine Psi and P covariates
# 1: psi(.) p(.)
#m5 <- occu(~1 ~1, data=UMF.enes.scaled)
#AIC: 315.7876

# 2: psi(treatment) p(.)
#m6 <- occu(~1 ~treatment, data=UMF.enes.scaled)
#AIC: 304.1865

# 3: psi(.) p(x) , x = date, treatment, or soil moisture
#m7 <- occu(~temp ~1, data=UMF.enes.scaled)
#AIC: 314.149

# 4: psi(treatment) p(x) , x = date, treatment, or soil moisture      *************************
m8 <- occu(~temp ~treatment, data=UMF.enes.scaled)
#AIC: 303.8532



## Extract treatment occupancy predictions and plot ---------------------------------------------------------
# Extract psi predictions from model 8
preds8 <- predict(m8, type="state")
sites_trt <- sitecovs[, c("site","treatment")]
site_preds8 <- cbind(sites_trt, preds8)


# Barplot of predictions for each treatment
ggplot(site_preds8, aes(x = treatment, y = Predicted, ymin = lower, ymax = upper, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.25, position = position_dodge(width = 0.7), linewidth = 1) +
  labs(title = "ENES: Treatment vs. Predicted Occupancy",
       x = "Treatment", y = "Predicted Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
ggsave("unmarked_occu_barplot_enes.png", 
       path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/figures/Unmarked_occu_TWS_2023")


# Boxplot of preds with confidence intervals
ggplot(site_preds8, aes(x = treatment, y = Predicted, fill = treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.25, position = position_dodge(width = 0.75), size = 1) +
  labs(title = "ENES: Treatment vs. Predicted Occupancy",
       x = "Treatment", y = "Predicted Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
ggsave("unmarked_occu_pt_est_enes.png", 
       path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/figures/Unmarked_occu_TWS_2023")

