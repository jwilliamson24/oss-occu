#### Manipulating 23-24 salamander data to fit Jay Jones model prep
## Last updated February 16th, 2025

#### Setup ####

## Set working directory
setwd("C:/Users/luke/Desktop/Jas Data Wrangling")

## Packages
library(tidyverse)

## Read in original data (taken from JW' GitHub)
df_dwd <- read.csv("input_files/dwd.complete.csv")
df_sals <- read.csv("input_files/sals.complete.csv")
df_site <- read.csv("input_files/site_level_matrix.csv")

#### Setup initial dataframes ####

## Select relevant metadata variables and name them accordingly
df_meta <- select(df_site,
                  Owner = landowner,
                  TreeFarm = tree_farm,
                  StandNo = site_id,
                  Year = year,
                  JulianDate = jul_date,
                  AirTemp = temp)

## Change to celsius
df_meta$AirTemp <- (df_meta$AirTemp - 32) * 5 / 9

## Replicate rows for subplots
df_meta <- df_meta[rep(seq(1, nrow(df_meta)), each = 7),]
rownames(df_meta) <- NULL
df_meta$SubPlot <- rep(seq(1,7), nrow(df_site))

## Create the abundance variables
df_meta$V1 <- NA
df_meta$V2 <- NA
df_meta$V3 <- NA

#### Counting DWD per subplot ####

## Loop through each subplot and get a count of DWD
## This only considers logs! Not stumps
df_meta$DW <- 0

for(i in 1:nrow(df_meta)){
  df_meta$DW[i] <- sum(df_dwd$site_id == df_meta$StandNo[i] &
                         df_dwd$subplot == df_meta$SubPlot[i] &
                         df_dwd$dwd_type == "L")
}

#### Counting salamanders per subplot ####

## Create two dataframes for ENES and OSS
df_enes <- df_meta
df_oss <- df_meta

## Loop through each subplot and get a count of sallies for each pass
for(i in 1:nrow(df_meta)){
  df_temp <- filter(df_sals, site_id == df_meta$StandNo[i], subplot == df_meta$SubPlot[i])
  df_enes$V1[i] <- sum(df_temp$spp == "ENES" & df_temp$pass == 1)
  df_enes$V2[i] <- sum(df_temp$spp == "ENES" & df_temp$pass == 2)
  df_enes$V3[i] <- sum(df_temp$spp == "ENES" & df_temp$pass == 3)
  df_oss$V1[i] <- sum(df_temp$spp == "OSS" & df_temp$pass == 1)
  df_oss$V2[i] <- sum(df_temp$spp == "OSS" & df_temp$pass == 2)
  df_oss$V3[i] <- sum(df_temp$spp == "OSS" & df_temp$pass == 3)
  print(i)
}

#### Replicate the treatment type dataframe ####

## Select columns; treament repeated between years as a thing to fit Jay's format
df_trt <- select(df_site,
                  Owner = landowner,
                  TreeFarm = tree_farm,
                  StandNo = site_id,
                  "2023" = trt,
                  "2024" = trt)

#### Finalize some file organization and save results ####

## Select variables to exactly match
df_enes <- select(df_enes, "Owner", "TreeFarm", "StandNo", "V1", "V2", "V3", 
                  "Year", "DW", "AirTemp", "JulianDate")
df_oss <- select(df_oss, "Owner", "TreeFarm", "StandNo", "V1", "V2", "V3", 
                  "Year", "DW", "AirTemp", "JulianDate")

## Save CSVs
write.csv(df_trt, "intermediate_files/treatment_state_jw.csv", row.names = FALSE)
write.csv(df_enes, "intermediate_files/enes_detections_jw.csv", row.names = FALSE)
write.csv(df_oss, "intermediate_files/oss_detections_jw.csv", row.names = FALSE)


