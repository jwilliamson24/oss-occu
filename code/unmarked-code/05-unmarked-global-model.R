## -------------------------------------------------------------------------------------------------------
##
## 05-unmarked-global-model.R 
##
## Build global model for 2023-2024 data - steps based on Josh Twining suggestions
##
## Jasmine Williamson
## Date Created: 03-18-2025
##

## goals: From Josh T
## Build a global model which contains everything you think that will impact either occupancy or detection
## all variables you can articulate a hypothesis about the impacts of on either psi or p
## Does this converge?
## check for 1. Estimates or SE above or below -5, 2. Boundary estimates (all estimates close to 0), 
## and 3. The easiest - convergence error such as "Hessian is singular"

## insights
## 


## settings -----------------------------------------------------------------------------------------------
    
    rm(list=ls())
    
    library(unmarked)
    library(ggplot2)
    library(stats)
    library(MASS)
    library(tidyverse)
    library(MuMIn)
    library(AICcmodavg)


## load data----------------------------------------------------------------------------------------------

    df_oss <- read.csv("data/occupancy/oss-for-UMF-1.csv")

    
## hypotheses -----------------------------------------------------------------------
    
  # occupancy covariates
    
    # treatment - greater anthro disturbance = lower occu
    # compound disturbance may impact forest floor habitat negatively = lower occu
    # salvage logging considered most severe disturbance = lowest occupancy
    
    # veg cover - highest in oss random forest model
    # more ground veg cover = more refugia and higher soil moisture = higher occu
    
    # canopy cover - high in random forest
    # higher canopy cover = lower temps and higher soil moist in forest floor = higher occu
    
    # soil moisture - med-high in random forest
    # higher moisture = more suitable habitat, less likely to desiccate = higher occu
    
    # fine woody debris cover - med-high in random forest
    # more fwd = higher moisture, more refugia = higher occu
    
    # dwd count - low in random forest
    # but literature and my experience says it can play a role
    # more wood = more refugia, higher moisture, better habitat = higher occupancy
    # i have dwd cover, dwd count, and dwd volume
    
  
  # detection covariates  
    
    # temp - zero in random forest
    # but important in literature for timing of emergence from underground refuge - quadratic relationship
    # temps below freezing = no emergence; emerge when consitently above freezing; hot temps + dry = return underground
    # temp and humidity were highly correllated so i removed humidity
    
    # soil moisture - med in random forest
    # wetter environment = more likely to emerge and risk aboveground movement = higher detection
    
    # days since rain - high in random forest
    # also important in literature for emergence timing
    # wetter environment = more likely to emerge and risk aboveground movement = higher detection
    
    # dwd cover - low in random forest
    # but literature and my experience says it can play a role
    # less dwd = easier to find an animal if it is present, more dwd = harder to find a present animal (needle in a haystack)
    
    # treatment
    # amount of downed wood, veg cover, fwd cover, and soil moisture can all vary with treatment
    # anecdotally i think these things can cause different det probs in different treatments
    
    
    
## Build OSS unmarkedFrameOccu Object -----------------------------------------------------------------------
    
    
    UMF.oss.1 <- unmarkedFrameOccu(
      y = df_oss[, grep("^X", names(df_oss))],  # selects cols starting with X
      siteCovs = df_oss[, c("trt","veg_cov","canopy_cov","soil_moist","fwd_cov","dwd_count")],
      obsCovs = list(
        temp = df_oss[, grep("temp.", names(df_oss))], # selects cols including temp-
        soilmoist = df_oss[, grep("soilmoist.", names(df_oss))],
        rain = df_oss[, grep("rain.", names(df_oss))],
        dwd = df_oss[, grep("dwdcov.", names(df_oss))],
        trt = df_oss[, grep("trt.", names(df_oss))]
      )
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    