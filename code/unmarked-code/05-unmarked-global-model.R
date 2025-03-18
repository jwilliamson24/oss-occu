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
## (check for 1. Estimates or SE above or below -5, 2. Boundary estimates (all estimates close to 0), 
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

    df_oss <- read.csv("data/occupancy/oss_forUMF_unscaled.csv")

    
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
    
    # days since rain - high in random forest
    # also important in literature for emergence timing
    # wetter environment = more likely to emerge and risk aboveground movement = higher detection
    
    # dwd cover - low in random forest
    # but literature and my experience says it can play a role
    # less dwd = easier to find an animal if it is present, more dwd = harder to find a present animal (needle in a haystack)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    