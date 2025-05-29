## =================================================
##
## Title: multi-scale-data-format
## Author: Jasmine Williamson
## Date Created: 5/29/2025
##
## Description: Create single matrix with: pre-fire, post-fire, and covariate data
## Use rows at the subplot level and columns at the repeat survey (1-3) level
##
## =================================================


## settings -----------------------------------------------------------------------------------------------

  rm(list=ls())
  setwd("/Users/jasminewilliamson/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
  
  library(unmarked)
  library(ggplot2)
  library(stats)
  library(MASS)
  library(tidyverse)
  library(dplyr)


## load data ----------------------------------------------------------------------------------------------

  site <- read.csv("site.complete.csv")
  subplot <- read.csv("subplot.complete.csv")
  
# run pre-fire-matrices.R
  head(xo) #pre-fire oss
  head(xe) #pre-fire enes
  
# run occu-matrices-updated052025.R
  head(dets.o) #post-fire oss
  head(dets.e) #post-fire enes

## post fire matrix -------------------------------------------------------------------------------------------
  
# add covariates
  











