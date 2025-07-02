## =================================================
##
## Title: prefire-lat-long-elev
## Author: Jasmine Williamson
## Date Created: 07/02/2025
##
## Description: Make lat, long, elev cols for pre-fire data matrix for multiv model,
## Using extracted lat/long/elev from JJ's shapefiles
##
## =================================================


## settings -----------------------------------------------------------------------------------------------

  #rm(list=ls())


## load data ------------------------------------------------------------------------

  pts.all <- read.csv("data/prefire-shp-attributes/oss_all_units_shp_attribute_table.csv") # lat/long/elev at all unit centroids
  plots13 <- read.csv("data/prefire-shp-attributes/2013-all-plots.csv") # lat/long for all plots all units
  units13 <- read.csv("data/prefire-shp-attributes/2013-units.csv") # utm/elev for all 2013 units
  pts15 <- read.csv("data/prefire-shp-attributes/2015-random-start-pts.csv") # lat/long/elev random start points given for 2015
  pts16 <- read.csv("data/prefire-shp-attributes/2016-random-start-pts.csv") # lat/long/elev random start points 2016
  pts17 <- read.csv("data/prefire-shp-attributes/2017-random-start-pts.csv") # lat/long random start points 2017
  pts18 <- read.csv("data/prefire-shp-attributes/2018-random-start-pts.csv") # lat/long random start points 2018
  pts19 <- read.csv("data/prefire-shp-attributes/2019-random-start-pts.csv") # lat/long random start points 2019
  





















