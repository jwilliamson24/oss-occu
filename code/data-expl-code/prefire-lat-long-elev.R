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


## load data ----------------------------------------------------------------

  pts.all <- read.csv("data/prefire-shp-attributes/oss_all_units_shp_attribute_table.csv") # lat/long/elev at all unit centroids
  plots13 <- read.csv("data/prefire-shp-attributes/2013-all-plots.csv") # lat/long for all plots all units
  units13 <- read.csv("data/prefire-shp-attributes/2013-units.csv") # utm/elev for all 2013 units
  pts15 <- read.csv("data/prefire-shp-attributes/2015-random-start-pts.csv") # lat/long/elev random start points given for 2015
  pts16 <- read.csv("data/prefire-shp-attributes/2016-random-start-pts.csv") # lat/long/elev random start points 2016
  pts17 <- read.csv("data/prefire-shp-attributes/2017-random-start-pts.csv") # lat/long random start points 2017
  pts18 <- read.csv("data/prefire-shp-attributes/2018-random-start-pts.csv") # lat/long random start points 2018
  pts19 <- read.csv("data/prefire-shp-attributes/2019-random-start-pts.csv") # lat/long random start points 2019
  
  xo <- read.csv("data/occupancy/dets.o.pre.csv") # pre-fire oss
  
  
## format -----------------------------------------------------------------
  
  # 2013: use all subplots lat/long, unit centroid elev
  # 2024: use unit centroid lat/long/elev
  # 2015-2016: use random start point lat/long/elev
  # 2017-2019: use random start point lat/long, unit centroid elev
  
  
# 2013 - done
  pts13 <- subset(plots13, select = c("X", "Y", "Stand__", "Pass")) # subplot points
  colnames(pts13) <- c("lat", "long", "stand", "subplot")
  pts13$year <- 2013
  
  elev13 <- subset(units13, select = c("ELEVATION_", "OFCLWCO__1")) # units elev
  colnames(elev13) <- c("elev", "stand")
  
  pts13 <- merge(pts13, elev13, by = "stand") # merge
  
  
  
# 2014 - done
  # finding sites surveyed in 2014 (listed as none in pts.all)
  units14 <- xo[xo$Year == 2014, ]
  names(units14)[names(units14) == "StandNo"] <- "stand"
  names(units14)[names(units14) == "SubPlot"] <- "subplot"
  units14 <- subset(units14, select = c("stand", "subplot"))
  
  pts <- subset(pts.all, select = c("Lat", "Long", "ELEVATION_", "OFCLWCO__1"))
  colnames(pts) <- c("lat", "long", "elev", "stand")
  
  # subset all pts to just the ones that match stand no from 2014 data
  pts2014 <- merge(pts, units14, by = "stand") 
  pts14$year = 2014
  
  
# 2015 - done
  pts15 <- subset(pts15, select = c("Lat", "Long", "ELEVATION_", "OFCLWCO__1"))
  colnames(pts15) <- c("lat", "long", "elev", "stand")
  pts15$year <- 2015
  
  
  
# 2016 - done
  pts16 <- subset(pts16, select = c("Lat", "Long", "ELEVATION_", "OFCLWCO__1"))
  colnames(pts16) <- c("lat", "long", "elev", "stand")
  pts16$year <- 2016
  
  
  
# 2017 - done
  pts17 <- subset(pts17, select = c("Lat", "Long", "OFCLWCO__1"))
  colnames(pts17) <- c("lat", "long", "stand")
  pts17$year <- 2017
  
  pts17 <- merge(pts17, pts[, c("stand", "elev")], by = "stand", all.x = TRUE)
  
  
  
# 2018 - done
  pts18 <- subset(pts18, select = c("Lat", "Long", "OFCLWCO__1"))
  colnames(pts18) <- c("lat", "long", "stand")
  pts18$year <- 2018
  
  pts18 <- merge(pts18, pts[, c("stand", "elev")], by = "stand", all.x = TRUE)
  
  
  
# 2019 - done
  pts19 <- subset(pts19, select = c("X", "Y", "OFCLWCO__1_min"))
  colnames(pts19) <- c("lat", "long", "stand")
  pts19$year <- 2019

  pts19 <- merge(pts19, pts[, c("stand", "elev")], by = "stand", all.x = TRUE)
  

  
  
## merge -----------------------------------------------------------------

  # merge 2014-2019, unit level data
  pts1419 <- rbind(pts14, pts15, pts16, pts17, pts18, pts19)
  
  # make it subplot level
  pts1419_expanded <- pts1419[rep(1:nrow(pts1419), each = 7), ]
  pts1419_expanded$subplot <- rep(1:7, times = nrow(pts1419))
  
  # merge with 2013, plot level
  geo.data.all.plots <- rbind(pts13, pts1419_expanded)
  

  write.csv(geo.data.all.plots, "data/prefire-shp-attributes/geo-data-all-plots-prefire.csv")
  
  


