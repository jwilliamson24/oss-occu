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
  # plots13 <- read.csv("data/prefire-shp-attributes/2013-all-plots.csv") # lat/long at plot level (missing some units surveyed)
  pts13 <- read.csv("data/prefire-shp-attributes/2013-random-start-pts.csv") # lat/long/elev random start points 2013 
  pts15 <- read.csv("data/prefire-shp-attributes/2015-random-start-pts.csv") # lat/long/elev random start points 2015
  pts16 <- read.csv("data/prefire-shp-attributes/2016-random-start-pts.csv") # lat/long/elev random start points 2016
  pts17 <- read.csv("data/prefire-shp-attributes/2017-random-start-pts.csv") # lat/long random start points 2017
  pts18 <- read.csv("data/prefire-shp-attributes/2018-random-start-pts.csv") # lat/long random start points 2018
  pts19 <- read.csv("data/prefire-shp-attributes/2019-random-start-pts.csv") # lat/long random start points 2019
  
  xo <- read.csv("data/occupancy/dets.o.pre.csv") # pre-fire oss
  

  
## format each year  --------------------------------------------------------
  
  # 2013: use random start point lat/long/elev
  # 2024: use unit centroid lat/long/elev
  # 2015-2016: use random start point lat/long/elev
  # 2017-2019: use random start point lat/long, unit centroid elev
  
  
# subset pts.all for colname consistency
  pts.all.sub <- subset(pts.all, select = c("Lat", "Long", "ELEVATION_", "OFCLWCO__1"))
  colnames(pts.all.sub) <- c("lat", "long", "elev", "stand")
  
  
# 2013 - missing some sites, also some elev data
  pts13 <- subset(pts13, select = c("X", "Y", "ELEVATION_", "OFCLWCO__1"))
  colnames(pts13) <- c("long", "lat", "elev", "stand")
  
  # delete rows that are completely zeros/blank
  pts13 <- pts13[!is.na(pts13$stand), ]
  
  pts13$year <- 2013

  
# 2014 - completely done
  # finding sites surveyed in 2014 (listed as none in pts.all)
  units14 <- xo[xo$Year == 2014, ]
  names(units14)[names(units14) == "StandNo"] <- "stand"
  # names(units14)[names(units14) == "SubPlot"] <- "subplot"
  # units14 <- subset(units14, select = c("stand", "subplot"))
  units14 <- unique(units14["stand"])
  
  # subset all pts to just the ones that match stand no from 2014 data
  pts14 <- merge(pts.all.sub, units14, by = "stand") 
  pts14$year = 2014
  
  
# 2015 - missing some elevation, remove extra site rows
  pts15 <- subset(pts15, select = c("Lat", "Long", "ELEVATION_", "OFCLWCO__1"))
  colnames(pts15) <- c("lat", "long", "elev", "stand")
  
  # delete rows that are completely zeros/blank
  pts15 <- pts15[!apply(pts15 == 0 | pts15 == "", 1, all), ]
  
  pts15$year <- 2015
  
  
# 2016 - missing some lat/long and elev, remove extra site rows
  pts16 <- subset(pts16, select = c("Lat", "Long", "ELEVATION_", "OFCLWCO__1"))
  colnames(pts16) <- c("lat", "long", "elev", "stand")
  pts16$year <- 2016
  
  
# 2017 - done, remove extra site rows
  pts17 <- subset(pts17, select = c("Lat", "Long", "OFCLWCO__1"))
  colnames(pts17) <- c("lat", "long", "stand")
  pts17$year <- 2017
  
  # add centroid elev
  pts17 <- merge(pts17, pts.all.sub[, c("stand", "elev")], by = "stand", all.x = TRUE)
  
  
# 2018 - done, remove extra site rows
  pts18 <- subset(pts18, select = c("Lat", "Long", "OFCLWCO__1"))
  colnames(pts18) <- c("lat", "long", "stand")
  pts18$year <- 2018
  
  # add centroid elev
  pts18 <- merge(pts18, pts.all.sub[, c("stand", "elev")], by = "stand", all.x = TRUE)
  
  
# 2019 - completely done
  pts19 <- subset(pts19, select = c("X", "Y", "OFCLWCO__1_min"))
  colnames(pts19) <- c("long", "lat", "stand")
  pts19$year <- 2019

  # add centroid elev
  pts19 <- merge(pts19, pts.all.sub[, c("stand", "elev")], by = "stand", all.x = TRUE)
  
  
# the rest of the to-do's listed will be done in the merge code below:
  
  
## merge, align all years -------------------------------------------------
  
# combine 2013-2019, unit level data
  pts1319 <- rbind(pts13, pts14, pts15, pts16, pts17, pts18, pts19)
  
  
# merge pts1319 with xo
  # goal: align pts1319 with xo so that units actually surveyed in xo are in pts1319
  
  pts1319$stand <- as.character(pts1319$stand)
  pts1319$year <- as.integer(pts1319$year)
  
  xo$StandNo <- as.character(xo$StandNo)
  xo$Year <- as.integer(xo$Year)
  
  # unique survey stand/year
  xo_keys <- unique(xo[, c("StandNo", "Year")])
  colnames(xo_keys) <- c("stand", "year")  # match pts1319
  
  # merge
  pts_aligned <- merge(xo_keys, pts1319, by = c("stand", "year"), all.x = TRUE)
  
  
# add missing data to pts_aligned from pts.all.sub
  
  str(pts_aligned)
  str(pts.all.sub)
  
  # add in reference info
  pts_aligned <- merge(
    pts_aligned, 
    pts.all.sub[, c("stand", "elev", "lat", "long")], 
    by = "stand", 
    all.x = TRUE, 
    suffixes = c("", ".ref")
  )
  
  # fill in if missing
  
  # elev (empty rows are NA and 0)
  fill_in <- is.na(pts_aligned$elev) | pts_aligned$elev == 0
  pts_aligned$elev[fill_in] <- pts_aligned$elev.ref[fill_in]
  
  # lat long (empty rows are NA)
  pts_aligned$lat[is.na(pts_aligned$lat)]   <- pts_aligned$lat.ref[is.na(pts_aligned$lat)]
  pts_aligned$long[is.na(pts_aligned$long)] <- pts_aligned$long.ref[is.na(pts_aligned$long)]
  
  anyNA(pts_aligned) # check for NA
  
  # drop ref cols
  pts_aligned <- pts_aligned[, !grepl(".ref$", names(pts_aligned))]
  
  # check that pts has same stands as xo
  tapply(pts_aligned$stand, pts_aligned$year, function(x) length(unique(x)))
  tapply(xo$StandNo, xo$Year, function(x) length(unique(x)))
  setequal(
    unique(pts_aligned[, c("stand", "year")]),
    unique(xo[, c("StandNo", "Year")] |> setNames(c("stand", "year")))
  )
  
  
# add subplots from xo
  
  # extract unique stand, year, plot combos from xo
  xo_subplots <- unique(xo[, c("StandNo", "Year", "SubPlot")])
  colnames(xo_subplots) <- c("stand", "year", "subplot")  # match pts_aligned
  
  # merge those into pts_aligned to get subplot level df
  pts_full <- merge(xo_subplots, pts_aligned, by = c("stand", "year"), all.x = TRUE)
  
  
  write.csv(pts_full, "data/prefire-shp-attributes/geo-data-all-plots-prefire.csv")
  
  


