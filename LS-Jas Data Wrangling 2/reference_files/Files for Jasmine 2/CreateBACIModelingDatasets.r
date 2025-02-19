#
# Project:      Oregon Slender Salamander (and ENES)
# Task:         Create modeling datasets for BACI analysis
#

library(tidyr)
library(dplyr)
library(ggplot2)

rm(list=ls())

# ----------------------
# 1. read in the data
# ----------------------

  dname <- "" # directory pathway

# OSS

  xo13 <- read.csv(paste(dname, "OSS detections 2013 - stacked.csv", sep=""))
  xo14 <- read.csv(paste(dname, "OSS detections 2014 - stacked.csv", sep=""))
  xo15 <- read.csv(paste(dname, "OSS detections 2015 - stacked.csv", sep=""))
  xo16 <- read.csv(paste(dname, "OSS detections 2016 - stacked.csv", sep=""))
  xo17 <- read.csv(paste(dname, "OSS detections 2017 - stacked.csv", sep=""))
  xo18 <- read.csv(paste(dname, "OSS detections 2018 - stacked.csv", sep=""))
  xo19 <- read.csv(paste(dname, "OSS detections 2019 - stacked.csv", sep=""))

## LUKE TWEAK
  xo13$DW <- sample(osslist$DW3, nrow(xo13))
  xo13$JulianDate <- sample(osslist$JD3, nrow(xo13))
  xo13$AirTemp <- sample(osslist$AT3, nrow(xo13))
  xo14$DW <- sample(osslist$DW3, nrow(xo14))
  xo14$JulianDate <- sample(osslist$JD3, nrow(xo14))
  xo14$AirTemp <- sample(osslist$AT3, nrow(xo14))
  xo15$DW <- sample(osslist$DW3, nrow(xo15))
  xo15$JulianDate <- sample(osslist$JD3, nrow(xo15))
  xo15$AirTemp <- sample(osslist$AT3, nrow(xo15))
  xo16$DW <- sample(osslist$DW3, nrow(xo16))
  xo16$JulianDate <- sample(osslist$JD3, nrow(xo16))
  xo16$AirTemp <- sample(osslist$AT3, nrow(xo16))
  xo17$DW <- sample(osslist$DW3, nrow(xo17))
  xo17$JulianDate <- sample(osslist$JD3, nrow(xo17))
  xo17$AirTemp <- sample(osslist$AT3, nrow(xo17))
  xo18$DW <- sample(osslist$DW3, nrow(xo18))
  xo18$JulianDate <- sample(osslist$JD3, nrow(xo18))
  xo18$AirTemp <- sample(osslist$AT3, nrow(xo18))
  xo19$DW <- sample(osslist$DW3, nrow(xo19))
  xo19$JulianDate <- sample(osslist$JD3, nrow(xo19))
  xo19$AirTemp <- sample(osslist$AT3, nrow(xo19))
  xo13[xo13 == "*" & !is.na(xo13)] <- NA
  xo14[xo14 == "*" & !is.na(xo14)] <- NA
  xo15[xo15 == "*" & !is.na(xo15)] <- NA
  xo16[xo16 == "*" & !is.na(xo16)] <- NA
  xo17[xo17 == "*" & !is.na(xo17)] <- NA
  xo18[xo18 == "*" & !is.na(xo18)] <- NA
  xo19[xo19 == "*" & !is.na(xo19)] <- NA
  
    
  xo13$Year <- rep("2013", nrow(xo13))
  xo14$Year <- rep("2014", nrow(xo14))
  xo15$Year <- rep("2015", nrow(xo15))
  xo16$Year <- rep("2016", nrow(xo16))
  xo17$Year <- rep("2017", nrow(xo17))
  xo18$Year <- rep("2018", nrow(xo18))
  xo19$Year <- rep("2019", nrow(xo18))
  
# ENES

  xe13 <- read.csv(paste(dname, "ENES detections 2013 - stacked.csv", sep=""))
  xe14 <- read.csv(paste(dname, "ENES detections 2014 - stacked.csv", sep=""))
  xe15 <- read.csv(paste(dname, "ENES detections 2015 - stacked.csv", sep=""))
  xe16 <- read.csv(paste(dname, "ENES detections 2016 - stacked.csv", sep=""))
  xe17 <- read.csv(paste(dname, "ENES detections 2017 - stacked.csv", sep=""))
  xe18 <- read.csv(paste(dname, "ENES detections 2018 - stacked.csv", sep=""))
  xe19 <- read.csv(paste(dname, "ENES detections 2019 - stacked.csv", sep=""))
  
  ## LUKE TWEAK
  xe13$DW <- sample(osslist$DW3, nrow(xe13))
  xe13$JulianDate <- sample(osslist$JD3, nrow(xe13))
  xe13$AirTemp <- sample(osslist$AT3, nrow(xe13))
  xe14$DW <- sample(osslist$DW3, nrow(xe14))
  xe14$JulianDate <- sample(osslist$JD3, nrow(xe14))
  xe14$AirTemp <- sample(osslist$AT3, nrow(xe14))
  xe15$DW <- sample(osslist$DW3, nrow(xe15))
  xe15$JulianDate <- sample(osslist$JD3, nrow(xe15))
  xe15$AirTemp <- sample(osslist$AT3, nrow(xe15))
  xe16$DW <- sample(osslist$DW3, nrow(xe16))
  xe16$JulianDate <- sample(osslist$JD3, nrow(xe16))
  xe16$AirTemp <- sample(osslist$AT3, nrow(xe16))
  xe17$DW <- sample(osslist$DW3, nrow(xe17))
  xe17$JulianDate <- sample(osslist$JD3, nrow(xe17))
  xe17$AirTemp <- sample(osslist$AT3, nrow(xe17))
  xe18$DW <- sample(osslist$DW3, nrow(xe18))
  xe18$JulianDate <- sample(osslist$JD3, nrow(xe18))
  xe18$AirTemp <- sample(osslist$AT3, nrow(xe18))
  xe19$DW <- sample(osslist$DW3, nrow(xe19))
  xe19$JulianDate <- sample(osslist$JD3, nrow(xe19))
  xe19$AirTemp <- sample(osslist$AT3, nrow(xe19))
  
  xe13[xe13 == "*" & !is.na(xe13)] <- NA
  xe14[xe14 == "*" & !is.na(xe14)] <- NA
  xe15[xe15 == "*" & !is.na(xe15)] <- NA
  xe16[xe16 == "*" & !is.na(xe16)] <- NA
  xe17[xe17 == "*" & !is.na(xe17)] <- NA
  xe18[xe18 == "*" & !is.na(xe18)] <- NA
  xe19[xe19 == "*" & !is.na(xe19)] <- NA
  
  xe13$Year <- rep("2013", nrow(xe13))
  xe14$Year <- rep("2014", nrow(xe14))
  xe15$Year <- rep("2015", nrow(xe15))
  xe16$Year <- rep("2016", nrow(xe16))
  xe17$Year <- rep("2017", nrow(xe17))
  xe18$Year <- rep("2018", nrow(xe18))
  xe19$Year <- rep("2019", nrow(xe18))
  

# Trt

  trt.wide <- read.csv("Harvest state 2013-2019.csv", na.strings="*")
  trt.wide$TreeFarm <- as.factor(ifelse(as.character(trt.wide$TreeFarm)=="SP ", "SP", as.character(trt.wide$TreeFarm)))
  
  # include only those sites that entered in 2016 or earlier
  # (otherwise random effects means may be poorly estimated)
  trt.wide <- trt.wide[!(is.na(trt.wide$X2013) & is.na(trt.wide$X2014) & is.na(trt.wide$X2015) & is.na(trt.wide$X2016)),]
  
  # stack
  trt <- gather(trt.wide, "Year", "HarvestState", 4:10)
  trt$StandID <- as.numeric(as.factor(trt$StandNo))
  trt$Trt <- as.factor(trt$HarvestState)
  trt$YearN <- as.numeric(as.factor(trt$Year))

  trt.grp <- trt %>% group_by(StandNo) %>% summarize(Group=min(as.numeric(Trt), na.rm=T)) # 1 = unit was cut; 2 = unit was never cut
  trt <- merge(trt, trt.grp)
  trt$TrtGrp <- as.factor(ifelse(trt$Group==1 & trt$Trt=="Cut", "PostTrt", 
                       ifelse(trt$Group==1 & trt$Trt=="Timbered", "PreTrt", "Control")))


# ------------------
# 2. merge the data
# ------------------

  xo <- rbind(xo13, xo14, xo15, xo16, xo17, xo18, xo19)
  xo$StandID <- as.numeric(as.factor(xo$StandNo))
  xo$YearN <- as.numeric(as.factor(xo$Year))
  xo$Owner <- factor(xo$Owner, levels=c("BLM", "ODF", "PBTF", "WY"))
  xo <- xo[!is.na(xo$DW) & !is.na(xo$JulianDate) & !is.na(xo$AirTemp),]
  
  ## TWEAK
  xo <- xo[!is.na(xo$V1),]
  xo$V1 <- as.integer(xo$V1)
  xo$V2 <- as.integer(xo$V2)
  xo$V3 <- as.integer(xo$V3)


  xe <- rbind(xe13, xe14, xe15, xe16, xe17, xe18, xe19)
  xe$Owner <- as.factor(ifelse(as.character(xe$Owner)=="PB", "PBTF", as.character(xe$Owner)))
  xe$StandID <- as.numeric(as.factor(xe$StandNo))
  xe$YearN <- as.numeric(as.factor(xe$Year))
  xe <- xe[!is.na(xe$DW) & !is.na(xe$JulianDate) & !is.na(xe$AirTemp),]
  
  ## TWEAK
  xe <- xe[!is.na(xe$V1),]
  xe$V1 <- as.integer(xe$V1)
  xe$V2 <- as.integer(xe$V2)
  xe$V3 <- as.integer(xe$V3)


  trt <- trt[trt$StandNo %in% unique(xo$StandNo),]
  trt$StandNo <- as.character(trt$StandNo)
  xo <- merge(xo, trt[, c("StandNo", "HarvestState", "TrtGrp", "YearN")])
  xe <- merge(xe, trt[, c("StandNo", "HarvestState", "TrtGrp", "YearN")])
  
  
  sum(xo[,c("V1", "V2", "V3")], na.rm=T) # 923
  sum(xe[,c("V1", "V2", "V3")], na.rm=T) # 615
  
  mean(apply(xo[,c("V1", "V2", "V3")], 1, sum, na.rm=T), na.rm=T) # 0.27
  mean(apply(xe[,c("V1", "V2", "V3")], 1, sum, na.rm=T), na.rm=T) # 0.18
  sd(apply(xo[,c("V1", "V2", "V3")], 1, sum, na.rm=T), na.rm=T) # 0.62
  sd(apply(xe[,c("V1", "V2", "V3")], 1, sum, na.rm=T), na.rm=T) # 0.48



# -------------------------------
# 3. create variables for JAGS
# -------------------------------

# indexes

  nstand <- length(unique(xo$StandNo))
  nplot <- length(unique(xo$SubPlot))
  nyear <- length(unique(xo$Year))
  nvisit <- 3
  nall <- nrow(xo)


# stand-level covariates
  cov.stand <- xo %>% group_by(StandID) %>% summarize(Ownership=Owner[1], TreeFarm=TreeFarm[1])
  Stand1 <- cov.stand$StandID
  OWPB1 <- ifelse(cov.stand$Ownership=="PBTF", 1, 0)
  OWBLM1 <- ifelse(cov.stand$Ownership=="BLM", 1, 0)
  OWODF1 <- ifelse(cov.stand$Ownership=="ODF", 1, 0)
  TFCL1 <- ifelse(cov.stand$TreeFarm=="CL", 1, 0)


# stand-year level covariates
  cov.standyear <- trt %>% group_by(StandID, YearN) %>% summarize(TrtGrp=TrtGrp[1])
  cov.standyear$TrtGrp <- as.factor(ifelse(is.na(cov.standyear$TrtGrp), "PreTrt", as.character(cov.standyear$TrtGrp)))
  Stand2 <- cov.standyear$StandID
  Year2 <- cov.standyear$YearN
  Year2014 <- matrix(ifelse(cov.standyear$YearN==2, 1, 0), nrow=nyear)
  Year2015 <- matrix(ifelse(cov.standyear$YearN==3, 1, 0), nrow=nyear)
  Year2016 <- matrix(ifelse(cov.standyear$YearN==4, 1, 0), nrow=nyear)
  Year2017 <- matrix(ifelse(cov.standyear$YearN==5, 1, 0), nrow=nyear)
  Year2018 <- matrix(ifelse(cov.standyear$YearN==6, 1, 0), nrow=nyear)
  Year2019 <- matrix(ifelse(cov.standyear$YearN==7, 1, 0), nrow=nyear)
  TrtGrp2 <- cov.standyear$TrtGrp
  PreTrt2 <- matrix(ifelse(cov.standyear$TrtGrp=="PreTrt", 1, 0), nrow=nyear)
  PostTrt2 <- matrix(ifelse(cov.standyear$TrtGrp=="PostTrt", 1, 0), nrow=nyear)
  

# plot-level covariates
  DW3 <- xo$DW
  JD3 <- xo$JulianDate
  AT3 <- xo$AirTemp
  Trt3 <- ifelse(xo$HarvestState=="Cut", 1, 0)
  Year3 <- xo$YearN
  OWPB3 <- ifelse(xo$Owner=="PBTF", 1, 0)
  OWBLM3 <- ifelse(xo$Owner=="BLM", 1, 0)
  OWODF3 <- ifelse(xo$Owner=="ODF", 1, 0)
  TFCL3 <- ifelse(xo$TreeFarm=="CL", 1, 0)
  Stand3 <- as.numeric(as.factor(xo$StandID))
  Year20143 <- ifelse(xo$YearN==2, 1, 0)
  Year20153 <- ifelse(xo$YearN==3, 1, 0)
  Year20163 <- ifelse(xo$YearN==4, 1, 0)
  Year20173 <- ifelse(xo$YearN==5, 1, 0)
  Year20183 <- ifelse(xo$YearN==6, 1, 0)
  Year20193 <- ifelse(xo$YearN==7, 1, 0)
  TrtGrp3 <- xo$TrtGrp
  PreTrt3 <- ifelse(xo$TrtGrp=="PreTrt", 1, 0)
  PostTrt3 <- ifelse(xo$TrtGrp=="PostTrt", 1, 0)
  Vomax <- xo$Vmax
  Vemax <- xe$Vmax
  yo <- as.matrix(xo[,c("V1", "V2", "V3")])
  yo <- ifelse(yo >= 1, 1, 0)
  ye <- as.matrix(xe[,c("V1", "V2", "V3")])
  ye <- ifelse(ye >= 1, 1, 0)





# ------------------------
# 4. package and save
# ------------------------

osslist <- list(nstand=nstand, nplot=nplot, nyear=nyear, nvisit=nvisit, nall=nall, 
                Stand1=Stand1, OWPB1=OWPB1, OWBLM1=OWBLM1, OWODF1=OWODF1, TFCL1=TFCL1,
                Stand2=Stand2, Year2=Year2, 
                Year2014=Year2014, Year2015=Year2015, Year2016=Year2016, Year2017=Year2017, Year2018=Year2018, Year2019=Year2019,
                TrtGrp2=TrtGrp2, PreTrt2=PreTrt2, PostTrt2=PostTrt2,
                DW3=DW3, JD3=JD3, Trt3=Trt3, Year3=Year3, Stand3=Stand3, yo=yo, ye=ye, TFCL3=TFCL3,AT3=AT3,
                OWPB3=OWPB3, OWBLM3=OWBLM3, OWODF3=OWODF3, 
                Year20143=Year20143, Year20153=Year20153, Year20163=Year20163, Year20173=Year20173, Year20183=Year20183,Year20193=Year20193,
                TrtGrp3=TrtGrp3, PreTrt3=PreTrt3, PostTrt3=PostTrt3)

save(osslist, file="OSS ENES packaged data thru 2019.RData")





# ----------------------
# 5. crude model fit
# ----------------------

x <- xo
x$y <- ifelse(pmax(x$V1, x$V2, x$V3, na.rm=T)>0, 1, 0)

by(Vomax, Trt3, mean) # ctrl=0.28, cut=0.15 => OR=0.45
by(Vemax, Trt3, mean) # ctrl=0.20, cut=0.09 => OR=0.40

# unadjusted means
by(x$y, x$TrtGrp, mean) # Cntrl=0.24, Pre=0.24, Post=0.13



# --------------------------
# 6. miscellaneous checks
# --------------------------

names(osslist)

# number of stands by year

  names(trt)
  trt %>% group_by(Year) %>% summarize(N=length(na.omit(HarvestState))) # 60, 56, 61, 76, 76, 78, 83
  as.data.frame(group_by(trt, Year, TrtGrp) %>% summarize(N=length(na.omit(HarvestState))))
    # 0/60, 0/56, 12/61, 22/76, 33/76, 40/78, 49/83
  dim(trt[trt$YearN=="5",])


# naive occ
  
  # any detections
  xo %>% group_by(StandNo) %>% summarize(AnyOcc = ifelse(max(Vmax, na.rm=T)==0, 0, 1)) %>% 
    summarize(N=n(), TotAnyOcc = sum(AnyOcc))
    
  temp.o <- na.omit(xo %>% group_by(StandNo, Year) %>% summarize(SiteOcc=ifelse(max(Vmax, na.rm=T)==0, 0, 1), 
                                                                 PlotOcc=mean(Vmax, na.rm=T),
                                                                 TrtGrp=TrtGrp[1]))
  temp.o$Group <- ifelse(temp.o$TrtGrp=="Control", "Control", "Trt")
  temp2.o <- temp.o %>% group_by(TrtGrp, Year) %>% summarize(N=length(Year), 
                                                             SiteOcc.m=mean(SiteOcc, na.rm=T),
                                                             PlotOcc.m=mean(PlotOcc, na.rm=T),
                                                             SiteOcc.se=sd(SiteOcc, na.rm=T)/sqrt(N))
  ggplot(temp2.o, aes(as.numeric(Year), SiteOcc.m, color=TrtGrp, shape=TrtGrp)) + geom_point() + geom_line() +
    ylim(c(0,1)) + theme_bw()
  ggplot(temp2.o, aes(as.numeric(Year), PlotOcc.m, color=TrtGrp, shape=TrtGrp)) + geom_point() + geom_line() +
    ylim(c(0,1)) + theme_bw() + xlab("Year") + ylab("Mean naive plot occupancy") + ggtitle("OSS")


  xe %>% group_by(StandNo) %>% summarize(AnyOcc = ifelse(max(Vmax, na.rm=T)==0, 0, 1)) %>% 
    summarize(N=n(), TotAnyOcc = sum(AnyOcc))
  
  xe %>% group_by(StandNo) %>% summarize(AnyOcc = ifelse(max(Vmax, na.rm=T)==0, 0, 1)) %>%
    filter(AnyOcc==0)
  xo %>% group_by(StandNo) %>% summarize(AnyOcc = ifelse(max(Vmax, na.rm=T)==0, 0, 1)) %>%
    filter(AnyOcc==0)
  
  temp.e <- na.omit(xe %>% group_by(StandNo, Year) %>% summarize(SiteOcc=ifelse(max(Vmax, na.rm=T)==0, 0, 1), 
                                                                 PlotOcc=mean(Vmax, na.rm=T),
                                                                 TrtGrp=TrtGrp[1]))
  temp2.e <- temp.e %>% group_by(TrtGrp, Year) %>% summarize(N=length(Year), 
                                                             SiteOcc.m=mean(SiteOcc, na.rm=T),
                                                             PlotOcc.m=mean(PlotOcc, na.rm=T),
                                                             SiteOcc.se=sd(SiteOcc, na.rm=T)/sqrt(N))
  ggplot(temp2.e, aes(as.numeric(Year), SiteOcc.m, color=TrtGrp, shape=TrtGrp)) + geom_point() + geom_line() +
    ylim(c(0,1)) + theme_bw()
  ggplot(temp2.e, aes(as.numeric(Year), PlotOcc.m, color=TrtGrp, shape=TrtGrp)) + geom_point() + geom_line() +
    ylim(c(0,1)) + theme_bw() + xlab("Year") + ylab("Mean naive plot occupancy") + ggtitle("ENES")
  

  tmp <- full_join(
    xe %>% rename(v1e = V1, v2e=V2, v3e=V3, vmaxe=Vmax),
    xo %>% rename(v1o = V1, v2o=V2, v3o=V3, vmaxo=Vmax)
  ) %>%
    mutate(Vmax = pmax(ifelse(vmaxe==0, 0, 1), ifelse(vmaxo==0, 0, 1)))
  mean(tmp$Vmax) # 0.29
  mean(ifelse(tmp$vmaxe==0, 0, 1)) # 0.15
  mean(ifelse(tmp$vmaxo==0, 0, 1)) # 0.21




