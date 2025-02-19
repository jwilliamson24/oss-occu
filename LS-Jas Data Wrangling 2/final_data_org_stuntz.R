#### Version of Jay Jones' original code edited by Luke Stuntz to work with JW's 23-24 data
## As little has been changed as possible while still making it work

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

setwd("C:/Users/luke/Desktop/Jas Data Wrangling")

# OSS
xo <- read.csv("intermediate_files/oss_detections_jw.csv")

# ENES
xe <- read.csv("intermediate_files/enes_detections_jw.csv")

# Trt

trt.wide <- read.csv("intermediate_files/treatment_state_jw.csv", na.strings="*")

# stack
trt <- gather(trt.wide, "Year", "HarvestState", 4:5)
trt$StandID <- as.numeric(as.factor(trt$StandNo))
trt$Trt <- as.factor(trt$HarvestState)
trt$YearN <- as.numeric(as.factor(trt$Year))

trt.grp <- trt %>% group_by(StandNo) %>% summarize(Group=min(as.numeric(Trt), na.rm=T)) # 1 = unit was cut; 2 = unit was never cut
trt <- merge(trt, trt.grp)
trt$TrtGrp <- trt$Trt


# ------------------
# 2. merge the data
# ------------------

xo$StandID <- as.numeric(as.factor(xo$StandNo))
xo$YearN <- as.numeric(as.factor(xo$Year))
xo$Owner <- factor(xo$Owner, levels=c("BLM", "ODF", "PB", "WY"))
xo <- xo[!is.na(xo$DW) & !is.na(xo$JulianDate) & !is.na(xo$AirTemp),]

xe$StandID <- as.numeric(as.factor(xe$StandNo))
xe$YearN <- as.numeric(as.factor(xe$Year))
xe$Owner <- factor(xe$Owner, levels=c("BLM", "ODF", "PB", "WY"))
xe <- xe[!is.na(xe$DW) & !is.na(xe$JulianDate) & !is.na(xe$AirTemp),]

trt <- trt[trt$StandNo %in% unique(xo$StandNo),]
trt$StandNo <- as.character(trt$StandNo)
xo <- merge(xo, trt[, c("StandNo", "HarvestState", "TrtGrp", "YearN")])
xe <- merge(xe, trt[, c("StandNo", "HarvestState", "TrtGrp", "YearN")])


sum(xo[,c("V1", "V2", "V3")], na.rm=T) 
sum(xe[,c("V1", "V2", "V3")], na.rm=T) 

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
OWPB1 <- ifelse(cov.stand$Ownership=="PB", 1, 0)
OWBLM1 <- ifelse(cov.stand$Ownership=="BLM", 1, 0)
OWODF1 <- ifelse(cov.stand$Ownership=="ODF", 1, 0)
TFCL1 <- ifelse(cov.stand$TreeFarm=="CL", 1, 0)
TFNC1 <- ifelse(cov.stand$TreeFarm=="NC", 1, 0)


# stand-year level covariates
cov.standyear <- trt %>% group_by(StandID, YearN) %>% summarize(TrtGrp=TrtGrp[1])
Stand2 <- cov.standyear$StandID
Year2 <- cov.standyear$YearN
Year2024 <- matrix(ifelse(cov.standyear$YearN==2, 1, 0), nrow=nyear)
TrtGrp2 <- cov.standyear$TrtGrp
BS2 <- matrix(ifelse(cov.standyear$TrtGrp=="BS", 1, 0), nrow=nyear)
BU2 <- matrix(ifelse(cov.standyear$TrtGrp=="BU", 1, 0), nrow=nyear)
HB2 <- matrix(ifelse(cov.standyear$TrtGrp=="HB", 1, 0), nrow=nyear)
HU2 <- matrix(ifelse(cov.standyear$TrtGrp=="HU", 1, 0), nrow=nyear)


# plot-level covariates
DW3 <- xo$DW
JD3 <- xo$JulianDate
AT3 <- xo$AirTemp
Year3 <- xo$YearN
OWPB3 <- ifelse(xo$Owner=="PB", 1, 0)
OWBLM3 <- ifelse(xo$Owner=="BLM", 1, 0)
OWODF3 <- ifelse(xo$Owner=="ODF", 1, 0)
TFCL3 <- ifelse(xo$TreeFarm=="CL", 1, 0)
TFNC3 <- ifelse(xo$TreeFarm=="NC", 1, 0)
Stand3 <- as.numeric(as.factor(xo$StandID))
Year20243 <- ifelse(xo$YearN==2, 1, 0)
TrtGrp3 <- xo$TrtGrp
BS3 <- ifelse(xo$TrtGrp=="BS", 1, 0)
BU3 <- ifelse(xo$TrtGrp=="BU", 1, 0)
HB3 <- ifelse(xo$TrtGrp=="HB", 1, 0)
HU3 <- ifelse(xo$TrtGrp=="HU", 1, 0)
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
                Stand1=Stand1, OWPB1=OWPB1, OWBLM1=OWBLM1, OWODF1=OWODF1, TFCL1=TFCL1, TFNC1=TFNC1,
                Stand2=Stand2, Year2=Year2, 
                Year2024=Year2024,
                TrtGrp2=TrtGrp2, BS2=BS2, BU2=BU2, HB2=HB2, HU2=HU2,
                DW3=DW3, JD3=JD3, Year3=Year3, Stand3=Stand3, yo=yo, ye=ye, TFCL3=TFCL3, TFNC3=TFNC3, AT3=AT3,
                OWPB3=OWPB3, OWBLM3=OWBLM3, OWODF3=OWODF3, 
                Year20243=Year20243,
                TrtGrp3=TrtGrp3, BS3=BS3, BU3=BU3, HB3=HB3, HU3=HU3)

save(osslist, file="output_files/oss_enes_data_packaged_jw.RData")





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




