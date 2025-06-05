
# Multi-scale occupancy data formatting
#setwd('C:/Users/twininjo/Documents/R/Salamanders_multiscale_occ')


## load data -------------------------------------------------------------------

  enes <- read.csv("data/occupancy/enes.prepost.multiscale.occu.csv") 
  oss <- read.csv("data/occupancy/oss.prepost.multiscale.occu.csv")
  head(enes)
  head(oss)
  

########################## data formatting #####################################

# 3D and 4D arrays
# same for ENES and OSS
  
# split into plots, sites, and years

unique(enes$site_id)
unique(enes$year)
plotnames=unique(enes$subplot)
sites=unique(enes$site_id)
years=unique(enes$year)
n.sites=length(sites)
n.years=length(years)

# how many plots at each site
W=rep(NA, n.sites)
site.plots=list(n.sites)
  for (i in 1:n.sites){
  W[i]=length(enes$subplot[enes$site_id==sites[i]])
  site.plots[[i]]=unique(enes$subplot[enes$site_id==sites[i]])
  }

# how many sites in each year
J=rep(NA, n.years)
year.sites= list(9)
for (t in 1:n.years){
  J[t] = length(enes$site_id[enes$year==years[t]])
  year.sites[[t]]=unique(enes$site_id[enes$year==years[t]])
}

# max number of sites in any year
maxJ=max(J)
# max number of plots at any site
maxW=max(W)
# total number of sites
totalJ=sum(J)
# total number of plots
totalW=sum(W)
# occasions per plot
K=3


# ENES detection data ------------------------------------------------------------

# pull out the detection/non-detection data
y <- enes[,(6:8)]
sum(y, na.rm=TRUE)

#Structure like this if you aren't going to index over year in your model and just want to use a stacked data approach
#3D matrix (plot, occ, site)
y.3D =array(0,dim=c(maxW,K,n.sites)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.site=which(sites==enes$site_id[i]) #get site for this row
  this.plot=which(site.plots[[this.site]]==enes$subplot[i]) #get plot for this row
  y.3D[this.plot,1:3,this.site]=as.numeric(enes[i,6:8]) #force numeric
}

str(y.3D)
sum(y.3D, na.rm=TRUE)

#Structure like this if you are going to index over year in your model (this would be optimal)
# 4D matrix (plot, occ, site, year)
y.4D =array(0,dim=c(maxW,K,maxJ, n.years)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.year=which(years==enes$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
  this.plot=which(site.plots[[this.site]]==enes$subplot[i]) #get plot for this row
  y.4D[this.plot,1:3,this.site,this.year]=as.numeric(enes[i,6:8]) #force numeric
}

str(y.4D)
sum(y.4D, na.rm=TRUE)

enes.4D <- aperm(y.4D, c(3, 1, 2, 4))  # reorder to [sites, subplots, surveys, years] to match model
str(enes.4D)

# understanding the arrays
str(y.4D)
y.4D[2,3,100,8] # detection value for plot 2, pass 3, site 100, year 8
y.4D[,,100,8] # detection data for site 100 in year 8


saveRDS(enes.4D, "data/occupancy/enes.4D.rds")




# next need to format your covariate data e.g., treatment ready to fed to model!
# Can you do this using above code as example?



# OSS detection data -------------------------------------------------------------

# pull out the detection/non-detection data
y2 <- oss[,(6:8)]
sum(y, na.rm=TRUE)

#Structure like this if you aren't going to index over year in your model and just want to use a stacked data approach
#3D matrix (plot, occ, site)
y2.3D =array(0,dim=c(maxW,K,n.sites)) #new data 
for(i in 1:nrow(oss)){ #loop through each row
  this.site=which(sites==oss$site_id[i]) #get site for this row
  this.plot=which(site.plots[[this.site]]==oss$subplot[i]) #get plot for this row
  y2.3D[this.plot,1:3,this.site]=as.numeric(oss[i,6:8]) #force numeric
}

str(y2.3D)
sum(y2.3D, na.rm=TRUE)

#Structure like this if you are going to index over year in your model (this would be optimal)
# 4D matrix (plot, occ, site, year)
y2.4D =array(0,dim=c(maxW,K,maxJ, n.years)) #new data 
for(i in 1:nrow(oss)){ #loop through each row
  this.year=which(years==oss$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==oss$site_id[i]) #get site for this row
  this.plot=which(site.plots[[this.site]]==oss$subplot[i]) #get plot for this row
  y2.4D[this.plot,1:3,this.site,this.year]=as.numeric(oss[i,6:8]) #force numeric
}


sum(y2.4D, na.rm=TRUE)


# understanding the arrays
str(y2.4D)
y2.4D[2,3,100,8] # detection value for plot 2, pass 3, site 100, year 8
y2.4D[,,100,8] # detection data for site 100 in year 8




# covariate data -----------------------------------------------------------------

# pull out covariate data
y2 <- enes[,c(4,9:11)]



# jul date

date.4D =array(0,dim=c(maxW,K,maxJ, n.years)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.year=which(years==enes$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
  this.plot=which(site.plots[[this.site]]==enes$subplot[i]) #get plot for this row
  date.4D[this.plot,1:3,this.site,this.year]=as.numeric(y2[i,2]) #force numeric
}

str(date.4D)
sum(date.4D, na.rm=TRUE)
date.4D[,,c(1:10),9] # first ten sites in year 9
date.4D[,,c(1:10),3]



# temp 

temp.4D =array(0,dim=c(maxW,K,maxJ, n.years)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.year=which(years==enes$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
  this.plot=which(site.plots[[this.site]]==enes$subplot[i]) #get plot for this row
  temp.4D[this.plot,1:3,this.site,this.year]=as.numeric(y2[i,3]) #force numeric
}

str(temp.4D)
sum(temp.4D, na.rm=TRUE)
temp.4D[,,c(1:10),9] # first ten sites in year 9
temp.4D[,,c(1:10),3]

# each replicate has identical temp for most of the data
# some of the temps are identical for all plots


# DW

DW.4D =array(0,dim=c(maxW,K,maxJ, n.years)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.year=which(years==enes$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
  this.plot=which(site.plots[[this.site]]==enes$subplot[i]) #get plot for this row
  DW.4D[this.plot,1:3,this.site,this.year]=as.numeric(y2[i,4]) #force numeric
}

str(DW.4D)
sum(DW.4D, na.rm=TRUE)
DW.4D[,,c(1:10),9] # first ten sites in year 9
DW.4D[,,c(1:10),3]



# treatment --------------------------------------------------------------------

# Make each treatment a dummy covariate
table(enes$trt)

#BU
enes$BU <- NA
enes$BU[enes$trt=="BU"]<- 1
enes$BU[enes$trt!="BU"]<- 0

# format each treatment into sites and years
BU.new =array(0,dim=c(maxJ,n.years)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.year=which(years==enes$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
  BU.new[this.site,this.year]=as.numeric(enes[i,12]) #force numeric
}

#BS
enes$BS <- NA
enes$BS[enes$trt=="BS"]<- 1
enes$BS[enes$trt!="BS"]<- 0

# format each treatment into sites and years
BS.new =array(0,dim=c(maxJ,n.years)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.year=which(years==enes$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
  BS.new[this.site,this.year]=as.numeric(enes[i,13]) #force numeric
}


#HB
enes$HB <- NA
enes$HB[enes$trt=="HB"]<- 1
enes$HB[enes$trt!="HB"]<- 0

# format each treatment into sites and years
HB.new =array(0,dim=c(maxJ,n.years)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.year=which(years==enes$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
  HB.new[this.site,this.year]=as.numeric(enes[i,14]) #force numeric
}


#HU
enes$HU <- NA
enes$HU[enes$trt=="HU"]<- 1
enes$HU[enes$trt!="HU"]<- 0

# format each treatment into sites and years
HU.new =array(0,dim=c(maxJ,n.years)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.year=which(years==enes$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
  HU.new[this.site,this.year]=as.numeric(enes[i,15]) #force numeric
}


#UU
enes$UU <- NA
enes$UU[enes$trt=="UU"]<- 1
enes$UU[enes$trt!="UU"]<- 0

# format each treatment into sites and years
UU.new =array(0,dim=c(maxJ,n.years)) #new data 
for(i in 1:nrow(enes)){ #loop through each row
  this.year=which(years==enes$year[i]) #get year for this row
  this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
  UU.new[this.site,this.year]=as.numeric(enes[i,16]) #force numeric
}


write.csv(BU.new, "data/occupancy/BU.new.csv")
write.csv(BS.new, "data/occupancy/BS.new.csv")
write.csv(HU.new, "data/occupancy/HU.new.csv")
write.csv(HB.new, "data/occupancy/HB.new.csv")

setwd("data/occupancy")
zip("trts.new.zip", c("BU.new.csv", "BS.new.csv", "HU.new.csv", "HB.new.csv"))
setwd("../..") 



