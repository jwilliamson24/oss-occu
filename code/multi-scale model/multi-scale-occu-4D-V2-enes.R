## =================================================
##
## Title: multi-scale-occu-4D-V2-enes
## Author: Jasmine Williamson + Josh Twining
## Date Created: 06/27/2025
##
## Description: Multi-scale occupancy model for all data, for ENES
## JW edits on model from JT
##
## =================================================

## Notes
# added lat/long/elev, removed mgmt
# current version 250,000 iter


# Load packages
  library(nimble)
  library(coda)

# JT data path
  #setwd('C:/Users/twininjo/Documents/R/Salamanders_multiscale_occ')
  #enes <- read.csv("enes.prepost.multiscale.occu.csv") 
  #oss <- read.csv("oss.prepost.multiscale.occu.csv")

# JW data path  
  enes <- read.csv("data/occupancy/enes.prepost.multiscale.occu.csv") 
  oss <- read.csv("data/occupancy/oss.prepost.multiscale.occu.csv")

# choose enes or oss 
  dat = oss
  
# Data Formatting to 3D and 4D ------------------------------------------------------

  # formatting the data in script to see if it helps spot issue 
  unique(dat$site_id)
  unique(dat$year)
  plotnames=sort(unique(dat$subplot))
  sites=sort(unique(dat$site_id))
  years=sort(unique(dat$year))
  n.sites=length(sites)
  n.years=length(years)
  table(dat$year)
  sum(table(dat$year))
  # make year formatting in chronological order!!!
  ######## RERUN after check date order and site numbers (max of 82).   # has this been resolved?


  # how many plots at each site
  J=rep(NA, n.sites)
  site.plots=list(n.sites)
  for (i in 1:n.sites){
    J[i]=length(dat$subplot[dat$site_id==sites[i]])
    site.plots[[i]]=unique(dat$subplot[dat$site_id==sites[i]])
  }
  
  # how many sites in each year
  I=rep(NA, n.years)
  year.sites= list(9)
  for (t in 1:n.years){
    I[t] = length(sort(unique(dat$site_id[dat$year==years[t]])))
    year.sites[[t]]=unique(dat$site_id[dat$year==years[t]])
  }
  
  # max number of sites in any year
  maxI=max(I)
  # max number of plots at any site
  maxJ=max(J)
  # total number of sites
  totalI=sum(I)
  # total number of plots
  totalJ=sum(J)
  # occasions per plot
  K=3
  
  
# ENES detection data ------------------------------------------------------------
  
  # pull out the detection/non-detection data
  y <- enes[,(6:8)]
  sum(y, na.rm=TRUE)
  

  # 4D matrix (plot, occ, site, year)
  y.4D =array(NA,dim=c(maxJ,K,maxI, n.years)) #new data 
  for(i in 1:nrow(enes)){ #loop through each row
    this.year=which(years==enes$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
    this.plot=which(site.plots[[this.site]]==enes$subplot[i]) #get plot for this row
    y.4D[this.plot,1:3,this.site,this.year]=as.numeric(enes[i,6:8]) #force numeric
  }
  
  str(y.4D)
  sum(y.4D, na.rm=TRUE)
  sum(y.4D == 0, na.rm=TRUE)
  

# OSS detection data ------------------------------------------------------------
  
  # pull out the detection/non-detection data
  y <- oss[,(6:8)]
  sum(y, na.rm=TRUE)
  
  
  # 4D matrix (plot, occ, site, year)
  y.4D =array(NA,dim=c(maxJ,K,maxI, n.years)) #new data 
  for(i in 1:nrow(oss)){ #loop through each row
    this.year=which(years==oss$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==oss$site_id[i]) #get site for this row
    this.plot=which(site.plots[[this.site]]==oss$subplot[i]) #get plot for this row
    y.4D[this.plot,1:3,this.site,this.year]=as.numeric(oss[i,6:8]) #force numeric
  }
  
  str(y.4D)
  sum(y.4D, na.rm=TRUE)
  sum(y.4D == 0, na.rm=TRUE)
  
  
# Covariates ------------------------------------------------------------
  
# scale covariates
  dat$DWscaled <- scale(dat$DW)
  dat$tempscaled <- scale(dat$temp)
  dat$elevscaled <- scale(dat$elev)
  dat$latscaled <- scale(dat$lat)
  dat$longscaled <- scale(dat$long)

  
# temperature - 4D matrix (plot, occ, site, year)
  temp.3D =array(0,dim=c(maxJ,maxI, n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    this.plot=which(site.plots[[this.site]]==dat$subplot[i]) #get plot for this row
    temp.3D[this.plot,this.site,this.year]=as.numeric(dat$tempscaled[i]) #force numeric
  }
  
  str(temp.3D)
  sum(temp.3D, na.rm=TRUE)
  sum(temp.3D == 0, na.rm=TRUE)
  
  
# downed wood  
  downedwood.3D =array(0,dim=c(maxJ, maxI, n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    this.plot=which(site.plots[[this.site]]==dat$subplot[i]) #get plot for this row
    downedwood.3D[this.plot,this.site,this.year]=as.numeric(dat$DWscaled[i]) #force numeric
  }
  
  str(downedwood.3D)
  sum(downedwood.3D, na.rm=TRUE)
  sum(downedwood.3D == 0, na.rm=TRUE)

  
# treatment
  # Make each treatment a dummy covariate
  table(dat$trt)
  
  #BU
  dat$BU <- NA
  dat$BU[dat$trt=="BU"]<- 1
  dat$BU[dat$trt!="BU"]<- 0
  
  # format each treatment into sites and years
  BU.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    BU.new[this.site,this.year]=as.numeric(dat$BU[i]) #force numeric
  }
  
  str(BU.new)
  
  #BS
  dat$BS <- NA
  dat$BS[dat$trt=="BS"]<- 1
  dat$BS[dat$trt!="BS"]<- 0
  
  # format each treatment into sites and years
  BS.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    BS.new[this.site,this.year]=as.numeric(dat$BS[i]) #force numeric
  }
  
  #HB
  dat$HB <- NA
  dat$HB[dat$trt=="HB"]<- 1
  dat$HB[dat$trt!="HB"]<- 0
  
  # format each treatment into sites and years
  HB.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    HB.new[this.site,this.year]=as.numeric(dat$HB[i]) #force numeric
  }
  
  #HU
  dat$HU <- NA
  dat$HU[dat$trt=="HU"]<- 1
  dat$HU[dat$trt!="HU"]<- 0
  
  # format each treatment into sites and years
  HU.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    HU.new[this.site,this.year]=as.numeric(dat$HU[i]) #force numeric
  }
  
  #UU
  dat$UU <- NA
  dat$UU[dat$trt=="UU"]<- 1
  dat$UU[dat$trt!="UU"]<- 0
  
  # format each treatment into sites and years
  UU.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    UU.new[this.site,this.year]=as.numeric(dat$UU[i]) #force numeric
  }

    
# fix NA's
  # There is loads of bloody NAs in here!! how did I miss this?
  table(y.4D)
  table(is.na(y.4D))
  
  # create operations matrix which we apply to obs in model to deal with lots of NAs in obs array
  K2D <- 1*(!is.na(y.4D))
  
  # turn NAs in detection/non-detection data to 0 (see trick below)
  y.4D[is.na(y.4D)]<- 0
  str(K2D)
  
  
# management type
  mgmt.2D =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    mgmt.2D[this.site,this.year]=as.numeric(dat$mgmt[i]) #force numeric
  }
  
# lat
  lat.2D =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    lat.2D[this.site,this.year]=as.numeric(dat$latscaled[i]) #force numeric
  }
  
# long
  lon.2D =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    lon.2D[this.site,this.year]=as.numeric(dat$longscaled[i]) #force numeric
  }
  
# elev
  elev.2D =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(dat)){ #loop through each row
    this.year=which(years==dat$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==dat$site_id[i]) #get site for this row
    elev.2D[this.site,this.year]=as.numeric(dat$elevscaled[i]) #force numeric
  }
  
  
## End formatting code ---------------------------------------------------------  
## Model   ---------------------------------------------------------------------
  
# set y to current dataset
y = y.4D
nyears = dim(y.4D)[4] # years


# run the chains
n.chains = 3
chains = vector("list", n.chains)
chains2 = vector("list", n.chains)

for(chain in 1:n.chains){
  
  # define constants
  constants <- list(
    I = I, # sites
    J = J, # subplots
    nsurveys = dim(y)[2], # surveys
    nyears = dim(y)[4], # years
    K2D = K2D,  #operation matrix
    HU = HU.new, BU = BU.new, BS = BS.new, HB = HB.new, # treatments
    temp = temp.3D, 
    downedwood = downedwood.3D, # count of dwd pieces
    #mgmt = mgmt.2D, # management type 0=private, 1=public
    lat = lat.2D,
    lon = lon.2D,
    elev = elev.2D) 

    # provide data
  Nimdata <- list(y=y) # y=obs
  str(y)
  
  # set inits
  
  # random yearly effect inits
  beta0.psi.year.init <- runif(n.years)
  beta0.theta.year.init <- runif(n.years)
  alpha0.year.init <- runif(n.years)
  

  # inits for latent states 
    
    # for z state of site i in year t - CHATGPT HELPED ME HERE - BIG UP 
    z_init <- array(0, dim = c(max(I), nyears))
    for (t in 1:nyears) {
      for (i in 1:I[t]) {
        site_obs <- y.4D[, , i, t]
        if (any(site_obs == 1, na.rm = TRUE)) {
          z_init[i, t] <- 1
        } else {
          z_init[i, t] <- 0  # set to 0 if no detection
        }
      }
    }
    
    # for w state of plot j in site i in year t
    w_init <- array(0, dim = c(maxJ, maxI, nyears))
    for (t in 1:nyears) {
      for (i in 1:I[t]) {
        for (j in 1:J[i]) {
          plot_obs <- y[j, , i, t]
          if (any(plot_obs == 1, na.rm = TRUE)) {
            w_init[j, i, t] <- 1
          } else {
            w_init[j, i, t] <- 0 # set to 0 if no detection
          }
        }
      }
    }
  
  # set initial values
  Niminits <- list(beta0.psi = 0, beta0.theta = 0, alpha0 = 0,
                  beta1.psi.BU = 0, beta2.psi.HB = 0, beta3.psi.HU = 0, beta4.psi.BS = 0, 
                  beta5.psi.lat = 0, beta6.psi.lon = 0, beta8.psi.elev = 0,
                  # beta7.psi.mgmt = 0, 
                  beta1.theta.DW = 0, alpha1 = 0, alpha2 = 0, 
                  beta0.psi.year = rnorm(nyears), beta0.theta.year = rnorm(nyears), alpha0.year = rnorm(nyears), 
                  sd.psi.year = runif(1, 0.1, 1), sd.theta.year = runif(1, 0.1, 1), 
                  sd.p.year = runif(1, 0.1, 1), z = z_init, w = w_init)
  
  # parameters to monitor
  parameters <- c("beta0.psi.year", "beta0.theta.year", "alpha0.year", 
                  "beta0.psi", "beta0.theta", "alpha0", 
                  "beta1.psi.BU", "beta2.psi.HB", "beta3.psi.HU", "beta4.psi.BS", 
                  "beta5.psi.lat", "beta6.psi.lon", "beta8.psi.elev",
                  # "beta7.psi.mgmt", 
                  'beta1.theta.DW', 'alpha1', 'alpha2')
 
  str(K2D)
  str(y.4D)

  # Multi-scale occupancy model for estimating the use of use of plot j, 
  # given the occupancy status of site i from k surveys across t years
  NimModel <- nimbleCode({
    # priors
    beta0.psi ~ dlogis(0,1)  # prior for psi intercept
    beta0.theta ~ dlogis(0,1)  # prior for theta intercept, mean plot use
    alpha0 ~ dlogis(0,1)  # prior for detection intercept, mean det prob
    sd.psi.year ~ dunif(0,5) # prior for sd for yearly psi random effect 
    sd.theta.year ~ dunif(0,5) # prior for sd for yearly theta random effect 
    sd.p.year ~ dunif(0,5)  # prior for sd for yearly p random effect 
    beta1.psi.BU ~ dnorm(0, sd = 5)  # slope term for burn effect on site occu
    beta2.psi.HB ~ dnorm(0, sd = 5)  # harvest burn
    beta3.psi.HU ~ dnorm(0, sd = 5)  # harvest
    beta4.psi.BS ~ dnorm(0, sd = 5)  # burn salvage
    beta5.psi.lat ~ dnorm(0, sd = 5) # latitude
    beta6.psi.lon ~ dnorm(0, sd = 5) # longitude
    #beta7.psi.mgmt ~ dnorm(0, sd = 5) # management type
    beta8.psi.elev ~ dnorm(0, sd = 5) # elevation
    beta1.theta.DW ~ dnorm(0, sd = 5) # downed wood effect on plot use
    alpha1 ~ dnorm(0, sd =5) # linear temp effect on detection
    alpha2 ~ dnorm(0, sd = 5) # quadratic temp
    
    # likelihood for state model
    # First submodel for whether site i is occupied
      for(t in 1:nyears) {
        # mean centered yearly random effect on psi
        beta0.psi.year[t] ~ dnorm(beta0.psi, sd=sd.psi.year)
                                  
        for (i in 1:I[t]){
      # estimate psi as function  of site by year covariates
      logit(psi[i,t]) <- beta0.psi.year[t] + 
                         beta1.psi.BU * BU[i,t] + beta2.psi.HB * HB[i,t] + beta3.psi.HU * HU[i,t] + 
                         beta4.psi.BS * BS[i,t] + beta5.psi.lat * lat[i,t] + beta6.psi.lon * lon[i,t] + 
                         # beta7.psi.mgmt * mgmt[i,t] + 
                         beta8.psi.elev * elev[i,t]
      z[i,t] ~ dbern(psi[i,t]) # is site occupied? z=1 yes, z=0 no
        }
      }
    
    # Next submodel for whether plot j is used given that site i is occupied (z=1)
    for(t in 1:nyears) {
      #centered yearly random effect on mean theta
      beta0.theta.year[t] ~ dnorm(beta0.theta,sd=sd.theta.year)
      # estimate theta (plot usage) as function of covs 
      for (i in 1:I[t]){
        for(j in 1:J[i]){
        logit(theta[j,i,t]) <- beta0.theta.year[t] + beta1.theta.DW * downedwood[j,i,t]
        w[j,i,t] ~ dbern(theta[j,i,t]*z[i,t]) #is plot j used given site i is occupied?
        }
      }
    }
        # likelihood for obs submodel - did we observe a critter at plot j, on survey k, at site i, in year t?
          #centered yearly random effect on mean detection
      for(t in 1:nyears) {
          alpha0.year[t] ~ dnorm(alpha0,sd=sd.p.year)
        for (i in 1:I[t]){
          for(j in 1:J[i]){
            for(k in 1:nsurveys){
              # estimate detection probability as function of covs (no covs currently)
          logit(p[j,k,i,t]) <- alpha0.year[t] + alpha1 * temp[j,i,t] + alpha2 * temp[j,i,t]^2 
          # observations as function of det prob conditional on the plot is used (w = 1)
          y[j,k,i,t] ~ dbern((p[j,k,i,t] * K2D[j,k,i,t]) * w[j,i,t])  
          } # use K2D here, so when sites were not surveyed, prob of detecting an animal = 0
        }
      }
    }
  
  }) # end model
  
  # Build the model, configure the mcmc, and compileConfigure
  start.time <- Sys.time()
  Rmodel <- nimbleModel(code=NimModel, constants=constants, data=Nimdata, check=FALSE, inits=Niminits)
  conf <- configureMCMC(Rmodel, monitors=parameters, monitors2 = parameters, thin = 50, thin2 = 5000,
                        useConjugacy=FALSE, calculateEfficiency = TRUE) # thin interval
  
  # custom RW updates - block parameteres which display high posterior correlation

    conf$addSampler(target = c("beta0.psi", "beta3.psi.HU"),
                    type = 'RW_block',control = list(adaptive=TRUE),silent = TRUE)
    # 
    # conf$addSampler(target = c("alpha0", "sd.p.year"),
    #                 type = 'RW_block',control = list(adaptive=TRUE),silent = TRUE)
    #
    conf$addSampler(target = c("beta0.theta","alpha0"),
                    type = 'RW_block',control = list(adaptive=TRUE),silent = TRUE)
    # 
  # Build and compile
  Rmcmc <- buildMCMC(conf)
  Cmodel <- compileNimble(Rmodel)
  Cmcmc <- compileNimble(Rmcmc, project = Rmodel)
  
  # Cmodel$check()
  
  # Run the model
  start.time2 <- Sys.time()
  Cmcmc$run(2500000,reset=FALSE) #Can keep extending the run by rerunning this line  # sets n.iterations
  end.time <- Sys.time()
  end.time - start.time  # total time for compilation, replacing samplers, and fitting
  end.time - start.time2 # post-compilation run time
  
  #get the chains
  mvSamples = as.matrix(Cmcmc$mvSamples)
  mvSamples2 = as.matrix(Cmcmc$mvSamples2)
  
  chains[[chain]]=mvSamples
  chains2[[chain]]=mvSamples2
  
}

# large sample subset: 45,000
  n.iter = 50000
  n.burn = 5000
# small sample subset: 450
  n.iter2 = 500
  n.burn2 = 50

#combine the chains and burn
  a = mcmc.list(mcmc(chains[[1]][n.burn:n.iter,]), #large
              mcmc(chains[[2]][n.burn:n.iter,]),
              mcmc(chains[[3]][n.burn:n.iter,]))
  a2 = mcmc.list(mcmc(chains2[[1]][n.burn2:n.iter2,]), #small
               mcmc(chains2[[2]][n.burn2:n.iter2,]),
               mcmc(chains2[[3]][n.burn2:n.iter2,]))

# save
  save(a, file = "multiscale_output_072525_oss_full.RData")
  save(a, constants, Nimdata, NimModel, Niminits, file = "multiscale_output_and_data_072525_oss_full.RData")

  load("multiscale_output_and_data_072125_small.RData")
  
  
## Diagnostics ------------------------------------------------------------
  
# R-hat values 
  gelman.diag(a2)
  # looks great, everything below 1.04
  
# Trace plots
  plot(a2)
  traceplot(a2[, c("beta0.psi", "beta1.psi.BU", "beta1.theta.DW", "alpha0")])
  # look pretty good, trace plots mostly overlapping with some variation
  
# Estimates
  summary(a2)
  # zero boundary estimates seem to be gone!
  
# Combine chains
  a=runjags::combine.mcmc(a2)
  colnames(mvSamples)

# Correlation matrix
    cor <- cor(a[, c("alpha0", "beta0.psi", "beta0.theta", 
                   "beta1.psi.BU", "beta2.psi.HB",  "beta3.psi.HU", "beta4.psi.BS", 
                   "beta0.theta.year[1]" )])
    
    threshold <- 0.6
    high_corr <- abs(cor) > threshold
    diag(high_corr) <- FALSE
    high_corr_pairs <- which(high_corr, arr.ind = TRUE)
    data.frame(
      Var1 = rownames(cor)[high_corr_pairs[, 1]],
      Var2 = colnames(cor)[high_corr_pairs[, 2]],
      Correlation = cor[high_corr])
    
    # some high correlations above 0.6 here: 
    # not sure what to do with that though. parameter redundancy? ################ ??
   
# Effective sample size: 
    effectiveSize(a)
    # all above 1000 ! even for the small sample subset 





