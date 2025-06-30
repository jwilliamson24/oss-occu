
# Load packages
library(nimble)
library(coda)

setwd('C:/Users/twininjo/Documents/R/Salamanders_multiscale_occ')

# data
  enes <- read.csv("enes.prepost.multiscale.occu.csv") 
  oss <- read.csv("oss.prepost.multiscale.occu.csv")

# jw data path  
  enes <- read.csv("data/occupancy/enes.prepost.multiscale.occu.csv") 
  oss <- read.csv("data/occupancy/oss.prepost.multiscale.occu.csv")

# Data Formatting to 3D and 4D ------------------------------------------------------

  # formatting the data in script to see if it helps spot issue 
  unique(enes$site_id)
  unique(enes$year)
  plotnames=sort(unique(enes$subplot))
  sites=sort(unique(enes$site_id))
  years=sort(unique(enes$year))
  n.sites=length(sites)
  n.years=length(years)
  table(enes$year)
  
  # make year formatting in chronological order!!!
  ######## RERUN after check date order and site numbers (max of 82)

  
  sum(table(enes$year))

  # how many plots at each site
  J=rep(NA, n.sites)
  site.plots=list(n.sites)
  for (i in 1:n.sites){
    J[i]=length(enes$subplot[enes$site_id==sites[i]])
    site.plots[[i]]=unique(enes$subplot[enes$site_id==sites[i]])
  }
  
  # how many sites in each year
  I=rep(NA, n.years)
  year.sites= list(9)
  for (t in 1:n.years){
    I[t] = length(sort(unique(enes$site_id[enes$year==years[t]])))
    year.sites[[t]]=unique(enes$site_id[enes$year==years[t]])
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
  
  
# Covariates ------------------------------------------------------------
  
  # scale covariates
  enes$DWscaled <- scale(enes$DW)
  enes$tempscaled <- scale(enes$temp)

  
# temperature - 4D matrix (plot, occ, site, year)
  temperature <- enes$temp
  
  temp.3D =array(0,dim=c(maxJ,maxI, n.years)) #new data 
  for(i in 1:nrow(enes)){ #loop through each row
    this.year=which(years==enes$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
    this.plot=which(site.plots[[this.site]]==enes$subplot[i]) #get plot for this row
    temp.3D[this.plot,this.site,this.year]=as.numeric(enes$tempscaled[i]) #force numeric
  }
  
  str(temp.3D)
  sum(temp.3D, na.rm=TRUE)
  sum(temp.3D == 0, na.rm=TRUE)
  
  
# downed wood  

  downedwood.3D =array(0,dim=c(maxJ, maxI, n.years)) #new data 
  for(i in 1:nrow(enes)){ #loop through each row
    this.year=which(years==enes$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
    this.plot=which(site.plots[[this.site]]==enes$subplot[i]) #get plot for this row
    downedwood.3D[this.plot,this.site,this.year]=as.numeric(enes$DWscaled[i]) #force numeric
  }
  
  str(downedwood.3D)
  sum(downedwood.3D, na.rm=TRUE)
  sum(downedwood.3D == 0, na.rm=TRUE)

  
# treatment

  # Make each treatment a dummy covariate
  table(enes$trt)
  
  #BU
  enes$BU <- NA
  enes$BU[enes$trt=="BU"]<- 1
  enes$BU[enes$trt!="BU"]<- 0
  
  # format each treatment into sites and years
  BU.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(enes)){ #loop through each row
    this.year=which(years==enes$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
    BU.new[this.site,this.year]=as.numeric(enes$BU[i]) #force numeric
  }
  
  #BS
  enes$BS <- NA
  enes$BS[enes$trt=="BS"]<- 1
  enes$BS[enes$trt!="BS"]<- 0
  
  # format each treatment into sites and years
  BS.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(enes)){ #loop through each row
    this.year=which(years==enes$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
    BS.new[this.site,this.year]=as.numeric(enes$BS[i]) #force numeric
  }
  
  #HB
  enes$HB <- NA
  enes$HB[enes$trt=="HB"]<- 1
  enes$HB[enes$trt!="HB"]<- 0
  
  # format each treatment into sites and years
  HB.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(enes)){ #loop through each row
    this.year=which(years==enes$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
    HB.new[this.site,this.year]=as.numeric(enes$HB[i]) #force numeric
  }
  
  #HU
  enes$HU <- NA
  enes$HU[enes$trt=="HU"]<- 1
  enes$HU[enes$trt!="HU"]<- 0
  
  # format each treatment into sites and years
  HU.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(enes)){ #loop through each row
    this.year=which(years==enes$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
    HU.new[this.site,this.year]=as.numeric(enes$HU[i]) #force numeric
  }
  
  #UU
  enes$UU <- NA
  enes$UU[enes$trt=="UU"]<- 1
  enes$UU[enes$trt!="UU"]<- 0
  
  # format each treatment into sites and years
  UU.new =array(0,dim=c(maxI,n.years)) #new data 
  for(i in 1:nrow(enes)){ #loop through each row
    this.year=which(years==enes$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
    UU.new[this.site,this.year]=as.numeric(enes$UU[i]) #force numeric
  }

    
# fix NA's
  
  # There is loads of bloody NAs in here!! how did I miss this?
  table(y.4D)
  table(is.na(y.4D))
  
  # create operations matrix which we apply to observations in model to deal with large number of NAs in observation array
  K2D <- 1*(!is.na(y.4D))
  
  # turn NAs in detection/non-detection data to 0 (see trick below)
  y.4D[is.na(y.4D)]<- 0

  str(K2D)
  
  
# management type
  
  mgmt.3D =array(0,dim=c(maxJ,maxI, n.years)) #new data 
  for(i in 1:nrow(enes)){ #loop through each row
    this.year=which(years==enes$year[i]) #get year for this row
    this.site=which(year.sites[[this.year]]==enes$site_id[i]) #get site for this row
    this.plot=which(site.plots[[this.site]]==enes$subplot[i]) #get plot for this row
    mgmt.3D[this.plot,this.site,this.year]=as.numeric(enes$mgmt_type[i]) #force numeric
  }  
   
  str(mgmt.3D)
  sum(mgmt.3D, na.rm=TRUE)
  sum(mgmt.3D == 0, na.rm=TRUE)
  
  
# End formatting code ------------------------------------------------------------  
  
# set y to current dataset
y = y.4D
nyears = dim(y.4D)[4] # years


# run the chains
n.chains = 3
chains = vector("list", n.chains)
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
    downedwood = downedwood.3D, 
    mgmt = mgmt.3D) 

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
                  beta1.psi.BU = 0, beta2.psi.HB = 0, beta3.psi.HU = 0, beta4.psi.BS = 0, beta7.psi.mgmt = 0,
                  beta1.theta.DW = 0, alpha1 = 0, alpha2 = 0,
                  # beta5.psi.lat = 0, beta6.psi.long = 0,
                  beta0.psi.year = rnorm(nyears), beta0.theta.year = rnorm(nyears), alpha0.year = rnorm(nyears), 
                  sd.psi.year = runif(1, 0.1, 1), sd.theta.year = runif(1, 0.1, 1), 
                  sd.p.year = runif(1, 0.1, 1), z = z_init, w = w_init)
  
  # parameters to monitor
  parameters <- c("beta0.psi.year", "beta0.theta.year", "alpha0.year", "beta0.psi", "beta0.theta", 
                  "alpha0", "beta1.psi.BU", "beta2.psi.HB", "beta3.psi.HU", "beta4.psi.BS", "beta7.psi.mgmt", 
                  'beta1.theta.DW', 'alpha1', 'alpha2')
 
  str(K2D)
  str(y.4D)

  # Multi-scale occupancy model for estimating the use of use of plot j, given the occupancy status of site i from k surveys across t years
  NimModel <- nimbleCode({
    # priors
    beta0.psi ~ dlogis(0,1)  # prior for psi intercept
    sd.psi.year ~ dunif(0,5) # prior for sd for yearly psi random effect 
    sd.theta.year ~ dunif(0,5) # prior for sd for yearly theta random effect 
    sd.p.year ~ dunif(0,5)  # prior for sd for yearly p random effect 
    beta0.theta ~ dlogis(0,1)  # prior for theta intercept, mean plot use
    alpha0 ~ dlogis(0,1)  # prior for detection intercept, mean det prob
    beta1.psi.BU ~ dnorm(0, sd = 5)  # slope term for burn effect on site occu
    beta2.psi.HB ~ dnorm(0, sd = 5)  # harvest burn
    beta3.psi.HU ~ dnorm(0, sd = 5)  # harvest
    beta4.psi.BS ~ dnorm(0, sd = 5)  # burn salvage
    # beta5.psi.lat ~ dnorm(0, sd = 5) # latitude
    # beta6.psi.long ~ dnorm(0, sd = 5) # longitude
    beta7.psi.mgmt ~ dnorm(0, sd = 5) # management type
    beta1.theta.DW ~ dnorm(0, sd = 5) # downed wood
    alpha1 ~ dnorm(0, sd =5) # linear temp
    alpha2 ~ dnorm(0, sd = 5)# quadratic temp
    
    # likelihood for state model
    # First submodel for whether site i is occupied
      for(t in 1:nyears) {
        # mean centered yearly random effect on psi
        beta0.psi.year[t] ~ dnorm(beta0.psi, sd=sd.psi.year)
                                  
        for (i in 1:I[t]){
      # estimate psi as function  of site by year covariates
      logit(psi[i,t]) <- beta0.psi.year[t] + beta1.psi.BU * BU[i,t] + beta2.psi.HB * HB[i,t] + beta3.psi.HU * HU[i,t] + 
                        beta4.psi.BS * BS[i,t] + beta7.psi.mgmt * mgmt[i,t]
                    # + beta5.psi.lat * lat[i,t] + beta6.psi.long * long[i,t]
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
        # likelihood for observation submodel - did we observe a critter at plot j, on survey k, at site i, in year t?
          #centered yearly random effect on mean detection
      for(t in 1:nyears) {
          alpha0.year[t] ~ dnorm(alpha0,sd=sd.p.year)
        for (i in 1:I[t]){
          for(j in 1:J[i]){
            for(k in 1:nsurveys){
              # estimate detection probability as function of covs (no covs currently)
          logit(p[j,k,i,t]) <- alpha0.year[t] + alpha1 * temp[j,i,t] + alpha2 * temp[j,i,t]^2 
          # observations as function of det prob conditional on the plot is used (w = 1)
          y[j,k,i,t] ~ dbern((p[j,k,i,t] * K2D[j,k,i,t]) * w[j,i,t])  # we use K2D here, so that when sites were not surveyed, then probability of detecting an animal = 0
          }
        }
      }
    }
  
  }) # end model
  
  # Build the model, configure the mcmc, and compileConfigure
  start.time <- Sys.time()
  Rmodel <- nimbleModel(code=NimModel, constants=constants, data=Nimdata, check=FALSE, inits=Niminits)
  conf <- configureMCMC(Rmodel, monitors=parameters, thin=4, useConjugacy=FALSE, calculateEfficiency = TRUE) # this sets thinning interval to every 2nd value
  
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
  Cmcmc$run(100000,reset=FALSE) #Can keep extending the run by rerunning this line  #this sets n.iterations
  end.time <- Sys.time()
  end.time - start.time  # total time for compilation, replacing samplers, and fitting
  end.time - start.time2 # post-compilation run time
  
  #get the chains
  mvSamples = as.matrix(Cmcmc$mvSamples)
  chains[[chain]]=mvSamples
}


n.iter = 25000
n.burn = 20000

# plot(mcmc(mvSamples[-c(1:n.burn),]))


#combine the chains and burn
a=mcmc.list(mcmc(chains[[1]][n.burn:n.iter,]),
            mcmc(chains[[2]][n.burn:n.iter,]),
            mcmc(chains[[3]][n.burn:n.iter,]))

plot(a)
summary(a)
a=runjags::combine.mcmc(a)

runjags::combine.jags()
colnames(mvSamples)

cor <- cor(a[, c("alpha0", "beta0.psi","beta0.theta", "beta1.psi.BU", "beta2.psi.HB",  "beta3.psi.HU", "beta0.theta.year[1]",
          "beta4.psi.BS", "beta7.psi.mgmt" )])


library('coda')

ESS(a)


save.image

