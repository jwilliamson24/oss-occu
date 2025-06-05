
# Load packages
library(nimble)
library(coda)

# data 
 
enes.4D <- readRDS("data/occupancy/enes.4D.rds")
str(enes.4D)
BU.new <- read.csv("data/occupancy/BU.new.csv")
BS.new <- read.csv("data/occupancy/BS.new.csv")
HU.new <- read.csv("data/occupancy/HU.new.csv")
HB.new <- read.csv("data/occupancy/HB.new.csv")


#### Model ----

# added in indexing by year (T) and renamed parameters to be more intuitive 

# set y to current dataset
y = enes.4D

# run the chains
n.chains = 3
chains = vector("list", n.chains)
for(chain in 1:n.chains){
  
  # define constants
  constants <- list(
    I = dim(y)[1], # sites
    J = dim(y)[2], # subplots
    K = dim(y)[3], # surveys
    T = dim(y)[4], # years
    HU = HU.new, BU = BU.new, BS = BS.new, HB = HB.new) # treatments
  
  # set initial values, all zero
  Niminits <- list(Intercept = 0, Plot.int = 0, Det.int = 0, 
                   BU.psi = 0, HB.psi = 0, HU.psi = 0, BS.psi = 0, 
                   z = ifelse(is.na(z.data), 1, z.data),
                   w = ifelse(is.na(w.data), 1, w.data))

  # data summaries used in model 
  w.data <- 1*(apply(y, c(1,2,4), sum) >0) # w[i,j,t] = 1 if any dets across k surveys 
  z.data <- 1*(apply(w.data, c(1,3), sum) >0) # z[i,t] = 1 if plots used at site i in yr t 
  w.data[w.data==0] <- NA
  z.data[z.data==0] <- NA
  
  # provide data
  Nimdata <- list(y=y, z=z.data, w=w.data) # y=obs, z=site level latent state, w=plot level latent state
  
  # parameters to monitor
  parameters <- c("Intercept", "Plot.int", "Det.int", "BU.psi", "HB.psi", "HU.psi", "BS.psi",
                  'zsum')
  # (zsum = number of subunits occupied at a site, given that the species is present at that site)
  
  
# Model 
# multi-scale occupancy model for estimating the occupancy state of site i, the use of plot j
  NimModel <- nimbleCode({
    
    # priors
    Intercept ~ dlogis(0,1)  # intercept for occupancy, mean occu
    Plot.int ~ dlogis(0,1)  # intercept for plot use, mean plot use
    Det.int ~ dlogis(0,1)  # intercept for detection, mean det prob
    BU.psi ~ dnorm(0, sd = 5)  # slope term for burn effect on site occu
    HB.psi ~ dnorm(0, sd = 5)  # harvest burn
    HU.psi ~ dnorm(0, sd = 5)  # harvest
    BS.psi ~ dnorm(0, sd = 5)  # burn salvage
    # DW ~ dnorm(0, sd = 5)  # downed wood effect
    # sd.theta ~ dunif(0,5)
    # beta1.theta ~ dnorm(0, sd = 5)
    # alpha1 ~ dnorm(0, sd = 5)
    
    # likelihood for state model
    for(i in 1:I) {
      for(t in 1:T) {
      
      # estimate psi as function  of covs
      logit(psi[i,t]) <- Intercept + BU.psi * BU[i,t] + HU.psi * HU[i,t] + BS.psi * BS[i,t] + 
                         HB.psi * HB[i,t]
      z[i,t] ~ dbern(psi[i,t]) # is site occupied? z=1 yes, z=0 no
      
      #centered Random Effect
      # plot.theta[i] ~ dnorm(beta0.theta,sd=sd.theta)
      
      # estimate theta (plot usage) as function of covs
      for(j in 1:J){
        logit(theta[i,j,t]) <- Plot.int 
        w[i,j,t] ~ dbern(theta[i,j,t]*z[i,t]) #is plot used given the site occupied?
        
        # likelihood for observation model
        for(k in 1:K){
          logit(p[i,j,k,t]) <- Det.int
          y[i,j,k,t] ~ dbern(p[i,j,k,t] * w[i,j,t]) # observations as function of det prob conditional on the plot is used (w = 1)
          
          }
        }
      }
    }
    
    zsum <- sum(z[1:I, 1:T])
    
  }) # end model
  
  
  # Build the model, configure the mcmc, and compileConfigure
  start.time <- Sys.time()
  Rmodel <- nimbleModel(code=NimModel, constants=constants, data=Nimdata, check=FALSE, inits=Niminits)
  conf <- configureMCMC(Rmodel, monitors=parameters, thin=5, useConjugacy=FALSE) # this sets thinning interval to every 5th value
  
  # Build and compile
  Rmcmc <- buildMCMC(conf)
  Cmodel <- compileNimble(Rmodel)
  Cmcmc <- compileNimble(Rmcmc, project = Rmodel)
  
  # Run the model
  start.time2 <- Sys.time()
  Cmcmc$run(100000,reset=FALSE) #Can keep extending the run by rerunning this line  #this sets n.iterations
  end.time <- Sys.time()
  end.time - start.time  # total time for compilation, replacing samplers, and fitting
  end.time - start.time2 # post-compilation run time
  
  burnin <- 10000
  
  #get the chains
  mvSamples = as.matrix(Cmcmc$mvSamples)
  chains[[chain]]=mvSamples
}


n.iter = 20000
n.burn = 15000
#combine the chains and burn
a=mcmc.list(mcmc(chains[[1]][n.burn:n.iter,]),
            mcmc(chains[[2]][n.burn:n.iter,]),
            mcmc(chains[[3]][n.burn:n.iter,]))









