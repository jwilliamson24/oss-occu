
# Load packages
library(nimble)
library(coda)

#### Model ----


# run the chains
n.chains = 3
chains = vector("list", n.chains)
for(chain in 1:n.chains){
  
  #fit model
  constants <- list(I = I, J = J, K = K, HU = HU, BU = BU, BS = BS, HB = HB) 
  
  Nimdata <- list(y=y2.3D)
  
  # set initial values
  Niminits <- list(beta0.psi = 0, beta2.psi = 0, beta3.psi = 0, beta4.psi = 0, beta5.psi = 0, 
                   beta0.theta = 0, alpha0=0)

  # data summaries
  w.data <- 1*(apply(y,c(1,2),sum)>0)
  z.data <- 1*(rowSums(w.data)>0)
  w.data[w.data==0] <- NA
  z.data[z.data==0] <- NA
  
  # provide data for model
  Nimdata <- list(y=y2.3D,z=z.data,w=w.data)
  
  #set parameters to monitor
  parameters <- c("beta0.psi", "beta2.psi", "beta3.psi", "beta4.psi", "beta5.psi",
                  "beta0.theta", "alpha0", 'zsum')
  #"beta1.psi", "alpha1", "beta1.theta", "beta1.psi")
  # (zsum = number of subunits occupied at a site, given that the species is present at that site)
  
  # multi-scale occupancy model for estimating the occupancy state of site i, the use of plot j
  NimModel <- nimbleCode({
    
    # priors
    beta0.psi ~ dlogis(0,1)
    beta0.theta ~ dlogis(0,1)
    alpha0 ~ dlogis(0,1)
    #beta1.psi ~ dnorm(0, sd = 5)
    beta2.psi ~ dnorm(0, sd = 5)
    beta3.psi ~ dnorm(0, sd = 5)
    beta4.psi ~ dnorm(0, sd = 5)
    beta5.psi ~ dnorm(0, sd = 5)
    # sd.theta ~ dunif(0,5)
    # beta1.theta ~ dnorm(0, sd = 5)
    # alpha1 ~ dnorm(0, sd = 5)
    
    # likelihood for state model
    for(i in 1:I){
      
      # estimate psi as function  of covs
      logit(psi[i]) <- beta0.psi + beta2.psi * BU[i] + beta3.psi * HU[i] + 
        beta4.psi * BS[i] + beta5.psi * HB[i] #+ beta1.psi * canopycover[i] + beta1.psi * canopycover[i]
      z[i] ~ dbern(psi[i]) #is site occupied? z = 1 is yes, z = 0 is no
      
      #centered Random Effect
      # plot.theta[i] ~ dnorm(beta0.theta,sd=sd.theta)
      
      # estimate theta (plot usage) as function of covs
      for(j in 1:J){
        logit(theta[i,j]) <- beta0.theta #+ beta1.theta * dwd[i,j]
        w[i,j] ~ dbern(theta[i,j]*z[i]) #is plot used given the site occupied?
        
        # likelihood for observation model
        for(k in 1:K){
          logit(p[i,j,k]) <- alpha0 #+ alpha1 * jasmineeffect[i,j,k]
          y[i,j,k] ~ dbern(p[i,j,k]*w[i,j]) # observations as a function of detection probability conditional on the plot is used (w = 1)
        }
      }
    }
    zsum <- sum(z[1:I])
  })# end model
  
  
  # Build the model, configure the mcmc, and compileConfigure
  start.time <- Sys.time()
  Rmodel <- nimbleModel(code=NimModel, constants=constants, data=Nimdata,check=FALSE,inits=Niminits)
  conf <- configureMCMC(Rmodel,monitors=parameters, thin=5, useConjugacy=FALSE) # this sets thinning interval to every 5th value
  
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









