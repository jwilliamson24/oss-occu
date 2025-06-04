

#### Model ----


# run the chains
n.chains = 3
chains = vector("list", n.chains)
for(chain in 1:n.chains){
  
  #fit model
  constants <- list(I=I,J=J,K=K, logged = logged, burnt = burnt, canopycover=canopycover, 
                    burntandlogged = burntandlogged, loggedandburnt = loggedandburnt) 
  # dwd=downedwood, canopycover=canopycover, jasmineeffect=jasmineeffect)
  Nimdata <- list(y=y)
  
  # set initial values
  Niminits <- list(Intercept = 0, Plot.int = 0, Det.int = 0, BU.psi = 0, HB.psi = 0, HU.psi = 0, 
                   BS.psi = 0, DW=0)
  #beta1.psi = 0, alpha1 = 0, beta1.theta=0, sd.theta = 0.5)
  
  # data summaries
  w.data <- 1*(apply(y,c(1,2),sum)>0)
  z.data <- 1*(rowSums(w.data)>0)
  w.data[w.data==0] <- NA
  z.data[z.data==0] <- NA
  
  # provide data for model
  Nimdata <- list(y=y,z=z.data,w=w.data)
  
  #set parameters to monitor
  parameters <- c("Intercept", "Plot.int", "Det.int", "BU.psi", "HB.psi", "HU.psi", "BS.psi",
                  "DW", 'zsum')
    # (zsum = number of subunits occupied at a site, given that the species is present at that site)
  
  # multi-scale occupancy model for estimating the occupancy state of site i, the use of plot j
  NimModel <- nimbleCode({
    
    # priors
    Intercept ~ dlogis(0,1)  # intercept for occupancy, mean occupancy
    Plot.int ~ dlogis(0,1)  # intercept for plot use, mean plot use
    Det.int ~ dlogis(0,1)  # intercept for detection, mean det prob
    BU.psi ~ dnorm(0, sd = 5)  # slope term for burn on site occupancy
    HB.psi ~ dnorm(0, sd = 5)  # harvest burn
    HU.psi ~ dnorm(0, sd = 5)  # harvest
    BS.psi ~ dnorm(0, sd = 5)  # burn salvage
    DW ~ dnorm(0, sd = 5)  # downed wood
    # Temp 
    # Jul.date
    # sd.theta ~ dunif(0,5)
    # beta1.theta ~ dnorm(0, sd = 5)
    # alpha1 ~ dnorm(0, sd = 5)
    
    # likelihood for state model
    for(i in 1:I){
      
      # estimate psi as function  of covs
      logit(psi[i]) <- beta0.psi + beta1.psi * canopycover[i] + beta2.psi * burnt[i] + beta3.psi * logged[i] + 
        beta4.psi * burntandlogged[i] + beta5.psi * loggedandburnt[i] #+ beta1.psi * canopycover[i]
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









