

## load packages
  library(nimble)
  library(coda)

  
## load data -------------------------------------------------------------------
  
  # 2023-2024 covariate data
  #site <- read.csv("data/site.complete.csv")
  #subplot <- read.csv("data/subplot.complete.csv")
  site.lvl <- read.csv("data/covariate matrices/site_level_matrix.csv") # jul date
  subplot.lvl <- read.csv("data/covariate matrices/habitat.occu.complete.csv") # temp
  dwd.count <- read.csv("data/covariate matrices/avg-dwd-subplot-matrix.csv") # dwd count
  
  # from occu-matrices-postfire.R
  dets.o <- read.csv("data/occupancy/dets.o.post.csv") # post-fire oss
  dets.e <- read.csv("data/occupancy/dets.e.post.csv") # post-fire enes
  
  
## format cov data -------------------------------------------------------------
  
  # site.lvl - site_id, jul_date, landowner
  site.info <- site.lvl %>%
    select(site_id, jul_date, landowner)

  # subplot.lvl - plot_id, site_id, subplot, temp_C, lat, long, elev, canopy_cov
  subplot.info <- subplot.lvl %>%
    select(plot_id, site_id, subplot, temp_C, lat, long, elev, canopy_cov)
  
  # dwd.count - change to long format
  dwd.long <- dwd.count %>%
    pivot_longer(cols = starts_with("X"),
                 names_to = "subplot",
                 names_prefix = "X",
                 values_to = "DW") %>%
    mutate(subplot = as.integer(subplot))
  
  
## add covariates to occupancy matrices ----------------------------------------
  
  # oss  
  
  dets.o <- dets.o %>%
    left_join(site.info, by = "site_id") %>%
    left_join(subplot.info, by = c("site_id", "subplot")) %>%
    left_join(dwd.long %>% select(site_id, subplot, DW), by = c("site_id", "subplot"))

  # enes
  
  dets.e <- dets.e %>%
    left_join(site.info, by = "site_id") %>%
    left_join(subplot.info, by = c("site_id", "subplot")) %>%
    left_join(dwd.long %>% select(site_id, subplot, DW), by = c("site_id", "subplot"))
  
  
  # landowner to management type column
  
  dets.o <- dets.o %>%
    mutate(mgmt_type = case_when(
      landowner == "PB" ~ "0", # private
      landowner == "WY" ~ "0",  # private
      landowner == "BLM" ~ "1",  # public
      landowner == "ODF" ~ "1",  # public
      TRUE ~ "default_value"  
    ))
  
  dets.e <- dets.e %>%
    mutate(mgmt_type = case_when(
      landowner == "PB" ~ "0", # private
      landowner == "WY" ~ "0",  # private
      landowner == "BLM" ~ "1",  # public
      landowner == "ODF" ~ "1",  # public
      TRUE ~ "default_value"  
    ))


  # add treatment columns
  
  treatments <- unique(dets.o$trt)
  
  for (trt in treatments) {
    dets.o[[trt]] <- as.numeric(dets.o$trt == trt)
  }
  
  for (trt in treatments) {
    dets.e[[trt]] <- as.numeric(dets.e$trt == trt)
  }
  

#### Model --------------------------------------------------------------------

  ## Define 
  I <- 889 # I = sites (subplots as sites)
  K <- 3 # K = occasions
  y = dets.e
  
# run the chains
  n.chains = 3
  chains = vector("list", n.chains)
  for(chain in 1:n.chains){
    
    #fit model
    constants <- list(I = I, K = K, 
                      HU = y$HU, HB = y$HB, BU = y$BU, BS = y$BS,
                      canopycover = y$canopy_cov,
                      temp = y$temp_C)
    
    Nimdata <- list(y=y)
    
    # set initial values
    Niminits <- list(beta0.psi = 0, beta1.psi.cc = 0, beta2.psi.BU = 0, beta3.psi.HU = 0, 
                     beta4.psi.BS = 0, beta5.psi.HB = 0, alpha0=0)
    
    # data summaries
    z.data <- 1*(rowSums(y, na.rm = TRUE)>0)
    z.data[z.data==0] <- NA
    
    # provide data for model
    Nimdata <- list(y=y,z=z.data)
    
    #set parameters to monitor
    parameters <- c("beta0.psi", "beta1.psi.cc", "beta2.psi.BU", "beta3.psi.HU", "beta4.psi.BS", 
                    "beta5.psi.HB", "alpha0", 'zsum')
  
  
    NimModel <- nimbleCode({
      
      # priors
      beta0.psi ~ dlogis(0,1) # mean occupancy (intercept)
      beta1.psi.cc ~ dnorm(0, sd = 5) # slope term for canopy cover on occupancy
      beta2.psi.BU ~ dnorm(0, sd = 5) # slope term for burn on occupancy
      beta3.psi.HU ~ dnorm(0, sd = 5) # slope for harvest
      beta4.psi.BS ~ dnorm(0, sd = 5) # slope for burn/salvage
      beta5.psi.HB ~ dnorm(0, sd = 5) # slope for harvest+burn
      alpha0 ~ dlogis(0,1) # mean detection probability (intercept for detection)
      
      # likelihood for state model
      for(i in 1:I){
        
          logit(psi[i]) <- beta0.psi + beta1.psi * canopycover[i] + beta2.psi * burnt[i] + beta3.psi * logged[i] + 
          beta4.psi * burntandlogged[i] + beta5.psi * loggedandburnt[i] + beta1.psi * canopycover[i]
        z[i] ~ dbern(psi[i]) 
        
        
      # likelihood for observation model
        for(k in 1:K){
            logit(p[i,k]) <- alpha0 
            y[i,k] ~ dbern(p[i,k]*z[i]) 
          }
        }
      }
      
      zsum <- sum(z[1:I])
      
    })# end model
    
   
  # Custom configurations from JT 
    # Build the model, configure the mcmc, and compileConfigure
    start.time <- Sys.time()
    Rmodel <- nimbleModel(code=NimModel, constants=constants, data=Nimdata,check=FALSE,inits=Niminits)
    conf <- configureMCMC(Rmodel,monitors=parameters, thin=5, useConjugacy=FALSE) # thinning interval
    
    # Build and compile
    Rmcmc <- buildMCMC(conf)
    Cmodel <- compileNimble(Rmodel)
    Cmcmc <- compileNimble(Rmcmc, project = Rmodel)
    
    # Run the model
    start.time2 <- Sys.time()
    Cmcmc$run(100000,reset=FALSE) # this sets n.iterations
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


