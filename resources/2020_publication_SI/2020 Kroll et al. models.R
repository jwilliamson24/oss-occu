#
# Project:      OSS/ENES BACI study
# Task:         define models to be used in the analysis
# start date:   2019.08.29
#


# -----------------------------------------------------------------------------------------
# 'hierarchical' model (i.e., random effects model)
#
#   Occupancy:  stand-level effects:  tree farm, ownership
#               stand-year level:     Year, treatment group {control, pre-trt, post-trt}
#               plot-level effects:   downed wood count
#               random effects:       random intercept by stand
#   detection:  plot-level effects:   Trt, AT, AT^2
#               random effects:       all plot-level effects by year
# -----------------------------------------------------------------------------------------


# 
model.h1 <- function(){
  
  # priors for occupancy model
  beta0 ~ dnorm(0, 0.333)
  betaTF ~ dnorm(0, 1)
  betaYr14 ~ dnorm(0, 1)
  betaYr15 ~ dnorm(0, 1)
  betaYr16 ~ dnorm(0, 1)
  betaYr17 ~ dnorm(0, 1)
  betaYr18 ~ dnorm(0, 1)
  betaYr19 ~ dnorm(0, 1)
  betaPre ~ dnorm(0, 4) # Trt and Control sites should be similar pre-harvest
  betaPost ~ dnorm(0, 0.25)
  betaDW ~ dnorm(0, 0.25)
  sd.b0 ~ dgamma(1, 2)
  tau.b0 <- 1/(sd.b0 * sd.b0)
  
  # treatment effect estimator
  TrtEffect <- betaPost - betaPre
  
  # priors for detection random effects
  mu.a0 ~ dnorm(0, 0.333)
  mu.a1 ~ dnorm(0, 1) # Trt
  mu.a2 ~ dnorm(0, 1) # AT
  mu.a3 ~ dnorm(0, 1) # AT^2
  mu.a4 ~ dnorm(0, 1) # Trt*AT
  mu.a5 ~ dnorm(0, 1) # Trt*AT^2
  sd.a0 ~ dgamma(1, 2)
  sd.a1 ~ dgamma(2, 1) # hyper-prior for Trt effect; allow for greater prior uncertainty than other effects.
  sd.a2 ~ dgamma(1, 2)
  sd.a3 ~ dgamma(1, 2)
  sd.a4 ~ dgamma(1, 2)
  sd.a5 ~ dgamma(1, 2)
  tau.a0 <- 1/(sd.a0 * sd.a0)
  tau.a1 <- 1/(sd.a1 * sd.a1)
  tau.a2 <- 1/(sd.a2 * sd.a2)
  tau.a3 <- 1/(sd.a3 * sd.a3)
  tau.a4 <- 1/(sd.a4 * sd.a4)
  tau.a5 <- 1/(sd.a5 * sd.a5)
  
  # detection model random effects (by year)
  for(i in 1:nyear){
    a0[i] ~ dnorm(mu.a0, tau.a0)
    aTrt[i] ~ dnorm(mu.a1, tau.a1)
    aAT[i] ~ dnorm(mu.a2, tau.a2)
    aAT2[i] ~ dnorm(mu.a3, tau.a3)
    aTrtAT[i] ~ dnorm(mu.a4, tau.a4)
    aTrtAT2[i] ~ dnorm(mu.a5, tau.a5)
  }
  
  for(i in 1:nstand){
    # occupancy stand-level effects
    mu1[i] <- beta0 + betaTF*TFCL1[i] 
    mu1i[i] ~ dnorm(mu1[i], tau.b0)
    b0[i] <- mu1i[i] - mu1[i] # stand-level random effect
    for(k in 1:nyear){
      # occupancy stand-year level effects
      mu2ik[i,k] <- mu1i[i] + betaYr14*Year2014[k,i] + betaYr15*Year2015[k,i] + betaYr16*Year2016[k,i] + 
        betaYr17*Year2017[k, i] + betaYr18*Year2018[k, i] + betaYr19*Year2019[k, i] + 
        betaPre*PreTrt2[k,i] + betaPost*PostTrt2[k,i]
    }
  }
  
  for(i in 1:nall){
    # occupancy plot-level effects
    mu3ikj[i] <- mu2ik[Stand3[i], Year3[i]] + betaDW*DW3[i]
    logit(psi[i]) <- mu3ikj[i]
    z[i] ~ dbern(psi[i])
    # detection model
    logit(p[i]) <- a0[Year3[i]] + aTrt[Year3[i]]*Trt3[i] + aAT[Year3[i]]*AT3[i] + aAT2[Year3[i]]*AT3[i]*AT3[i] +
      aTrtAT[Year3[i]]*Trt3[i]*AT3[i] + aTrtAT2[Year3[i]]*Trt3[i]*AT3[i]*AT3[i]
    p.eff[i] <- z[i] * p[i]
    
    for(j in 1:nvisit){
      # likelihood
      y[i,j] ~ dbern(p.eff[i])
    }
  }
  
}





model.a1 <- function(){
  # Royle-Nichols abundance model
  
  # priors for occupancy model
  beta0 ~ dnorm(0, 0.333)
  betaTF ~ dnorm(0, 1)
  betaYr14 ~ dnorm(0, 1)
  betaYr15 ~ dnorm(0, 1)
  betaYr16 ~ dnorm(0, 1)
  betaYr17 ~ dnorm(0, 1)
  betaYr18 ~ dnorm(0, 1)
  betaYr19 ~ dnorm(0, 1)
  betaPre ~ dnorm(0, 4) # Trt and Control sites should be similar pre-harvest
  betaPost ~ dnorm(0, 0.25)
  betaDW ~ dnorm(0, 0.25)
  sd.b0 ~ dgamma(1, 2)
  tau.b0 <- 1/(sd.b0 * sd.b0)
  
  # treatment effect estimator
  TrtEffect <- betaPost - betaPre
  
  # priors for detection random effects
  mu.a0 ~ dnorm(0, 0.333)
  mu.a1 ~ dnorm(0, 1) # Trt
  mu.a2 ~ dnorm(0, 1) # AT
  mu.a3 ~ dnorm(0, 1) # AT^2
  mu.a4 ~ dnorm(0, 1) # Trt*AT
  mu.a5 ~ dnorm(0, 1) # Trt*AT^2
  sd.a0 ~ dgamma(1, 2)
  sd.a1 ~ dgamma(2, 1)
  sd.a2 ~ dgamma(1, 2)
  sd.a3 ~ dgamma(1, 2)
  sd.a4 ~ dgamma(1, 2)
  sd.a5 ~ dgamma(1, 2)
  tau.a0 <- 1/(sd.a0 * sd.a0)
  tau.a1 <- 1/(sd.a1 * sd.a1)
  tau.a2 <- 1/(sd.a2 * sd.a2)
  tau.a3 <- 1/(sd.a3 * sd.a3)
  tau.a4 <- 1/(sd.a4 * sd.a4)
  tau.a5 <- 1/(sd.a5 * sd.a5)
  
  # detection model random effects (by year)
  for(i in 1:nyear){
    a0[i] ~ dnorm(mu.a0, tau.a0)
    aTrt[i] ~ dnorm(mu.a1, tau.a1)
    aAT[i] ~ dnorm(mu.a2, tau.a2)
    aAT2[i] ~ dnorm(mu.a3, tau.a3)
    aTrtAT[i] ~ dnorm(mu.a4, tau.a4)
    aTrtAT2[i] ~ dnorm(mu.a5, tau.a5)
  }
  
  for(i in 1:nstand){
    # occupancy stand-level effects
    mu1[i] <- beta0 + betaTF*TFCL1[i] 
    mu1i[i] ~ dnorm(mu1[i], tau.b0)
    b0[i] <- mu1i[i] - mu1[i] # random effect
    for(k in 1:nyear){
      # occupancy stand-year level effects
      mu2ik[i,k] <- mu1i[i] + betaYr14*Year2014[k,i] + betaYr15*Year2015[k,i] + betaYr16*Year2016[k,i] + 
        betaYr17*Year2017[k, i] + betaYr18*Year2018[k, i] + betaYr19*Year2019[k, i] + 
        betaPre*PreTrt2[k,i] + betaPost*PostTrt2[k,i]
    }
  }
  
  for(i in 1:nall){
    # occupancy plot-level effects
    mu3ikj[i] <- mu2ik[Stand3[i], Year3[i]] + betaDW*DW3[i]
    log(lambda[i]) <- mu3ikj[i]
    N[i] ~ dpois(lambda[i])
    # detection model
    logit(theta[i]) <- a0[Year3[i]] + aTrt[Year3[i]]*Trt3[i] + aAT[Year3[i]]*AT3[i] + aAT2[Year3[i]]*AT3[i]*AT3[i] +
      aTrtAT[Year3[i]]*Trt3[i]*AT3[i] + aTrtAT2[Year3[i]]*Trt3[i]*AT3[i]*AT3[i]
    innerterm[i] <- 1 - theta[i]
    p[i] <- 1 - pow(innerterm[i], N[i])
    
    for(j in 1:nvisit){
      # likelihood
      y[i,j] ~ dbin(p[i], 1)
    }
  }
  
}
