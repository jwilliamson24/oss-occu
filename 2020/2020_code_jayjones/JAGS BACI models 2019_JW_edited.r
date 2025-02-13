#
# Project:      OSS/ENES BACI study
# Task:         Adapt 2020 occu model to run with my two years of data and five treatments
# start date:   2024.09.23
#


# -----------------------------------------------------------------------------------------
# 'hierarchical' model (i.e., random effects model)
#
#   occupancy:  stand-level effects:  tree farm, ownership
#               stand-year level:     year, treatment group 
#               plot-level effects:   downed wood count
#               random effects:       random intercept by stand
#   detection:  plot-level effects:   treatment group, AT, AT^2, interactions
#               random effects:       all plot-level effects by year
# -----------------------------------------------------------------------------------------


# 
model.h1 <- function(){
  
  # priors for occupancy model
  beta0 ~ dnorm(0, 0.333) # when do i not include vs include one of the categories?
  betaBlockM ~ dnorm(0, 1) # block: north, middle, south
  betaBlockS ~ dnorm(0, 1)
  betaOwnPB ~ dnorm(0, 1) # ownership: WY, PB, ODF, BLM
  betaOwnODF ~ dnorm(0, 1)
  betaOwnBLM ~ dnorm(0, 1)
  betaYr23 ~ dnorm(0, 1) # just 2023-2024 data
  betaYr24 ~ dnorm(0, 1)
  #betaPre ~ dnorm(0, 4) # Trt and Control sites should be similar pre-harvest
  #betaPost ~ dnorm(0, 0.25) 
  betaHarv ~ dnorm(0, 0.25) # treatments: control, harvest, burn, harvburn, salvage
  betaBurn ~ dnorm(0, 0.25) # unsure about the values for my treatment betas
  betaHarvBurn ~ dnorm(0, 0.25)
  betaSalv ~ dnorm(0, 0.25)
  betaDW ~ dnorm(0, 0.25) # dwd count
  sd.b0 ~ dgamma(1, 2) # did not change 
  tau.b0 <- 1/(sd.b0 * sd.b0) # did not change
  
  # treatment effect estimators
  SalvEffect <- betaSalv - betaBurn # did i do this right?
  BurnEffect <- betaHarvBurn - betaHarv
  
  # priors for detection random effects
  mu.a0 ~ dnorm(0, 0.333) ### need to add mu/sd/tau terms for all my trt betas, including interactions, write out model to keep track
  mu.a1 ~ dnorm(0, 1) # harv
  mu.a2 ~ dnorm(0, 1) # AT
  mu.a3 ~ dnorm(0, 1) # AT^2
  mu.a4 ~ dnorm(0, 1) # harv*AT
  mu.a5 ~ dnorm(0, 1) # harv*AT^2
  
  mu.a6 ~ dnorm(0, 1) # burn
  mu.a7 ~ dnorm(0, 1) # burn*AT
  mu.a8 ~ dnorm(0, 1) # burn*AT^2
  
  mu.a9 ~ dnorm(0, 1) # harvburn
  mu.a10 ~ dnorm(0, 1) # harvburn*AT
  mu.a11 ~ dnorm(0, 1) # harvburn*AT^2
  
  mu.a12 ~ dnorm(0, 1) # salv
  mu.a13 ~ dnorm(0, 1) # salv*AT
  mu.a14 ~ dnorm(0, 1) # salv*AT^2
  
  sd.a0 ~ dgamma(1, 2) # used in tau below; one for each alpha term
  sd.a1 ~ dgamma(2, 1) # hyper-prior for Trt effect; allow for greater prior uncertainty than other effects. what to do with this value?
  sd.a2 ~ dgamma(1, 2)
  sd.a3 ~ dgamma(1, 2)
  sd.a4 ~ dgamma(1, 2)
  sd.a5 ~ dgamma(1, 2)
  sd.a6 ~ dgamma(1, 2)
  sd.a7 ~ dgamma(1, 2)
  sd.a8 ~ dgamma(1, 2)
  sd.a9 ~ dgamma(1, 2)
  sd.a10 ~ dgamma(1, 2)
  sd.a11 ~ dgamma(1, 2)
  sd.a12 ~ dgamma(1, 2)
  sd.a13 ~ dgamma(1, 2)
  sd.a14 ~ dgamma(1, 2)
  
  tau.a0 <- 1/(sd.a0 * sd.a0) # one for each alpha term
  tau.a1 <- 1/(sd.a1 * sd.a1)
  tau.a2 <- 1/(sd.a2 * sd.a2)
  tau.a3 <- 1/(sd.a3 * sd.a3)
  tau.a4 <- 1/(sd.a4 * sd.a4)
  tau.a5 <- 1/(sd.a5 * sd.a5)
  tau.a6 <- 1/(sd.a6 * sd.a6)
  tau.a7 <- 1/(sd.a7 * sd.a7)
  tau.a8 <- 1/(sd.a8 * sd.a8)
  tau.a9 <- 1/(sd.a9 * sd.a9)
  tau.a10 <- 1/(sd.a10 * sd.a10)
  tau.a11 <- 1/(sd.a11 * sd.a11)
  tau.a12 <- 1/(sd.a12 * sd.a12)
  tau.a13 <- 1/(sd.a13 * sd.a13)
  tau.a14 <- 1/(sd.a14 * sd.a14)

  # detection model random effects (by year)
  for(i in 1:nyear){ ### use all terms from above?
    a0[i] ~ dnorm(mu.a0, tau.a0) # each plot level effect by year
    aHarv[i] ~ dnorm(mu.a1, tau.a1) # harvest
    aAT[i] ~ dnorm(mu.a2, tau.a2) # ambient temp
    aAT2[i] ~ dnorm(mu.a3, tau.a3) # ambient temp squared (quadratic)
    aHarvAT[i] ~ dnorm(mu.a4, tau.a4) # interaction
    aHarvAT2[i] ~ dnorm(mu.a5, tau.a5) # interaction
    aBurn[i] ~ dnorm(mu.a6, tau.a6) # burned
    aBurnAT[i] ~ dnorm(mu.a7, tau.a7) # interaction
    aBurnAT2[i] ~ dnorm(mu.a8, tau.a8) # interaction
    aHarvBurn[i] ~ dnorm(mu.a9, tau.a9) # harvested and burned
    aHarvBurnAT[i] ~ dnorm(mu.a10, tau.a10) # interaction
    aHarvBurnAT2[i] ~ dnorm(mu.a11, tau.a11) # interaction
    aSalv[i] ~ dnorm(mu.a12, tau.a12) # salvage logged
    aSalvAT[i] ~ dnorm(mu.a13, tau.a13) # interaction
    aSalvAT2[i] ~ dnorm(mu.a14, tau.a14) # interaction
  }
  
  for(i in 1:nstand){ 
    # occupancy stand-level effects
    mu1[i] <- beta0 + betaBlockM*BlockM[i] + betaBlockS*BlockS[i] + # added multiple block designations
      betaOwnPB*OwnPB[i] + betaOwnODF*OwnODF[i] + betaOwnBLM*OwnBLM[i] # added ownership designations
    mu1i[i] ~ dnorm(mu1[i], tau.b0)
    b0[i] <- mu1i[i] - mu1[i] 
    for(k in 1:nyear){
      # occupancy stand-year level effects
      mu2ik[i,k] <- mu1i[i] + betaYr23*Year2023[k,i] + betaYr24*Year2024[k,i] + 
        betaHarv*Harv2[k,i] + betaBurn*Burn2[k,i] + betaHarvBurn*HarvBurn2[k,i] * betaSalv*Salv2[k,i] # need to format data to match 
    } 
  }
  
  for(i in 1:nall){
    # occupancy plot-level effects
    mu3ikj[i] <- mu2ik[Stand3[i], Year3[i]] + betaDW*DW3[i]
    logit(psi[i]) <- mu3ikj[i]
    z[i] ~ dbern(psi[i])
    # full detection model
    logit(p[i]) <- a0[Year3[i]] + aHarv[Year3[i]]*Harv3[i] + aAT[Year3[i]]*AT3[i] + aAT2[Year3[i]]*AT3[i]*AT3[i] + #harv
      aHarvAT[Year3[i]]*Harv3[i]*AT3[i] + aHarvAT2[Year3[i]]*Harv3[i]*AT3[i]*AT3[i] +
      aBurn[Year3[i]]*Burn3[i] + aBurnAT[Year3[i]]*Burn3[i]*AT3[i] + aBurnAT2[Year3[i]]*Burn3[i]*AT3[i]*AT3[i] + #burn
      aHarvBurn[Year3[i]]*HarvBurn3[i] + aHarvBurnAT[Year3[i]]*HarvBurn3[i]*AT3[i] + aHarvBurnAT2[Year3[i]]*HarvBurn3[i]*AT3[i]*AT3[i] + #harvburn
      aSalv[Year3[i]]*Salv3[i] + aSalvAT[Year3[i]]*Salv3[i]*AT3[i] + aSalvAT2[Year3[i]]*Salv3[i]*AT3[i]*AT3[i] + #salv
     
    p.eff[i] <- z[i] * p[i]                 # need to make the data fit the names I gave in above equation
    
    for(j in 1:nvisit){
      # likelihood
      y[i,j] ~ dbern(p.eff[i])
    }
  }
  
}



