# --------------------------------------------------------------------------------------------
##
## 11-jags-analysis-2324
##
## Task: Fit OSS/ENES JAGS Occupancy Model to 2023-2024 data
##
## Start Date: 02/19/2025
## Jasmine Williamson
##

## goals
# adapt Jay Jones' code to fit his jags model that i adapted to run 2023-2024 data

## insights
# cant get it to run: 
# >   system.time(out.oh1 <- jags(test.data.o, inits.o, params.o, model.h1, n.chains=4,
#                                 +                               n.thin=10, n.iter=1100, n.burnin=100)) # 49s
# Error in jags.model(model.file, data = data, inits = init.values, n.chains = n.chains,  : 
#                       
#                       Error parsing model file:
#                       syntax error on line 50 near "<-"
#                     
#                     Timing stopped at: 0.002 0 0.002

## settings --------------------------------------------------------------------------------

  library(R2jags)
  #library(beepr)
  
  rm(list=ls())




# --------------------------------
# 2. load JAGS models
# --------------------------------

source("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/2020/2020_code_jayjones/JAGS BACI models 2019_JW_edited.r")
#source("JAGS BACI models 2019.r")
  
  
# --------------------------------
# 1. read in the data
# --------------------------------
  
load("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/LS-jas data wrangling/output_files/oss_enes_data_packaged_jw.RData")
#load("OSS ENES packaged data thru 2019.RData")
str(osslist)
  
  
# --------------------------------
# 3. fit the 'hierarchical' model
# --------------------------------

# diagnostic check
myFun <- function(n, y, p){
  out <- matrix(ncol=3, nrow=n)
  miss <- is.na(y)
  for(i in 1:n){
    out[i,1] <- sum(((as.numeric(y) - rep(p[i,], 3))^2)[!miss]) # 253
    out[i,2] <- sum(((rbinom(length(miss), 1, rep(p[i,], 3)) - rep(p[i,], 3))^2)[!miss]) # 252.2
  }
  out[,3] <- ifelse(out[,1] > out[,2], 1, 0)
  out
}



# OSS

  # model setup
  zst.o <- apply(osslist$yo, 1, max, na.rm=T)
  inits.o <- function(){list(z=zst.o)}
  params.o <- c("beta0", "betaTFCL", "betaTFNC", "betaOWPB", "betaOWODF", "betaOWBLM", "betaYr24", "betaHU",
                "betaBU", "betaHB", "betaBS", "betaDW", "SalvEffect", "BurnEffect",
                "mu.a0", "mu.a1", "mu.a2", "mu.a3", "mu.a4", "mu.a5", "mu.a6", "mu.a7", "mu.a8", "mu.a9", "mu.a10",
                "mu.a11", "mu.a12", "mu.a13", "mu.a14", "p.eff", "b0", "sd.b0", "a0")
  
  ### THIS NEEDS TO BE UPDATED
  ### Add in the level 3 treatment variables (HU3, etc)
  test.data.o <- with(osslist, list(y=yo, nyear=nyear, nstand=nstand, Year2024=Year2024, TFCL1=TFCL1, TFNC1=TFNC1, 
                                    HB2=HB2, BS2=BS2, HU2=HU2, BU2=BU2, DW3=(DW3-3)/2, AT3=(AT3-12)/5.5, Trt3=TrtGrp3, #what is he doing here with these calculations?
                                    nall=nall, nvisit=nvisit, Year3=Year3, Stand3=Stand3))

  # fit the model
  system.time(out.oh1 <- jags(test.data.o, inits.o, params.o, model.h1, n.chains=4,
                              n.thin=10, n.iter=1100, n.burnin=100)) # 49s
                              #n.thin=10, n.iter=11000, n.burnin=1000)) # 1968s

  # check diagnostics
  hist(out.oh1$BUGSoutput$summary[,8]) # ~all <1.05
  out.oh1$BUGSoutput$summary[rownames(out.oh1$BUGSoutput$summary) %in% c("betaBU", "betaHB", "betaBS", "betaDW", "SalvEffect", "BurnEffect",
                                                                         "mu.a0", "mu.a1"),]
  #dont really know what this is doing
  p.eff.oh1 <- out.oh1$BUGSoutput$sims.list$p.eff
  d.oh1 <- myFun(400, y=as.numeric(osslist$yo), p=p.eff.oh1) #jw 9/18 i changed to 400 from 1000 bc error: Error in p[i, ] : subscript out of bounds
  summary(d.oh1) # 0.54 => consistent with model

  
  
# edited code to here
  
  
  
# ENES

  # model setup
  zst.e <- apply(osslist$ye, 1, max, na.rm=T)
  inits.e <- function(){list(z=zst.e)}
  params.e <- c("beta0", "betaTF", "betaYr14", "betaYr15", "betaYr16", "betaYr17", "betaYr18", "betaYr19",
                "betaDW", "TrtEffect", "betaPre", "betaPost",
                "mu.a0", "mu.a1", "mu.a2", "mu.a3", "mu.a4", "mu.a5", "p.eff",
                "b0", "sd.b0", "a0")
  
  ### THIS NEEDS TO BE UPDATED
  test.data.e <- with(osslist, list(y=ye, nyear=nyear, nstand=nstand, Year2014=Year2014, Year2015=Year2015, 
                                    Year2016=Year2016, Year2017=Year2017, Year2018=Year2018, Year2019=Year2019,
                                    TFCL1=TFCL1, PreTrt2=PreTrt2, 
                                    PostTrt2=PostTrt2, DW3=(DW3-3)/2, AT3=(AT3-12)/5.5, Trt3=Trt3,
                                    nall=nall, nvisit=nvisit, Year3=Year3, Stand3=Stand3))
  
  # fit the model
  system.time(out.eh1 <- jags(test.data.e, inits.e, params.e, model.h1, n.chains=4,
                              n.thin=10, n.iter=1100, n.burnin=100)) #47s
                              #n.thin=10, n.iter=11000, n.burnin=1000)) # 1864s

  # diagnostic checks
  out.eh1$BUGSoutput$summary[rownames(out.eh1$BUGSoutput$summary) %in% c("TrtEffect", "betaPre", "betaPost", "beta0", "betaDW", "betaTF",
                                                                         "betaYr14", "betaYr15", "betaYr16", "betaYr17", "betaYr18", "betaYr19",
                                                                         "mu.a0", "mu.a1"),]
  hist(out.eh1$BUGSoutput$summary[,8]) # small fraction (<1%?) gt than 1.05

  p.eff.eh1 <- out.eh1$BUGSoutput$sims.list$p.eff
  d.eh1 <- myFun(400, y=as.numeric(osslist$ye), p=p.eff.eh1) #jw 9/18 i changed to 400 from 1000
  summary(d.eh1) # 0.54 => consistent with model
  

# --------------------------------
# 4. Fit the abundance models  
# --------------------------------

# OSS

  # model setup
  zst.o <- apply(osslist$yo, 1, max, na.rm=T)
  inits.o <- function(){list(N=zst.o)}
  params.o <- c("beta0", "betaTF", "betaYr14", "betaYr15", "betaYr16", "betaYr17", "betaYr18", "betaYr19",
                "betaDW", "betaPre", "betaPost", "TrtEffect", 
                "mu.a0", "mu.a1", "mu.a2", "mu.a3", "mu.a4", "mu.a5",
                "a0", "aTrt", "aAT", "aAT2", "aTrtAT", "aTrtAT2",
                "N", "theta", "p", "b0", "sd.b0")
  test.data.o <- with(osslist, list(y=yo, nyear=nyear, nstand=nstand, Year2014=Year2014, Year2015=Year2015, 
                                    Year2016=Year2016, Year2017=Year2017, Year2018=Year2018, Year2019=Year2019,
                                    TFCL1=TFCL1, PreTrt2=PreTrt2, 
                                    PostTrt2=PostTrt2, DW3=(DW3-3)/2, AT3=(AT3-12)/5.5, Trt3=Trt3,
                                    nall=nall, nvisit=nvisit, Year3=Year3, Stand3=Stand3))

  # fit the model  
  system.time(out.oa1 <- jags(test.data.o, inits.o, params.o, model.a1, n.chains=4,
                             n.thin=10, n.iter=1100, n.burnin=100)) # 104s
                             #n.thin=10, n.iter=11000, n.burnin=1000)) # 4000s
                             #n.thin=20, n.iter=42000, n.burnin=2000)) # s
  hist(out.oa1$BUGSoutput$summary[,8]) # small fraction (<1%?) less than 1.05
  #rownames(out.oa1$BUGSoutput$summary[out.oa1$BUGSoutput$summary[,8]>1.1,])
  out.oa1$BUGSoutput$summary[rownames(out.oa1$BUGSoutput$summary) %in% c("TrtEffect", "betaPre", "betaPost", "betaDW", "betaTF",
                                                                       "betaYr14", "betaYr15", "betaYr16", "betaYr17", "betaYr18", "betaYr19",
                                                                       "mu.a0", "mu.a1"),]
  #beep(sound=8)

  # diagnostic check  
  p.eff.oa1 <- out.oa1$BUGSoutput$sims.list$p
  d.oa1 <- myFun(400, y=as.numeric(osslist$yo), p=p.eff.oa1)
  summary(d.oa1) # 0.55
  

# ENES
  
  # model setup
  Nst.e <- apply(osslist$ye, 1, max, na.rm=T)
  inits.e <- function(){list(N=Nst.e)}
  params.e <- c("beta0", "betaTF", "betaYr14", "betaYr15", "betaYr16", "betaYr17", "betaYr18", "betaYr19",
                "betaDW", "betaPre", "betaPost", "TrtEffect", 
                "mu.a0", "mu.a1", "mu.a2", "mu.a3", "mu.a4", "mu.a5",
                "a0", "aTrt", "aAT", "aAT2", "aTrtAT", "aTrtAT2",
                "N", "theta", "p", "b0", "sd.b0")
  test.data.e <- with(osslist, list(y=ye, nyear=nyear, nstand=nstand, Year2014=Year2014, Year2015=Year2015, 
                                    Year2016=Year2016, Year2017=Year2017, Year2018=Year2018, Year2019=Year2019,
                                    TFCL1=TFCL1, PreTrt2=PreTrt2, 
                                    PostTrt2=PostTrt2, DW3=(DW3-3)/2, AT3=(AT3-12)/5.5, Trt3=Trt3,
                                    nall=nall, nvisit=nvisit, Year3=Year3, Stand3=Stand3))
  
  # fit the model  
  system.time(out.ea1 <- jags(test.data.e, inits.e, params.e, model.a1, n.chains=4,
                              n.thin=10, n.iter=1100, n.burnin=100)) # 101s
                              #n.thin=10, n.iter=11000, n.burnin=1000))
                              #n.thin=20, n.iter=42000, n.burnin=2000)) # s
  hist(out.ea1$BUGSoutput$summary[,8]) # small fraction (<1%?) less than 1.05
  #rownames(out.ea1$BUGSoutput$summary[out.ea1$BUGSoutput$summary[,8]>1.1,])
  out.ea1$BUGSoutput$summary[rownames(out.ea1$BUGSoutput$summary) %in% c("TrtEffect", "betaPre", "betaPost", "betaDW", "betaTF",
                                                                         "betaYr14", "betaYr15", "betaYr16", "betaYr17", "betaYr18", "betaYr19",
                                                                         "mu.a0", "mu.a1"),]
  
  # diagnostic check  
  p.eff.ea1 <- out.ea1$BUGSoutput$sims.list$p
  d.ea1 <- myFun(400, y=as.numeric(osslist$ye), p=p.eff.ea1)
  summary(d.ea1) # 0.58
  
  
# --------------------------
# 5. export model objects
# --------------------------
  
  save(out.oh1, out.eh1, out.oa1, out.ea1, file="Model objects.RData")

  






