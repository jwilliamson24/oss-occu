#
# project:      OSS BACI
# task:         fit BACI model to 2013-2019 data (2015=first year with post-harvest data)
# start date:   2019.08.29
#
#

library(R2jags)
#library(beepr)

rm(list=ls())


# --------------------------------
# 1. read in the data
# --------------------------------


load("OSS ENES packaged data thru 2019.RData")
str(osslist)



# --------------------------------
# 2. load JAGS models
# --------------------------------

source("JAGS BACI models 2019.r")


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
  params.o <- c("beta0", "betaTF", "betaYr14", "betaYr15", "betaYr16", "betaYr17", "betaYr18", "betaYr19",
                "betaDW", "betaPre", "betaPost", "TrtEffect", 
                "mu.a0", "mu.a1", "mu.a2", "mu.a3", "mu.a4", "mu.a5", "p.eff",
                "b0", "sd.b0", "a0")
  test.data.o <- with(osslist, list(y=yo, nyear=nyear, nstand=nstand, Year2014=Year2014, Year2015=Year2015, 
                                    Year2016=Year2016, Year2017=Year2017, Year2018=Year2018, Year2019=Year2019,
                                    TFCL1=TFCL1, PreTrt2=PreTrt2, 
                                    PostTrt2=PostTrt2, DW3=(DW3-3)/2, AT3=(AT3-12)/5.5, Trt3=Trt3,
                                    nall=nall, nvisit=nvisit, Year3=Year3, Stand3=Stand3))

  # fit the model
  system.time(out.oh1 <- jags(test.data.o, inits.o, params.o, model.h1, n.chains=4,
                              n.thin=10, n.iter=1100, n.burnin=100)) # 212s
                              #n.thin=10, n.iter=11000, n.burnin=1000)) # 1968s

  # check diagnostics
  hist(out.oh1$BUGSoutput$summary[,8]) # ~all <1.05
  out.oh1$BUGSoutput$summary[rownames(out.oh1$BUGSoutput$summary) %in% c("TrtEffect", "betaPre", "betaPost", "beta0", "betaDW", "betaTF",
                                                                         "betaYr14", "betaYr15", "betaYr16", "betaYr17", "betaYr18", "betaYr19",
                                                                         "mu.a0", "mu.a1"),]
  #dont really know what this is doing
  p.eff.oh1 <- out.oh1$BUGSoutput$sims.list$p.eff
  d.oh1 <- myFun(400, y=as.numeric(osslist$yo), p=p.eff.oh1) #jw 9/18 i changed to 400 from 1000 bc error: Error in p[i, ] : subscript out of bounds
  summary(d.oh1) # 0.54 => consistent with model


# ENES

  # model setup
  zst.e <- apply(osslist$ye, 1, max, na.rm=T)
  inits.e <- function(){list(z=zst.e)}
  params.e <- c("beta0", "betaTF", "betaYr14", "betaYr15", "betaYr16", "betaYr17", "betaYr18", "betaYr19",
                "betaDW", "TrtEffect", "betaPre", "betaPost",
                "mu.a0", "mu.a1", "mu.a2", "mu.a3", "mu.a4", "mu.a5", "p.eff",
                "b0", "sd.b0", "a0")
  test.data.e <- with(osslist, list(y=ye, nyear=nyear, nstand=nstand, Year2014=Year2014, Year2015=Year2015, 
                                    Year2016=Year2016, Year2017=Year2017, Year2018=Year2018, Year2019=Year2019,
                                    TFCL1=TFCL1, PreTrt2=PreTrt2, 
                                    PostTrt2=PostTrt2, DW3=(DW3-3)/2, AT3=(AT3-12)/5.5, Trt3=Trt3,
                                    nall=nall, nvisit=nvisit, Year3=Year3, Stand3=Stand3))
  
  # fit the model
  system.time(out.eh1 <- jags(test.data.e, inits.e, params.e, model.h1, n.chains=4,
                              n.thin=10, n.iter=1100, n.burnin=100))
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
                             n.thin=10, n.iter=1100, n.burnin=100)) # s
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
                              n.thin=10, n.iter=1100, n.burnin=100)) # 229s
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

  






