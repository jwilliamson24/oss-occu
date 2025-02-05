# -------------------------------------------------------------------------------------------------------
##
## 06-rework-nimble-models.R 
##
## Rerun the nimble occupancy models from 2023 bayesian class
##
## Jasmine Williamson
## Date Created: 07-10-2024
##
## 

## settings -----------------------------------------------------------------------------------------------

  rm(list=ls())
  #setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")
  setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
  library(tidyr)
  library(nimble)
  library(ggplot2)
  library(data.table)
  library(tidyverse)
  library(mcmcplots)
  library(MCMCvis)
  library(boot)
  source('attach.nimble_v2.R')


## saved model outputs -----------------------------------------------------------------------------------

# all species model:
# load("./all.spp_model_2.RData")

# oss model:
# load("./oss_model.RData")

# enes model:
# load("./enes_model.RData")


## load Data--------------------------------------------------------------------------------------------------

    site <- read.csv("site.complete.csv")
    subplot <- read.csv("subplot.complete.csv")
    sals <- read.csv("sals.complete.csv", 
                     colClasses = c(landowner="factor", stand="character", trt="factor",
                                    obs="factor", subplot="factor", recap="factor",
                                    pass="factor", spp="factor", cover_obj="factor", 
                                    substrate="factor", age_class="factor"))
    
    oss.dets <- read.csv("oss.occu.wide.csv")
    enes.dets <- read.csv("enes.occu.wide.csv")
    
    all.long <- read.csv("all.occu.long.csv")
    oss.long <- read.csv("oss.occu.long.csv")
    enes.long <- read.csv("enes.occu.long.csv")

    # add df with precip data
    env_subset_corr <- read.csv("env_subset_corr.csv")


## format data --------------------------------------------------------------------------------------------------
# we need two parts: one subplot-level matrix, and one site-level matrix


#### subplot-level matrix:
# "date" "site" "temp" "humidity" "soil.moist" "all.obs" "oss.obs" "enes.obs" 
    
    # subset and merge the various dataframes
    subplot_subset <- subset(subplot[,c(8,1,9,18)]) #subset subplot variables
    
    df1 <- subset(site[,c(1,10,11)]) #subset site variables
    
    precip <- subset(env_subset_corr[,c(1,14)]) #subset precip to just site id and days since rain
    colnames(precip)[1] <- "site_id"
    
    #merge site subset with precip subset
    df2 <- full_join(df1,precip,by="site_id")
    
    dfmerge <- full_join(subplot_subset,df2,by="site_id") #merge site and subplot
    #prev line joins temp and hum to subplot matrix, which automatically repeats the 
    #temp and hum in blank subplot cells from same site id for both years because
    #its currently only listed for each site
    
    #rename sal columns
    colnames(oss.long)[3] <- "oss.obs"
    colnames(enes.long)[3] <- "enes.obs"
    colnames(all.long)[3] <- "all.obs"
    
    #merge sal columns with env variables
    merge1 <- merge(dfmerge, all.long, by=c("site_id","subplot"))
    merge1 <- merge(merge1, oss.long, by=c("site_id","subplot"))
    complete.merge <- merge(merge1,enes.long,by=c("site_id","subplot"))
    
    colnames(complete.merge) <- c("site","subplot","date","soil.moist","temp",
                                  "humidity","days.since.rain","all.obs","oss.obs","enes.obs")


#### site-level matrix:
# "site" and "treatment
    site_subset <- subset(site[,c(1,5)])
    site_subset$trt <- as.factor(site_subset$trt)
    site_subset$trt <- as.numeric(site_subset$trt)
    table(site_subset$trt)
    colnames(site_subset)[2] <- "treatment"

#  1 = BS     
#  2 = BU     
#  3 = HB     
#  4 = HU     
#  5 = UU   


## name data for model --------------------------------------------------------------------------------------------------
    data <- complete.merge
    data$all.obs <- as.numeric(data$all.obs)
    data$oss.obs <- as.numeric(data$oss.obs)
    data$enes.obs <- as.numeric(data$enes.obs)
    
    data2 <- site_subset
    table(data2$treatment)
    
    scaled_temp <- c(scale(data$temp))

#how to treat days since rain as detection covariate? priors?    
    ggplot(complete.merge, aes(x = days.since.rain, y = oss.obs)) +
      geom_point(color = "blue", size = 2) +  
      geom_smooth(method = "lm", color = "red", se = TRUE) 
    
## ALL SPP MODEL ------------------------------------------------------------------------------------------------

all.spp.model.2 <- nimbleCode ({
  
  # Priors 
  # uninformative, vague priors

  for(t in 1:n.treatments){
    TreatmentIntercept[t] ~ dunif(-10,10)
  }#t
  
  DetectionIntercept ~ dunif(-5,5) # different detection intercept for each treatment? but leave betas the same, same shape. 1/30
  betaTemp ~ dunif(-5, 5)
  betaTemp2 ~ dunif(-5, 0)
  
  # Likelihood
  
  # Process/Biological model = Occupancy
  # need two pieces: one defining psi(occu prob coeff) and covariates, and one defining z dist
  for(i in 1:n.sites) {
      logit(psi[i]) <- TreatmentIntercept[treatment[i]]  #psi=occupancy probability
      z[i] ~ dbern(psi[i])  # z=1 if occupied, z=latent true occupancy
  }#i
  
  # Observation model = Detection
  # need two pieces: one for p(det prob coeff) and one defining Y distribution
  for(j in 1:n.obs) {
      logit(p[j]) <- DetectionIntercept + betaTemp*temp[j] + betaTemp2*temp[j]^2 
      #using temp as covariate with a quadratic relationship
      #p=detection probability for site i and survey j
      Y[j] ~ dbern(p[j] * z[site[j]]) #Y=my actual data observations
      #z=1 or 0, turns this on or off
  }#j

})

# Parameters monitored
parameters <- c("z","p","TreatmentIntercept","DetectionIntercept","betaTemp", "betaTemp2")

# MCMC Settings
ni <- 40000
nt <- 40
nb <- 20000
nc <- 3

# Data
nimble.data = list(Y=data$all.obs,
                   temp=scaled_temp)

nimble.constants = list(n.sites = length(unique(data$site)),
                        n.treatments = length(unique(data2$treatment)),
                        treatment=data2$treatment,
                        site=as.numeric(as.factor(data$site)),
                        n.obs = length(data$all.obs))

mcmc.output.2 <- nimbleMCMC(code = all.spp.model.2,
                          data = nimble.data,
                          constants=nimble.constants,
                          monitors = parameters,
                          niter = ni,
                          nburnin = nb,
                          nchains = nc,
                          thin=nt,
                          summary=TRUE,
                          samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output.2$samples)
save(mcmc.output.2, file="./all.spp_model_2.RData")
load("./all.spp_model_2.RData")


## assessing convergence------------------------------------------------------------------------------------------------------
#mcmcplot(mcmc.output.1$samples)

# Gelman-Rubin diagnostic (AKA RHat or PSRF)
z <- mcmc.output.2$samples
g <- matrix(NA, nrow=nvar(z), ncol=2)
for (v in 1:nvar(z)) { g[v,] <- gelman.diag(z[,v])$psrf }
PSRF <- bind_cols(colnames(z$chain1),g) %>% rename(Parameter = ...1 ,PSRF = ...2 ,PSRFUpperCI = ...3) 

PSRF # Values are below 1.05, so that's good


## interpreting model outputs----------------------------------------------------------------------------------------------------
#z

# Inverse logit the detection intercept to get detection probabilities
det.probs.inv <- inv.logit(DetectionIntercept)
hist(det.probs.inv)

# Looking at trace plots and parameter estimates
MCMCtrace(object = mcmc.output.1$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = c("DetectionIntercept", "betaTemp", "TreatmentIntercept"))
#this looks good:
#caterpillars on traceplot are mostly lined up/overlapping
#parameter estimate lines on density plot are mostly overlapping


mean(det.probs.inv) # = 0.3654191
mean(det.probs.inv>0)  # = 1
median(det.probs.inv)  # = 0.3651926
boxplot(det.probs.inv)

# Inv logit TreatmentIntercept to get Occupancy Estimates
trt.int.inv <- inv.logit(TreatmentIntercept)
median(trt.int.inv[,1]) # 0.8125127    BS
median(trt.int.inv[,2]) # 0.8454782    BU
median(trt.int.inv[,3]) # 0.6822505    HB
median(trt.int.inv[,4]) # 0.7557727    HU
median(trt.int.inv[,5]) # 0.8389480    UU



## base R boxplot for treatment effect -----------------------------------------------------------------------------------------------

# Renaming and reordering the treatment intercepts for the boxplot
# treatment_matrix <- TreatmentIntercept
trt.int.inv <- inv.logit(TreatmentIntercept)
treatment_matrix <- trt.int.inv # Using the inv logit treatment estimates

new.names <- c("Salvage Logged", "Wildfire", "Harvest, Wildfire", "Harvest", "Control")
colnames(treatment_matrix) <- new.names
desired.order <- c("Control", "Wildfire", "Harvest, Wildfire", "Harvest", "Salvage Logged")

box.colors <- c('lightgreen','steelblue', 'coral2', '#f9d62e', '#b967ff' )

#boxplot of Treatment Estimates - occu prob for each treatment
boxplot(treatment_matrix[, match(desired.order, colnames(treatment_matrix))], 
        main = "Treatment Intercepts for All Species", 
        xlab = "Treatment", ylab = "Occupancy Probability",
        col = box.colors)


## ggplot boxplot treatment effect -----------------------------------------------------------------------------------------------

#define treatments using the inv logit treatment estimates
treatment_matrix <- trt.int.inv 
#treatments are numbered, need to give them names
new.names <- c("Salvage Logged", "Wildfire", "Harvest, Wildfire", "Harvest", "Control") 
colnames(treatment_matrix) <- new.names


treatment_matrix <- as.data.frame(treatment_matrix)
#reshape data to long format bc ggplot required
long_format <- gather(treatment_matrix, key = "Treatment", value = "Occupancy_Probability")

#specify treatment order for plot
long_format$Treatment <- factor(long_format$Treatment, 
                                levels = c("Control", "Wildfire", "Harvest, Wildfire", "Harvest", "Salvage Logged")) # Replace "DesiredOrder", "Treat1", etc., with your actual treatment names in the desired order


box.colors <- c('lightgreen','steelblue', 'coral2', '#f9d62e', '#b967ff' )

p <- ggplot(long_format, aes(x = Treatment, y = Occupancy_Probability, fill = Treatment)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = box.colors) +
  labs(title = "Treatment Intercepts for All Species", x = "Treatment", y = "Occupancy Probability") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

ggsave(filename = "boxplot_trt_occu_prob_nimble.png", plot = p, device = "png", 
       path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/figures/06-rework-nimble-models",
       width = 10, height = 6, units = "in", dpi = 300)


## OSS MODEL ------------------------------------------------------------------------------------------------

oss.model <- nimbleCode ({
  
  # Priors 
  # uninformative, vague priors
  
  for(t in 1:n.treatments){
    TreatmentIntercept[t] ~ dunif(-10,10)
    DetectionIntercept[t] ~ dunif(-5,5)
  }#t
  
  betaTemp ~ dunif(-5, 5)
  betaTemp2 ~ dunif(-5, 0)
  betaRain ~ dnorm(-1, 1) #days since rain and occu have slight negative relationship,
  # set prior as normal distribution with a negative mean (mu = -1) and a moderate stdev (sd = 1).
  
  # Likelihood
  
  # Process/Biological model = Occupancy
  # need two pieces: one defining psi(occu prob coeff) and covariates, and one defining z dist
  for(i in 1:n.sites) {
    logit(psi[i]) <- TreatmentIntercept[treatment[i]]  #psi=occupancy probability
    z[i] ~ dbern(psi[i])  # z=1 if occupied, z=latent true occupancy
  }#i
  
  # Observation model = Detection
  # need two pieces: one for p(det prob coeff) and one defining Y distribution
  for(j in 1:n.obs) {
    logit(p[j]) <- DetectionIntercept[treatment[site[j]]] + 
                   betaTemp*temp[j] + 
                   betaTemp2*temp[j]^2 +
                   betaRain*days.since.rain[j]
    #using temp as covariate with a quadratic relationship
    #p=detection probability for site i and survey j
    Y[j] ~ dbern(p[j] * z[site[j]]) #Y=my actual data observations
    #z=1 or 0, turns this on or off
  }#j
  
})

# Parameters monitored
parameters <- c("z","p","TreatmentIntercept","DetectionIntercept","betaTemp", "betaTemp2", "betaRain")

# MCMC Settings
ni <- 40000
nt <- 40
nb <- 20000
nc <- 3

# Data
nimble.data = list(Y=data$oss.obs,
                   temp=scaled_temp,
                   days.since.rain=data$days.since.rain)

nimble.constants = list(n.sites = length(unique(data$site)),
                        n.treatments = length(unique(data2$treatment)),
                        treatment=data2$treatment,
                        site=as.numeric(as.factor(data$site)),
                        n.obs = length(data$all.obs))

mcmc.output.3 <- nimbleMCMC(code = oss.model,
                            data = nimble.data,
                            constants=nimble.constants,
                            monitors = parameters,
                            niter = ni,
                            nburnin = nb,
                            nchains = nc,
                            thin=nt,
                            summary=TRUE,
                            samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output.3$samples)
save(mcmc.output.3, file="./oss_model.RData")
load("./oss_model.RData")


## assessing convergence------------------------------------------------------------------------------------------------------
#mcmcplot(mcmc.output.1$samples)

# Gelman-Rubin diagnostic (AKA RHat or PSRF)
z <- mcmc.output.3$samples
g <- matrix(NA, nrow=nvar(z), ncol=2)
for (v in 1:nvar(z)) { g[v,] <- gelman.diag(z[,v])$psrf }
PSRF <- bind_cols(colnames(z$chain1),g) %>% rename(Parameter = ...1 ,PSRF = ...2 ,PSRFUpperCI = ...3) 

PSRF # Values are mostly below 1.05 (good), but some are over


## name model outputs specific to OSS ------------------------------------------------------------------------------------
    
    oss.det.prob <- inv.logit(DetectionIntercept)
    oss.occu.prob <- inv.logit(TreatmentIntercept)


## interpreting model outputs----------------------------------------------------------------------------------------------------
#z

# Inverse logit the detection intercept to get detection probabilities
det.probs.inv <- inv.logit(DetectionIntercept)
hist(det.probs.inv)

# Looking at trace plots and parameter estimates
MCMCtrace(object = mcmc.output.3$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = c("DetectionIntercept", "betaTemp", "TreatmentIntercept"))
#this looks good:
#caterpillars on traceplot are mostly lined up/overlapping
#parameter estimate lines on density plot are mostly overlapping

#unsure about trtint 1 & 3 trace plots

## detection and occupancy estimates ------------------------------------------------------------

mean(det.probs.inv) # = 0.26108
mean(det.probs.inv>0)  # = 1
median(det.probs.inv)  # = 0.2597028
boxplot(det.probs.inv)

# Median Detection Estimates
median(det.probs.inv[,1]) # 0.3240405   BS 
median(det.probs.inv[,2]) # 0.2747833    BU
median(det.probs.inv[,3]) # 0.2573965    HB
median(det.probs.inv[,4]) # 0.2087794    HU
median(det.probs.inv[,5]) # 0.2392232    UU

# Inv logit TreatmentIntercept to get Occupancy Estimates

# occu estimates before adding separate detection intercepts
trt.int.inv <- inv.logit(TreatmentIntercept)
median(trt.int.inv[,1]) # 0.6392119    BS 
median(trt.int.inv[,2]) # 0.8090534    BU
median(trt.int.inv[,3]) # 0.5784290    HB
median(trt.int.inv[,4]) # 0.6915276    HU
median(trt.int.inv[,5]) # 0.9920444    UU

# occu estimates before adding separate detection intercepts
trt.int.inv <- inv.logit(TreatmentIntercept)
median(trt.int.inv[,1]) # 0.5962445    BS 
median(trt.int.inv[,2]) # 0.7885033    BU
median(trt.int.inv[,3]) # 0.5864983    HB
median(trt.int.inv[,4]) # 0.8241218    HU
median(trt.int.inv[,5]) # 0.9940393    UU

# median occupancy probabilities are:
#   
# BS: 59.6%
# BU: 78.9%
# HB: 58.6%
# HU: 82.4%
# UU: 99.4%

CI_BS <- quantile(trt.int.inv[,1], probs = c(0.025, 0.975))  
CI_BU <- quantile(trt.int.inv[,2], probs = c(0.025, 0.975))  
CI_HB <- quantile(trt.int.inv[,3], probs = c(0.025, 0.975))   
CI_HU <- quantile(trt.int.inv[,4], probs = c(0.025, 0.975))   
CI_UU <- quantile(trt.int.inv[,5], probs = c(0.025, 0.975))  



## calculations for abstract writing
 
percentage_difference_lower <- ((CI_UU[1] - CI_HB[1]) / CI_UU[1]) * 100
percentage_difference_upper <- ((CI_UU[2] - CI_HB[2]) / CI_UU[2]) * 100



## base R boxplot for treatment effect -----------------------------------------------------------------------------------------------

# Renaming and reordering the treatment intercepts for the boxplot
# treatment_matrix <- TreatmentIntercept
trt.int.inv <- inv.logit(TreatmentIntercept)
treatment_matrix <- trt.int.inv # Using the inv logit treatment estimates

new.names <- c("Salvage Logged", "Wildfire", "Harvest, Wildfire", "Harvest", "Control")
colnames(treatment_matrix) <- new.names
desired.order <- c("Control", "Wildfire", "Harvest, Wildfire", "Harvest", "Salvage Logged")

box.colors <- c('lightgreen','steelblue', 'coral2', '#f9d62e', '#b967ff' )

#boxplot of Treatment Estimates - occu prob for each treatment
boxplot(treatment_matrix[, match(desired.order, colnames(treatment_matrix))], 
        main = "Treatment Intercepts for All Species", 
        xlab = "Treatment", ylab = "Occupancy Probability",
        col = box.colors)


## ggplot boxplot treatment effect -----------------------------------------------------------------------------------------------

new.names <- c("Salvage Logged", "Wildfire", "Harvest, Wildfire", "Harvest", "Control") #treatments are numbered, need to give them names
colnames(oss.occu.prob) <- new.names
oss.occu.prob <- as.data.frame(oss.occu.prob)

oss.long <- gather(oss.occu.prob, key = "Treatment", value = "Occupancy_Probability") #reshape data to long format bc ggplot required

oss.long$Treatment <- factor(oss.long$Treatment,#specify treatment order for plot
                                levels = c("Control", "Wildfire", "Harvest, Wildfire", "Harvest", "Salvage Logged")) # Replace "DesiredOrder", "Treat1", etc., with your actual treatment names in the desired order


box.colors <- c('lightgreen','steelblue', 'coral2', '#f9d62e', '#b967ff' )

p <- ggplot(oss.long, aes(x = Treatment, y = Occupancy_Probability, fill = Treatment)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = box.colors) +
  labs(title = "Treatment Intercepts for OSS", x = "Treatment", y = "Occupancy Probability") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

ggsave(filename = "oss_trt_occu_prob_0204.png", plot = p, device = "png", 
       path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/figures/06-rework-nimble-models",
       width = 10, height = 6, units = "in", dpi = 300)


## ENES MODEL ------------------------------------------------------------------------------------------------

enes.model <- nimbleCode ({
  
  # Priors 
  # uninformative, vague priors
  
  for(t in 1:n.treatments){
    TreatmentIntercept[t] ~ dunif(-10,10)
    DetectionIntercept[t] ~ dunif(-5,5)
  }#t
  
  betaTemp ~ dunif(-5, 5)
  betaTemp2 ~ dunif(-5, 0)
  betaRain ~ dnorm(-1, 1)
  
  # Likelihood
  
  # Process/Biological model = Occupancy
  # need two pieces: one defining psi(occu prob coeff) and covariates, and one defining z dist
  for(i in 1:n.sites) {
    logit(psi[i]) <- TreatmentIntercept[treatment[i]]  #psi=occupancy probability
    z[i] ~ dbern(psi[i])  # z=1 if occupied, z=latent true occupancy
  }#i
  
  # Observation model = Detection
  # need two pieces: one for p(det prob coeff) and one defining Y distribution
  for(j in 1:n.obs) {
    logit(p[j]) <- DetectionIntercept[treatment[site[j]]] + 
                    betaTemp*temp[j] + 
                    betaTemp2*temp[j]^2 +
                    betaRain*days.since.rain[j]
    #using temp as covariate with a quadratic relationship
    #p=detection probability for site i and survey j
    Y[j] ~ dbern(p[j] * z[site[j]]) #Y=my actual data observations
    #z=1 or 0, turns this on or off
  }#j
  
})

# Parameters monitored
parameters <- c("z","p","TreatmentIntercept","DetectionIntercept","betaTemp", "betaTemp2", "betaRain")

# MCMC Settings
ni <- 80000
nt <- 80
nb <- 40000
nc <- 3

# Data
nimble.data = list(Y=data$enes.obs,
                   temp=scaled_temp,
                   days.since.rain=data$days.since.rain)

nimble.constants = list(n.sites = length(unique(data$site)),
                        n.treatments = length(unique(data2$treatment)),
                        treatment=data2$treatment,
                        site=as.numeric(as.factor(data$site)),
                        n.obs = length(data$all.obs))

mcmc.output.4 <- nimbleMCMC(code = enes.model,
                            data = nimble.data,
                            constants=nimble.constants,
                            monitors = parameters,
                            niter = ni,
                            nburnin = nb,
                            nchains = nc,
                            thin=nt,
                            summary=TRUE,
                            samplesAsCodaMCMC = TRUE)

#######     warning: logProb of data node Y[724]: logProb is -Inf. what does this mean??


attach.nimble(mcmc.output.4$samples)
save(mcmc.output.4, file="./enes_model.RData")
load("./enes_model.RData")


## Assessing Convergence------------------------------------------------------------------------------------------------------

# mcmcplot(mcmc.output.4$samples)

# Gelman-Rubin diagnostic (AKA RHat or PSRF)
z <- mcmc.output.4$samples
g <- matrix(NA, nrow=nvar(z), ncol=2)
for (v in 1:nvar(z)) { g[v,] <- gelman.diag(z[,v])$psrf }
PSRF <- bind_cols(colnames(z$chain1),g) %>% rename(Parameter = ...1 ,PSRF = ...2 ,PSRFUpperCI = ...3) 

PSRF # some values were 1.05 and above, but i dont know what that means. 
# fixed that after I changed iterations from 40000 to 80000

# ACF plots
acf(DetectionIntercept)
acf(TreatmentIntercept)

## name model outputs specific to OSS ------------------------------------------------------------------------------------

    enes.det.prob <- inv.logit(DetectionIntercept)
    enes.occu.prob <- inv.logit(TreatmentIntercept)

## Interpreting Model Outputs----------------------------------------------------------------------------------------------------
#z

# Inverse logit the detection intercept to get detection probabilities
det.probs.inv <- inv.logit(DetectionIntercept)
hist(det.probs.inv)

# Looking at trace plots and parameter estimates
MCMCtrace(object = mcmc.output.4$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = c("DetectionIntercept", "betaTemp", "TreatmentIntercept"))
#treatment 2 and 3 caterpillars dont look great


mean(det.probs.inv) # = 0.2403386
mean(det.probs.inv>0)  # = 1
median(det.probs.inv)  # = 0.2394667
boxplot(det.probs.inv)

# Inv logit TreatmentIntercept to get Occupancy Estimates
trt.int.inv <- inv.logit(TreatmentIntercept)
median(trt.int.inv[,1]) # 0.6271816    BS
median(trt.int.inv[,2]) # 0.5330411    BU
median(trt.int.inv[,3]) # 0.4669251    HB
median(trt.int.inv[,4]) # 0.5864338    HU
median(trt.int.inv[,5]) # 0.5549356    UU



## base R boxplot for treatment effect -----------------------------------------------------------------------------------------------

# Renaming and reordering the treatment intercepts for the boxplot
trt.int.inv2 <- inv.logit(TreatmentIntercept)
treatment_matrix2 <- trt.int.inv2 # Using the inv logit treatment estimates

new.names <- c("Salvage Logged", "Wildfire", "Harvest, Wildfire", "Harvest", "Control")
colnames(treatment_matrix2) <- new.names
desired.order <- c("Control", "Wildfire", "Harvest, Wildfire", "Harvest", "Salvage Logged")

box.colors <- c('lightgreen','steelblue', 'coral2', '#f9d62e', '#b967ff' )

#boxplot of Treatment Estimates - occu prob for each treatment
boxplot(treatment_matrix2[, match(desired.order, colnames(treatment_matrix2))], 
        main = "Treatment Intercepts for All Species", 
        xlab = "Treatment", ylab = "Occupancy Probability",
        col = box.colors)


## ggplot boxplot treatment effect -----------------------------------------------------------------------------------------------

new.names <- c("Salvage Logged", "Wildfire", "Harvest, Wildfire", "Harvest", "Control") 
colnames(enes.occu.prob) <- new.names
enes.occu.prob <- as.data.frame(enes.occu.prob)

enes.long <- gather(enes.occu.prob, key = "Treatment", value = "Occupancy_Probability") #reshape data to long format bc ggplot required

#specify treatment order for plot
enes.long$Treatment <- factor(enes.long$Treatment, 
                                levels = c("Control", "Wildfire", "Harvest, Wildfire", "Harvest", "Salvage Logged")) # Replace "DesiredOrder", "Treat1", etc., with your actual treatment names in the desired order


box.colors <- c('lightgreen','steelblue', 'coral2', '#f9d62e', '#b967ff' )

p2 <- ggplot(enes.long, aes(x = Treatment, y = Occupancy_Probability, fill = Treatment)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = box.colors) +
  labs(title = "Treatment Intercepts for ENES", x = "Treatment", y = "Occupancy Probability") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

ggsave(filename = "enes_trt_occu_prob.png", plot = p2, device = "png", 
       path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/figures/06-rework-nimble-models",
       width = 10, height = 6, units = "in", dpi = 300)


## ggplot with both species -----------------------------------------------------------------------------------------------


## Combine both datasets

  oss.long$spp <- "OSS"
  enes.long$spp <- "ENES"
  combined_data <- rbind(enes.long, oss.long)
  
  
## boxplot

  p3 <- ggplot(combined_data, aes(x = Treatment, y = Occupancy_Probability, fill = spp)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +  # Dodging for grouped boxplots
    theme_classic() +
    scale_fill_manual(values = c("#1f77b4", 'coral2')) +  # Use different colors for the species
    labs(title = "Treatment Intercepts for Both Species", x = "Treatment", y = "Occupancy Probability") +
    theme(plot.title = element_text(hjust = 0.5)) # Center the title

ggsave(filename = "both_occu_prob_0204.png", plot = p3, device = "png", 
         path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/figures/06-rework-nimble-models",
         width = 10, height = 6, units = "in", dpi = 300)
