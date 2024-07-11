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
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")
library(nimble)
library(ggplot2)
library(data.table)
library(tidyverse)
library(mcmcplots)
library(MCMCvis)
library(boot)
source('attach.nimble_v2.R')

load("./all.spp_model_2.RData")


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


## format data --------------------------------------------------------------------------------------------------
# we need two parts: one subplot-level matrix, and one site-level matrix


#### subplot-level matrix:
# "date" "site" "temp" "humidity" "soil.moist" "all.obs" "oss.obs" "enes.obs" 

subplot_subset <- subset(subplot[,c(8,1,9,18)]) #subset subplot info

df1 <- subset(site[,c(1,10,11)]) #subset site info
dfmerge <- full_join(subplot_subset,df1,by="site_id") #merge site and subplot
#prev line joins temp and hum to subplot matrix, which automatically repeats the 
#temp and hum in blank subplot cells from same site id for both years because
#its currently only listed for each site

colnames(oss.long)[3] <- "oss.obs"
colnames(enes.long)[3] <- "enes.obs"
colnames(all.long)[3] <- "all.obs"

merge1 <- merge(dfmerge, all.long, by=c("site_id","subplot"))
merge1 <- merge(merge1, oss.long, by=c("site_id","subplot"))
complete.merge <- merge(merge1,enes.long,by=c("site_id","subplot"))

colnames(complete.merge) <- c("site","subplot","date","soil.moist","temp",
                              "humidity","all.obs","oss.obs","enes.obs")


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
#data$site <- as.numeric(data$site)
data$all.obs <- as.numeric(data$all.obs)
data$oss.obs <- as.numeric(data$oss.obs)
data$enes.obs <- as.numeric(data$enes.obs)

data2 <- site_subset
table(data2$treatment)

#  1 = BS     
#  2 = BU     
#  3 = HB     
#  4 = HU     
#  5 = UU     

scaled_temp <- c(scale(data$temp))


## Build Model------------------------------------------------------------------------------------------------

all.spp.model.2 <- nimbleCode ({
  
  # Priors 
  # uninformative, vague priors

  for(t in 1:n.treatments){
    TreatmentIntercept[t] ~ dunif(-10,10)
  }#t
  
  DetectionIntercept ~ dunif(-5,5)
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


## Assessing Convergence------------------------------------------------------------------------------------------------------
#mcmcplot(mcmc.output.1$samples)

# Gelman-Rubin diagnostic (AKA RHat or PSRF)
z <- mcmc.output.2$samples
g <- matrix(NA, nrow=nvar(z), ncol=2)
for (v in 1:nvar(z)) { g[v,] <- gelman.diag(z[,v])$psrf }
PSRF <- bind_cols(colnames(z$chain1),g) %>% rename(Parameter = ...1 ,PSRF = ...2 ,PSRFUpperCI = ...3) 

PSRF # Values are below 1.05, so that's good


## Interpreting Model Outputs----------------------------------------------------------------------------------------------------
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




