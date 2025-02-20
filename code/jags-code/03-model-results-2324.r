# --------------------------------------------------------------------------------------------
##
## 03-model-results-2324
##
## Task: Summarize fitted model results for OSS/ENES JAGS Occupancy Model 2023-2024
##
## Start Date: 02/20/2025
## Jasmine Williamson
##

## goals
# adapt Jay Jones' code to interpret model results

## insights
# 

## settings --------------------------------------------------------------------------------


    library(R2jags)
    library(grid)
    library(gridExtra)
    library(abind)
    library(tidyr)
    library(ggplot2)
    
    rm(list=ls())
    
    expit <- function(x) 1/(1+exp(-x))



# --------------------------------
# 0. read in the data
# --------------------------------

load("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/LS-jas data wrangling/output_files/oss_enes_data_packaged_jw.RData")
    
str(osslist)

load("jags-occu-model-objects.RData")



# ----------------------------
# 1. descriptive summaries
# ----------------------------

# naive occupancy
mean(apply(osslist$yo, 1, max, na.rm=T), na.rm=T) # 0.21 oss
mean(apply(osslist$ye, 1, max, na.rm=T), na.rm=T) # 0.15 enes
mean(apply(osslist$yo, 1, max, na.rm=T) + apply(osslist$ye, 1, max, na.rm=T) > 0, na.rm=T) # 0.29 any
mean(apply(osslist$yo, 1, max, na.rm=T) * apply(osslist$ye, 1, max, na.rm=T)) # 0.06 both
mean(apply(osslist$yo, 1, max, na.rm=T) - apply(osslist$ye, 1, max, na.rm=T) > 0, na.rm=T) # 0.14 oss only
mean(apply(osslist$yo, 1, max, na.rm=T) - apply(osslist$ye, 1, max, na.rm=T) < 0, na.rm=T) # 0.08 enes only

# unit-level naive occupancy
mean(unlist(lapply(split(apply(osslist$yo, 1, max, na.rm=T), osslist$Stand3), max)))
mean(unlist(lapply(split(apply(osslist$ye, 1, max, na.rm=T), osslist$Stand3), max)))
mean(unlist(lapply(split(apply(osslist$yo, 1, max, na.rm=T) + apply(osslist$ye, 1, max, na.rm=T) >0, osslist$Stand3), max)))


# ---------------------------
# 2. Harvest impact
# ---------------------------

oh1.sl <- out.oh1$BUGSoutput$sims.list
eh1.sl <- out.eh1$BUGSoutput$sims.list

# oa1.sl <- out.oa1$BUGSoutput$sims.list    #not doing abundance models yet
# ea1.sl <- out.ea1$BUGSoutput$sims.list


# 2a. OSS occupancy

  # trt effect estimator
  out.oh1$BUGSoutput$summary[rownames(out.oh1$BUGSoutput$summary) %in% 
                               c("beta0", "betaTFCL", "betaTFNC", "betaOWPB", "betaOWODF", "betaOWBLM", "betaYr24", "betaHU",
                                  "betaBU", "betaHB", "betaBS", "betaDW", "SalvEffect", "BurnEffect",
                                  "mu.a0", "mu.a1"),]
  
  # mean occ by year by trt arm
  temp.oh1 <- with(oh1.sl, cbind(beta0+betaTF/2, beta0+betaYr14+betaTF/2, beta0+betaYr15+betaTF/2, beta0+betaYr16+betaTF/2,
                                 beta0 + betaYr17 + betaTF/2, beta0 + betaYr18 + betaTF/2, beta0 + betaYr19 + betaTF/2,
                                  beta0+betaPre+betaTF/2, beta0+betaPre+betaYr14+betaTF/2, beta0+betaPre+betaYr15+betaTF/2,
                                  beta0+betaPost+betaYr16+betaTF/2, beta0 + betaPost + betaYr17 + betaTF/2, 
                                 beta0 + betaPost + betaYr18 + betaTF/2, beta0+betaPost+betaYr19+betaTF/2))  
  oh1.occ <- expand.grid(Quantity=c("Mean", "x5", "x25", "x75", "x95"), Year=2013:2019, Trt=c("Control", "Treatment"))
  oh1.occ$Occ <- as.numeric(apply(temp.oh1, 2, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.25, 0.75, 0.95))))))
  oh1.occ$HarvestState <- ifelse(oh1.occ$Year>2015 & oh1.occ$Trt=="Treatment", "Cut", "Timbered")
  #oh1.occ2 <- spread(oh1.occ, Quantity, Occ) # replaced with code below, spread is now pivot_wider
  oh1.occ2 <- pivot_wider(oh1.occ, names_from = Quantity, values_from = Occ)
  oh1.occ2$Species <- rep("OSS", nrow(oh1.occ2))
  

# 2b. ENES occupancy

  # trt effect estimator
  exp(c(mean(eh1.sl$TrtEffect), quantile(eh1.sl$TrtEffect, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))) # 0.20 (0.08, 0.47)
  out.eh1$BUGSoutput$summary[rownames(out.eh1$BUGSoutput$summary) %in% 
                               c("beta0", "betaTFCL", "betaTFNC", "betaOWPB", "betaOWODF", "betaOWBLM", "betaYr24", "betaHU",
                                 "betaBU", "betaHB", "betaBS", "betaDW", "SalvEffect", "BurnEffect",
                                 "mu.a0", "mu.a1"),]
  
  # mean occ by year by trt arm
  temp.eh1 <- with(eh1.sl, cbind(beta0+betaTF/2, beta0+betaYr14+betaTF/2, beta0+betaYr15+betaTF/2, beta0+betaYr16+betaTF/2,
                                 beta0 + betaYr17 + betaTF/2, beta0 + betaYr18 + betaTF/2, beta0 + betaYr19 + betaTF/2,
                                 beta0+betaPre+betaTF/2, beta0+betaPre+betaYr14+betaTF/2, beta0+betaPre+betaYr15+betaTF/2,
                                 beta0+betaPost+betaYr16+betaTF/2, beta0 + betaPost + betaYr17 + betaTF/2, 
                                 beta0 + betaPost + betaYr18 + betaTF/2, beta0+betaPost+betaYr19+betaTF/2))  
  eh1.occ <- expand.grid(Quantity=c("Mean", "x5", "x25", "x75", "x95"), Year=2013:2019, Trt=c("Control", "Treatment"))
  eh1.occ$Occ <- as.numeric(apply(temp.eh1, 2, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.25, 0.75, 0.95))))))
  eh1.occ$HarvestState <- ifelse(eh1.occ$Year>2015 & eh1.occ$Trt=="Treatment", "Cut", "Timbered")
  #eh1.occ2 <- spread(eh1.occ, Quantity, Occ) # replaced with code below, spread is now pivot_wider
  eh1.occ2 <- pivot_wider(eh1.occ, names_from = Quantity, values_from = Occ)
  eh1.occ2$Species <- rep("ENES", nrow(eh1.occ2))
  

### FIGURE 1 ###
  h1.occ <- merge(oh1.occ2, eh1.occ2, all=T)
  h1.occ$Species <- factor(h1.occ$Species, levels=c("OSS", "ENES"))
  h1.occ$Year <- with(h1.occ, ifelse(Trt=="Control", Year-0.05, Year+0.05))
  oh1.occ2$Year <- with(oh1.occ2, ifelse(Trt=="Control", Year-0.05, Year+0.05))

  ggplot(h1.occ, aes(Year, Mean, shape=Trt)) + geom_point(cex=2) + geom_line(aes(group=Trt)) +
    theme_bw() + ylim(c(0,1)) + ylab("Occupancy probability") + facet_grid(Trt~Species) + 
    scale_shape_manual(values=c(16, 1)) + theme(legend.title=element_blank(), legend.position="top") +
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95))
  
#  win.metafile("OSS 2016 - Occ vs. Year by Trt.emf", width=8, height=6)
  ggplot(h1.occ, aes(Year, Mean, shape=Trt)) + geom_point(cex=2) + geom_line(aes(group=Trt)) +
    theme_bw() + ylim(c(0,1)) + ylab("Occupancy probability") + facet_grid(Species ~ Trt) + 
    scale_shape_manual(values=c(16, 1)) + theme(legend.title=element_blank(), legend.position="top") +
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95))
#  dev.off()
  
  ggplot(h1.occ, aes(Year, Mean, color=Trt, shape=Trt)) + geom_point(cex=2) + 
    scale_shape_manual(values=c(16,1)) + geom_line(aes(group=Trt, linetype=Trt)) +
    theme_bw() + ylim(c(0,1)) + ylab("Occupancy probability") + facet_wrap(~Species) + 
    theme(legend.title=element_blank(), legend.position="top") +
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95)) +
    geom_vline(xintercept=2015.5, lty=2)

  ggplot(oh1.occ2, aes(Year, Mean, color=Trt, shape=Trt)) + geom_point(cex=2) + 
    scale_shape_manual(values=c(16,1)) + geom_line(aes(group=Trt)) +
    theme_bw() + ylim(c(0,1)) + ylab("Occupancy probability (90% CRI)") + 
    theme(legend.title=element_blank(), legend.position="top") +
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95)) +
    geom_vline(xintercept=2015.5, lty=2)
  

# 2c. OSS abundance

  # trt effect estimator
  exp(c(mean(oa1.sl$TrtEffect), quantile(oa1.sl$TrtEffect, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))) # 0.84 (0.54, 1.4)
  
  # mean abundance by year by trt arm
  temp.oa1 <- with(oa1.sl, cbind(beta0+betaTF/2, beta0+betaYr14+betaTF/2, beta0+betaYr15+betaTF/2, beta0+betaYr16+betaTF/2,
                                 beta0 + betaYr17 + betaTF/2, beta0 + betaYr18 + betaTF/2, beta0 + betaYr19 + betaTF/2,
                                 beta0+betaPre+betaTF/2, beta0+betaPre+betaYr14+betaTF/2, beta0+betaPre+betaYr15+betaTF/2,
                                 beta0+betaPost+betaYr16+betaTF/2, beta0 + betaPost + betaYr17 + betaTF/2,
                                 beta0 + betaPost + betaYr18 + betaTF/2, beta0 + betaPost + betaYr19 + betaTF/2))  
  oa1.ab <- expand.grid(Quantity=c("Mean", "x5", "x25", "x75", "x95"), Year=2013:2019, Trt=c("Control", "Treatment"))
  oa1.ab$N <- as.numeric(apply(temp.oa1, 2, function(x) exp(c(mean(x), quantile(x, probs=c(0.05, 0.25, 0.75, 0.95))))))
  oa1.ab$HarvestState <- ifelse(oa1.ab$Year>2015 & oa1.ab$Trt=="Treatment", "Cut", "Timbered")
  oa1.ab2 <- spread(oa1.ab, Quantity, N)
  oa1.ab2$Species <- rep("OSS", nrow(oa1.ab2))
  


# 2d. ENES abundance

  # trt effect estimator
  exp(c(mean(ea1.sl$TrtEffect), quantile(ea1.sl$TrtEffect, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))) # 0.37 (0.22, 0.65)
  
  # mean abundance by year by trt arm
  temp.ea1 <- with(ea1.sl, cbind(beta0+betaTF/2, beta0+betaYr14+betaTF/2, beta0+betaYr15+betaTF/2, beta0+betaYr16+betaTF/2,
                                 beta0 + betaYr17 + betaTF/2, beta0 + betaYr18 + betaTF/2, beta0 + betaYr19 + betaTF/2,
                                 beta0+betaPre+betaTF/2, beta0+betaPre+betaYr14+betaTF/2, beta0+betaPre+betaYr15+betaTF/2,
                                 beta0+betaPost+betaYr16+betaTF/2, beta0 + betaPost + betaYr17 + betaTF/2,
                                 beta0 + betaPost + betaYr18 + betaTF/2, beta0 + betaPost + betaYr19 + betaTF/2))  
  ea1.ab <- expand.grid(Quantity=c("Mean", "x5", "x25", "x75", "x95"), Year=2013:2019, Trt=c("Control", "Treatment"))
  ea1.ab$N <- as.numeric(apply(temp.ea1, 2, function(x) exp(c(mean(x), quantile(x, probs=c(0.05, 0.25, 0.75, 0.95))))))
  ea1.ab$HarvestState <- ifelse(ea1.ab$Year>2015 & oa1.ab$Trt=="Treatment", "Cut", "Timbered")
  ea1.ab2 <- spread(ea1.ab, Quantity, N)
  ea1.ab2$Species <- rep("ENES", nrow(ea1.ab2))
  

  ### FIGURE 2 ###  
  a1.ab <- merge(oa1.ab2, ea1.ab2, all=T)
  a1.ab$Species <- factor(a1.ab$Species, levels=c("OSS", "ENES"))
  a1.ab$Year <- with(a1.ab, ifelse(Trt=="Control", Year-0.05, Year+0.05))
  oa1.ab2$Year <- with(oa1.ab2, ifelse(Trt=="Control", Year-0.05, Year+0.05))
  
  ggplot(a1.ab, aes(Year, Mean, shape=Trt)) + geom_point(cex=2) + geom_line(aes(group=Trt)) +
    theme_bw() + ylab("Abundance") + facet_grid(Trt~Species) +  
    scale_shape_manual(values=c(16,1)) + geom_line(aes(group=Trt)) + 
    theme(legend.title=element_blank(), legend.position="top") + 
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95))
  
#  win.metafile("OSS 2016 - Abundance vs. Year by Trt.emf", width=8, height=6)
  ggplot(a1.ab, aes(Year, Mean, shape=Trt)) + geom_point(cex=2) + geom_line(aes(group=Trt)) +
    theme_bw() + ylab("Abundance") + facet_grid(Species ~ Trt) +  
    scale_shape_manual(values=c(16,1)) + geom_line(aes(group=Trt)) + 
    theme(legend.title=element_blank(), legend.position="top") + 
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95))
#  dev.off()
    
  ggplot(a1.ab, aes(Year, Mean, shape=Trt, color=Trt)) + geom_point(cex=2) + 
    geom_line(aes(group=Trt, linetype=Trt)) +
    theme_bw() + ylab("Abundance") + facet_wrap(~Species) + 
    scale_shape_manual(values=c(16,1)) + theme(legend.title=element_blank(), legend.position="top") + 
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95)) +
    geom_vline(xintercept=2015.5, lty=2)
  
  ggplot(oa1.ab2, aes(Year, Mean, shape=Trt, color=Trt)) + geom_point(cex=2) + 
    geom_line(aes(group=Trt)) + ylim(c(0, 2.1)) +
    theme_bw() + ylab("Abundance (90% CRI)") + #facet_wrap(~Species) + 
    scale_shape_manual(values=c(16,1)) + theme(legend.title=element_blank(), legend.position="top") + 
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95)) +
    geom_vline(xintercept=2015.5, lty=2)
  

  ## plot abundance and occupancy in one figure.
  h1.occ$Model <- rep("Occupancy", nrow(h1.occ))
  a1.ab$Model <- rep("Abundance", nrow(a1.ab))
  x1 <- merge(h1.occ, a1.ab, all=T)

#  win.metafile("OSS 2017 - Abund and occ vs. Year by Trt.emf", width=8, height=6)
  ggplot(x1, aes(Year, Mean, color=Trt, shape=Trt)) + geom_point(cex=2) + 
    scale_shape_manual(values=c(16,1)) + geom_line(aes(group=Trt)) +
    theme_bw() + ylab("Mean estimate (90% CRI)") + facet_grid(Model~Species, scales="free_y") + 
    theme(legend.title=element_blank(), legend.position="top") +
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95))
#  dev.off()  
  
  p1 <- ggplot(h1.occ, aes(Year, Mean, color=Trt, shape=Trt)) + geom_point(cex=2) + 
    scale_shape_manual(values=c(16,1)) + geom_line(aes(group=Trt)) +
    theme_bw() + ylim(c(0,1)) + ylab("Occupancy probability (90% CRI)") + facet_wrap(~Species) + 
    theme(legend.title=element_blank(), legend.position="top") +
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95)) +
    geom_vline(xintercept=2015.5, lty=2)
  p2 <- ggplot(a1.ab, aes(Year, Mean, shape=Trt, color=Trt)) + geom_point(cex=2) + 
    geom_line(aes(group=Trt)) +
    theme_bw() + ylab("Abundance (90% CRI)") + facet_wrap(~Species) + 
    scale_shape_manual(values=c(16,1)) + theme(legend.title=element_blank(), legend.position="top") + 
    geom_segment(aes(x=Year, y=x25, xend=Year, yend=x5)) + 
    geom_segment(aes(x=Year, y=x75, xend=Year, yend=x95)) +
    geom_vline(xintercept=2015.5, lty=2)
  grid.arrange(p1, p2, ncol=1)
  
  x1$Year <- round(x1$Year, 0)
  write.csv(x1, "Occ and Ab est by year by trt by species _ 2019.csv", row.names=F)
  
  
  
# 2e. Post harvest mean occ & abundance by species and trt
  
  # mean occ by trt arm
  temp.oh1 <- with(oh1.sl, cbind(beta0 + betaYr14/7 + betaYr15/7 + betaYr16/7 + betaYr17/7 + 
                                   betaYr18/7 + betaYr19/7 + betaTF/2,
                                 beta0 + betaYr14/7 + betaYr15/7 + betaYr16/7 + betaYr17/7 + 
                                   betaYr18/7 + betaYr19/7 + betaTF/2 + betaPost - betaPre))  
  oh1.occ <- expand.grid(Quantity=c("Mean", "x5", "x25", "x75", "x95"), Trt=c("Control", "Treatment"))
  oh1.occ$Occ <- as.numeric(apply(temp.oh1, 2, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.25, 0.75, 0.95))))))
  oh1.occ2 <- spread(oh1.occ, Quantity, Occ)
  oh1.occ2$Species <- rep("OSS", nrow(oh1.occ2))
  oh1.occ2$Response <- rep("Occ", nrow(oh1.occ2))
  
  
  # mean occ by trt arm
  temp.eh1 <- with(eh1.sl, cbind(beta0 + betaYr14/7 + betaYr15/7 + betaYr16/7 + betaYr17/7 + 
                                   betaYr18/7 + betaYr19/7 + betaTF/2,
                                 beta0 + betaYr14/7 + betaYr15/7 + betaYr16/7 + betaYr17/7 + 
                                   betaYr18/7 + betaYr19/7 + betaTF/2 + betaPost - betaPre))  
  eh1.occ <- expand.grid(Quantity=c("Mean", "x5", "x25", "x75", "x95"), Trt=c("Control", "Treatment"))
  eh1.occ$Occ <- as.numeric(apply(temp.eh1, 2, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.25, 0.75, 0.95))))))
  eh1.occ2 <- spread(eh1.occ, Quantity, Occ)
  eh1.occ2$Species <- rep("ENES", nrow(eh1.occ2))
  eh1.occ2$Response <- rep("Occ", nrow(eh1.occ2))
  

  # mean abundance by trt arm
  temp.oa1 <- with(oa1.sl, cbind(beta0 + betaYr14/7 + betaYr15/7 + betaYr16/7 + betaYr17/7 + 
                                   betaYr18/7 + betaYr19/7 + betaTF/2,
                                 beta0 + betaYr14/7 + betaYr15/7 + betaYr16/7 + betaYr17/7 + 
                                   betaYr18/7 + betaYr19/7 + betaTF/2 + betaPost - betaPre))  
  oa1.ab <- expand.grid(Quantity=c("Mean", "x5", "x25", "x75", "x95"), Trt=c("Control", "Treatment"))
  oa1.ab$Ab <- as.numeric(apply(temp.oa1, 2, function(x) exp(c(mean(x), quantile(x, probs=c(0.05, 0.25, 0.75, 0.95))))))
  oa1.ab2 <- spread(oa1.ab, Quantity, Ab)
  oa1.ab2$Species <- rep("OSS", nrow(oa1.ab2))
  oa1.ab2$Response <- rep("Abundance", nrow(oa1.ab2))
  

  # mean occ by trt arm
  temp.ea1 <- with(ea1.sl, cbind(beta0 + betaYr14/7 + betaYr15/7 + betaYr16/7 + betaYr17/7 + 
                                   betaYr18/7 + betaYr19/7 + betaTF/2,
                                 beta0 + betaYr14/7 + betaYr15/7 + betaYr16/7 + betaYr17/7 + 
                                   betaYr18/7 + betaYr19/7 + betaTF/2 + betaPost - betaPre))  
  ea1.ab <- expand.grid(Quantity=c("Mean", "x5", "x25", "x75", "x95"), Trt=c("Control", "Treatment"))
  ea1.ab$Ab <- as.numeric(apply(temp.ea1, 2, function(x) exp(c(mean(x), quantile(x, probs=c(0.05, 0.25, 0.75, 0.95))))))
  ea1.ab2 <- spread(ea1.ab, Quantity, Ab)
  ea1.ab2$Species <- rep("ENES", nrow(ea1.ab2))
  ea1.ab2$Response <- rep("Abundance", nrow(ea1.ab2))
  
  
  ossmean <- full_join(oh1.occ2, eh1.occ2) %>% full_join(oa1.ab2) %>% full_join(ea1.ab2)
  write_csv(ossmean, "Post harvest occ and ab by spp by trt _ 2019.csv")
  
# -------------------------------
# 3. Downed wood associations
# -------------------------------
  
  boxplot(split(osslist$DW3, osslist$Trt3)) # very similar distributions
  summary(osslist$DW3) # 0, 1, 3, 5, 18
  quantile(osslist$DW3, probs=(0:20)/20) # 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 7, 8, 18
  lapply(split(osslist$DW3, osslist$Trt3), mean)
  lapply(split(osslist$DW3, osslist$Trt3), sd)
  
  nsim <- 2000
  
# 3a. OSS Occ model                                           ####################### code stops working here

  exp(c(mean(0.5*oh1.sl$betaDW), quantile(0.5*oh1.sl$betaDW, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))) # 1.2 (1.1, 1.3)
  
  oh1.dw.mat <- matrix(nrow=8, ncol=nsim)
  samp <- sample(1:length(oh1.sl$beta0), nsim)   #Error in sample.int(length(x), size, replace, prob) : cannot take a sample larger than the population when 'replace = FALSE'
  for(i in 1:nsim){
    oh1.dw.mat[,i] <- oh1.sl$beta0[samp[i]] + oh1.sl$betaTF[samp[i]]/2 + oh1.sl$betaDW[samp[i]]*((0:7)-3)/2 +
      oh1.sl$betaYr14[samp[i]]/7 + oh1.sl$betaYr15[samp[i]]/7 + oh1.sl$betaYr16[samp[i]]/7 + oh1.sl$betaYr17[samp[i]]/7 +
      oh1.sl$betaYr18[samp[i]]/7 + oh1.sl$betaYr19[samp[i]]/7
  }
  oh1.dw <- as.data.frame(t(apply(oh1.dw.mat, 1, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.95)))))))
  names(oh1.dw) <- c("Occ", "x5", "x95")
  oh1.dw$DW <- 0:7
  oh1.dw$Species <- rep("OSS", nrow(oh1.dw))

  
# 3b. ENES Occ model

  exp(c(mean(0.5*eh1.sl$betaDW), quantile(0.5*eh1.sl$betaDW, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))) # 1.1 (1.0, 1.2)
  
  eh1.dw.mat <- matrix(nrow=8, ncol=nsim)
  samp <- sample(1:length(eh1.sl$beta0), nsim)
  for(i in 1:nsim){
    eh1.dw.mat[,i] <- eh1.sl$beta0[samp[i]] + eh1.sl$betaTF[samp[i]]/2 + eh1.sl$betaDW[samp[i]]*((0:7)-3)/2 +
      eh1.sl$betaYr14[samp[i]]/7 + eh1.sl$betaYr15[samp[i]]/7 + eh1.sl$betaYr16[samp[i]]/7 + eh1.sl$betaYr17[samp[i]]/7 +
      eh1.sl$betaYr18[samp[i]]/7 + eh1.sl$betaYr19[samp[i]]/7
  }
  eh1.dw <- as.data.frame(t(apply(eh1.dw.mat, 1, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.95)))))))
  names(eh1.dw) <- c("Occ", "x5", "x95")
  eh1.dw$DW <- 0:7
  eh1.dw$Species <- rep("ENES", nrow(eh1.dw))
  
  h1.dw <- merge(oh1.dw, eh1.dw, all=T)
  h1.rug <- data.frame(Occ=rep(0, 3), DW=c(1,2,4))

  ### FIGURE 3 ###  
  h1.dw$Species <- factor(h1.dw$Species, levels=c("OSS", "ENES"))
#  win.metafile("OSS 2016 - Occ vs. CWD by species.emf", width=8, height=5)
  ggplot(h1.dw, aes(DW, Occ)) + facet_wrap(~Species) + theme_bw() + geom_line() + ylim(c(0,1)) +
    geom_line(aes(DW, x5), lty=2) + geom_line(aes(DW, x95), lty=2) + ylab("Occupancy probability") +
    xlab("Downed wood (count)") + geom_rug(data=h1.rug, sides="b", col="red", lwd=1)
#  dev.off()    
  
# 3c. OSS abundance model

  exp(c(mean(0.5*oa1.sl$betaDW), quantile(0.5*oa1.sl$betaDW, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))) # 1.09 (1.06, 1.11)
  
  oa1.dw.mat <- matrix(nrow=8, ncol=nsim)
  samp <- sample(1:length(oa1.sl$beta0), nsim)
  for(i in 1:nsim){
    oa1.dw.mat[,i] <- oa1.sl$beta0[samp[i]] + oa1.sl$betaTF[samp[i]]/2 + oa1.sl$betaDW[samp[i]]*((0:7)-3)/2 +
      oa1.sl$betaYr14[samp[i]]/7 + oa1.sl$betaYr15[samp[i]]/7 + oa1.sl$betaYr16[samp[i]]/7 + oa1.sl$betaYr17[samp[i]]/7 +
      oa1.sl$betaYr18[samp[i]]/7 + oa1.sl$betaYr19[samp[i]]/7
  }
  oa1.dw <- as.data.frame(t(apply(oa1.dw.mat, 1, function(x) exp(c(mean(x), quantile(x, probs=c(0.05, 0.95)))))))
  names(oa1.dw) <- c("N", "x5", "x95")
  oa1.dw$DW <- 0:7
  oa1.dw$Species <- rep("OSS", nrow(oa1.dw))
  
# 3d. ENES abundance model  
  
  exp(c(mean(0.5*ea1.sl$betaDW), quantile(0.5*ea1.sl$betaDW, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))) # 1.06 (1.03, 1.10)

  ea1.dw.mat <- matrix(nrow=8, ncol=nsim)
  samp <- sample(1:length(ea1.sl$beta0), nsim)
  for(i in 1:nsim){
    ea1.dw.mat[,i] <- ea1.sl$beta0[samp[i]] + ea1.sl$betaTF[samp[i]]/2 + ea1.sl$betaDW[samp[i]]*((0:7)-3)/2 +
      ea1.sl$betaYr14[samp[i]]/7 + ea1.sl$betaYr15[samp[i]]/7 + ea1.sl$betaYr16[samp[i]]/7 + ea1.sl$betaYr17[samp[i]]/7 +
      ea1.sl$betaYr18[samp[i]]/7 + ea1.sl$betaYr19[samp[i]]/7
  }
  ea1.dw <- as.data.frame(t(apply(ea1.dw.mat, 1, function(x) exp(c(mean(x), quantile(x, probs=c(0.05, 0.95)))))))
  names(ea1.dw) <- c("N", "x5", "x95")
  ea1.dw$DW <- 0:7
  ea1.dw$Species <- rep("ENES", nrow(ea1.dw))
  
  a1.dw <- merge(oa1.dw, ea1.dw, all=T)
  a1.rug <- data.frame(N=rep(0, 3), DW=c(1,2,4))

  ### FIGURE 4 ###  
  a1.dw$Species <- factor(a1.dw$Species, levels=c("OSS", "ENES"))
#  win.metafile("OSS 2016 - Abundance vs. CWD by species.emf", width=8, height=5)
  ggplot(a1.dw, aes(DW, N)) + facet_wrap(~Species) + theme_bw() + geom_line() + #ylim(c(0,1)) +
    geom_line(aes(DW, x5), lty=2) + geom_line(aes(DW, x95), lty=2) + ylab("Abundance") +
    xlab("Downed wood (count)") + geom_rug(data=a1.rug, sides="b", col="red", lwd=1)
#  dev.off()  
  
  
  
## combined figure

  a1.dw$Mean <- a1.dw$N
  a1.dw$Model <- "Abundance"
  h1.dw$Mean <- h1.dw$Occ
  h1.dw$Model <- "Occupancy"
  xx1 <- merge(a1.dw, h1.dw, all=T)
  a1.rug$Mean <- a1.rug$N
  h1.rug$Mean <- h1.rug$Occ
  xx1.rug <- merge(a1.rug, h1.rug)
  summary(xx1)
  names(h1.dw)
  xx1$Species2 <- factor(ifelse(xx1$Species=="OSS", "Oregon slender", "Ensatina"), levels=c("Oregon slender", "Ensatina"))
  
  
  p3 <- ggplot(a1.dw, aes(DW, N)) + facet_wrap(~Species) + theme_bw() + geom_line() + #ylim(c(0,1)) +
    geom_line(aes(DW, x5), lty=2) + geom_line(aes(DW, x95), lty=2) + ylab("Abundance") +
    xlab("Downed wood (count)") + geom_rug(data=a1.rug, sides="b", col="red", lwd=1)
  p4 <- ggplot(h1.dw, aes(DW, Occ)) + facet_wrap(~Species) + theme_bw() + geom_line() + ylim(c(0,1)) +
    geom_line(aes(DW, x5), lty=2) + geom_line(aes(DW, x95), lty=2) + ylab("Occupancy probability") +
    xlab("Downed wood (count)") + geom_rug(data=h1.rug, sides="b", col="red", lwd=1)
  grid.arrange(p4, p3, ncol=1)
  
#  win.metafile("OSS 2017 - Abund and occ vs. CWD by Trt.emf", width=6, height=4)
  ggplot(xx1, aes(DW, Mean)) + theme_bw() + geom_line() + facet_grid(Model ~ Species2, scales="free_y") +
    geom_line(aes(DW, x5), lty=2) + geom_line(aes(DW, x95), lty=2) + ylab("Mean estimate (90% CRI)") +
    xlab("Downed wood (count)") + geom_rug(data=xx1.rug, sides="b", col="red", lwd=1)
#  dev.off()  

  ggplot(xx1[xx1$Species2 != "Ensatina",], aes(DW, Mean)) + theme_bw() + geom_line() + facet_grid(Model~., scales="free_y") +
    geom_line(aes(DW, x5), lty=2) + geom_line(aes(DW, x95), lty=2) + ylab("Mean estimate (90% CRI)") +
    xlab("Downed wood (count)") + geom_rug(data=xx1.rug, sides="b", col="red", lwd=1)
  
  
  write.csv(xx1[,-c(7,8)], "Occ and ab by spp by CWD 2019.csv", row.names=F)
  
  
# ------------------------------------------------
# 4. Detection/Capture probability relationships
# ------------------------------------------------
  
  summary(osslist$AT3)
  summary((osslist$AT3 - 12)/5.5) # -1.77, -0.94, -0.06, 0.94, 2.23
  ATsum <- (rep(seq(0, 35, 1), 2) - 12)/5.5
  
  nsim <- 2000
  
  names(oh1.sl)
  #
  # Detection probability
  # Trt effect - OSS:  -0.45 (-1.4, 0.5)
  # Trt effect - ENES: -0.28 (-1.4, 0.8)
  #
  
  
  
  
# 4a. OSS Occ model
  
  oh1.at.mat <- matrix(nrow=length(ATsum), ncol=nsim)
  trt <- rep(c(0,1), each=length(ATsum)/2)
  samp <- sample(1:length(oh1.sl$mu.a0), nsim)
  
  for(i in 1:nsim){
    oh1.at.mat[,i] <- oh1.sl$mu.a0[samp[i]] + oh1.sl$mu.a1[samp[i]]*trt + oh1.sl$mu.a2[samp[i]]*ATsum +
      oh1.sl$mu.a3[samp[i]]*ATsum*ATsum + oh1.sl$mu.a4[samp[i]]*trt*ATsum + oh1.sl$mu.a5[samp[i]]*trt*ATsum*ATsum
  }
  oh1.at <- as.data.frame(t(apply(oh1.at.mat, 1, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.95)))))))
  names(oh1.at) <- c("DetProb", "x5", "x95")
  oh1.at$AT <- (ATsum*5.5 + 12)
  oh1.at$Species <- rep("OSS", nrow(oh1.at))
  oh1.at$Trt <- trt
  
# 4b. ENES Occ model
  
  eh1.at.mat <- matrix(nrow=length(ATsum), ncol=nsim)
  trt <- rep(c(0,1), each=length(ATsum)/2)
  samp <- sample(1:length(eh1.sl$mu.a0), nsim)
  
  for(i in 1:nsim){
    eh1.at.mat[,i] <- eh1.sl$mu.a0[samp[i]] + eh1.sl$mu.a1[samp[i]]*trt + eh1.sl$mu.a2[samp[i]]*ATsum +
      eh1.sl$mu.a3[samp[i]]*ATsum*ATsum + eh1.sl$mu.a4[samp[i]]*trt*ATsum + eh1.sl$mu.a5[samp[i]]*trt*ATsum*ATsum
  }
  eh1.at <- as.data.frame(t(apply(eh1.at.mat, 1, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.95)))))))
  names(eh1.at) <- c("DetProb", "x5", "x95")
  eh1.at$AT <- (ATsum*5.5 + 12)
  eh1.at$Species <- rep("ENES", nrow(eh1.at))
  eh1.at$Trt <- trt
  
  
# merge and plot
  
  h1.at <- merge(oh1.at, eh1.at, all=T)
  h1.at.rug <- data.frame(DetProb=rep(0,5), AT=c(0, 8, 11, 15.5, 33.3))
  h1.at$Treatment <- ifelse(h1.at$Trt==0, "Control", "Treatment")
  h1.at$Species <- factor(h1.at$Species, levels=c("OSS", "ENES"))
  
  ggplot(h1.at, aes(AT, DetProb, color=Treatment)) + theme_bw() + facet_wrap(~Species) + geom_line(aes(group=Treatment)) +
    ylab("Detection probability") + xlab("Air Temperature (C)") + geom_rug(data=h1.at.rug, sides="b", col="red", lwd=1) +
    geom_line(aes(AT, x5), lty=2) + geom_line(aes(AT, x95), lty=2) + 
    ylim(c(0,1)) + xlim(c(5,25)) +
    theme(legend.title=element_blank(), legend.position="top")
    
#  win.metafile("OSS 2016 - DetProb vs. AT by species by trt.emf", width=8, height=6)
  ggplot(h1.at, aes(AT, DetProb)) + theme_bw() + facet_grid(Species ~ Treatment) + geom_line() +
    geom_line(aes(AT, x5), lty=2) + geom_line(aes(AT, x95), lty=2) + ylim(c(0,1)) + xlim(c(5,25)) +
    theme(legend.title=element_blank(), legend.position="top") + ylab("Detection probability") + xlab("Air temperature (C)")
#  dev.off()
  
  ggplot(h1.at, aes(AT, DetProb)) + theme_bw() + facet_grid(Treatment ~ Species) + geom_line() +
    geom_line(aes(AT, x5), lty=2) + geom_line(aes(AT, x95), lty=2) + xlim(c(5,25)) + ylim(c(0, 0.5)) +
    xlab("Air temp (C)") + ylab("Detection probability") +
    theme(legend.title=element_blank(), legend.position="top") 

  write.csv(h1.at, "Occ det prob vs air temp by species by trt _ 2019.csv", row.names=F)
    

# 4c. OSS Abundance model
  
  ATsum <- (rep(seq(0, 35, 1), 2) - 12)/5.5

  oa1.at.mat <- matrix(nrow=length(ATsum), ncol=nsim)
  trt <- rep(c(0,1), each=length(ATsum)/2)
  samp <- sample(1:length(oa1.sl$mu.a0), nsim)
  
  for(i in 1:nsim){
    oa1.at.mat[,i] <- oa1.sl$mu.a0[samp[i]] + oa1.sl$mu.a1[samp[i]]*trt + oa1.sl$mu.a2[samp[i]]*ATsum +
      oa1.sl$mu.a3[samp[i]]*ATsum*ATsum + oa1.sl$mu.a4[samp[i]]*trt*ATsum + oa1.sl$mu.a5[samp[i]]*trt*ATsum*ATsum
  }
  oa1.at <- as.data.frame(t(apply(oa1.at.mat, 1, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.95)))))))
  names(oa1.at) <- c("DetProb", "x5", "x95")
  oa1.at$AT <- (ATsum*5.5 + 12)
  oa1.at$Species <- rep("OSS", nrow(oa1.at))
  oa1.at$Trt <- trt
  
# 4d. ENES Abundance model
  
  ea1.at.mat <- matrix(nrow=length(ATsum), ncol=nsim)
  trt <- rep(c(0,1), each=length(ATsum)/2)
  samp <- sample(1:length(ea1.sl$mu.a0), nsim)
  
  for(i in 1:nsim){
    ea1.at.mat[,i] <- ea1.sl$mu.a0[samp[i]] + ea1.sl$mu.a1[samp[i]]*trt + ea1.sl$mu.a2[samp[i]]*ATsum +
      ea1.sl$mu.a3[samp[i]]*ATsum*ATsum + ea1.sl$mu.a4[samp[i]]*trt*ATsum + ea1.sl$mu.a5[samp[i]]*trt*ATsum*ATsum
  }
  ea1.at <- as.data.frame(t(apply(ea1.at.mat, 1, function(x) expit(c(mean(x), quantile(x, probs=c(0.05, 0.95)))))))
  names(ea1.at) <- c("DetProb", "x5", "x95")
  ea1.at$AT <- (ATsum*5.5 + 12)
  ea1.at$Species <- rep("ENES", nrow(ea1.at))
  ea1.at$Trt <- trt
  
  
# merge and plot
  
  a1.at <- merge(oa1.at, ea1.at, all=T)
  a1.at.rug <- data.frame(DetProb=rep(0,5), AT=c(0, 8, 11, 15.5, 33.3))
  a1.at$Treatment <- ifelse(a1.at$Trt==0, "Control", "Treatment")
  a1.at$Species <- factor(a1.at$Species, levels=c("OSS", "ENES"))
  
  ggplot(a1.at, aes(AT, DetProb, color=Treatment)) + theme_bw() + facet_wrap(~Species) + geom_line(aes(group=Treatment)) +
    ylab("Capture probability") + xlab("Air temperature (C)") + geom_rug(data=a1.at.rug, sides="b", col="red", lwd=1) +
    geom_line(aes(AT, x5), lty=2) + geom_line(aes(AT, x95), lty=2) + 
    xlim(c(5,25)) +
    theme(legend.title=element_blank(), legend.position="top")

#  win.metafile("OSS 2016 - CapProb vs. AT by species by trt.emf", width=8, height=6)
  ggplot(a1.at, aes(AT, DetProb)) + theme_bw() + facet_grid(Species ~ Treatment) + geom_line() +
    geom_line(aes(AT, x5), lty=2) + geom_line(aes(AT, x95), lty=2) +xlim(c(5,25)) +
    theme(legend.title=element_blank(), legend.position="top") + ylab("Capture probability") + xlab("Air temperature (C)")
#  dev.off()
  
  ggplot(a1.at, aes(AT, DetProb)) + theme_bw() + facet_grid(Treatment ~ Species) + geom_line() +
    geom_line(aes(AT, x5), lty=2) + geom_line(aes(AT, x95), lty=2) +xlim(c(5,25)) +
    theme(legend.title=element_blank(), legend.position="top") + ylim(c(0, 0.5))
  

  
# 4e OSS detection prob by year by trt vs. temp
  
satDat <- read.csv("Soil and air temp summaries by year by month by trt.csv")
satDat$Trt <- ifelse(satDat$HarvestState=="Cut", 1, 0)
satDat$YearF <- as.numeric(satDat$Year)
satDat$Temp <- (satDat$AirTemp.mean - 12)/5.5

oa1.m <- out.oa1$BUGSoutput$mean
ea1.m <- out.ea1$BUGSoutput$mean

DetProb.o <- matrix(ncol=length(oa1.sl$aAT[,1]), nrow=nrow(satDat))
DetProb.e <- matrix(ncol=length(ea1.sl$aAT[,1]), nrow=nrow(satDat))

for(i in 1:nrow(satDat)){
  DetProb.o[i,] <- 1/(1+exp(-(oa1.sl$a0[,satDat$YearF[i]] + oa1.sl$aAT[,satDat$YearF[i]]*satDat$Temp[i] + oa1.sl$aAT2[,satDat$YearF[i]]*satDat$Temp[i]^2 +
    oa1.sl$aTrt[,satDat$YearF[i]]*satDat$Trt[i] + oa1.sl$aTrtAT[,satDat$YearF[i]]*satDat$Trt[i]*satDat$Temp[i] + oa1.sl$aTrtAT2[,satDat$YearF[i]]*satDat$Trt[i]*satDat$Temp[i]^2)))
  DetProb.e[i,] <- 1/(1+exp(-(ea1.sl$a0[,satDat$YearF[i]] + ea1.sl$aAT[,satDat$YearF[i]]*satDat$Temp[i] + ea1.sl$aAT2[,satDat$YearF[i]]*satDat$Temp[i]^2 +
    ea1.sl$aTrt[,satDat$YearF[i]]*satDat$Trt[i] + ea1.sl$aTrtAT[,satDat$YearF[i]]*satDat$Trt[i]*satDat$Temp[i] + ea1.sl$aTrtAT2[,satDat$YearF[i]]*satDat$Trt[i]*satDat$Temp[i]^2)))
}

satDat$DetProb.o <- apply(DetProb.o, 1, mean)
satDat$DetProb.o.ll <- apply(DetProb.o, 1, quantile, probs=0.05)
satDat$DetProb.o.ul <- apply(DetProb.o, 1, quantile, probs=0.95)
satDat$DetProb.e <- apply(DetProb.e, 1, mean)
satDat$DetProb.e.ll <- apply(DetProb.e, 1, quantile, probs=0.05)
satDat$DetProb.e.ul <- apply(DetProb.e, 1, quantile, probs=0.95)

#satDat$CapProb.o <- 1 - (1 - satDat$DetProb.o)^3
#satDat$CapProb.e <- 1 - (1 - satDat$DetProb.e)^3
satDat$CapProb.o <- apply(1 - (1 - DetProb.o)^3, 1, mean)
satDat$CapProb.o.ll <- apply(1 - (1 - DetProb.o)^3, 1, quantile, probs=0.05)
satDat$CapProb.o.ul <- apply(1 - (1 - DetProb.o)^3, 1, quantile, probs=0.95)
satDat$CapProb.e <- apply(1 - (1 - DetProb.e)^3, 1, mean)
satDat$CapProb.e.ll <- apply(1 - (1 - DetProb.e)^3, 1, quantile, probs=0.05)
satDat$CapProb.e.ul <- apply(1 - (1 - DetProb.e)^3, 1, quantile, probs=0.95)


write.csv(satDat, "Detection prob by species by year by month _ 2019.csv", row.names=F)  
  
  

# ------------------------------------------------
# 5. naive statistics
# ------------------------------------------------

# 5a. proportion of sub-plots occupied by OSS and ENES

summary(apply(osslist$yo, 1, max, na.rm=T))
summary(apply(osslist$ye, 1, max, na.rm=T))

# 5b. proportion with both
summary(apply(osslist$yo, 1, max, na.rm=T) * apply(osslist$ye, 1, max, na.rm=T))










