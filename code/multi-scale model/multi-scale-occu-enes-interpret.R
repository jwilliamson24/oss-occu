## =================================================
##
## Title: msc-occu-enes-marginal-plots
## Author: Jasmine Williamson 
## Date Created: 07/22/2025
##
## Description: Visualize and interpret outputs for 
## multi-scale occupancy model, for ENES, all-years dataset
##
## =================================================

# look at yearly effects next?

# Load packages and data
  library(ggplot2)
  library(dplyr)

  load("multiscale_output_and_data_072125_small.RData")
  load("data/occupancy/msc-enes-data-workspace.RData")
  
# Estimates
  summary(a2)

  
# Quick trt effect on occupancy barplot
  logit_psi <- c(
    UU = summary(a2)$statistics["beta0.psi", "Mean"],
    BU = summary(a2)$statistics["beta0.psi", "Mean"] + summary(a2)$statistics["beta1.psi.BU", "Mean"],
    HB = summary(a2)$statistics["beta0.psi", "Mean"] + summary(a2)$statistics["beta2.psi.HB", "Mean"],
    HU = summary(a2)$statistics["beta0.psi", "Mean"] + summary(a2)$statistics["beta3.psi.HU", "Mean"],
    BS = summary(a2)$statistics["beta0.psi", "Mean"] + summary(a2)$statistics["beta4.psi.BS", "Mean"]
  )
  psi_probs <- plogis(logit_psi) # back-transform
  
  df <- data.frame(Treatment = names(psi_probs), Occupancy = psi_probs)
  ggplot(df, aes(x = Treatment, y = Occupancy)) +
    geom_col(fill = "steelblue") +
    ylim(0,1) +
    labs(title = "Effect of Trt on Site Occu",
         y = "Predicted Occu Prob", x = "") +
    theme_minimal()

  
  
##### Plot predicted occupancy for each treatment type - JT ---------------------
 
# This example produces marginal estimates of occupancy for different treatment types
  
  a=runjags::combine.mcmc(a2)
  b <- a
  
  # number of posterior samples
  n.samples = nrow(b) # why is this such a weird number?
  
# BU    
  # range of BU
  r<- range(BU.new)
  
  # create sequence along range of BU
  BU_data <- seq(r[1], r[2], length.out=2) #is this not identical to r created above?
  
  # set all other covs at mean
  HB = 0
  HU = 0
  BS = 0
  lat = 0
  long = 0
  elev = 0
  
  # create matrices to stick estimates in
  logit_psi = matrix(NA, n.samples, length(BU_data))
  
  # psi predictions for enes
  # Sample from posterior for the sequence of values for BU - raw linear predictor for each MCMC sample 
  for (i in 1:n.samples){
    for (j in 1:length(BU_data)){
      # create the linear predictors for dominant species
      logit_psi[i,j] = b[,'beta0.psi'][[i]] + b[,'beta1.psi.BU'][[i]] * BU_data[j] + b[,'beta2.psi.HB'][[i]] * HB + b[,'beta3.psi.HU'][[i]] * HU
      + b[,'beta4.psi.BS'][[i]] * BS + b[,'beta5.psi.lat'][[i]] * lat +  b[,'beta6.psi.lon'][[i]] * long +  b[,'beta8.psi.elev'][[i]] * elev 
    }}
  
  # create array 
  BU_psi = matrix(NA, n.samples, length(BU_data))
  
  # transform psi off logit-scale back to probability scale - for each sample
  BU_psi <- plogis(logit_psi)
  
  # calculate means and credible intervals - mean predicted occupancy probability across all posterior samples
  BU_psi_means = colMeans(BU_psi) # - taking the mean of all samples (transformed occu prob)
  BU_psi_CIs <- apply(BU_psi,2,quantile, c(0.025,0.975), na.rm=TRUE)
  
  # stuff into df
  BU_psi_preds <- data.frame(yesno = BU_data, 
                             predicted = BU_psi_means, 
                             treatment = "BU",
                             LCI = BU_psi_CIs[1,],
                             UCI = BU_psi_CIs[2,])
  
# HB
  r<- range(HB.new)
  HB_data <- seq(r[1], r[2], length.out=2)
  
  # set all other covs at mean
  BU = 0
  HU = 0
  BS = 0
  lat = 0
  long = 0
  elev = 0
  
  # create matrices to stick estimates in
  logit_HB_psi = matrix(NA, n.samples, length(HB_data))
  
  # psi predictions for enes
  # Sample from posterior for the sequence of values for HB
  for (i in 1:n.samples){
    for (j in 1:length(HB_data)){
      # create the linear predictors for dominant species
      logit_psi[i,j] = b[,'beta0.psi'][[i]] + b[,'beta1.psi.BU'][[i]] * BU + b[,'beta2.psi.HB'][[i]] * HB_data[j] + b[,'beta3.psi.HU'][[i]] * HU
      + b[,'beta4.psi.BS'][[i]] * BS + b[,'beta5.psi.lat'][[i]] * lat +  b[,'beta6.psi.lon'][[i]] * long +  b[,'beta8.psi.elev'][[i]] * elev 
    }}
  
  HB_psi = matrix(NA, n.samples, length(HB_data)) # create array 
  HB_psi <- plogis(logit_psi) # transform psi off logit-scale back to probability scale
  HB_psi_means = colMeans(HB_psi) # calculate means and credible intervals
  HB_psi_CIs <- apply(HB_psi,2,quantile, c(0.025,0.975), na.rm=TRUE)
  
  HB_psi_preds <- data.frame(yesno = HB_data, 
                             predicted = HB_psi_means, 
                             treatment = "HB",
                             LCI = HB_psi_CIs[1,],
                             UCI = HB_psi_CIs[2,])
  
# HU   
  r<- range(HU.new)
  HU_data <- seq(r[1], r[2], length.out=2)
  
  # set all other covs at mean
  BU = 0
  HB = 0
  BS = 0
  lat = 0
  long = 0
  elev = 0
  
  # create matrices to stick estimates in
  logit_HU_psi = matrix(NA, n.samples, length(HU_data))
  
  # psi predictions for enes
  # Sample from posterior for the sequence of values for HU 
  for (i in 1:n.samples){
    for (j in 1:length(HU_data)){
      # create the linear predictors for dominant species
      logit_psi[i,j] = b[,'beta0.psi'][[i]] + b[,'beta1.psi.BU'][[i]] * BU + b[,'beta2.psi.HB'][[i]] * HB + b[,'beta3.psi.HU'][[i]] * HU_data[j]
      + b[,'beta4.psi.BS'][[i]] * BS + b[,'beta5.psi.lat'][[i]] * lat +  b[,'beta6.psi.lon'][[i]] * long +  b[,'beta8.psi.elev'][[i]] * elev 
    }}
  
  HU_psi = matrix(NA, n.samples, length(HU_data))   # create array 
  HU_psi <- plogis(logit_psi)   # transform psi off logit-scale back to probability scale
  HU_psi_means = colMeans(HU_psi)   # calculate means and credible intervals
  HU_psi_CIs <- apply(HU_psi,2,quantile, c(0.025,0.975), na.rm=TRUE)
  
  HU_psi_preds <- data.frame(yesno = HU_data, 
                             predicted = HU_psi_means, 
                             treatment = "HU",
                             LCI = HU_psi_CIs[1,],
                             UCI = HU_psi_CIs[2,])
  
# BS
  r<- range(BS.new)
  BS_data <- seq(r[1], r[2], length.out=2)
  
  # set all other covs at mean
  BU = 0
  HB = 0
  HU = 0
  lat = 0
  long = 0
  elev = 0
  
  # create matrices to stick estimates in
  logit_BS_psi = matrix(NA, n.samples, length(BS_data))
  
  # psi predictions for enes
  # Sample from posterior for the sequence of values for BS 
  for (i in 1:n.samples){
    for (j in 1:length(BS_data)){
      # create the linear predictors for dominant species
      logit_psi[i,j] = b[,'beta0.psi'][[i]] + b[,'beta1.psi.BU'][[i]] * BU + b[,'beta2.psi.HB'][[i]] * HB + b[,'beta3.psi.HU'][[i]] * HU +
        b[,'beta4.psi.BS'][[i]] * BS_data[j] + b[,'beta5.psi.lat'][[i]] * lat +  b[,'beta6.psi.lon'][[i]] * long +  b[,'beta8.psi.elev'][[i]] * elev 
    }}
  
  BS_psi = matrix(NA, n.samples, length(BS_data))   # create array 
  BS_psi <- plogis(logit_psi)   # transform psi off logit-scale back to probability scale
  BS_psi_means = colMeans(BS_psi)  # calculate means and credible intervals
  BS_psi_CIs <- apply(BS_psi,2,quantile, c(0.025,0.975), na.rm=TRUE)
  
  BS_psi_preds <- data.frame(yesno = BS_data, 
                             predicted = BS_psi_means, 
                             treatment = "BS",
                             LCI = BS_psi_CIs[1,],
                             UCI = BS_psi_CIs[2,])
  
# UU  
  UU_psi_preds <- subset(BS_psi_preds, yesno == 0)
  UU_psi_preds$treatment <- "UU"
  
# Combine
  alltreatment_preds <- rbind(BS_psi_preds, HU_psi_preds, BU_psi_preds, HB_psi_preds)
  alltreatment_preds_noUU <- subset(alltreatment_preds, yesno==1)
  alltreatment_preds <- rbind(alltreatment_preds_noUU, UU_psi_preds)
 
# Plot
  
  #tiff("forest_and_fire_treatment_salamnder_psi_preds_withUU.tif", units="in", width=4, height=3, res=500)
  
  ggplot(alltreatment_preds, aes(x = treatment, y = predicted)) +
    geom_point(position = position_dodge(0.5), size = 1.5)+ geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.1, position = position_dodge(0.5))+
    ylab(bquote("Predicted "*psi~""))+ xlab("Treatment Group")+
    labs(title="Predicted Occupancy Estimates by Treatment - ENES") +
    theme_classic()
  #dev.off()  
  
  
  
#### Coefficient Plot ----------------------------------------------------------
  
  summary_table <- summary(a2)
  
  params_to_plot <- c("beta0.psi", "beta0.theta", "alpha0", 
                      "beta1.psi.BU", "beta2.psi.HB", "beta3.psi.HU", "beta4.psi.BS", 
                      "beta5.psi.lat", "beta6.psi.lon", "beta8.psi.elev",
                      "beta1.theta.DW", "alpha1", "alpha2")
  
  # summarize
  coef_df <- data.frame(
    Parameter = rownames(summary_table$statistics),
    Mean = summary_table$statistics[, "Mean"],
    LCI = summary_table$quantiles[, "2.5%"],
    UCI = summary_table$quantiles[, "97.5%"]
  )
  
  # subset parameters
  coef_df <- subset(coef_df, Parameter %in% params_to_plot)
  
  # add submodel column to group by
  coef_df$Submodel <- case_when(
    coef_df$Parameter %in% c("beta0.psi", "beta1.psi.BU", "beta2.psi.HB", "beta3.psi.HU", "beta4.psi.BS", 
                             "beta5.psi.lat", "beta6.psi.lon", "beta8.psi.elev") ~ "Site Occupancy (ψ)",
    
    coef_df$Parameter %in% c("beta0.theta", "beta1.theta.DW") ~ "Subplot Use (θ)",
    
    coef_df$Parameter %in% c("alpha0", "alpha1", "alpha2") ~ "Detection (p)",
    
    TRUE ~ "Other"
  )
  
  # order submodel groupings in plot
  coef_df$Submodel <- factor(coef_df$Submodel, levels = c(
    "Site Occupancy (ψ)",
    "Subplot Use (θ)", 
    "Detection (p)"
  ))
  
  # rename - optional
  # coef_df$Parameter <- recode(coef_df$Parameter,
  #                             "beta1.psi.BU" = "Burned (BU)",
  #                             "beta2.psi.HB" = "Harvest+Burn (HB)",
  #                             "beta3.psi.HU" = "Harvested (HU)",
  #                             "beta4.psi.BS" = "Burn+Salvage (BS)",
  #                             "beta0.psi" = "Site Occu Intercept",
  #                             "beta0.theta" = "Plot Use Intercept",
  #                             "alpha0" = "Detection Intercept",
  #                             "beta5.psi.lat" = "Latitude",
  #                             "beta6.psi.lon" = "Longitude",
  #                             "beta8.psi.elev" = "Elevation",
  #                             "beta1.theta.DW" = "Downed Wood",
  #                             "alpha1" = "Linear Temp",
  #                             "alpha2" = "Quadratic Temp")
  
  
  ggplot(coef_df, aes(x = Mean, y = reorder(Parameter, Mean))) +
    geom_point() +
    geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~ Submodel,scales = "free_y", ncol=1) +
    labs(title = "Posterior Coefficient Estimates",
         x = "Estimate (logit scale)",
         y = "Parameter") 
  
  
#### Marginal parameter plots -------------------------------------------  
  
# Isolate the effect of a single covariate by varying it across a range,
# holding everything else constant
  
  # a=runjags::combine.mcmc(a2)
  # b <- a
  # n.samples = nrow(b) # number of posterior samples
  
  
# Latitude on psi
  
  # range of lat
  r<- range(lat.2D)
  dim(lat.2D)
  
  # create sequence along range
  lat_data <- seq(r[1], r[2], length.out=50) 
  
  # set all other covs at mean
  HB = 0
  HU = 0
  BS = 0
  BU = 0
  long = 0
  elev = 0
  
  # create matrices to stick estimates in
  logit_lat_psi = matrix(NA, n.samples, length(lat_data))
  
  # psi predictions for enes
  # Sample from posterior for the sequence of values for cov - raw linear predictor for each MCMC sample 
  for (i in 1:n.samples){
    for (j in 1:length(lat_data)){
      # create the linear predictors for dominant species
      logit_lat_psi[i,j] = 
        b[,'beta0.psi'][[i]] + 
        b[,'beta1.psi.BU'][[i]] * BU + 
        b[,'beta2.psi.HB'][[i]] * HB + 
        b[,'beta3.psi.HU'][[i]] * HU + 
        b[,'beta4.psi.BS'][[i]] * BS + 
        b[,'beta5.psi.lat'][[i]] * lat_data[j] +  
        b[,'beta6.psi.lon'][[i]] * long +  
        b[,'beta8.psi.elev'][[i]] * elev 
    }}
  
  # create array 
  lat_psi = matrix(NA, n.samples, length(lat_data))
  
  # transform psi off logit-scale back to probability scale - for each sample
  lat_psi <- plogis(logit_lat_psi)
  
  # calculate means and credible intervals - mean predicted occupancy probability across all posterior samples
  lat_psi_means = colMeans(lat_psi) # - taking the mean of all samples (transformed occu prob)
  lat_psi_CIs <- apply(lat_psi,2,quantile, c(0.025,0.975), na.rm=TRUE)
  
  # stuff into df
  lat_psi_preds <- data.frame(predicted = lat_psi_means, 
                             lat_untransf = lat_data,
                             LCI = lat_psi_CIs[1,],
                             UCI = lat_psi_CIs[2,])
  
  ggplot(lat_psi_preds, aes(x = lat_untransf, y = predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2) +
    ylab(bquote("Predicted "*psi~"")) +
    xlab("Latitude (standardized)") +
    labs(title = "Marginal Effect of Latitude on Occupancy") +
    theme_classic()
  
  
  
# Longitude on psi
  
  # range
  r<- range(lon.2D)
  # create sequence along range
  lon_data <- seq(r[1], r[2], length.out=50) 
  
  # set all other covs at mean
  HB = 0
  HU = 0
  BS = 0
  BU = 0
  lat = 0
  elev = 0
  
  # create matrices to stick estimates in
  logit_lon_psi = matrix(NA, n.samples, length(lon_data))
  
  # psi predictions for enes
  # Sample from posterior for the sequence of values for cov
  for (i in 1:n.samples){
    for (j in 1:length(lon_data)){
      logit_lon_psi[i,j] = 
        b[,'beta0.psi'][[i]] + 
        b[,'beta1.psi.BU'][[i]] * BU + 
        b[,'beta2.psi.HB'][[i]] * HB + 
        b[,'beta3.psi.HU'][[i]] * HU + 
        b[,'beta4.psi.BS'][[i]] * BS + 
        b[,'beta5.psi.lat'][[i]] * lat +  
        b[,'beta6.psi.lon'][[i]] * lon_data[j] +  
        b[,'beta8.psi.elev'][[i]] * elev 
    }}
  
  # create array 
  lon_psi = matrix(NA, n.samples, length(lon_data))
  
  # transform psi off logit-scale back to probability scale
  lon_psi <- plogis(logit_lon_psi)
  
  # calculate means and credible intervals 
  lon_psi_means = colMeans(lon_psi) 
  lon_psi_CIs <- apply(lon_psi,2,quantile, c(0.025,0.975), na.rm=TRUE)
  
  # stuff into df
  lon_psi_preds <- data.frame(predicted = lon_psi_means, 
                              lon_untransf = lon_data,
                              LCI = lon_psi_CIs[1,],
                              UCI = lon_psi_CIs[2,])
  
  ggplot(lon_psi_preds, aes(x = lon_untransf, y = predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2) +
    ylab(bquote("Predicted "*psi~"")) +
    xlab("Longitude (standardized)") +
    labs(title = "Marginal Effect of Longitude on Occupancy") +
    theme_classic()
  
  
  
# Elevation on psi
  
  # range
  r<- range(elev.2D)
  # create sequence along range
  elev_data <- seq(r[1], r[2], length.out=50) 
  
  # set all other covs at mean
  HB = 0
  HU = 0
  BS = 0
  BU = 0
  lat = 0
  long = 0
  
  # create matrices to stick estimates in
  logit_elev_psi = matrix(NA, n.samples, length(elev_data))
  
  # psi predictions for enes
  # Sample from posterior for the sequence of values for cov 
  for (i in 1:n.samples){
    for (j in 1:length(elev_data)){
      logit_elev_psi[i,j] = 
        b[,'beta0.psi'][[i]] + 
        b[,'beta1.psi.BU'][[i]] * BU + 
        b[,'beta2.psi.HB'][[i]] * HB + 
        b[,'beta3.psi.HU'][[i]] * HU + 
        b[,'beta4.psi.BS'][[i]] * BS + 
        b[,'beta5.psi.lat'][[i]] * lat +  
        b[,'beta6.psi.lon'][[i]] * long +  
        b[,'beta8.psi.elev'][[i]] * elev_data[j] 
    }}
  
  # create array 
  elev_psi = matrix(NA, n.samples, length(elev_data))
  
  # transform psi off logit-scale back to probability scale
  elev_psi <- plogis(logit_elev_psi)
  
  # calculate means and credible intervals
  elev_psi_means = colMeans(elev_psi) 
  elev_psi_CIs <- apply(elev_psi,2,quantile, c(0.025,0.975), na.rm=TRUE)
  
  # stuff into df
  elev_psi_preds <- data.frame(predicted = elev_psi_means, 
                              elev_untransf = elev_data,
                              LCI = elev_psi_CIs[1,],
                              UCI = elev_psi_CIs[2,])
  
  ggplot(elev_psi_preds, aes(x = elev_untransf, y = predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2) +
    ylab(bquote("Predicted "*psi~"")) +
    xlab("Elev (standardized)") +
    labs(title = "Marginal Effect of Elevation on Occupancy") +
    theme_classic()
  
  
  
# Downed Wood effect on Plot Use
  
  # range
  r<- range(downedwood.3D)
  # create sequence along range
  DW_data <- seq(r[1], r[2], length.out=50) 
  
  # create matrices to stick estimates in
  logit_DW_theta = matrix(NA, n.samples, length(DW_data))
  
  # theta predictions for enes
  # Sample from posterior for the sequence of values for cov 
  for (i in 1:n.samples){
    for (j in 1:length(DW_data)){
      logit_DW_theta[i,j] = 
        b[,'beta0.theta'][[i]] + 
        b[,'beta1.theta.DW'][[i]] * DW_data[j] 
    }}
  

  # create array 
  DW_theta = matrix(NA, n.samples, length(DW_data))
  
  # transform psi off logit-scale back to probability scale
  DW_theta <- plogis(logit_DW_theta)
  
  # calculate means and credible intervals
  DW_means = colMeans(DW_theta) 
  DW_CIs <- apply(DW_theta,2,quantile, c(0.025,0.975), na.rm=TRUE)
  
  # stuff into df
  DW_psi_preds <- data.frame(predicted = DW_means, 
                               DW_untransf = DW_data,
                               LCI = DW_CIs[1,],
                               UCI = DW_CIs[2,])
  
  ggplot(DW_psi_preds, aes(x = DW_untransf, y = predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2) +
    ylab(bquote("Predicted "*theta~"")) +
    xlab("Downed Wood Count") +
    labs(title = "Marginal Effect of Downed Wood on Plot Use") +
    theme_classic()  
  
  
  
# Temp effect on Detection
  
  # do i need to include the yearly detection intercept?
  # creating an average across years (they look almost identical)
  alpha0_mean <- rowMeans(b[, grep("alpha0.year\\[", colnames(b))])
  
  # range
  r<- range(temp.3D)
  # create sequence along range
  temp_data <- seq(r[1], r[2], length.out=50) 
  
  # create matrices to stick estimates in
  logit_temp = matrix(NA, n.samples, length(temp_data))
  
  # theta predictions for enes
  # Sample from posterior for the sequence of values for cov 
  for (i in 1:n.samples){
    for (j in 1:length(temp_data)){
      logit_temp[i,j] = 
        alpha0_mean[i] +
        #b[,'alpha0'][[i]] + 
        b[,'alpha1'][[i]] * temp_data[j] + 
        b[,'alpha2'][[i]] * temp_data[j]^2 
    }}
  
  
  # create array 
  temp_p = matrix(NA, n.samples, length(temp_data))
  
  # transform psi off logit-scale back to probability scale
  temp_p <- plogis(logit_temp)
  
  # calculate means and credible intervals
  temp_means = colMeans(temp_p ) 
  temp_CIs <- apply(temp_p ,2,quantile, c(0.025,0.975), na.rm=TRUE)
  
  # stuff into df
  temp_preds <- data.frame(predicted = temp_means, 
                             temp_untransf = temp_data,
                             LCI = temp_CIs[1,],
                             UCI = temp_CIs[2,])
  
  ggplot(temp_preds, aes(x = temp_untransf, y = predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2) +
    ylab(bquote("Predicted p")) +
    xlab("Temperature") +
    labs(title = "Marginal Effect of Temp on Detection") +
    theme_classic()  
  
  

