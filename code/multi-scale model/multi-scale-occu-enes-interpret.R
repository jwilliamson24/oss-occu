## =================================================
##
## Title: multi-scale-occu-enes-interpret
## Author: Jasmine Williamson 
## Date Created: 07/22/2025
##
## Description: Visualize and interpret outputs for all years
## multi-scale occupancy model, for ENES
##
## =================================================

# Load packages and data
  library(ggplot2)
  load("multiscale_output_and_data_072125_small.RData")

  
# Estimates
  summary(a2)


# Treatment effect on occupancy
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
    labs(title = "Effect of Treatment on Site Occupancy",
         y = "Predicted Occupancy Probability", x = "") +
    theme_minimal()
  


  # do these need to be back transformed first?
# occu across lat
  lat_range <- seq(min(lat.2D), max(lat.2D), length.out = 100)
  beta <- summary(a2)$statistics
  psi_lat <- plogis(beta["beta0.psi", "Mean"] + beta["beta5.psi.lat", "Mean"] * lat_range)
  
  plot(lat_range, psi_lat, type='l', ylab="Pr(Site Occupancy)", xlab="Latitude")

    
# occu across elev
  elev_range <- seq(min(elev.2D), max(elev.2D), length.out = 100)
  beta <- summary(a2)$statistics
  psi_elev <- plogis(beta["beta0.psi", "Mean"] + beta["beta8.psi.elev", "Mean"] * elev_range)
  
  plot(elev_range, psi_elev, type='l', ylab="Pr(Site Occupancy)", xlab="Elev")
  
  
# dw on plot use  
  dw_range <- seq(0, max(downedwood.3D, na.rm=TRUE), length.out=100)
  theta_dw <- plogis(summary(a2)$statistics["beta0.theta", "Mean"] +
                       summary(a2)$statistics["beta1.theta.DW", "Mean"] * dw_range)
  
  plot(dw_range, theta_dw, type='l', ylab="Pr(Plot Use)", xlab="Downed Wood Count")
  
  
  
  # look at yearly effects next
  
  
  
  
  
  
  
  

