## -------------------------------------------------------------------------------------------------------
##
## 06-two-step-model-sel.R 
##
## Model selection for global model in previous script
##
## Jasmine Williamson
## Date Created: 03-18-2025
##

## goals: From Josh T
# two step model selection:

# Create a model where psi is null (~1) and detection has all the covariates of interest 
  # this model might look like ~ rainfall+ temp + avg vol + treatment ~ 1. 
  # If this converges go to step b, if this doesn't remove treatment.

# Conduct model selection on this using the function 'dredge' in MuMIn - take the top model 
  # ^^ watch out for parameter redundancy! (see Arnold, 2010). 

# Create a model using the top detection model, and all covariates of interest on psi 
  # this model might look like ~ top model (whatever came from step 2) ~ treatment + canopy + avg vol

# Conduct model selection this using function 'dredge' take top model <- check for parameter redundancy

## insights
## 


## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    
    library(unmarked)
    library(MuMIn)
    library(AICcmodavg)


## load data----------------------------------------------------------------------------------------------

    df_oss <- read.csv("data/occupancy/oss-for-UMF-1.csv")


## create UMF object ------------------------------------------------------------------------------------
    
    UMF.oss.1 <- unmarkedFrameOccu(
      y = df_oss[, grep("^X", names(df_oss))],  # selects cols starting with X
      siteCovs = df_oss[, c("trt","veg_cov","canopy_cov","soil_moist","fwd_cov","dwd_count")],
      obsCovs = list(
        temp = df_oss[, grep("temp.", names(df_oss))], # selects cols including temp-
        soilmoist = df_oss[, grep("soilmoist.", names(df_oss))],
        rain = df_oss[, grep("rain.", names(df_oss))],
        dwd = df_oss[, grep("dwdcov.", names(df_oss))],
        trt = df_oss[, grep("trt.", names(df_oss))]
      )
    )
    
    
## first model ------------------------------------------------------------------------------------
    
    # psi is null (~1) and detection has all the covariates of interest
    # formula: occu(~p ~psi, data=UMF, se=TRUE)
    
    m2 <- occu( ~ temp + soilmoist + rain + dwd + trt
                ~ 1,
                data = UMF.oss.1, se = TRUE)
    
    # i think this technically converges, but some estimates show high SEs compared to their magnitude 

    
    m3 <- occu( ~ temp + soilmoist + rain + dwd
                ~ 1,
                data = UMF.oss.1, se = TRUE)

    # some estimates are a little better without treatment

    
## first model selection  ---------------------------------------------------------------------------------
    
    # psi(1) and p(global)
    
    ms1 <- dredge(m3, rank = "AIC")
    
    # Model selection table 
    #    psi(Int)  p(Int) p(dwd)     p(ran)   p(slm)   p(tmp) df   logLik   AIC delta weight
    # 2    0.8060 -1.4750 0.2933                               3 -405.584 817.2  0.00  0.225
    # 6    0.7765 -1.0980 0.2940            -0.01478           4 -404.848 817.7  0.53  0.173
    # 10   0.8284 -1.7890 0.2989                     0.005448  4 -405.468 818.9  1.77  0.093
    # 14   0.8033 -1.5380 0.3022            -0.01658 0.008452  5 -404.580 819.2  1.99  0.083
    # 4    0.8035 -1.4690 0.2927 -0.0012510                    4 -405.582 819.2  2.00  0.083
    
    # winner
    # p(dwd)
 
       
    
## second model  ---------------------------------------------------------------------------------
   
    # p(dwd) psi(global)
    
    m4 <- occu( ~ dwd
                ~ trt + veg_cov + canopy_cov + soil_moist + fwd_cov + dwd_count,
                data = UMF.oss.1, se = TRUE)
    
     
    # converges, estimates/SE are ok (not great)
    
    
    
## second model selection  ---------------------------------------------------------------------------------
    
    
    ms2 <- dredge(m4, rank = "AIC")
    
    # Error in h(simpleError(msg, call)) : 
    #   error in evaluating the argument 'x' in selecting a method for function 'diag': Lapack routine dgesv: system is exactly singular: U[1,1] = 0
    # In addition: Warning message:
    #   Hessian is singular. Try providing starting values or using fewer covariates. 
    
    # why do i get these errors now if the model seemed to converge?
    
    
    
    