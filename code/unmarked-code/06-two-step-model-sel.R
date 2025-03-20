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

    #df_oss <- read.csv("data/occupancy/oss-for-UMF-1.csv")

    df_oss <- read.csv("https://raw.githubusercontent.com/jwilliamson24/oss-occu/refs/heads/main/data/occupancy/data-UMF-1.csv")
    

## create UMF object ------------------------------------------------------------------------------------
    
    UMF.oss.1 <- unmarkedFrameOccu(
      y = df_oss[, grep("^OD", names(df_oss))],  # selects cols starting with OD = oss detections
      siteCovs = df_oss[, c("trt","veg_cov","canopy_cov","soil_moist","fwd_cov","dwd_count")],
      obsCovs = list(
        temp = df_oss[, grep("temp.", names(df_oss))], # selects cols including temp-
        soilmoist = df_oss[, grep("soilmoist.", names(df_oss))],
        rain = df_oss[, grep("rain.", names(df_oss))],
        dwd_cov = df_oss[, grep("dwdcov.", names(df_oss))],
        trt = df_oss[, grep("trt.", names(df_oss))]
      )
    )
    
    
## first model ------------------------------------------------------------------------------------
    
    # psi is null (~1) and detection has all the covariates of interest
    # formula: occu(~p ~psi, data=UMF, se=TRUE)
    
    m2 <- occu( ~ scale(temp) + scale(soilmoist) + scale(rain) + dwd_cov + trt
                ~ 1,
                data = UMF.oss.1, se = TRUE)
    
    # i think this technically converges, but some estimates show high SEs compared to their magnitude 

    
    m3 <- occu( ~ scale(temp) + scale(soilmoist) + scale(rain) + dwd_cov
                ~ 1,
                data = UMF.oss.1, se = TRUE)

    # still have some estimates with relatively high SE

    
## first model selection  ---------------------------------------------------------------------------------
    
    # psi(1) and p(global)
    
    ms1 <- dredge(m3, rank = "QAIC", chat = 1.24)
    
    # Model selection table 
    #     psi(Int)  p(Int) p(dwd) p(scl(ran)) p(scl(slm)) p(scl(tmp)) df  logLik  QAIC delta weight
    # 2    0.8060 -1.4750 0.2933                                      3 -405.584 662.2  0.00  0.216
    # 6    0.7764 -1.4560 0.2940                 -0.1251              4 -404.848 663.0  0.81  0.144
    # 1    0.7667 -1.0020                                             2 -407.874 663.9  1.69  0.093
    # 10   0.8288 -1.4980 0.2990                             0.05448  4 -405.468 664.0  1.81  0.087
    # 4    0.8038 -1.4720 0.2927   -0.005882                          4 -405.582 664.2  2.00  0.080    
    
    # winner
    # p(dwd)
 
       
    
## second model  ---------------------------------------------------------------------------------
   
    # p(dwd) psi(global)
    
    m4 <- occu( ~ dwd_cov
                ~ trt + veg_cov + canopy_cov + scale(soil_moist) + fwd_cov + scale(dwd_count),
                data = UMF.oss.1, se = TRUE)
    
     
    # converges, estimates/SE are ok (not great)
    
    
    
## second model selection  ---------------------------------------------------------------------------------
    
    
    ms2 <- dredge(m4, rank = "AIC", fixed = "psi(trt)")
    
    # Model selection table 
    #      psi(Int) psi(cnp_cov) psi(fwd_cov) psi(scl(dwd_cnt)) psi(scl(sol_mst)) psi(trt) psi(veg_cov) p(Int) p(dwd_cov) df   logLik   AIC delta
    # 14 -0.43300       1.0340                        0.68920            0.3730        +              -1.0180             9 -398.540 815.1  0.00
    # 16  0.73360       1.1560    -0.482100           0.81790            0.4297        +              -1.0210            10 -397.581 815.2  0.08
    # 6  -0.45730       0.9718                        0.62310                          +              -1.0090             8 -399.688 815.4  0.30
    # 8   0.53450       1.0510    -0.404400           0.69870                          +              -1.0080             9 -398.956 815.9  0.83
    # 46 -0.45200       1.0490                        0.68730            0.3719        +              -0.8081    -0.1376 10 -398.047 816.1  1.01# 
    
    # winner
    # psi(dwd_count, soil_moist, trt) p(.)
    
    
    
    
    