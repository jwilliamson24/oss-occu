# -------------------------------------------------------------------------------------------------------
##
## 08-new-relationships.R 
##
## Look at relationships between habitat and sal variables including new habitat covariates
##
## Jasmine Williamson
## Date Created: 02-05-2025
##
## 
## goals --------------------------------------------------------------------------------------------------

# i want to Look at relationships between habitat and sal variables 
# including new habitat covariates: precip, days since rain, aspect
# and including insights from the random forest importanct plot

## insights --------------------------------------------------------------------------------------------------

# this might be a waste of time

## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(dplyr)
    library(tidyr)
    library(tidyverse)


## load data ----------------------------------------------------------------------------------------------


# site level 
    
    # add df with precip data
    env_subset_corr <- read.csv("env_subset_corr.csv", row.names = 1)
    #env_subset_corr <- rownames_to_column(env_subset_corr, var = "site_id")
    
    # all site data - has sal site counts
    dat <- read.csv("site_aspect_precip_all_vars.csv") 
    dat <- subset(dat, select = -X)
    row.names(dat) <- dat[,1]
    
    sal.site.count <- dat[26:27]
    #sal.site.count <- rownames_to_column(sal.site.count, var = "site_id")
    
    #merge
    site_merge <- full_join(env_subset_corr,sal.site.count,by="site_id")
    rownames(site_merge) <- site_merge[,1]
    
# subplot level
    
    subplot <- read.csv("subplot.complete.csv")

    #need a subplot level df with new variables
    dfmerge1 <- full_join(subplot,env_subset_corr,by="site_id")
    
    #sals counts
    oss.counts.sub <- read.csv("oss_counts_subplot.csv")
        colnames(oss.counts.sub)[colnames(oss.counts.sub) == "counts"] <- "oss.counts"
    enes.counts.sub <- read.csv("enes_counts_subplot.csv")
        colnames(enes.counts.sub)[colnames(enes.counts.sub) == "counts"] <- "enes.counts"
    dfmerge <- full_join(oss.counts.sub,enes.counts.sub,by=c("site_id","subplot"))

    #merge sals with subplot env vars
    subplot_merge <- full_join(dfmerge,dfmerge1,by=c("site_id","subplot"))
    subplot_merge <- subset(subplot_merge[,c(1:4,20:25,27:34)])

    
#### using site level covariates and sal counts --------------------------------------------------
    
    plot(site_merge$aspect, sals$oss)    
    plot(site_merge$days_since_rain, sals$oss) 
    
    
    ggplot(site_merge, aes(x = days_since_rain, y = oss)) +
      geom_point(color = "blue", size = 2) +  
      geom_smooth(method = "lm", color = "red", se = TRUE) 
    
    ggplot(site_merge, aes(x = avg_volume, y = oss)) +
      geom_point(color = "blue", size = 2) +  
      geom_smooth(method = "lm", color = "red", se = TRUE) 
    
####
    
    correlation_matrix <- cor(cbind(sal.site.count, env_subset_corr), use = "pairwise.complete.obs")
    correlation_matrix[,1]


summary(lm(sal.site.count$oss~env_subset_corr$veg_cov))










