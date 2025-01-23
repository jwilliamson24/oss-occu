##
## 02.2-env-corr.R 
##
## Correllations and standardizations with env data
## Based on what we did in multivariate class Fall 2024
##
## Jasmine Williamson
## Date Created: 01-23-2025
##
##
## see decision making process for standardizations in:
## "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/multivariate-analysis/homework/homework_2.Rmd"
##
## settings -------------------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    
    library(dplyr)
    library(tidyr)
    library(vegan)
    library(pastecs)
    library(corrplot)
    library(ggplot2)
    library(gridExtra)
    source("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/multivariate-analysis/biostats.R")

#### load and subset data ---------------------------------------------------------------------------------------

    #site-level data
    dat <- readRDS("site_level_matrix.rds")
    row.names(dat) <- dat[,1]
    
    sals <- dat[26:27]
    env <- dat[1:25]
    
    #extra dwd metrics
    dwd <- read.csv("dwd.extra.metrics.csv")
    
    #env cont
    drop <- c("lat","long","stand","tree_farm","landowner","year","weather","trt","jul_date","elev","site_id")
    env_sub <- env[,!(colnames(env) %in% drop)]
    
    #add dwd dens and avg volume from dwd to env df
    env_merge <- cbind(env_sub, dwd[,c("dwd_dens","avg_volume")])


#### standardize environmental data -----------------------------------------------------------------

    env.zscore <- decostand(env_merge, "standardize") #Z-scores the data in each column

    
    
#### checking for correllations between covariates -----------------------------------------------------------------

#cor command looks for correlation, pearson is the default, order does not make a difference
#cor (var, var, method = "spearman" or "kendall for those")
#cor.test tests hypothesis that they're not correlated
#cov gives covariance
#pairs gives all possible pairwise plots
#pairs with [,1:3] tests only columns 1:3
#with plots, looking for linear pattern

    P.corr <- cor(env_merge, method = "pearson", use = "complete.obs")


    corrplot(P.corr, 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45)


    corrplot(P.corr, method="pie")


    cor.test(env_merge$temp,env_merge$hum,method="pearson") #statistically significant correllation
    cor.test(env_merge$length_cl,env_merge$stumps,method="pearson") #statistically significant correllation


    pairs(env_merge[,c(9,14)])

# figure out which ones are above 0.7
    # Set a threshold
    threshold <- 0.7
    
    # Get pairs with correlation above the threshold
    cor_pairs <- which(abs(P.corr) > threshold, arr.ind = TRUE)
    
    # Filter to keep only unique pairs (avoid duplicates and self-correlations)
    cor_pairs <- cor_pairs[cor_pairs[, 1] < cor_pairs[, 2], ]
    
    # Display the variable pairs and their correlation values
    result <- data.frame(
      Var1 = rownames(P.corr)[cor_pairs[, 1]],
      Var2 = colnames(P.corr)[cor_pairs[, 2]],
      Correlation = P.corr[cor_pairs]
    )


    # > result
    # Var1       Var2 Correlation
    # 1      temp        hum  -0.7599229   remove hum
    # 2 dwd_count     stumps   0.7717364   remove stumps
    # 3    stumps  length_cl  -0.7859440   
    # 4 dwd_count   dwd_dens   0.9931322   remove dwd_dens
    # 5    stumps   dwd_dens   0.7636342
    # 6      logs avg_volume   0.8001072   remove logs

    
#### reorganize env subset df and try corr again -------------------------------------

    drop <- c("hum","stumps","dwd_dens","logs")
    env_subset_corr <- env_merge[,!(colnames(env_merge) %in% drop)]
    
    
    env_corr <- cor(env_subset_corr, method = "pearson", use = "complete.obs")
    
    corrplot(env_corr, 
             type = "upper", 
             order = "hclust", 
             tl.col = "black", 
             tl.srt = 45)    
 
    # Set a threshold
    threshold <- 0.6
    
    # Get pairs with correlation above the threshold
    cor_pairs <- which(abs(env_corr) > threshold, arr.ind = TRUE)
    
    # Filter to keep only unique pairs (avoid duplicates and self-correlations)
    cor_pairs <- cor_pairs[cor_pairs[, 1] < cor_pairs[, 2], ]
    
    # Display the variable pairs and their correlation values
    result <- data.frame(
      Var1 = rownames(env_corr)[cor_pairs[, 1]],
      Var2 = colnames(env_corr)[cor_pairs[, 2]],
      Correlation = env_corr[cor_pairs]
    )
    result
    
    
    # Var1       Var2 Correlation
    # 1 canopy_cov  length_cl   0.6621768       
    # 2  length_cl avg_volume   0.6592068
    
    
    cor.test(env_subset_corr$length_cl,env_subset_corr$canopy_cov,method="pearson") #statistically significant correllation
    cor.test(env_subset_corr$length_cl,env_subset_corr$avg_volume,method="pearson")
    
    pairs(env_subset_corr[,c(2,11)])
    pairs(env_subset_corr[,c(12,11)])
    
    
#### visualize covariate relationships with sal data to determine importance -------------------------------------
    
    plot(env_subset_corr$length_cl, sals$oss)    
    plot(env_subset_corr$length_cl, sals$enes) 
 
    merged <- cbind(env_subset_corr,sals)
    
    ggplot(merged, aes(x = length_cl, y = oss)) +
      geom_point(color = "blue", size = 2) +  
      geom_smooth(method = "lm", color = "red", se = TRUE) 
    
    ggplot(merged, aes(x = avg_volume, y = oss)) +
      geom_point(color = "blue", size = 2) +  
      geom_smooth(method = "lm", color = "red", se = TRUE) 
    
    
    # Function to generate scatter plots for each covariate
    generate_plot <- function(covariate) {
      ggplot(merged, aes_string(x = covariate, y = "oss")) +
        geom_point(color = "blue", size = 2) +  # Scatterplot points
        geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression line
        labs(
          title = paste("Salamander Count vs.", covariate),
          x = covariate,
          y = "Salamander Count"
        ) +
        theme_minimal()
    }
    
    # Generate the plots for all covariates
    plots <- lapply(env_subset_corr, generate_plot)
    
    # Arrange all plots into a grid (e.g., 2 rows and 2 columns)
    grid.arrange(grobs = plots, ncol = 3)
 
       
#### reorganize env subset df and try corr again -------------------------------------
    
# avg volume seems to have a slightly larger positive relationship with oss than length class does
# so I will remove length class because that will remove the correllation with canopy cover as well
    
    
    env_subset_corr <- env_subset_corr[,!colnames(env_subset_corr) %in% "length_cl"]

    corr3 <- cor(env_subset_corr, method = "pearson", use = "complete.obs") 
    
    corrplot(corr3, 
             type = "upper", 
             order = "hclust", 
             tl.col = "black", 
             tl.srt = 45)  
    
    # Set a threshold
    threshold <- 0.5
    
    # Get pairs with correlation above the threshold
    cor_pairs <- which(abs(corr3) > threshold, arr.ind = TRUE)
    
    # Filter to keep only unique pairs (avoid duplicates and self-correlations)
    cor_pairs <- cor_pairs[cor_pairs[, 1] < cor_pairs[, 2], ]
    
    # Display the variable pairs and their correlation values
    result <- data.frame(
      Var1 = rownames(corr3)[cor_pairs[, 1]],
      Var2 = colnames(corr3)[cor_pairs[, 2]],
      Correlation = corr3[cor_pairs]
    )
    result
    
    # Var1     Var2 Correlation
    # 1 canopy_cov decay_cl   0.5273213
    # 2    fwd_cov  char_cl  -0.5417591  
    
## these are somewhat correlated but the value is below 0.55 so I wont remove them right now.
    
    
    
    
    