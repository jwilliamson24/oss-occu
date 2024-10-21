##
## 02.1-dwd-exploration.R 
##
## Efforts to organize, explore, and summarize dwd data
##
## Jasmine Williamson
## Date Created: 10-18-2024
##
## -------------------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

    library(dplyr)
    library(tidyr)

#### Load data -----------------------------------------------------------------
#     site <- read.csv("site.complete.csv")
#     dwd_all <- read.csv("dwd.complete.csv")
#     subplot <- read.csv("subplot.complete.csv")
#     sals <- read.csv("sals.complete.csv", 
#                  colClasses = c(landowner="factor", stand="character", trt="factor",
#                                 obs="factor", subplot="factor", recap="factor",
#                                 pass="factor", spp="factor", cover_obj="factor", 
#                                 substrate="factor", age_class="factor"))
    dat <- readRDS("site_level_matrix.RDS")  
    row.names(dat) <- dat[,1]
    dwd <- dat[,c(5,19:25)]

#### Add density per m^2 to site matrix -----------------------------------------------------

## add log/stump dens
    dwd$dwd_dens <- round(dwd$dwd_count/567,2)
    dwd$log_dens <- round(dwd$logs/567,2)
    dwd$stump_dens <- round(dwd$stump/567,2)

## visualize    
    p1 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = dwd_count), fill = "#FF5050", alpha=0.8)
    p2 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = stumps), fill = "#FF5050", alpha=0.8)
    p3 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = logs), fill = "#FF5050", alpha=0.8)
    p4 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = size_cl), fill = "#FF5050", alpha=0.8)
    p5 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = decay_cl), fill = "#FF5050", alpha=0.8)
    p6 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = char_cl), fill = "#FF5050", alpha=0.8)
    p7 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = length_cl), fill = "#FF5050", alpha=0.8)
    p8 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = dwd_dens), fill = "#FF5050", alpha=0.8)
    p9 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = log_dens), fill = "#FF5050", alpha=0.8)
    p10 <- ggplot(dwd) + geom_boxplot(aes(x = trt, y = stump_dens), fill = "#FF5050", alpha=0.8)
    ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, ncol=3, nrow=4)    
    
    
# need to use literature to determine a metric for volume or area, the count of logs is not
# robust because there were larger diameter and longer logs in some plots (unharvested).

# could do a volume of each piece of wood on the original dwd df, then avg vol per m^2.
# could also calculate proportion of ground surface area occupied by logs by calculating
# cross-sectional area and est how much of the subplot is covered by wood.
    
# there is a paper (Woodall 2008) that uses a complex formula based on some data that I
# dont have...seems pretty robust. also has a decay factor. dont know how to make this
# useful for my data.
    
    
    
    
    
    
    