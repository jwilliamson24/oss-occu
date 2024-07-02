## -------------------------------------------------------------------------------------------------------
##
## 03-initial-figures.R 
##
## Figures using the code from 2023 data summary
##
## Jasmine Williamson
## Date Created: 07-02-2024
##
## -------------------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

library(ggplot2)
library(unmarked)
library(RColorBrewer)
library(tidyverse)
library(ggpattern)
library(dplyr)

#### Load data --------------------------------------------------------------------------------------------

site <- read.csv("site.complete.csv")
dwd <- read.csv("dwd.complete.csv")
subplot <- read.csv("subplot.complete.csv")
sals <- read.csv("sals.complete.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", subplot="factor", recap="factor",
                                pass="factor", spp="factor", cover_obj="factor", 
                                substrate="factor", age_class="factor"))

sals_2023 <- read.csv("sals.2023.csv")
sals_2024 <- read.csv("sals.2024.csv")

#### Format data -----------------------------------------------------------------------------------------

# add detection col
sals$detect <- 1


# Make count by spp data frame

# Group by site_id and spp, then count
df_count <- sals %>%
  group_by(site_id, trt, year, spp) %>%
  summarize(n = n())

# Reshape the data to have one row per site and a count column for each species
spp_count <- df_count %>%
  pivot_wider(names_from = spp, values_from = n, values_fill = 0)

# delete spp i dont care about
spp_count <- spp_count[,-4]
spp_count <- spp_count[,-6]
spp_count <- spp_count[,-6]
spp_count <- spp_count[,-6]

spp_count <- as.data.frame(spp_count)
head(spp_count)

# Reshape the data for count by species
reshaped_spp_count <- spp_count %>%
  pivot_longer(cols = c(ENES,OSS), names_to = "spp", values_to = "count")

reshaped_spp_count <- as.data.frame(reshaped_spp_count)
head(reshaped_spp_count)



# Standardize the counts by treatment sample size

# Aggregate to data frame with one row for each treatment
trt_sal_counts <- aggregate(count ~ trt + spp, data = reshaped_spp_count, FUN = sum)

trt_sample_size <- as.data.frame(table(site$trt))#Count # times each trt type was sampled
names(trt_sample_size) <- c("trt","sample_size")

trt_counts_merged <- merge(trt_sal_counts, trt_sample_size, by = "trt")#merge the two

# Standardized counts = total observed sal count / number of sites sampled in that treatment
trt_counts_merged$stzd.count <- trt_counts_merged$count / trt_counts_merged$sample_size
trt_counts_merged$trt <- factor(trt_counts_merged$trt, 
                                      levels = c("UU", "BU", "HB", "HU", "BS"))




#### bar plot of standardized counts per treatment by species -----------------------------------------

# Standardized barplot per treatment 
png("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/figures/03-initial-figures/barplot_stzd_counts.png")
ggplot(trt_counts_merged, aes(x=trt, y=stzd.count, fill=trt, color=spp)) +
  geom_bar(stat='identity', position='dodge', width=0.8, alpha=1) + # Set alpha to 1 for full opacity
  ggtitle('Salamander Counts by Treatment and Species') +
  xlab('Treatment') +
  ylab('Count') +
  scale_fill_manual(values=c('lightgreen','steelblue', 'coral2', '#f9d62e', '#b967ff')) + # Custom fill colors for treatments
  scale_color_manual(values=c("black", "transparent")) # Specify black for one species and transparent for another
dev.off()


# bar plot counts by date 2024

# boxpot trt effect size

# boxplot trt occu prob