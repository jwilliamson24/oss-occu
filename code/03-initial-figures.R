##
## 03-initial-figures.R 
##
## Figures using the code from 2023 data summary
##
## Jasmine Williamson
## Date Created: 07-01-2024
##
#### settings ---------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

library(ggplot2)
library(unmarked)
library(RColorBrewer)
library(tidyverse)
library(ggpattern)
library(dplyr)
library(ggthemes)


#### load data --------------------------------------------------------------------------------------------

site <- read.csv("site.complete.csv")
dwd <- read.csv("dwd.complete.csv")
subplot <- read.csv("subplot.complete.csv")
sals <- read.csv("sals.complete.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", subplot="factor", recap="factor",
                                pass="factor", spp="factor", cover_obj="factor", 
                                substrate="factor", age_class="factor"))

#sals_2023 <- read.csv("sals.2023.csv")
sals_2023 <- subset(sals, year == "2023")
#sals_2024 <- read.csv("sals.2024.csv")
sals_2024 <- subset(sals, year == "2024")
#### format data -----------------------------------------------------------------------------------------

# add detection col
sals$detect <- 1


#### bar plot of standardized counts per treatment by species -----------------------------------------


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




#### bar plot of counts by date ---------------------------------------------------------


# group by date and year and add count column
days_count <- sals %>%
  group_by(date_mdy, year) %>%
  summarize(n = n())
names(days_count) <- c("date","year","count")
days_count <- as.data.frame(days_count)

# adding month column
days_count$month <- month(days_count$date)

days_count_2023 <- subset(days_count, year == 2023)
days_count_2024 <- subset(days_count, year == 2024)


# Define colors
colors <- c("salmon","chartreuse3", "turquoise3", "purple")

# both years
# Manually map colors to each unique month
color_map <- setNames(colors, unique(days_count$month))
days_count$month <- as.factor(days_count$month)
barplot(days_count$count,
        space=1,
        main="Counts by Date",
        ylab="Daily Count",
        xlab="Month",
        col=color_map[days_count$month])


# 2023
ggplot(days_count_2023, aes(x = date, y = count, fill = month)) +
  geom_col() + 
  theme_classic() + 
  labs(title = "Counts by Date", x = "Date", y = "Daily Count") +
  scale_fill_manual(values = colors) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5), # Center the title
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
        legend.background = element_rect(fill = "transparent", color = NA), # Transparent legend background
        legend.box.background = element_rect(fill = "transparent", color = NA)) 
ggsave("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/figures/03-initial-figures/barplot_2023_counts_by_date.png",
       plot = last_plot(), bg = "transparent")


# 2024
ggplot(days_count_2024, aes(x = date, y = count, fill = month)) +
  geom_col() + 
  theme_classic() + 
  labs(title = "Counts by Date", x = "Date", y = "Daily Count") +
  scale_fill_manual(values = colors) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5), # Center the title
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
        legend.background = element_rect(fill = "transparent", color = NA), # Transparent legend background
        legend.box.background = element_rect(fill = "transparent", color = NA)) 
ggsave("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/figures/03-initial-figures/barplot_2024_counts_by_date.png",
       plot = last_plot(), bg = "transparent")


#### transparent temp trend line ---------------------------------------------------------


# 2023 (trend line also found in Pleth_conf_exploratory)
site_2023 <- subset(site, year == 2023)

p <- ggplot(site_2023, aes(x = date_mdy, y = temp, group = 1)) +
  geom_point(color="steelblue") +
  geom_line(color="steelblue", linetype="solid", linewidth=2) + 
  theme_classic() +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )
ggsave(filename = "temp_line_2023.png", plot = p, device = "png", 
       path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/figures/03-initial-figures",
       width = 15, height = 8, units = "in", dpi = 300, bg = "transparent")



# 2024
site_2024 <- subset(site, year == 2024)

p1 <- ggplot(site_2024, aes(x = date_mdy, y = temp, group = 1)) +
  geom_point(color="lightgreen") +
  geom_line(color="lightgreen", linetype="solid", size=2) + 
  theme_classic() +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )
ggsave(filename = "temp_line_2024.png", plot = p1, device = "png", 
       path = "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/figures/03-initial-figures",
       width = 15, height = 8, units = "in", dpi = 300, bg = "transparent")



