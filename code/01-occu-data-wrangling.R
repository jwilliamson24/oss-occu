##
## 01-occu-data-wrangling.R 
##
## Creates working csv's for all four data sheets/types:
## downed wood data = dwd.complete.csv
## site data = site.complete.csv
## subplot data = subplot.complete.csv
## salamander data = sals.complete.csv
## all data = habitat.occu.complete.csv
##
## Jasmine Williamson
## Date Created: 06-20-2024
##
## -------------------------------------------------------------------------------------------------------


rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data")

library(dplyr)
library(tidyr)
library(tidyverse)

## Notes on site data 6/24/24
#realized that stand numbers are not equal by treatment
#changed 10185 from HB to BS in all three original 2023 csv sheets
#16804 is listed as BU in 2023 data sheet (but listed as BS on my original detailed stand info google sheet)
#i have two extra HU sites. i used the wrong sheet to tally numbers for 2024 season plans, so i went over
#final counts: BS:24, BU:26, HB:25, HU:27, UU:25

#### Downed Wood Data -----------------------------------------------------------------
# downed wood data - done
# length class col = 0 if row is a stump


#2023 data
dwd_2023 <- read.csv("oss_2023_dwd.csv")

#identify rows with NA in decay class column
na_rows <- which(is.na(dwd_2023$decay_cl))
dwd_2023 <- dwd_2023[-na_rows, ] #Remove these rows from the data frame

#separate subplot code column into three columns by underscore
dwd_2023 <- dwd_2023 %>%
      separate(subplot_code, into = c("stands", "trt", "subplot"), sep = "_")

#separate "stands" column into two columns by letters and numbers
dwd_2023 <- dwd_2023 %>%
  mutate(landowner = str_extract(stands, "^([A-Za-z]+)"),
         stand = str_extract(stands, "(?<=\\D)(\\d+)")) %>% #call new column "stand"
  select(-stands) #remove original "stands" column 
dwd_2023 <- dwd_2023[, -6] #delete "id" column
dwd_2023$date_mdy <- as.Date(dwd_2023$date, format = "%m/%d/%Y") #format date, add column
dwd_2023$subplot <- as.numeric(gsub("^0+", "", dwd_2023$subplot)) #remove leading 0 so both years match


#add site_id column
dwd_2023$site_id <- paste(dwd_2023$stand,"_",dwd_2023$site_rep,"_",dwd_2023$year)


#make columns factors to check for data entry errors
dwd_2023$trt <- as.factor(dwd_2023$trt)
dwd_2023$subplot <- as.factor(dwd_2023$subplot)
dwd_2023$dwd_type <- as.factor(dwd_2023$dwd_type)
dwd_2023$size_cl <- as.factor(dwd_2023$size_cl)
dwd_2023$decay_cl <- as.factor(dwd_2023$decay_cl)
dwd_2023$char_cl <- as.factor(dwd_2023$char_cl)
dwd_2023$landowner <- as.factor(dwd_2023$landowner)



#2024 data
dwd_2024 <- read.csv("oss_2024_dwd.csv", 
                colClasses = c(landowner="factor", stand="character", trt="factor", 
                               subplot="factor", dwd_type="factor", size_cl="factor",
                               decay_cl="factor", char_cl="factor"))
dwd_2024$date_mdy <- as.Date(dwd_2024$date, format = "%m/%d/%Y")


#add site_id column
dwd_2024$site_id <- paste(dwd_2024$stand,"_",dwd_2024$site_rep,"_",dwd_2024$year)



#combine both years of downed wood
dwd.complete <- rbind(dwd_2023, dwd_2024)

#changing blank NA cells to zeros in the length class column
dwd.complete$length_cl[dwd.complete$length_cl == "" | is.na(dwd.complete$length_cl)] <- 0
dwd.complete$length_cl <- as.factor(dwd.complete$length_cl) #making it a factor


new_order <- c("site_id","landowner","stand","site_rep","trt","year","subplot",
               "date","date_mdy","dwd_type","size_cl","decay_cl","char_cl","length_cl"               )
dwd.complete <- dwd.complete[,new_order]

summary(dwd.complete)


# save as csv
write.csv(dwd.complete, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/dwd.complete.csv", 
          row.names = FALSE)




#### Salamander Data -----------------------------------------------------------------

# Notes on sal data 6/15/24
# this data frame includes all captures. unoccupied sites not included.
# svl = NA for animals that we didn't measure (non-OSS animals and OSS recaps)
# id = NA for all 2023 animals, blank for 2024 animals that are not OSS clips

#load and format 2023 data
sals_2023 <- read.csv("oss_2023_sals.csv", 
                      colClasses = c(obs="factor", pass="factor", 
                                     spp="factor", cover_obj="factor", 
                                     substrate="factor", age_class="factor"))

#separate subplot code column by underscore
sals_2023 <- sals_2023 %>%
  separate(subplot_code, into = c("stands", "trt", "subplot"), sep = "_")

#that left the landowner and stand number in one column. separate those out:
sals_2023 <- sals_2023 %>%
  mutate(landowner = str_extract(stands, "^([A-Za-z]+)"),
         stand = str_extract(stands, "(?<=\\D)(\\d+)")) %>%
  select(-stands) #replace "stands" col above with new "stand" col

#delete unnecessary col, add year, format date, delete leading zero
sals_2023 <- sals_2023[, -8] #delete "under" column
sals_2023$year <- 2023 #add year column
sals_2023$date_mdy <- as.Date(sals_2023$date, format = "%m/%d/%Y") #format date
sals_2023$subplot <- as.numeric(gsub("^0+", "", sals_2023$subplot)) #remove leading zero

#add unique site code, combo of stand#_replicate#_year
sals_2023$site_id <- paste(sals_2023$stand, "_", sals_2023$site_rep, "_", sals_2023$year)

#create a new column for sal_id
#single digit for each animal found within each site
#this groups the rows by the site identifier, then numbers each row sequentially within sites
sals_2023 <- sals_2023 %>%
  group_by(site_id) %>%
  mutate(sal_rep = row_number())

#add unique sal code for each animal, combo of site id and sal replicate
sals_2023$sal_id <- paste(sals_2023$site_id,"_",sals_2023$sal_rep)
#back to data frame, somehow became a tibble
sals_2023 <- as.data.frame(sals_2023) 

#reorder
new_order <- c("site_id","landowner","stand","site_rep","trt","year","date",
               "date_mdy","sal_id","sal_rep","obs","subplot","pass","spp",
               "cover_obj","substrate","age_class")
sals_2023 <- sals_2023[,new_order]


#save as csv
write.csv(sals_2023, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/sals.2023.csv", 
          row.names = FALSE)




#load and format 2024 data
sals_2024 <- read.csv("oss_2024_sals.csv", 
                 colClasses = c(landowner="factor", stand="character", trt="factor",
                                obs="factor", pass="factor", spp="factor", 
                                cover_obj="factor", substrate="factor"))

sals_2024$date_mdy <- as.Date(sals_2024$date, format = "%m/%d/%Y")

#add unique site code, combo of stand#_replicate#_year
sals_2024$site_id <- paste(sals_2024$stand, "_", sals_2024$site_rep, "_", sals_2024$year)

#create a new column for sal_id
#single digit for each animal found within each site
#this groups the rows by the site identifier, then numbers each row sequentially within sites
sals_2024 <- sals_2024 %>%
  group_by(site_id) %>%
  mutate(sal_rep = row_number())

#add unique sal code for each animal, combo of site id and sal replicate
sals_2024$sal_id <- paste(sals_2024$site_id,"_",sals_2024$sal_rep)
#back to data frame
sals_2024 <- as.data.frame(sals_2024)

#reorder
new_order <- c("site_id","landowner","stand","site_rep","trt","year","date",
               "date_mdy","sal_id","sal_rep","obs","subplot","pass","spp",
               "cover_obj","substrate","age_class","svl","sample_id","recap")
sals_2024 <- sals_2024[,new_order]


#save as csv
write.csv(sals_2024, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/sals.2024.csv", 
          row.names = FALSE)





#data frame with both years of sal data
sals <- bind_rows(sals_2024,sals_2023)

#change column classes to check df summary
sals$landowner <- as.factor(sals$landowner)
sals$trt <- as.factor(sals$trt)
sals$subplot <- as.factor(sals$subplot)
sals$age_class <- as.character(sals$age_class)
#deal with missing values from 2023 set
sals$age_class[sals$age_class == "" | is.na(sals$age_class)] <- "U"
sals$age_class <- as.factor(sals$age_class)
sals$recap <- as.factor(sals$recap)
sals$recap[sals$recap == "" | is.na(sals$recap)] <- 0 #change blank recap to zero


summary(sals)
head(sals)

# save as csv
write.csv(sals, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/sals.complete.csv", 
          row.names = FALSE)


#### Site Data -----------------------------------------------------------------

# making one data frame that includes site data for both years, one row for each site

site_2023 <- read.csv("oss_2023_sitesubplot.csv")
site_2023$year <- 2023 #add year column to 2023 data
site_2023$date_mdy <- as.Date(site_2023$date, format = "%m/%d/%Y") #format date
summary(site_2023)

site_2024 <- read.csv("oss_2024_site.csv")
site_2024$date_mdy <- as.Date(site_2024$date, format = "%m/%d/%Y") #format date
summary(site_2024)

#subset 2023/2024 data to include only columns in common
site_2023_subset <- names(site_2023) %in% names(site_2024) #vector of 2023 col names that match 2024 col names
site_2024_subset <- names(site_2024) %in% names(site_2023) #vector of 2024 col names that match 2023 col  names

#rbind to include only those cols in common
site_2023_2024 <- bind_rows(site_2023[site_2023_subset],site_2024[site_2024_subset])



#2023 has data for all subplots, so we need to work on that
#subset new data frame to inlcude only cols we care about for 2023
new_site_2023_subset <- subset(site_2023_2024, subset = year==2023)

#i want to average elev, temp, hum for all rows that have the same stand number
#this code creates new data frame with that info
site_2023_avg <- aggregate(cbind(temp,hum,elev)~stand,new_site_2023_subset,mean)

#prepare to merge site_2023_avg cols with other site cols
new_site_2023_subset[, c(6:8)] <- list(NULL) #remove old temp,hum,elev cols
new_site_2023_subset <- unique(new_site_2023_subset) #keep only one of each stand, remove duplicates
site_2023_joined <- inner_join(site_2023_avg,new_site_2023_subset,by="stand") #merge
summary(site_2023_joined)


#combine subsetted 2023 data with the 2024 site data and format
site.complete <- bind_rows(site_2023_joined,subset(site_2023_2024,subset=year==2024))
site.complete$landowner <- as.factor(site.complete$landowner)
site.complete$trt <- as.factor(site.complete$trt)
site.complete$stand <- as.integer(site.complete$stand)



#add unique site code, combo of stand#_replicate#_year
site.complete$site_id <- paste(site.complete$stand, "_", site.complete$site_rep,
                               "_", site.complete$year)

#reorder
new_order <- c("site_id","landowner","stand","site_rep","trt","year", "date",
               "date_mdy","elev","temp","hum")
site.complete <- site.complete[,new_order]

summary(site.complete)

# save as csv
write.csv(site.complete, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/site.complete.csv", 
          row.names = FALSE)



#### Subplot Data -----------------------------------------------------------------

site_2023 <- read.csv("oss_2023_sitesubplot.csv")
site_2023$year <- 2023 #add year column to 2023 data
site_2023$date_mdy <- as.Date(site_2023$date, format = "%m/%d/%Y") #format date

subplot_24 <- read.csv("oss_2024_subplot.csv", 
                colClasses = c(landowner="factor", stand="character", trt="factor"))
subplot_24$date_mdy <- as.Date(subplot_24$date, format="%m/%d/%Y") #format date
subplot_24$stand <- as.integer(subplot_24$stand) #change stand so matches 2023 for merging

#subset 2023/2024 data so that merged will only include cols they both have in common
subplot_23_subset <- names(site_2023) %in% names(subplot_24)
subplot_24_subset <- names(subplot_24) %in% names(site_2023)
#combine
subplot_23_24 <- bind_rows(site_2023[subplot_23_subset],subplot_24[subplot_24_subset])


#add site_id column
subplot_23_24$site_id <- paste(subplot_23_24$stand,"_",subplot_23_24$site_rep,
                               "_",subplot_23_24$year)


#format
subplot_23_24$landowner <- as.factor(subplot_23_24$landowner)
subplot_23_24$trt <- as.factor(subplot_23_24$trt)
subplot_23_24$subplot <- as.factor(subplot_23_24$subplot)
subplot_23_24$weather <- as.factor(subplot_23_24$weather)
summary(subplot_23_24)



#reorder
new_order <- c("site_id","landowner","stand","site_rep","trt","year", "date",
               "date_mdy","subplot","lat","long","time","weather","canopy_cov",
               "dwd_cov","veg_cov","fwd_cov","soil_moist_avg")
subplot_23_24 <- subplot_23_24[,new_order]



# save as csv
write.csv(subplot_23_24, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/subplot.complete.csv", 
          row.names = FALSE)


#### All Site/Subplot/Occupancy Together -----------------------------------------------------------------

### create one df that has site and subplot data for both years, one row for each subplot


### 2023 site data, which includes subplot data

    habitat_2023 <- read.csv("oss_2023_sitesubplot.csv")
    habitat_2023$year <- 2023 #add year column to 2023 data
    habitat_2023$date_mdy <- as.Date(habitat_2023$date, format = "%m/%d/%Y") #format date
    summary(habitat_2023)
    colnames(habitat_2023)
    drop <- c(20:30)
    habitat_2023 <- habitat_2023[, -drop]
    
    #add unique site code, combo of stand#_replicate#_year
    habitat_2023$site_id <- paste(habitat_2023$stand, "_", habitat_2023$site_rep,
                                  "_", habitat_2023$year)
    #add unique plot code, combo of stand#_replicate#_year_subplot
    habitat_2023$plot_id <- paste(habitat_2023$stand, "_", habitat_2023$site_rep,
                              "_", habitat_2023$year, "_", habitat_2023$subplot)
    
    colnames(habitat_2023)
    drop <- c("subplot_code","litter_avg","time","site_rep")
    habitat_2023 <- habitat_2023[,!(names(habitat_2023) %in% drop)]

### 2024 site and subplot data merge

    #site data
    site_2024 <- read.csv("oss_2024_site.csv")
    site_2024$date_mdy <- as.Date(site_2024$date, format = "%m/%d/%Y") #format date
    summary(site_2024)
    
    #add unique site code, combo of stand#_replicate#_year_subplot
    site_2024$site_id <- paste(site_2024$stand, "_", site_2024$site_rep,
                               "_", site_2024$year)    
    
    #subplot data
    subplot_24 <- read.csv("oss_2024_subplot.csv", 
                               colClasses = c(landowner="factor", stand="character", trt="factor"))
    subplot_24$date_mdy <- as.Date(subplot_24$date, format="%m/%d/%Y") #format date
    subplot_24$stand <- as.integer(subplot_24$stand) #change stand so matches 2023 for merging
    
    #add unique site code, combo of stand#_replicate#_year
    subplot_24$site_id <- paste(subplot_24$stand, "_", subplot_24$site_rep,
                               "_", subplot_24$year)
    summary(subplot_24)
    
    #2024 merge
    df1 <- subplot_24
    df2 <- site_2024
    #search columns from df2 that are not in df1
    # df2_unique <- df2[, !(names(df2) %in% names(df1))]
    # print(df2_unique)
    #keep only ones I want
    df2 <- df2[,c("temp","elev","hum","site_id","tree_farm")] 
    
    df_merged <- merge(df1, df2, by="site_id")
    
    sort(colnames(habitat_2023))
    sort(colnames(df_merged))
    drop <- c("moisture_1","moisture_2","moisture_3","site_rep","time","acc","time")
    habitat_2024 <- df_merged[,!(names(df_merged) %in% drop)]
    
    #create plot_id that has subplot in it
    habitat_2024$plot_id <- paste(habitat_2024$site_id, "_", habitat_2024$subplot)
    
    
    
### merge 2023 and 2024

    df1 <- habitat_2023
    df2 <- habitat_2024
    
    # sort(colnames(df1))
    # sort(colnames(df2))
    
    merge3 <- df1%>%
      full_join(df2)
    
    new_order <- c("plot_id", "site_id","landowner","tree_farm", "stand","subplot",
                   "trt","year","date","date_mdy","lat","long","weather",
                   "elev","temp","hum","canopy_cov","veg_cov","dwd_cov","fwd_cov",
                   "soil_moist_avg","obs")
    habitat_23_24 <- merge3[,new_order]
    
    
    na_count <- colSums(is.na(habitat_23_24)) # need to add obs for 2023 data...wasnt entered in master data sheet ############################
    print(na_count)


### add salamander occupancy data
    
    sals <- read_csv("sals.complete.csv")
    #add unique subplot code
    sals$plot_id <- paste(sals$site_id,"_", sals$subplot)
    
    #subset data
    sals_occu <- sals[,c("plot_id","spp")]

    # Count species occurrences and reshape the data
    condensed_df <- sals_occu %>%
      group_by(plot_id, spp) %>%
      summarise(count = n(), .groups = 'drop') %>%  # Count occurrences for each species
      pivot_wider(names_from = spp, values_from = count, values_fill = list(count=0))  # Reshape data
    
    
    #merging 
    df1 <- habitat_23_24
    df2 <- condensed_df
    df_merged <- merge(df1, df2, by="plot_id", all.x=TRUE)
    
    
    #changing NA's in occu columns to zeros
    cols_to_update <- c("PLDU","ENES","OSS","AMGR","TAGR","ANFE")  
    # Replace NA with 0 for specific columns across all rows
    df_merged[cols_to_update][is.na(df_merged[cols_to_update])] <- 0

    na_count <- colSums(is.na(df_merged))
    print(na_count)
 
       
### save as csv
    
    write.csv(df_merged, "C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/oss-occu/data/habitat.occu.complete.csv", 
              row.names = FALSE)
    
    
    