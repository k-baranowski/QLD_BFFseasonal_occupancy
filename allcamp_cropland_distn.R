setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(tidyverse) #tidying 
library(ggplot2) #viz
library(data.table)


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### PURPOSE OF ANALYSIS ######## ######### ######### ######## ######## 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########

## importing raw data
files = list.files(pattern="*.csv$") # file list of all .csv files
crops = lapply(files,read.csv, sep = ",") #gives out warnings about the last line not being complete but reads in fine


#map the years onto each list 
yr<- c("2003", "2007", "2011", "2015", "2019")
crops<- Map(cbind, crops , year= yr)

# unlist all lists and change column name
crops <- rbindlist(crops, fill = T)
crops[,c(1,8:13)]<- NULL



crops$area_ha<- crops$Shape_Area*0.0001
#crops$PAR = crops$Shape_Length/crops$Shape_Area

crop_sums<- crops %>%
  group_by(camp_name, year) %>%
  dplyr:: summarize(
    sum_area_ha = sum(area_ha),
    min_area_ha= min(area_ha),
    max_area_ha= max(area_ha), 
    sum_freq_patches =  sum(FREQUENCY))




yrlycamp_sumpatches<- crop_sums%>%                        # Add lagged column of rate of loss
  group_by(camp_name) %>%
  dplyr::mutate(change_crop_area_since03_ha = sum_area_ha - first(sum_area_ha),
                change_crop_patch_num_since03 = sum_freq_patches- first(sum_freq_patches),
                prop_crop_area_change_since03  = change_crop_area_since03_ha /first(sum_area_ha) )   %>%
  as.data.frame()


yrlycamp_sumpatches$sum_area_ha <-round(yrlycamp_sumpatches$sum_area_ha , digits = 3)
yrlycamp_sumpatches$change_crop_area_since03_ha <-round(yrlycamp_sumpatches$change_crop_area_since03_ha, digits = 3)

#

write.csv(crop_sums, "allcamps_crop_10kmbuffint_sums_230325.csv", row.names = F)



