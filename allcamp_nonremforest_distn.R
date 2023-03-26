setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(tidyverse) #tidying 
library(ggplot2) #viz
library(data.table)
library(reshape2)


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### PURPOSE OF ANALYSIS ######## ######### ######### ######## ######## 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########

## importing raw data
files = list.files(pattern="*.csv$") # file list of all .csv files
forests = lapply(files,read.csv, sep = ",") #gives out warnings about the last line not being complete but reads in fine


#map the years onto each list 
yr<- c("2007", "2009", "2011", "2013", "2015", "2017", "2019")
forests<- Map(cbind, forests , year= yr)

# unlist all lists and change column name
forests <- rbindlist(forests, fill = T)
forests[,1]<- NULL



#add in cummulative sum column by veg group 
forests$fors_type<-
  ifelse(forests$gridcode == 0,"non-forest",
         ifelse(forests$gridcode == 1,"nonrem_sparse",
          ifelse(forests$gridcode == 2,"nonrem_forest",
           NA)))

#add in cummulative sum column by veg group 
#forests$forest<-  ifelse(forests$fors_type == "non-forest", paste("non-forest"),paste("nonrem_forest"))
forests$area_ha<- forests$Shape_Area*0.0001

#forests$PAR = forests$Shape_Length/forests$Shape_Area

sums<- forests %>%
  group_by(camp_name, year,  fors_type) %>%
  dplyr:: summarize(
    sum_patcharea_ha   = sum(area_ha),
    max_patch_size = max(area_ha)) %>%
  as.data.frame()



#forest_rshp<- reshape::reshape()
forest_rshp<- reshape(sums, idvar = c("camp_name", "year"), timevar = "fors_type", direction = "wide")

yrlycamp_sumpatches<- sums %>%                        # Add lagged column of rate of loss
  group_by(camp_name, fors_type) %>%
  dplyr::mutate(change_area_since97_ha = sum_total_area_ha - first(sum_total_area_ha),
                change_patch_num_since97 = total_patches - first(total_patches),
                prop_area_change_since97  = (sum_total_area_ha /first(sum_total_area_ha) -1))   %>%
  as.data.frame()


#yrlycamp_sumpatches$sum_area_ha <-round(yrlycamp_sumpatches$sum_area_ha , digits = 3)




write.csv(forest_rshp, "allcamps_nonrem_forestsrshp10kmbuffint_sums_230325.csv", row.names = F)



