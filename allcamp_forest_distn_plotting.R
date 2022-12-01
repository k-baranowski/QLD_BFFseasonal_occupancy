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
forests = lapply(files,read.csv, sep = ",") #gives out warnings about the last line not being complete but reads in fine


#map the years onto each list 
yr<- c("2007", "2009", "2011", "2013", "2015", "2017", "2019", "2020")
forests<- Map(cbind, forests , year= yr)

# unlist all lists and change column name
forests <- rbindlist(forests, fill = T)
forests[,1]<- NULL



#add in cummulative sum column by veg group 
forests$fors_type<-
  ifelse(forests$gridcode == 0,"Non-forest",
         ifelse(forests$gridcode == 1,"Sparse",
                ifelse(forests$gridcode == 2,"Forest",
                       NA)))

#add in cummulative sum column by veg group 
forests$forest<-  ifelse(forests$fors_type == "Non-forest", paste("Non-forest"),paste("Forest"))



write.csv(forests, "allcamps_woodyveg10kmbuffint_2yrsums_221110.csv", row.names = F)



