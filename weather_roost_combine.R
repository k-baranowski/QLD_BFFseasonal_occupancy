setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(tidyverse) #tidying 
library(ggplot2) #viz
library(data.table)

#occ<- read.csv("allmonthly_obsfilled_20072019_20230312.csv",header = T)


#read and clean 
anoms<- read.csv("winter_weather_anoms_rsclags_20230319.csv", header = T)
#anoms<- subset(anoms, year >= 2007)


buff10<- read.csv("anomgrid_allwintroosts_10kmbuffint.csv", header = T)

##the numeric x,y vars have  diff levels of decimal, set  standard and merge with xy
buff10$x=round(buff10$x,3)
buff10$y=round(buff10$y,1)
buff10$x_chr<-as.character(buff10$x)
buff10$y_chr<-as.character(buff10$y)
buff10$xy<-as.character(paste(buff10$x_chr, buff10$y_chr, sep = ","))

buff10_cl<-buff10[,c(2,7)]
#table(buff10_cl$camp_name)


##merge 
buff10_anoms<- merge(anoms, buff10_cl, by = "xy", all.x =  T)
#take out  all  the grid cells  not within  a 10km buffer  range of any roosts 
buff10_anoms2<- buff10_anoms[complete.cases(buff10_anoms), ]
#pull camp name to the front  
buff10_anoms2<- buff10_anoms2[,c(56,1:55)]
buff10_anomscl<-  as.data.frame(unique(bff10_anoms2))


####  figure out how to take the average of thecells and  simplify  to 1 row per roost per month 
buff10_anom_sums<- buff10_anomscl %>%
  group_by(camp_name,xy, x,y,x_chr,y_chr, month, month_num, month_num2, yrmon,  year) %>%
  dplyr::summarise(
    mean_prcp_anom
    
  )

#then run the model you're almost ther! 


