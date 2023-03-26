setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(tidyverse) #tidying 
library(reshape2)
library(mgcv)
library(scales)

###read in data 
remveg<- read.csv("allcamps_remveg_10kmbuff_sum_20230324.csv", header = T, stringsAsFactors = F)
nonrem<- read.csv("allcamps_nonrem_forestsrshp10kmbuffint_sums_230325.csv", header = T, stringsAsFactors = F )
crop<- read.csv("allcamps_crop_10kmbuffint_sums_230325.csv", header = T, stringsAsFactors = F)
anoms<- read.csv("winter_weather_anoms_rsclags_20230319.csv", header = T, stringsAsFactors = F)

