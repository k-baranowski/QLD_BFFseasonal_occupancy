setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(lubridate) #messing with time 
library(sf) #measuring movement
library(tidyverse) #tidying 
library(ggplot2) #viz
library(stringr) #viz
library(scales)#viz
#library(raster)

######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### PURPOSE OF ANALYSIS ######## ######### ######### ######## ######## 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#analyzing gps tracking data supplied by email by Adrienne Dale on Friday July 15, 2022
#I asked to see some foraging data to derive an ecologically relevant way for spatially attributing foraging areas
#to roosts for my dissertation chapter 4. I am in no way presenting any gps foraging data collected by AD and LM

#update:: Thursday July 21, 2022
  #had conversation with Nita about the accuracy of the data provided and it was suggested to analyze the raw data 
  #to inform a point density buffer of bff space use around roost. NB to set up meeting with LM so I can present
  #what I am doing, why this data could be informative, & what would be our agreement of data sharing/authorship 
#update:: Mon July 25, 2022 -meeting set up for friday july 29, 11am  
#####################################################################################################################

#read in the data that has all gps points (forages, commuting, roosting - point classified on tag speed
#AD classified movements into 3 groups based on speed:: 0-0.5m/s = roosting, 0.5-2m/s = foraging, >2m/s flying/switching) 
  ## update Nita says actual foraging would be still while they ate/drank so to include all speeds <2m/s as foraging and 
  ## to classify ground speed 0 observations at the origin point (or other roost loc) as roosting points, others are foraging/flying 
  # *note #Adrienne's original file shared had 96 variables.. we cleaned out 64 that were known incorrect/duplicates
  #all_ff_gpspts_og<- read.csv( "Added_ALL_df3_20220704_BNRTmod2.csv", header = T, stringsAsFactors = F) #128,008 obs, 32 variables 

######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### DATA READING, CLEANING, & PROJECTING ######## ######### ######### ########
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
#read in the semi-cleaned dataframe Adrienne and I walked through together on 7/15/22
all_ff_gpspts<- read.csv( "ADbfftracking_kbcln_20220726.csv", header = T, stringsAsFactors = F) #128,008 obs, 32 variables 
sum(is.na(all_ff_gpspts$dt_aest))  #0

#turn time column into posixc format
all_ff_gpspts$time<- as.POSIXct(all_ff_gpspts$dt_aest, format = "%m/%d/%Y  %H:%M:%S", tz = "GMT" ) 
sum(is.na(all_ff_gpspts$time)) 

#order the observations by batID night and time 
all_ff_gpspts<-all_ff_gpspts[(order( all_ff_gpspts$night_ID, all_ff_gpspts$time)),]

#clean out columns she calculated and old dt_aest column
ff_gps_cl<-all_ff_gpspts[,-c(5,7,8,10,15,16,19:23,26:32)]
#move my time column to beginning of df with lat/long next to it 
ff_gps_cl <- ff_gps_cl %>% relocate(time, .before = timeLag)

head(ff_gps_cl)
# night_ID    ID       lat     long          time         timeLag      ground_speed_tag  height_above_msl  sex
# 1 Akela_00  Akela  -27.60097  151.9436  12/8/20 18:35:04    4.666667     0.00000000            664           male
# 2 Akela_00  Akela  -27.60104  151.9436  12/8/20 18:39:44    5.216667     0.17491111            643           male
# 3 Akela_00  Akela  -27.60103  151.9436  12/8/20 18:44:57    5.166667     0.01028889            730           male
# 4 Akela_00  Akela  -27.60099  151.9436  12/8/20 18:50:07    5.183333     0.06173333            696           male
# 5 Akela_00  Akela  -27.60095  151.9436  12/8/20 18:55:18    5.166667     0.01543333            662           male
# 6 Akela_00  Akela  -27.60092  151.9436  12/8/20 19:00:28    5.183333     0.09774444            676           male
# taxon_canonical_name    deployment    night roost     last_lat last_long
# 1      Pteropus alecto  12/8/20 11:00   0   Toowoomba   -27.60084  151.9438
# 2      Pteropus alecto  12/8/20 11:00   0   Toowoomba   -27.60084  151.9438
# 3      Pteropus alecto  12/8/20 11:00   0   Toowoomba   -27.60084  151.9438
# 4      Pteropus alecto  12/8/20 11:00   0   Toowoomba   -27.60084  151.9438
# 5      Pteropus alecto  12/8/20 11:00   0   Toowoomba   -27.60084  151.9438
# 6      Pteropus alecto  12/8/20 11:00   0   Toowoomba   -27.60084  151.9438

#transform into spatial points df for changing projection 
xy<- data.frame(x = ff_gps_cl$long, y=ff_gps_cl$lat)
ff_spdf <- SpatialPointsDataFrame(coords = xy, data = ff_gps_cl,
            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
crs(ff_spdf)
#australian albers projection info 
newcrs<- CRS("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#reproject into Australian albers and pull out to of df, (this is clunky but it works and isn't that long) 
ff_spdf_albers <- spTransform(ff_spdf, CRS = newcrs)
ff_albers<- as.data.frame(ff_spdf_albers)
colnames(ff_albers)[16]<- "x.albers"
colnames(ff_albers)[17]<- "y.albers"

######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### BAT MOVEMENT/ TIME - DISTANCE ANALYSIS ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#testing the distance calculations if you use lat long, want to see how far off dist is once you account for Aus crazy projection
#convert albers df to an sf object so I can do distance by group in next part (struggled to get other examples of calc. dist by group to work for some reason )
# ff_sf_test<- st_as_sf(
#   ff_gps_cl, 
#   coords = c('long', 'lat'),
#   crs = "+init=epsg:4326"
# )
# 
# ff_sf_test<- ff_sf_test%>%
#   group_by(night_ID) %>%
#   mutate(
#     lead = geometry[row_number() + 1],
#     dist = st_distance(geometry, lead, by_element = T))
# ff_sf_test$dist<-as.numeric(ff_sf_test$dist)


#convert albers df to an sf object so I can do distance by group in next part (struggled to get other examples of calc. dist by group to work for some reason )
ff_albers_sf<- st_as_sf(
  ff_albers, 
  coords = c('x.albers', 'y.albers'),
  crs = "+init=epsg:3577"
)

class(ff_albers_sf) #sf obj check 

#calculate pairwise distance for every bat night. last obs. is left  NA, so value must be assigned to 1st pt in df
ff_albers_sf<- ff_albers_sf%>%
  group_by(night_ID) %>%
  mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T))
ff_albers_sf$dist<-as.numeric(ff_albers_sf$dist)

hist(ff_albers_sf$dist)

#convert the time lag minutes decimal field to seconds for m/s speed calc   
ff_albers_sf$timelag_sec<- as.numeric( lubridate::dminutes(ff_albers_sf$timeLag))

#calc new m/s speed field by K.B.
ff_albers_sf$speed2_ms<- ff_albers_sf$dist/ff_albers_sf$timelag_sec

#check for missing bff count data here
missing <- ff_albers_sf[is.na(ff_albers_sf$dist),] #947 NAs for dist, but thats okay bc theres 947 bat/nights
#and each bat/night needs an end point with no distance calculated 

#remove the last obs of each night with na for dist so I can get quantiles in next step 
ff_albers_sf_distcl <- ff_albers_sf[!is.na(ff_albers_sf$dist),] 

#get the 95% C.I. of the measured distance btwn gps track points, trims off the points likely to be in error
qts <- quantile(ff_albers_sf_distcl$dist,probs=c(.05,.95))
## 1.99, 951.9 m,   
#(952 seems reasonable, https://australian.museum/learn/animals/mammals/black-flying-fox/ says they can fly 35-40km/hr or ~2.91-3.33km/5min,
#SP Thomas 1975 shows 9m/s BFF in wind tunnel, RE Carpenter 1984 measures GHFF ~8.4 m/s,so dists of 2.4-2.97 km/5min are reasonable?)
#assumption::wind tunnel in papers replicate natural flight across landscape, however GPS data here may dispute that cutoff

#show the central 95%cutoff
hist(ff_albers_sf_distcl$dist)
abline(v=qts[1],col="red")
abline(v=qts[2],col="red")


#show histogram of distances between 5 min interval, max is >6000km, which in arcgis looks real,
# & at 20m/s speed checks out but isn't 100% correct. the math doesn't always fit. arc shows max should be 
ggplot(ff_albers_sf_distcl, aes(x= dist))+
  geom_histogram(fill="black", color="black") +
  theme_bw(base_size = 16) +
  geom_vline(xintercept = c(2, 952), color = "red")

#find the furthest distance away per night bat 

ff_albers_sf_distcl$cumsum_distKm<- ave(ff_albers_sf_distcl$dist, ff_albers_sf_distcl$night_ID, FUN = cumsum)*0.001
hist(ff_albers_sf_distcl$cumsum_distKm)
##this  is  wrong
# ff_albers_sf2<- ff_albers_sf%>%
#   group_by(night_ID) %>%
#   group_split()
# #apply a function to each list
# distance_per_group <- map(ff_albers_sf2, function(x){
#   distance_matrix <- st_distance(x)
#   biggest_distance <- as.numeric(which(distance_matrix == max(distance_matrix), arr.ind = TRUE)[1,])
#   farthest_apart <- x[biggest_distance,]
# })
# 
# #rbind them to dataframe
# distance_per_group<- do.call("rbind", distance_per_group)
# colnames(distance_per_group)[18]<- "nightly_max_displacement_meters"
# 



##from earlier code copied over for summaries 
## see if traveling distances are diff by roost 
roosts_sums<- as.data.frame(nightbats %>%
                          group_by(roost) %>%
                           dplyr::summarise(n = n(),
                                            mean_maxdisp_km= mean(max_disp_KM),
                                            med_maxdisp_km= median(max_disp_KM), 
                                            max_maxdisp_km = max(max_disp_KM)))
forage_sums<- as.data.frame(forage_nights2 %>%
                              group_by(roost) %>%
                              dplyr::summarise(n = n(),
                                                mean_maxdisp_km= mean(max_disp_KM),
                                               med_maxdisp_km= median(max_disp_KM))) 
