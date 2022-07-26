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
library(raster)

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
#Aug update: have the metadata files but raw data didnt come through correctly, wrong dates and serial numbers
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
sum(is.na(all_ff_gpspts$time)) #0 check

#order the observations by batID night and time 
all_ff_gpspts<-all_ff_gpspts[(order( all_ff_gpspts$night_ID, all_ff_gpspts$time)),]

#clean out columns previously calculated 
ff_gps_cl<-all_ff_gpspts[,-c(5,7,8,10,15,16,19:23,26:32)]

#move my time column from the end to beginning of df with lat/long next to it 
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

#convert albers df to an sf object so I can do distance by group in next part (struggled to get other examples of calc. dist by group to work for some reason )
#more stackoverflow times shows this is most suggested for efficiency so keeping it 
ff_albers_sf<- st_as_sf(
  ff_albers, 
  coords = c('x.albers', 'y.albers'),
  crs = "+init=epsg:3577")

class(ff_albers_sf) #sf obj check 

#calculate pairwise distance for every bat night. last obs. is left  NA, so value is assigned to 1st coord pt in df
ff_albers_sf<- ff_albers_sf%>%
  group_by(night_ID) %>%
  mutate(
    lead = geometry[row_number() + 1],
    dist_btwn_pts_m = st_distance(geometry, lead, by_element = T))
ff_albers_sf$dist_btwn_pts_m<-as.numeric(ff_albers_sf$dist_btwn_pts_m)

hist(ff_albers_sf$dist_btwn_pts_m)

#re-attach og xy albers coords that have been reformated in 'geometry' column 
ff_albers_sf$x.albers = ff_albers$x.albers
ff_albers_sf$y.albers = ff_albers$y.albers

#convert the time lag minutes decimal field to seconds for new m/s speed calc   
ff_albers_sf$timelag_sec<- as.numeric( lubridate::dminutes(ff_albers_sf$timeLag))

#calc new m/s speed field by K.B.
ff_albers_sf$speed2_ms<- ff_albers_sf$dist_btwn_pts_m/ff_albers_sf$timelag_sec

#check for missing bff count data here
#missing <- ff_albers_sf[is.na(ff_albers_sf$dist_btwn_pts_m),] #947 NAs for dist, but thats okay bc theres 947 bat/nights
#and each bat/night needs an end point with no distance calculated 

#remove the last obs of each night with na for dist so I can get quantiles in next step 
#ff_albers_sf_distcl <- ff_albers_sf[!is.na(ff_albers_sf$dist_btwn_pts_m),] 

#get the 95% C.I. of the measured distance btwn gps track points, trims off the roosting pts going 0m & the odd bats that go very far 
#qts <- quantile(ff_albers_sf_distcl$dist_btwn_pts_m,probs=c(.05,.95))
## 1.12, 951.9 m,   
  #(952 seems reasonable, https://australian.museum/learn/animals/mammals/black-flying-fox/ says they can fly 35-40km/hr or ~2.91-3.33km/5min,
  #SP Thomas 1975 shows 9m/s BFF in wind tunnel, RE Carpenter 1984 measures GHFF ~8.4 m/s,so dists of 2.4-2.97 km/5min are reasonable?)
  #assumption::wind tunnel in papers replicate natural flight across landscape, however GPS data here may dispute that cutoff


#show histogram of distances between 5 min interval, max is >6000km, which in arcgis looks real,
# & at 20m/s speed checks out but isn't 100% correct. the math doesn't always fit. arc shows max should be 
# ggplot(ff_albers_sf_distcl, aes(x= dist_btwn_pts_m))+
#   geom_histogram(fill="black", color="black") +
#   theme_bw(base_size = 16) +
#   geom_vline(xintercept = c(2, 952), color = "red")

#find the total distance traveled by each bat night, cumulative sum of distance (not max displacement)
ff_albers_sf$cumsum_distKm<- ave(ff_albers_sf$dist_btwn_pts_m, ff_albers_sf$night_ID, FUN = cumsum)*0.001
hist(ff_albers_sf$cumsum_distKm)

## get max tracklength for each bat
csum_dists_nghtID<- as.data.frame(ff_albers_sf %>%
                                    group_by(night_ID) %>%
                                    dplyr::summarise(
                                    cumsumdist_km = max(cumsum_distKm, na.rm = T)))

######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### MAXIMUM DISPLACEMENT OF EACH BAT NIGHT ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#group by night_ID and calculate euclidean distance of every xy albers point to the origin **of that night_ID, nor true original roost 
ff_albers_sf2<- ff_albers_sf %>% 
  group_by(night_ID) %>%
  mutate(eucdist_nt_origin = as.matrix(dist(cbind(x.albers, y.albers)))[1, ])
ff_albers_sf2$eucdist_nt_origin_KM<- ff_albers_sf2$eucdist_nt_origin*0.001

#find the further euc. distance for each night_ID, i.e. the maximum displacement that bat night tracked
dist_sums_nghtID<- as.data.frame(ff_albers_sf2 %>%
                                          group_by(night_ID, roost) %>%
                                          dplyr::summarise(
                                          max_displacement_km = max(eucdist_nt_origin_KM, na.rm = T),
                                          #max_speed2_ms = max(speed2_ms),
                                          #max_dist_btwn_pts_m = max(dist_btwn_pts_m),
                                          mean_dist_btwn_pts_m = mean(dist_btwn_pts_m, na.rm = T )))

#get the 95% C.I. of the measured distance btwn gps track points, trims off the points likely to be in error
#qts2 <- quantile(dist_sums_nghtID$max_displacement_km ,probs=c(.05,.95))
#0.463km, 24.24km 

##############################################
#plotting histogram of nightly distance 
# ggplot(dist_sums_nghtID, aes(x= max_displacement_km, fill = roost))+
#   geom_histogram() +
#   scale_fill_manual(values = c("royalblue4", "orange3")) +
#   theme_bw(base_size = 12) +
#   labs(y= "Count of Bat Nights Tracked", x = "Maximum Displacement KM", fill = "Roost") +
#   geom_vline(xintercept = c(0.463, 24.24), color = "red", size = 1, linetype = "dashed") +
#   theme(legend.position = c(0.85,0.9))
# 
# ggsave("MaxDispKm_histbyroost_95cilabel_kb20220726.eps", plot = last_plot() , width=7,  height =7, units = c("in"), dpi = 350)
# 
# ggplot(dist_sums_nghtID, aes(x= max_displacement_km, fill = roost))+
#   geom_density() +
#   scale_fill_manual(values = c("royalblue4", "orange3")) +
#   theme_bw(base_size = 12) +
#   scale_x_continuous(breaks = c(0,5,10,15,20,25,30,40,50,100,150),
#     labels = c(0,5,10,15,20,25,30,40,50,100,150)) +
#   labs(y= "Density of Bat Nights Tracked", x = "Maximum Displacement KM", fill = "Roost") +
#   geom_vline(xintercept = c(0.463, 24.24), color = "red", size = 1, linetype = "dashed") +
#   theme(legend.position = c(0.85,0.9))
# 
# ggsave("MaxDispKm_densbyroost_95cilabel_kb20220726.eps", plot = last_plot() , width=7,  height =7, units = c("in"), dpi = 350)
# 


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### CLASSIFY BAT NIGHTS AS RETURN/LEAVE NIGHTLY ORIGIN ROOST  ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#(are first and last roost point within x distance of each other)
#take out the first and last observation of each bat night tracked 
ff_alb_frstlst<- ff_albers_sf2 %>% 
  group_by(night_ID) %>%
  slice(c(1, n())) %>%
  ungroup()
#qts3 <- quantile(ff_alb_frstlst$eucdist_nt_origin,probs=c(.10,.90))

ff_alb_last<-subset(ff_alb_frstlst, eucdist_nt_origin >0)

# ff_alb_last %>% 
#   filter(eucdist_nt_origin <1000 & eucdist_nt_origin > 50) %>%
# ggplot( aes(x= eucdist_nt_origin, fill = roost))+
#   geom_density(alpha = 0.5) +
#   scale_fill_manual(values = c("royalblue4", "orange3")) +
#   theme_bw(base_size = 14) +
#   scale_x_continuous(breaks = c(0,100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
#                      labels = c(0,100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
#   labs(y= "Density of Bat-Nights Tracked", x = "Euclidean distance from origin point each night (m)", fill = "Roost") +
#   #geom_vline(xintercept = c(0.442, 19.339), color = "red", size = 1, linetype = "dashed") +
#   theme(legend.position = c(0.85,0.9))


#paste 1/0 if the bat returned to the original rooos of that night, its hard bc they never fly back
#exactly where they started so what cutoff is good as still being 'within the roost'
ff_return_nights<-  ff_alb_frstlst %>% 
  group_by(night_ID) %>%
  mutate(close_to_roost = ifelse(eucdist_nt_origin <= 200, 1, 0), #testing cutoff at 200m away or like 500ft 
  rtrn_nt_origin=min(close_to_roost))

ff_return_nights_only<- unique(data.frame("night_ID" = ff_return_nights$night_ID,"rtn_nt_origin" = ff_return_nights$rtrn_nt_origin))
table(ff_return_nights_only$rtn_nt_origin)
# 0   1 
# 190 757


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### CLASSIFY THE NIGHTS WHERE ORIGIN POINTS IS AT TRACKING ROOSTS  ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#(is first roost point within x distance of roost where tracker was deployed)
    ##note **when I try to select for nights when eucdist ==0 i get 951, 
          #so there are 4 nights where an animal returns within 1m of its origin spot
#get just the first observations of every bat night tracked 
ff_alb_frst <- ff_alb_frstlst[!is.na(ff_alb_frstlst$cumsum_distKm),]

#pull out toowooomba roost 
twb_alb_frst<- subset(ff_alb_frst, roost == "Toowoomba")

#absolute diff between albers loc column and location of TWB or RCF roost 
twb_alb_frst$absdiffx<- abs(twb_alb_frst$x.albers - 1936878)
twb_alb_frst$absdiffy<- abs(twb_alb_frst$y.albers - -3145726)

#make conditional column, if both starting locations are less than 300m away from roost loc then it starts at TWB
twb_alb_frst$origin<- ifelse(twb_alb_frst$absdiffx <= 500 & twb_alb_frst$absdiffy <= 500, 1 , 0  )

table(twb_alb_frst$origin) #300/519 bat nights where started at Toowoomba
############## ############## Redcliffe ############## ############## 
rcf_alb_frst<- subset(ff_alb_frst, roost == "Redcliffe")

#absolute diff between albers loc column and location of TWB or RCF roost 
rcf_alb_frst$absdiffx<- abs(rcf_alb_frst$x.albers - 2054895)
rcf_alb_frst$absdiffy<- abs(rcf_alb_frst$y.albers - -3122912)

#make conditional column, if both starting locations are less than 300m away from roost loc then it starts at TWB
rcf_alb_frst$origin<- ifelse(rcf_alb_frst$absdiffx <= 500 & rcf_alb_frst$absdiffy <= 500, 1 , 0  )

table(rcf_alb_frst$origin) #252/428 bat nights where started at Redcliffe, 4 more nights if I change to 900m

#put back together
ff_alb_originpt<- as.data.frame(rbind(twb_alb_frst, rcf_alb_frst))
ff_alb_originpt[,2:27]<- NULL
colnames(ff_alb_originpt)[2]<- "origin_at_trcking_roost"
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### SUMMARIZING ALL BAT NIGHTS TRACKED ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########

night_ID_sums<- merge( dist_sums_nghtID,csum_dists_nghtID, by = c("night_ID", "geometry") )
night_ID_sums[2]<-NULL #geometry still somehow comes through

night_ID_sums<- cbind(night_ID_sums, ff_return_nights_only$rtn_nt_origin)
night_ID_sums<- merge(night_ID_sums,ff_alb_originpt, by = "night_ID")
colnames(night_ID_sums)[6]<- "rtrn_night_origin"

mean(night_ID_sums$max_displacement_km) #8.480
median(night_ID_sums$max_displacement_km) #4.914
max(night_ID_sums$max_displacement_km) #149.040
write.csv(night_ID_sums, "ADbfftracking_nightID_sums_20220816.csv", row.names = F)


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### # CHARACTERIZE DISTANCES FOR ANIMALS RETURNING TO ORIGINAL ROOST POINT ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########

ff_clnsums_df<- merge(ff_albers_sf2, night_ID_sums, by = c("night_ID", "roost"), all.x =T) 
#ff_clnsums_df[28]<- NULL #take out lead column
ff_clnsums_df<-as.data.frame(ff_clnsums_df)

class(ff_clnsums_df)

write.csv(ff_clnsums_df, "ADbfftracking_KBclnsumsdf_20220816_fix.csv", row.names = F)

ff_gps_cl_return<- subset(ff_clnsums_df, rtrn_night_origin == 1)
ff_gps_cl_rtrn_trackroost<- subset(ff_clnsums_df, rtrn_night_origin == 1 & origin_at_trcking_roost == 1)
#453 roost nights where animal starts at tracking roost and returns to tracking roost 



ggplot(ff_gps_cl_return, aes(x= max_displacement_km, fill = roost))+
  geom_histogram() +
  scale_fill_manual(values = c("royalblue4", "orange3")) +
  theme_bw(base_size = 12) +
  labs(y= "Count of Bat Nights Tracked with Return", x = "Maximum Displacement KM", fill = "Roost") +
  geom_vline(xintercept = c(0.442, 19.339), color = "red", size = 1, linetype = "dashed") +
  theme(legend.position = c(0.85,0.9))

qts4 <- quantile(ff_gps_cl_return$max_displacement_km,probs=c(.05,.95))
#0.442, #19.339
ggsave("MaxDispKm_histbyroost_95cilabel_rtntorign_kb20220727.eps", plot = last_plot() , width=7,  height =7, units = c("in"), dpi = 350)


ggplot(ff_gps_cl_return, aes(x= max_displacement_km, fill = roost))+
  geom_density() +
  scale_fill_manual(values = c("royalblue4", "orange3")) +
  theme_bw(base_size = 12) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,40,50,100,150),
                     labels = c(0,5,10,15,20,25,30,40,50,100,150)) +
  labs(y= "Density of Bat Nights Tracked with Return", x = "Maximum Displacement KM", fill = "Roost") +
  geom_vline(xintercept = c(0.463, 19.339), color = "red", size = 1, linetype = "dashed") +
  theme(legend.position = c(0.85,0.9))

####get distribution of just central 95% data
ff_gps_cl_return %>%
  filter(max_displacement_km <= 25)%>%
ggplot( aes(x= max_displacement_km, fill = roost))+
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("royalblue4", "orange3")) +
  theme_bw(base_size = 15) +
  scale_x_ADbfftracking_KBclnsumsdf_20220727.csvontinuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26),
                     labels = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26)) +
  labs(y= "Density of Bat Nights Tracked with Return", x = "Maximum Displacement KM", fill = "Roost") +
  geom_vline(xintercept = c(0.463, 19.339), color = "red", size = 1, linetype = "dashed") +
  theme(legend.position = c(0.85,0.9))

#ggsave("MaxDispKm_densitybyroost_95cilabel_rtntorign_kb20220727.eps", plot = last_plot() , width=7,  height =7, units = c("in"), dpi = 350)

######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### # Characterize movement of animals to/from other known roosts  ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########

######## ######### # (Jumping in point instead of processing everything above)######## ######### ######### 
   ff<- read.csv("ADbfftracking_KBclnsumsdf_20220816_fix.csv", header = T, stringsAsFactors = F)
#identify roosts where origin point is not at another roost but the animals return to within 500m of their origin point that night 
ff_return_notorigin<- subset(ff, origin_at_trcking_roost == 0 & rtrn_night_origin == 1) #41525

#take out the first observation of each bat night tracked 
ff_return_frst<- ff_return_notorigin %>% 
  group_by(night_ID) %>%
  slice(1) %>%
  ungroup()

#read in roost locations in queensland 
roosts<-read.csv("flying-fox-camps-qld.csv", header = T, stringsAsFactors = F)
#reduce to subtrop area to reduce processing time 
subtrop_roosts<- subset(roosts, Latitude < -23.26)
colnames(subtrop_roosts)[6]<- "lat"
colnames(subtrop_roosts)[7]<- "long"

#check if the origin point is within 500m of a known roost, paste roost name 
library(geosphere)
origin_to_anyroost = distm(ff_return_frst[c("long","lat")], subtrop_roosts[c("long","lat")])

rownames(origin_to_anyroost) = ff_return_frst$night_ID
colnames(origin_to_anyroost) = subtrop_roosts$Name.of.camp
class(origin_to_anyroost)

t<- which(origin_to_anyroost < 500, arr.ind = T)

#next step print the roost its within 500m of 