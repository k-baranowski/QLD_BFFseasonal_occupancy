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
library(data.table)


######## ######### ######### PURPOSE OF ANALYSIS ######## ######### ######### ######## ######## 
#analyzing gps tracking data supplied by email by Liam McGuire on Thursday Sep 1, 2022

#read in the semi-cleaned dataframe Adrienne and I walked through together on 7/15/22
all_ff_gpspts<- read.csv( "deployed_clean_tzfix_20220909.csv", header = T, stringsAsFactors = F, numerals = "no.loss") #128,008 obs, 32 variables in adriennes, mine's 134,201 
sum(is.na(all_ff_gpspts$recorded_at2))  #0

#turn time column into posixc format
all_ff_gpspts$recorded_at2<- as.POSIXct(all_ff_gpspts$recorded_at2, format = "%Y-%m-%d  %H:%M:%S", tz = "GMT" ) 
sum(is.na(all_ff_gpspts$recorded_at2)) #0 check

#all_ff_gpspts$diff_sec <- difftime(all_ff_gpspts$recorded_at2, all_ff_gpspts$recorded_at2, units="secs")

#order the observations by batID night and time 
all_ff_gpspts<-all_ff_gpspts[(order( all_ff_gpspts$ID, all_ff_gpspts$recorded_at2)),]

all_ff_gpspts2<-subset(all_ff_gpspts, lat != 0 )


all_ff_gpspts <- all_ff_gpspts2 %>%
  arrange(ID, recorded_at2) %>%
  group_by(ID) %>%
  mutate(lag =  strptime(recorded_at2, "%Y-%m-%d  %H:%M:%S", tz = "GMT") - lag(strptime(recorded_at2, "%Y-%m-%d  %H:%M:%S", tz = "GMT"), 
              default = strptime(recorded_at2, "%Y-%m-%d  %H:%M:%S", tz = "GMT")[1]))
all_ff_gpspts<- as.data.frame(all_ff_gpspts)

#parse out times associated with observations and lags between
all_ff_gpspts$lag_sec = as.numeric(str_sub(all_ff_gpspts$lag, end=5))
all_ff_gpspts$day<- as.double(format(all_ff_gpspts$recorded_at2, '%d'))
all_ff_gpspts$TOD <- format(all_ff_gpspts$recorded_at2, format="%H:%M:%S")


all_ff_gpspts<- all_ff_gpspts%>%
  arrange(ID, recorded_at2) %>%
  group_by(ID) %>%
  mutate(period = if_else((TOD > "17:00:00") & ( lag_sec > 20000), "new_day", "same_night"))  %>%
  as.data.frame()

#set up column denoting number of nights tracked
head(all_ff_gpspts)
all_ff_gpspts <- as.data.table(all_ff_gpspts)
all_ff_gpspts[,night_ID := rleid(period, ID)]
 
#fix the sequential count since it basically counts twice every new night
all_ff_gpspts$night_ID[all_ff_gpspts$period== "new_day"] <- (all_ff_gpspts$night_ID[all_ff_gpspts$period == "new_day"] - 1)
all_ff_gpspts$night_ID[all_ff_gpspts$period== "same_night"] <- (all_ff_gpspts$night_ID[all_ff_gpspts$period == "same_night"] - 2)

#rerun sequence number generator after correcting for double counting changes in previous step 
all_ff_gpspts[,night_ID := rleid(night_ID), by = ID]
#make night id number with two digits starting at 0 like Adrienne 
all_ff_gpspts$night_ID<- sprintf("%02d", as.numeric(all_ff_gpspts$night_ID -1))

#paste animal id and night number together 
all_ff_gpspts$night_ID = paste(all_ff_gpspts$ID, (all_ff_gpspts$night_ID), sep = "_")

ff_gps_cl<- all_ff_gpspts[,-c(12:17,21 )]
ff_gps_cl<- ff_gps_cl[,c(1,15,3,4,2,14,12,13,6,7,8:11 )]


# #add in column for season 
getseason <- function(dates) { # function for AUS seasons
  SS = as.Date("2013-12-01", format = "%Y-%m-%d") # Winter Solstice for US
  FE = as.Date("2013-3-01",  format = "%Y-%m-%d") # Spring Equinox for US
  WS = as.Date("2013-6-01",  format = "%Y-%m-%d") # Summer Solstice for US
  SE = as.Date("2013-9-01",  format = "%Y-%m-%d") # Fall Equinox for US
  
  # convert dates from any year to 2013 dates
  dates = ff_gps_cl$recorded_at
  d = as.Date(strftime(dates, format="2013-%m-%d"))
  ifelse (d >= SS | d < FE, "summer",
  ifelse (d >= SE & d < SS, "spring",
  ifelse (d >= WS & d < SE, "winter", "autumn")))
}
ff_gps_cl$season = getseason(ff_gps_cl$recorded_at)

table(ff_gps_cl$season)
# autumn  spring  summer  winter 
# 41632   12722   43526   36187 


#transform into spatial points df for changing projection 
xy<- data.frame(x = ff_gps_cl$lng, y=ff_gps_cl$lat)
ff_spdf <- SpatialPointsDataFrame(coords = xy, data = ff_gps_cl,
            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
crs(ff_spdf)
#australian albers projection info 
newcrs<- CRS("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#reproject into Australian albers and pull out to of df, (this is clunky but it works and isn't that long) 
ff_spdf_albers <- spTransform(ff_spdf, CRS = newcrs)
spdf_df<- as.data.frame(ff_spdf_albers)
colnames(spdf_df)[16]<- "xalbers"
colnames(spdf_df)[17]<- "yalbers"
spdf_df2<- subset(spdf_df, lat != 0)
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### BAT MOVEMENT/ TIME - DISTANCE ANALYSIS ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########

#convert albers df to an sf object so I can do distance by group in next part (struggled to get other examples of calc. dist by group to work for some reason )
#more stackoverflow times shows this is most suggested for efficiency so keeping it 
ff_sf<- st_as_sf(
  spdf_df, 
  coords = c('xalbers', 'yalbers'),
  crs = "+init=epsg:3577")

class(ff_sf) #sf obj check 

#there are 31 observations with 0,0 lat long coords, need to remove those so it doesn't mess up distances 
ff_sf<- subset(ff_sf, lat != 0)

#calculate pairwise distance for every bat night. last obs. is left  NA, so value is assigned to 1st coord pt in df
ff_sf<- ff_sf %>%
  arrange(ID, recorded_at2) %>%
  group_by(night_ID) %>%
  mutate(
    lead = geometry[row_number() + 1],
    dist_btwn_pts_m = st_distance(geometry, lead, by_element = T))
ff_sf$dist_btwn_pts_m<-as.numeric(ff_sf$dist_btwn_pts_m)


#re-attach og xy albers coords that have been reformated in 'geometry' column 
ff_sf$xalbers = spdf_df2$xalbers
ff_sf$yalbers = spdf_df$yalbers


#calc new m/s speed field by K.B.
ff_sf$speed2_ms<- ff_sf$dist_btwn_pts_m/ff_sf$lag_sec

#check for missing bff count data here
missing <- ff_sf[is.na(ff_sf$dist_btwn_pts_m),] #used to be 947 #958 NAs for dist, but thats okay bc theres 947 bat/nights
#and each bat/night needs an end point with no distance calculated 

#remove the last obs of each night with na for dist so I can get quantiles in next step 
ff_sf_distcl <- ff_sf[!is.na(ff_sf$dist_btwn_pts_m),] 

#get the 95% C.I. of the measured distance btwn gps track points, trims off the roosting pts going 0m & the odd bats that go very far 
qts <- quantile(ff_sf_distcl$dist_btwn_pts_m,probs=c(.05,.95))
## 2.8, 958.8 m,   
  #(952 seems reasonable, https://australian.museum/learn/animals/mammals/black-flying-fox/ says they can fly 35-40km/hr or ~2.91-3.33km/5min,
  #SP Thomas 1975 shows 9m/s BFF in wind tunnel, RE Carpenter 1984 measures GHFF ~8.4 m/s,so dists of 2.4-2.97 km/5min are reasonable?)
  #assumption::wind tunnel in papers replicate natural flight across landscape, however GPS data here may dispute that cutoff



#find the total distance traveled by each bat night, cumulative sum of distance (not max displacement)
ff_sf$cumsum_distKm<- ave(ff_sf$dist_btwn_pts_m, ff_sf$night_ID, FUN = cumsum)*0.001
hist(ff_sf$cumsum_distKm)

## get max tracklength for each bat
csum_dists_nghtID<- as.data.frame(ff_sf %>%
                                    group_by(night_ID) %>%
                                    dplyr::summarise(
                                    cumsumdist_km = max(cumsum_distKm, na.rm = T)))

######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### MAXIMUM DISPLACEMENT OF EACH BAT NIGHT ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#group by night_ID and calculate euclidean distance of every xy albers point to the origin **of that night_ID, nor true original roost 
ff_sf<- ff_sf %>% 
  group_by(night_ID) %>%
  mutate(eucdist_nt_origin = as.matrix(dist(cbind(xalbers, yalbers)))[1, ])
ff_sf$eucdist_nt_origin_KM<- ff_sf$eucdist_nt_origin*0.001

#find the further euc. distance for each night_ID, i.e. the maximum displacement that bat night tracked
dist_sums_nghtID<- as.data.frame(ff_sf %>%
                                          group_by(night_ID, Site) %>%
                                          dplyr::summarise(
                                          max_displacement_km = max(eucdist_nt_origin_KM, na.rm = T),
                                          #max_speed2_ms = max(speed2_ms),
                                          #max_dist_btwn_pts_m = max(dist_btwn_pts_m),
                                          mean_dist_btwn_pts_m = mean(dist_btwn_pts_m, na.rm = T )))

#get the 95% C.I. of the measured distance btwn gps track points, trims off the points likely to be in error
qts2 <- quantile(dist_sums_nghtID$max_displacement_km ,probs=c(.05,.95))
#0.953km, 24.946km 

##############################################
#plotting histogram of nightly distance 
# ggplot(dist_sums_nghtID, aes(x= max_displacement_km, fill = Site))+
#   geom_histogram() +
#   scale_fill_manual(values = c("royalblue4", "orange3", "forestgreen")) +
#   theme_bw(base_size = 12) +
#   labs(y= "Count of Bat Nights Tracked", x = "Maximum Displacement KM", fill = "Roost") +
#   geom_vline(xintercept = c(0.463, 24.24), color = "red", size = 1, linetype = "dashed") +
#   theme(legend.position = c(0.85,0.9))

table(dist_sums_nghtID$Site)
# ggsave("MaxDispKm_histbyroost_95cilabel_kb20220726.eps", plot = last_plot() , width=7,  height =7, units = c("in"), dpi = 350)
# 
# ggplot(dist_sums_nghtID, aes(x= max_displacement_km, fill = Site))+
#   geom_density(alpha = 0.5) +
#   scale_fill_manual(values = c("forestgreen", "royalblue4", "orange3")) +
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
ff_frstlst<- ff_sf %>% 
  group_by(night_ID) %>%
  slice(c(1, n())) %>%
  ungroup()
#qts3 <- quantile(ff_frstlst$eucdist_nt_origin,probs=c(.10,.90))

ff_last<-subset(ff_frstlst, eucdist_nt_origin >0)

# ff_last %>% 
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
ff_return_nights<-  ff_frstlst %>% 
  group_by(night_ID) %>%
  mutate(close_to_roost = ifelse(eucdist_nt_origin <= 200, 1, 0), #testing cutoff at 200m away or like 500ft 
  rtrn_nt_origin=min(close_to_roost))

ff_return_nights_only<- unique(data.frame("night_ID" = ff_return_nights$night_ID,"rtn_nt_origin" = ff_return_nights$rtrn_nt_origin))
table(ff_return_nights_only$rtn_nt_origin)
# 0   1 
# 192 766


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### CLASSIFY THE NIGHTS WHERE ORIGIN POINTS IS AT TRACKING ROOSTS  ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#(is first roost point within x distance of roost where tracker was deployed)
    ##note **when I try to select for nights when eucdist ==0 i get 951, 
          #so there are 4 nights where an animal returns within 1m of its origin spot
#get just the first observations of every bat night tracked 
ff_frst <- ff_frstlst[!is.na(ff_frstlst$cumsum_distKm),]


#pull out toowooomba roost 
twb_alb_frst<- subset(ff_frst, Site == "Toowoomba")

#absolute diff between albers loc column and location of TWB or RCF roost 
twb_alb_frst$absdiffx<- abs(twb_alb_frst$xalbers - 1936878)
twb_alb_frst$absdiffy<- abs(twb_alb_frst$yalbers - -3145726)

#make conditional column, if both starting locations are less than 300m away from roost loc then it starts at TWB
twb_alb_frst$origin<- ifelse(twb_alb_frst$absdiffx <= 500 & twb_alb_frst$absdiffy <= 500, 1 , 0  )

table(twb_alb_frst$origin) #291/519 bat nights where started at Toowoomba
############## ############## Redcliffe ############## ############## 
rcf_alb_frst<- subset(ff_frst, Site == "Redcliffe")

#absolute diff between albers loc column and location of TWB or RCF roost 
rcf_alb_frst$absdiffx<- abs(rcf_alb_frst$xalbers - 2054895)
rcf_alb_frst$absdiffy<- abs(rcf_alb_frst$yalbers - -3122912)

#make conditional column, if both starting locations are less than 300m away from roost loc then it starts at TWB
rcf_alb_frst$origin<- ifelse(rcf_alb_frst$absdiffx <= 500 & rcf_alb_frst$absdiffy <= 500, 1 , 0  )

table(rcf_alb_frst$origin) #252/428 bat nights where started at Redcliffe, 4 more nights if I change to 900m
#236/390
############## ############## Redcliffe ############## ############## 
bdb_alb_frst<- subset(ff_frst, Site == "Bundaberg")

#absolute diff between albers loc column and location of TWB or RCF roost 
bdb_alb_frst$absdiffx<- abs(bdb_alb_frst$xalbers - 2036276)
bdb_alb_frst$absdiffy<- abs(bdb_alb_frst$yalbers - -2846193)

#make conditional column, if both starting locations are less than 300m away from roost loc then it starts at TWB
bdb_alb_frst$origin<- ifelse(bdb_alb_frst$absdiffx <= 500 & bdb_alb_frst$absdiffy <= 500, 1 , 0  )

table(bdb_alb_frst$origin) #252/428 bat nights where started at Redcliffe, 4 more nights if I change to 900m

#6/82?

#put back together
ff_originpt<- as.data.frame(rbind(twb_alb_frst, rcf_alb_frst, bdb_alb_frst))
ff_originpt[,c(1,3:26)]<- NULL
colnames(ff_originpt)[2]<- "origin_at_trcking_roost"
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### SUMMARIZING ALL BAT NIGHTS TRACKED ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########

night_ID_sums<- merge( dist_sums_nghtID,csum_dists_nghtID, by = c("night_ID", "geometry") )
night_ID_sums[2]<-NULL #geometry still somehow comes through

night_ID_sums<- cbind(night_ID_sums, ff_return_nights_only$rtn_nt_origin)
night_ID_sums<- merge(night_ID_sums,ff_originpt, by = "night_ID")
colnames(night_ID_sums)[6]<- "rtrn_night_origin"

mean(night_ID_sums$max_displacement_km) #9.04
median(night_ID_sums$max_displacement_km) #5.294054
max(night_ID_sums$max_displacement_km) #194.045
write.csv(night_ID_sums, "LMbfftracking_nightID_sums_20220912.csv", row.names = F)


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### # CHARACTERIZE DISTANCES FOR ANIMALS RETURNING TO ORIGINAL ROOST POINT ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########

ff_clnsums_df<- merge(ff_sf, night_ID_sums, by = c("night_ID", "Site"), all.x =T) 
#ff_clnsums_df[28]<- NULL #take out lead column
ff_clnsums_df<-as.data.frame(ff_clnsums_df)

class(ff_clnsums_df)

write.csv(ff_clnsums_df, "ADbfftracking_KBclnsumsdf_202208912_fixmaybe.csv", row.names = F)

ff_cl_return<- subset(ff_clnsums_df, rtrn_night_origin == 1)
ff_cl_rtrn_trackroost<- subset(ff_clnsums_df, rtrn_night_origin == 1 & origin_at_trcking_roost == 1)
#453 roost nights where animal starts at tracking roost and returns to tracking roost 

write.csv(ff_cl_return, "ADbfftracking_KBclnsumsdf_return_20220912_maybe.csv", row.names = F)


ggplot(ff_gps_cl_return, aes(x= max_displacement_km, fill = Site))+
  geom_histogram() +
  scale_fill_manual(values = c("forestgreen","royalblue4", "orange3")) +
  theme_bw(base_size = 12) +
  labs(y= "Count of Bat Nights Tracked with Return", x = "Maximum Displacement KM", fill = "Roost") +
  geom_vline(xintercept = c(0.442, 19.339), color = "red", size = 1, linetype = "dashed") +
  theme(legend.position = c(0.85,0.9))

qts4 <- quantile(ff_gps_cl_return$max_displacement_km,probs=c(.05,.95))
#0.947, #20.009
ggsave("MaxDispKm_histbyroost_95cilabel_rtntorign_kb20220727.eps", plot = last_plot() , width=7,  height =7, units = c("in"), dpi = 350)


ggplot(ff_gps_cl_return, aes(x= max_displacement_km, fill = Site))+
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("forestgreen","royalblue4", "orange3")) +
  theme_bw(base_size = 12) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,40,50,100,150),
                     labels = c(0,5,10,15,20,25,30,40,50,100,150)) +
  labs(y= "Density of Bat Nights Tracked with Return", x = "Maximum Displacement KM", fill = "Roost") +
  geom_vline(xintercept = c(0.463, 19.339), color = "red", size = 1, linetype = "dashed") +
  theme(legend.position = c(0.85,0.9))

####get distribution of just central 95% data
ff_gps_cl_return %>%
  filter(max_displacement_km <= 25)%>%
ggplot( aes(x= max_displacement_km, fill = Site))+
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("royalblue4", "tomato3", "orange2")) +
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26),
                     labels = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26)) +
  labs(y= "Density of Bat Nights Tracked with Return", x = "Maximum Displacement KM", fill = "Roost") +
  geom_vline(xintercept = c(0.463, 19.339), color = "red", size = 1, linetype = "dashed") +
  theme(legend.position = c(0.85,0.9))

#ggsave("MaxDispKm_densitybyroost_95cilabel_rtntorign_kb20220727.eps", plot = last_plot() , width=7,  height =7, units = c("in"), dpi = 350)

######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### # Characterize movement of animals to/from other known roosts  ######## ######### ######### 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########

######## ######### # (Jumping in point instead of processing everything above)######## ######### ######### 
   #ff<- read.csv("ADbfftracking_KBclnsumsdf_202208912_fixmaybe.csv", header = T, stringsAsFactors = F)
#identify roosts where origin point is not at another roost but the animals return to within 500m of their origin point that night 
ff_return_notorigin<- subset(ff_clnsums_df, origin_at_trcking_roost != 1 & rtrn_night_origin == 1) #41525
ff_return_origin<- subset(ff_clnsums_df, origin_at_trcking_roost == 1 & rtrn_night_origin == 1) #41525

#take out the first observation of each bat night tracked 
ff_return_frst<- ff_return_origin %>% 
  group_by(night_ID) %>%
  slice(1) %>%
  ungroup() 

#read in roost locations in queensland 
roosts<-read.csv("flying-fox-camps-qld.csv", header = T, stringsAsFactors = F)
#reduce to subtrop area to reduce processing time 
subtrop_roosts<- subset(roosts, Latitude < -23.26)
colnames(subtrop_roosts)[6]<- "lat"
colnames(subtrop_roosts)[7]<- "lng"
subtrop_roosts$num<-  1:nrow(subtrop_roosts) 

#check if the origin point is within 500m of a known roost, paste roost name 
library(geosphere)
origin_to_anyroost = distm(ff_return_frst[c("lng","lat")], subtrop_roosts[c("lng","lat")])

rownames(origin_to_anyroost) = ff_return_frst$night_ID
colnames(origin_to_anyroost) = subtrop_roosts$Name.of.camp
class(origin_to_anyroost)

#find which cells in matrix have night starting point less than 500m from any roost
t<- as.data.frame(which(origin_to_anyroost < 500, arr.ind = T))

#clean up data and merge it so we have night_ID & roost it started from 
t <- cbind(rownames(t), data.frame(t, row.names=NULL))
colnames(t)<- c("night_ID", "row","col")
t2<- merge( t, subtrop_roosts, by.y ="num", by.x="col", all.x = T)
t2$ID<- word(t$night_ID,1,sep = "\\_")


t2[,c(1,3,5:10)]<- NULL
write.csv(t2, "nightIDs_origin_originalroosts_202208912.csv", row.names = F)
#19 other roosts that animals visited 

# others<- merge(t2, ff, by ="night_ID" , all.x = T)
# table(t$ID)
#write.csv(others, "rawdata_origin_otherroosts_20220819.csv", row.names = F)

ff_dates<- data.frame(ff_return_frst$night_ID, ff_return_frst$recorded_at2, ff_return_frst$season)
colnames(ff_dates)= c("night_ID", "time", "season")

#specify date format and add year column

t2s<- merge(t2, ff_dates, by = "night_ID")
t2s$year <- as.factor(format(t2s$time,'%Y'))

t2019<- subset (t2s, year == "2019")
t2020<-subset (t2s, year == "2020")


t2019_camps <- t2019 %>%                           
  group_by(Name.of.camp, season) %>%
  dplyr::summarise(animals_tracked = n_distinct(ID), 
                   batnights_tracked = n_distinct(night_ID) )
t2019_camps <- t2019_camps[order(-t2019_camps$batnights_tracked),] 

t2019_camps$year = "2019"

t2020_camps <- t2020 %>%                       
  group_by(Name.of.camp, season) %>%
  dplyr::summarise(animals_tracked = n_distinct(ID), 
                   batnights_tracked = n_distinct(night_ID) )
t2020_camps <- t2020_camps[order(-t2020_camps$batnights_tracked),] 

t2020_camps$year = "2020"

t4 <- t2s %>%                              # Applying group_by & summarise
  group_by(season) %>%
  dplyr::summarise(animals_tracked = n_distinct(ID), 
           batnights_tracked = n_distinct(night_ID) )
t4 <- t4[order(-t4$batnights_tracked),] 

t_og<- rbind(t2019_camps, t2020_camps)
write.csv(t4, "batnights_tracked_origin_seasons_20220912.csv", row.names = F)
