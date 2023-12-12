setwd(".")

library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(tokenizers)
library(stringi)

##read in REDD table, qld floral list, and Peggys 2008 GHFF diet species list 
ff<- read.csv("qld_ff_20072021_filledmonths_20231005.csv", header = TRUE)
table(ff$camp.name)

weather<- read.csv("camp_monthly_rawvals_preciptemps19912020_20231113.csv", header = T)

weather2<-subset(weather, year >2005)
weather[,c(2:4)]<-NULL


##################
#make a dataframe wiith all the lags in it 
weather_lags<- weather2 %>%
  arrange(yrmon) %>%
  group_by(camp_name) %>%
  mutate(prcp_lag1mo = lag(mean_csum_precip_20kbuff, n = 1),
         prcp_lag2mo = lag(mean_csum_precip_20kbuff, n = 2),
         prcp_lag3mo = lag(mean_csum_precip_20kbuff, n = 3),
         prcp_lag4mo = lag(mean_csum_precip_20kbuff, n = 4),
         prcp_lag5mo = lag(mean_csum_precip_20kbuff, n = 5),
         prcp_lag6mo = lag(mean_csum_precip_20kbuff, n = 6),
         prcp_lag7mo = lag(mean_csum_precip_20kbuff, n = 7),
         prcp_lag8mo = lag(mean_csum_precip_20kbuff, n = 8),
         prcp_lag9mo = lag(mean_csum_precip_20kbuff, n = 9),
         prcp_lag10mo = lag(mean_csum_precip_20kbuff, n = 10),
         prcp_lag11mo = lag(mean_csum_precip_20kbuff, n = 11),
         prcp_lag12mo = lag(mean_csum_precip_20kbuff, n = 12),
         tmax_lag1mo = lag(mean_maxtemp_20kbuff, n = 1),
         tmax_lag2mo = lag(mean_maxtemp_20kbuff, n = 2),
         tmax_lag3mo = lag(mean_maxtemp_20kbuff, n = 3),
         tmax_lag4mo = lag(mean_maxtemp_20kbuff, n = 4),
         tmax_lag5mo = lag(mean_maxtemp_20kbuff, n = 5),
         tmax_lag6mo = lag(mean_maxtemp_20kbuff, n = 6),
         tmax_lag7mo = lag(mean_maxtemp_20kbuff, n = 7),
         tmax_lag8mo = lag(mean_maxtemp_20kbuff, n = 8),
         tmax_lag9mo = lag(mean_maxtemp_20kbuff, n = 9),
         tmax_lag10mo = lag(mean_maxtemp_20kbuff, n = 10),
         tmax_lag11mo = lag(mean_maxtemp_20kbuff, n = 11),
         tmax_lag12mo = lag(mean_maxtemp_20kbuff, n = 12),
         tmin_lag1mo = lag(mean_mintemp_20kbuff, n = 1),
         tmin_lag2mo = lag(mean_mintemp_20kbuff, n = 2),
         tmin_lag3mo = lag(mean_mintemp_20kbuff, n = 3),
         tmin_lag4mo = lag(mean_mintemp_20kbuff, n = 4),
         tmin_lag5mo = lag(mean_mintemp_20kbuff, n = 5),
         tmin_lag6mo = lag(mean_mintemp_20kbuff, n = 6),
         tmin_lag7mo = lag(mean_mintemp_20kbuff, n = 7),
         tmin_lag8mo = lag(mean_mintemp_20kbuff, n = 8),
         tmin_lag9mo = lag(mean_mintemp_20kbuff, n = 9),
         tmin_lag10mo = lag(mean_mintemp_20kbuff, n = 10),
         tmin_lag11mo = lag(mean_mintemp_20kbuff, n = 11),
         tmin_lag12mo = lag(mean_mintemp_20kbuff, n = 12),
         tmean_lag1mo = lag(mean_meantemp_20kbuff, n = 1),
         tmean_lag2mo = lag(mean_meantemp_20kbuff, n = 2),
         tmean_lag3mo = lag(mean_meantemp_20kbuff, n = 3),
         tmean_lag4mo = lag(mean_meantemp_20kbuff, n = 4),
         tmean_lag5mo = lag(mean_meantemp_20kbuff, n = 5),
         tmean_lag6mo = lag(mean_meantemp_20kbuff, n = 6),
         tmean_lag7mo = lag(mean_meantemp_20kbuff, n = 7),
         tmean_lag8mo = lag(mean_meantemp_20kbuff, n = 8),
         tmean_lag9mo = lag(mean_meantemp_20kbuff, n = 9),
         tmean_lag10mo = lag(mean_meantemp_20kbuff, n = 10),
         tmean_lag11mo = lag(mean_meantemp_20kbuff, n = 11),
         tmean_lag12mo = lag(mean_meantemp_20kbuff, n = 12))

####################### #read in the anomalies #####
anomalies<- read.csv("camp_monthly_anomvals_lags_preciptemps20002020_202310120.csv", header = T)
anomalies2<-subset(anomalies, year >2005 & year < 2021)

env<- merge(weather_lags,anomalies, by = c("camp_name", "yrmon", "month", "year", "yrmon", "month_num2"))
###################
##

############### # build a merging attribute 
t<- ff[,c(1,3:18,20:26)]
colnames(t)[1]= "camp_name"
t<- subset(t, year < 2021)
t$mon2<- sprintf("%02d", as.numeric(t$month_num))
t$yrmon<- paste(t$year, t$mon2, sep = "_")
t[,c(15,17,19,25)]<-NULL

#merge the observation data onto the monthly aly data 
weather2t<- merge(env, t, by = c("camp_name", "yrmon"), all.x = T)




#########################

winter00<- read.csv("camps_prop_winter_RE2000_GBIFupperwint8m_veg.csv", header = T)
winter07<- read.csv("camps_prop_winter_RE2007_GBIFupperwint8m_veg.csv", header = T)
winter07[2]<-NULL
winter09<- read.csv("camps_prop_winter_RE2009_GBIFupperwint8m_veg.csv", header = T)
winter11<- read.csv("camps_prop_winter_RE2011_GBIFupperwint8m_veg.csv", header = T)
winter13<- read.csv("camps_prop_winter_RE2013_GBIFupperwint8m_veg.csv", header = T)
winter15<- read.csv("camps_prop_winter_RE2015_GBIFupperwint8m_veg.csv", header = T)
winter17<- read.csv("camps_prop_winter_RE2017_GBIFupperwint8m_veg.csv", header = T)
winter19<- read.csv("camps_prop_winter_RE2019_GBIFupperwint8m_veg.csv", header = T)

winter_allyr<- cbind(winter00,winter07, winter09, winter11, winter13, winter15, winter17, winter19)
winter_allyr[,c(4,7,10,13,16,19,22)]<- NULL
##############################################################################################
############################# SET UP FOR FUZZY MATCHING ######################################

#set up from https://github.com/Adamishere/Fuzzymatching/blob/master/Fuzzy%20String%20Match%20FunctionV1.R 

library(stringdist)

string1<- weather2t$camp_name
string2<- winter_allyr$camp_name

dataset1<-data.frame(string1,stringsAsFactors =FALSE)
dataset2<-data.frame(string2,stringsAsFactors =FALSE)

fuzzymatch<-function(dat1,dat2,string1,string2,meth,id1,id2){
  #initialize Variables:
  matchfile <-NULL #iterate appends
  x<-nrow(dat1) #count number of rows in input, for max number of runs
  
  #Check to see if function has ID values. Allows for empty values for ID variables, simple list match
  if(missing(id1)){id1=NULL}
  if(missing(id2)){id2=NULL}
  
  #### lowercase text only
  dat1[,string1]<-as.character(unlist(dat1[,string1]))#force character, if values are factors
  dat2[,string2]<-as.character(unlist(dat2[,string2]))
  
  #Loop through dat1 dataset iteratively. This is a work around to allow for large datasets to be matched
  #Can run as long as dat2 dataset fits in memory. Avoids full Cartesian join.
  for(i in 1:x) {
    d<-merge(dat1[i,c(string1,id1), drop=FALSE],dat2[,c(string2,id2), drop=FALSE])#drop=FALSE to preserve 1var dataframe
    
    #Calculate String Distatnce based method specified "meth"
    d$dist <- stringdist(d[,string1],d[,string2], method=meth)
    
    #dedupes A_names selects on the smallest distance.
    d<- d[order(d[,string1], d$dist, decreasing = FALSE),]
    d<- d[!duplicated(d[,string1]),]
    
    #append demos on matched file
    matchfile <- rbind(matchfile,d)
    # print(paste(round(i/x*100,2),"% complete",sep=''))
    
  }
  return(matchfile)
}

#run the matching 
match<-fuzzymatch(dataset1, dataset2, "string1", "string2", meth = "osa")
match[3]<- NULL
colnames(match) = c("camp.name", "camp")

ff_match<-cbind(weather2t,match)
ff_match[110]<- NULL

wint_ff_match<- merge(ff_match, winter_allyr, by.x = "camp" , by.y = "camp_name", all = T)


library(zoo)

wint_ff_match$yr_month<- zoo::as.yearmon(wint_ff_match$yearmon, format = "%b %Y")


wint_ff_match$pres<- ifelse(wint_ff_match$bff.presence == 0, "0",
                      ifelse(wint_ff_match$bff.presence == 0.5, "NA", 
                      ifelse(wint_ff_match$bff.presence == 1, "1", NA)))


#write.csv(wint_ff_match, "winterupper8m_bff_filledocc_2007_2022_2023004.csv", row.names = F)

library(ggplot2)
#library(ggsignif)

all<- wint_ff_match

all$prop_20kbuff_yrobs<- ifelse(all$year == 2007,all$prop_20kbuff_RE2007,
                                    ifelse(all$year == 2008, all$prop_20kbuff_RE2007, 
                                    ifelse(all$year == 2009, all$prop_20kbuff_RE2009,
                                  ifelse( all$year == 2010, all$prop_20kbuff_RE2009, 
                                    ifelse(all$year == 2011, all$prop_20kbuff_RE2011, 
                                   ifelse(all$year == 2012, all$prop_20kbuff_RE2011,
                                   ifelse( all$year == 2013, all$prop_20kbuff_RE2013,
                                     ifelse(  all$year == 2014, all$prop_20kbuff_RE2013,
                                       ifelse(  all$year == 2015, all$prop_20kbuff_RE2015,
                                      ifelse(  all$year == 2016, all$prop_20kbuff_RE2015,
                                  ifelse( all$year == 2017, all$prop_20kbuff_RE2017,
                                   ifelse(all$year == 2018, all$prop_20kbuff_RE2017,
                                  ifelse(all$year == 2019, all$prop_20kbuff_RE2019,
                                 ifelse(all$year == 2020, all$prop_20kbuff_RE2019,
                                 ifelse(all$year == 2021, all$prop_20kbuff_RE2019,
                                  ifelse(all$year == 2022, all$prop_20kbuff_RE2019,NA))))))))))))))))


#half<- subset(all, bff.presence == 0.5)


############################### ADD IN THE HENDRA 
#hev1<- read.csv("allhev19942022_in20kmbuff_allffroosts.csv", header = T)
hev2<- read.csv("allhev19942022_2dd_in20kmbuff_allffroosts.csv", header = T)

colnames(hev2)[1]= "camp_name"
#colnames(hev)[4]= "date_hev"
colnames(hev2)[3]= "month_num2"
colnames(hev2)[4]= "year"
all_hev<-merge(all, hev2, by = c("camp_name","year", "month_num2"), all = T)
all_hev2<- subset(all_hev, year > 2006)


all_hev2$spill20kmbuff<- ifelse(!is.na(all_hev2$spillover_name), "1", "0")
all_hev2$spill20kmbuff[is.na(all_hev2$spill20kmbuff)] <- 0

ffxy<- ff[c(1,3,4)]
ffxy<- as.data.frame(unique(ffxy))
colnames(ffxy) = c("camp_name", "lat_roost", "long_roost")
all_hev2<- merge(all_hev2, ffxy, by = "camp_name")


all_hev2$latbin = cut(all_hev2$lat_roost, breaks = c(-28.5, -24, -20, -15))
all_hev2$latbin<- droplevels(all_hev2$latbin)

onezero<- subset(all_hev2, bff.presence == 0 | bff.presence == 1)
range(all_hev2$lat, na.rm = T)
table(all_hev2$latbin)

all_checked<-all_hev2[!is.na(all_hev2$lat),]

all_checked_spill<- subset(all_checked, spill20kmbuff >0)

all_spills<-all_hev2[!is.na(all_hev2$spillover_name),]

#######################################

#t<- checked_spill[,c(1:7,9,11,15:27,31:89,91:92)]
#checked2<- checked[,c(1:7,9,11,15:27,31:89,91:92)]
#unchecked2<- unchecked[,c(1:7,9,11,15:27,31:89,91:92)]




ggplot(checked %>% filter(year > 2000 & year < 2021)
         %>% arrange(pres)) +
geom_smooth(aes(x = yrmon , y = mean_csum_precip_20kbuff, group=pres, color = pres), linewidth = 1) +
 # geom_smooth( aes(x = yrmon , y = mean_mintemp_20kbuff, group=pres, color = pres) ) +
 # geom_smooth( aes(x = yrmon , y = mean_maxtemp_20kbuff, group=pres, color = pres),linewidth =1.4) +
  #geom_hline(yintercept=0, color = "black", linetype = "dashed")+
  labs(x ='Time', y ='Mean Cumulative Precip (mm) in 20km Rooost Foraging Buffers', color = "Presence") +
  scale_color_manual(values = c("grey60", "blue3")) +
  theme_classic(base_size = 10) +
  # ylim(-3,3) +
  #facet_wrap(~pres, ncol = 1)+
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 85, vjust =1, hjust =1 ))


##########
all_hev2%>%
  filter(year > 2000 & year < 2015 & month_num2 > 4 & month_num2 < 11) %>%
  arrange(spill20kmbuff) %>%
  ggplot() +
 # geom_point( aes(x = prop_20kbuff_yrobs , y = mean_csum_precip_20kbuff, color = as.factor(year))) +
 # geom_point( aes(x = prop_20kbuff_yrobs , y = mean_csum_precip_20kbuff, shape = as.factor(year),color = spillover_name), size = 3) +
 # geom_point( aes(x = prop_20kbuff_yrobs , y =prcpanom_lag1mo,color =spillover_name), size = 3) +
 geom_point( aes(x = prop_20kbuff_yrobs , y =tmaxanom_lag3mo, color =spillover_name), size = 3) +

   geom_hline(yintercept=1, color = "black", linetype = "dashed")+
#labs(x ='Proportion of Winter Habitat at time of Survey', y ='Mean Cum. Precip Anomaly in 20km Rooost Foraging Buffers 1mo lag', shape = "year", color = "Spillover ID") +
  labs(x ='Proportion of Winter Habitat at time of Survey', y ='Mean Max Temp Anomaly in 20km Rooost Foraging Buffers 3mo lag', shape = "year", color = "Spillover ID") +
  
  #  scale_color_manual(values = c("grey60", "red3")) +
  theme_bw(base_size = 10) +
  # ylim(0,5) +
  facet_wrap(~yrmon , ncol = 6)+
  theme(legend.position  = "none", axis.text.x = element_text(angle = 35, vjust =1, hjust =1 ))
 

##
write.csv(all_hev2, "all_hev2_megadf_witheverything_20231113.csv", row.names =F)


##### extract out some variables and melt separately 

all_precipmini_anom<- all_hev2[,c(1,2,3,5,49,51,53,55:66,102,104,128:134)]
all_tempmini_anom<- all_hev2[,c(1,2,3,5,49,51,53,67:90,102,104,128:134)]

all_prcp_melt_anom<- reshape2::melt(all_precipmini_anom, id = c("camp_name", "lat_roost", "long_roost", "year", "yrmon", "month_num2" ,"bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff", "latbin" ))
all_temp_melt_anom<- reshape2::melt(all_tempmini_anom, id = c("camp_name",  "lat_roost","long_roost","year", "yrmon", "month_num2" , "bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" , "latbin"))



all_prcp_melt_anom %>%
  arrange(spill20kmbuff) %>%
  filter(year > 2006 & year < 2015 &
           variable != "mean_csum_precip_anom_20kbuff" & variable != "mean_maxtemp_anom_20kbuff" 
         & variable != "mean_mintemp_anom_20kbuff"  ) %>%
  ggplot() +
  geom_point( aes(x = prop_20kbuff_yrobs , y =value, color = spillover_name), size = 1.2) +
  geom_hline(yintercept=1, color = "black", linetype = "dashed")+
   labs(x ='Proportion of Winter Habitat at time of Survey', y ='Mean Cumulative Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover ID") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  
 #  scale_color_manual(values = c("grey60", "red3")) +
  #  scale_shape_manual(values = c( 16,15,17,9,3)) +
 #scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1 ))+
  theme_bw(base_size = 8) +
  # ylim(-3,3) +
  facet_wrap(~variable, ncol = 8)+
  theme(legend.position  = "none", axis.text.x = element_text(angle = 0, vjust =1, hjust =1 ))

#range(all_prcp_melt_anom$value, na.rm = T)


ggsave("roost20kmbuff_lagged_precipanoms_byyr_dots_20231024.png", plot = last_plot() , width=10,  height = 13, units = c("in"), dpi = 350)


all_temp_melt_anom %>%
  arrange(spill20kmbuff) %>%
  filter(year > 2006 & year < 2015 &
           variable != "mean_csum_precip_anom_20kbuff" & variable != "mean_maxtemp_anom_20kbuff" 
         & variable != "mean_mintemp_anom_20kbuff"  ) %>%
  ggplot() +
  geom_point( aes(x = prop_20kbuff_yrobs , y =value, color = spillover_name), size = 1.2) +
  geom_hline(yintercept=1, color = "black", linetype = "dashed")+
  labs(x ='Proportion of Winter Habitat at time of Survey', y ='Mean Min and Max Temp Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover ID") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  
  #  scale_color_manual(values = c("grey60", "red3")) +
  #  scale_shape_manual(values = c( 16,15,17,9,3)) +
 #scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1 ))+
  theme_bw(base_size = 8) +
  # ylim(-3,3) +
  facet_wrap(~variable*latbin, ncol = 12)+
  theme(legend.position  = "none", axis.text.x = element_text(angle = 0, vjust =1, hjust =1 ))


ggsave("roost20kmbuff_lagged_tempanoms_dots_20231025.png", plot = last_plot() , width=13,  height = 7, units = c("in"), dpi = 350)

##############


all_temp_melt_anom %>%
  arrange(spill20kmbuff) %>%
  filter(year > 2006 & year < 2015 &
           variable != "mean_csum_precip_anom_20kbuff" & variable != "mean_maxtemp_anom_20kbuff" 
         & variable != "mean_mintemp_anom_20kbuff"  ) %>%
  ggplot() +
  geom_point( aes(x = yrmon , y =value, color = spill20kmbuff), size = 1.3) +
  geom_hline(yintercept=0, color = "black", linetype = "dashed")+
  #labs(x ='Proportion of Winter Habitat at time of Survey', y ='Mean Temperature Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover ID") +
   labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Roost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  
  scale_color_manual(values = c("grey60", "red3")) +
  #  scale_shape_manual(values = c( 16,15,17,9,3)) +
  #scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1 ))+
  theme_bw(base_size = 10) +
  # ylim(-3,3) +
  facet_wrap(~variable, ncol = 6)+
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 85, vjust =1, hjust =1 ))





##############################

checked_precipmini_anom<- checked[,c(1,2,3,5,49,51,53,55:66,91,92,102,104,128:131)]
checked_tempmini_anom<- checked[,c(1,2,3,5,49,51,53,67:92,102,104,128:131)]

checked_prcp_melt_anom<- reshape2::melt(checked_precipmini_anom, id = c("camp_name", "lat", "year", "yrmon", "month_num2" ,"bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" ))
checked_temp_melt_anom<- reshape2::melt(checked_tempmini_anom, id = c("camp_name","lat","year", "yrmon", "month_num2" , "bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" ))

##############################
checked_spill_prcpmini_anom<- checked_spill[,c(1,2,3,5,49,51,53,55:66,91,92,102,104,128:131)]
checked_spill_tempmini_anom<- checked_spill[,c(1,2,3,5,49,51,53,67:92,102,104,128:131)]

checked_spill_prcp_melt_anom<- reshape2::melt(checked_spill_prcpmini_anom, id = c("camp_name", "lat", "year", "yrmon", "month_num2" ,"bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" ))
checked_spill_temp_melt_anom<- reshape2::melt(checked_spill_tempmini_anom, id = c("camp_name","lat","year", "yrmon", "month_num2" , "bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" ))


##regular vals 
checked_spill_prcpmini<- checked_spill[,c(1,2,3,5,7,9,11,13:24,49,51,53,102,104,128:131)]
#checked_spill_tempmini<- checked_spill[,c(1,2,3,5,7,9,11,25:49,51,53,67:90, 102,104,128:131)]
checked_spill_tempmini<- checked_spill[,c(1,2,3,5,25:49,102,104,128:131)]


checked_spill_prcp_melt<- reshape2::melt(checked_spill_prcpmini, id = c("camp_name","year", "yrmon", "month_num2" ,"bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" ))
checked_spill_temp_melt<- reshape2::melt(checked_spill_tempmini, id = c("camp_name","year", "yrmon", "month_num2" , "bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" ))


checked_spill_prcp_melt%>%
  filter(year > 2000 & year < 2015 & 
    variable != "mean_csum_precip_20kbuff" & variable != "mean_maxtemp_20kbuff" & variable != "mean_mintemp_20kbuff" ) %>%
  ggplot() +
   geom_point( aes(x = spillover_name , y =value, shape = as.factor(year),color = spillover_name), size = 3) +
 # labs(x ='Proportion of Winter Habitat at time of Survey', y ='Mean Cumulative Precipitation (mm) in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover ID") +
  #  scale_color_manual(values = c("grey60", "red3")) +
  scale_shape_manual(values = c( 16,15,17,9,3)) +
  theme_bw(base_size = 10) +
  # ylim(-3,3) +
  facet_wrap(~variable, ncol = 9)+
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 85, vjust =1, hjust =1 ))


checked_spill_prcp_melt_anom%>%
  filter(year > 2000 & year < 2015  & month_num2 > 4 & month_num2 < 11 &
           variable != "mean_csum_precip_anom_20kbuff" & variable != "mean_maxtemp_anom_20kbuff" & variable != "mean_mintemp_anom_20kbuff" &variable != "long"  ) %>%
  ggplot() +
  geom_point( aes(x = spillover_name , y =value, shape = as.factor(year),color = lat), size = 3) +
  #geom_hline(yintercept=0, color = "black", linetype = "dashed")+
  # labs(x ='Proportion of Winter Habitat at time of Survey', y ='Mean Cumulative Precipitation (mm) in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover ID") +
  #  scale_color_manual(values = c("grey60", "red3")) +
  scale_shape_manual(values = c( 16,15,17,9,3)) +
  theme_bw(base_size = 10) +
  # ylim(-3,3) +
  facet_wrap(~variable, ncol = 7)+
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 85, vjust =1, hjust =1 ))



checked_temp_melt_anom %>%
  arrange(spill20kmbuff) %>%
  filter(year > 2000 & year < 2015 & 
           variable != "mean_csum_precip_anom_20kbuff" & variable != "mean_maxtemp_anom_20kbuff" & variable != "mean_mintemp_anom_20kbuff" & variable !="long") %>%
  ggplot() +
  geom_point( aes(x = yrmon , y =value, color = spill20kmbuff), size = 2, fill = "grey24") +
  #geom_hline(yintercept=0, color = "black", linetype = "dashed")+
  # labs(x ='Proportion of Winter Habitat at time of Survey', y ='Mean Cumulative Precipitation (mm) in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover ID") +
  scale_color_manual(values = c("grey60", "red3")) +
  #  scale_shape_manual(values = c( 16,15,17,9,3)) +
  theme_bw(base_size = 10) +
  # ylim(-3,3) +
  facet_wrap(~variable, ncol = 6)+
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 85, vjust =1, hjust =1 ))

#################################
onezero_hev2%>%
  filter(year > 2000 & year < 2015) %>%
  arrange(spill20kmbuff) %>%
  ggplot() +
  # geom_point( aes(x = prop_20kbuff_yrobs , y = mean_csum_precip_20kbuff, color = as.factor(year))) +
  # geom_point( aes(x = prop_20kbuff_yrobs , y = mean_csum_precip_20kbuff, shape = as.factor(year),color = spillover_name), size = 3) +
  geom_point( aes(x = prop_20kbuff_yrobs , y =prcp_lag7mo, color = spillover_name), size = 3) +
  #geom_hline(yintercept=0, color = "black", linetype = "dashed")+
  labs(x ='Proportion of Winter Habitat at time of Survey', y ='Cumulative Precip in 20km Rooost Foraging Buffers 7 month lag', shape = "year", color = "Spillover ID") +
  #  scale_color_manual(values = c("grey60", "red3")) +
  theme_bw(base_size = 10) +
  # ylim(-3,3) +
  #facet_wrap(~year, ncol = 1)+
  theme(legend.position  = c(0.9,0.70), axis.text.x = element_text(angle = 85, vjust =1, hjust =1 ))

