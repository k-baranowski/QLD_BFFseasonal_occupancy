##########################################################
setwd(".")
# libraries
library(plyr)
library(dplyr) #manipulating dfs
library(ggplot2)
library(stringr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(lubridate)
library(gdata)
library(data.table)

##########################################################
#importing data for QLD roost locations 
qld<- read.csv("cleaned_qld_ff_data_Apr03_Mar22_kb_20221129.csv", header = T, stringsAsFactors = F)

#specify date format and add factor year column
#qld$date2 <- as.Date(qld$date, format = '%Y-%m-%d')
dates_fix<- parse_date_time(x = qld$date,
                            orders = c('%Y-%M-%d', '%Y-%m-%d'))


qld$date = dates_fix
qld$year = as.numeric(format(qld$date, format = "%Y"))
qld$month = as.numeric(format(qld$date, format = "%m"))

qld$month2<- sprintf("%02d", as.numeric(qld$month))
#qld$month <- sprintf("%02d", as.numeric(as.character(qld$month)))

qld$date[order(qld$date)]

#sum(is.na(qld$month))

#pull out summer observations to get wrap around season 
summer_roosts<- subset(qld, season == "summer")

summer_roosts$year2<- ifelse(summer_roosts$month == 01, summer_roosts$year - 1,
                      ifelse(summer_roosts$month == 02, summer_roosts$year - 1,
                      ifelse(summer_roosts$month == 12, (summer_roosts$year + 1),
                      NA)))

summer_roosts$time<- ifelse(summer_roosts$month == 01, paste(summer_roosts$year2, summer_roosts$year, summer_roosts$season),
                     ifelse(summer_roosts$month == 02, paste(summer_roosts$year2, summer_roosts$year, summer_roosts$season),
                     ifelse(summer_roosts$month == 12, paste(summer_roosts$year, summer_roosts$year2, summer_roosts$season),
                     NA)))

summer_roosts$time<- as.factor(summer_roosts$time)
summer_roosts[24]<- NULL

#pull out other seasons that don't cross over years and make time column 
other_roosts<- subset(qld, season != "summer")
other_roosts$time<- as.factor(paste(other_roosts$year, other_roosts$season))

allroosts<- rbind(other_roosts, summer_roosts)
str(allroosts)


##########################################
# #only consider roosts after 2013
qld_recent<- subset(allroosts, allroosts$date > "2006-12-30" & date < "2022-03-01")  #11500

qld_recent$yearmon = format(as.character(format(qld_recent$date, format = "%Y-%m")))

#add in abbreviations for month name 
qld_recent$mon <-month.abb[qld_recent$month]
#fix month order
levels<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
qld_recent$mon <- reorder.factor(qld_recent$mon, new.order=levels)

##fill in all the months in between FF surveys (~180 between 2007 and 2022)
times<-read.csv("fulltimesJan2007Dec2022.csv", header = T)
times$month2<- sprintf("%02d", as.numeric(times$month))
times$yearmon= paste(times$year, sep = "-",times$month2 )

##assign season numbers 
times <- times %>%
  arrange(yearmon) %>% 
  mutate(month_num = rleid(yearmon))


qld_alltimes<- merge(qld_recent, times, by = c("year", "month", "month2", "yearmon"), all=T)


#some obs have NA for  bff.presence but by ff.counts & ghff counts  we know BFF were absent
qld_alltimes$bff.presence <- qld_alltimes$bff.presence %>% replace_na(0)

##assign season numbers 
qld_alltimes<- qld_alltimes %>%
  arrange(date) %>% 
  mutate(ssn_num = rleid(season),
         ssn_diff_lastsamp = ssn_num-lag(ssn_num))

##assign difference in days between surveys at same camp  
qld_alltimes<- qld_alltimes %>%
  arrange(date) %>%
  group_by(camp.name) %>%
  mutate(diff_date = c(0,diff(date)),
         diff_month = c(0,diff(month_num)),
         presence_change= bff.presence- lag(bff.presence))

#replace NAs from new roost surveys to a 9 to represent new roost survey in numeric 
qld_alltimes$presence_change <- qld_alltimes$presence_change %>% replace_na(9)


table(qld_alltimes$year)
# 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 
#  322  462  765  978 1130 1334 1423 1198  534  487  437  357  568  661  688  175 

table(qld_alltimes$season)
# autumn spring summer winter 
# 2598    3128    2991   2790


#make binary column of change or no change 
qld_alltimes$pres_change2<- ifelse(qld_alltimes$presence_change != 0, 1, 0)


#fill in the dataframe with the 'hold' roosts in between months they weren't checked 
f<- unique(qld_alltimes[c("camp.name", "yearmon", "year", "month2")])
 

##fill in the years for all the roosts so I can find 'hold' roosts 
f<- f%>%
  complete(year, month2)

f$status<- ifelse(is.na(f$yearmon), "hold", "surveyed")
#fill in the times not sampled at each roost 
t<- merge(qld_alltimes, f, by = c("camp.name", "yearmon","year","month2"), all = T)




#################################################### Seasonal sums ###################################################################
winter<- subset(qld_recent, season == "winter")
autumn<- subset(qld_recent, season == "autumn")
spring<- subset(qld_recent, season == "spring")
summer<- subset(qld_recent, season == "summer")

winter_sums = as.data.frame(winter %>%
                  arrange(time) %>%
                  group_by(camp.name) %>%
                  dplyr::summarise(
                  num.wint.sampled = n_distinct(time),
                  wint.sample.dates = n(),
                  sum.wint.occ.bff = sum(bff.presence, na.rm = T),
                  sum.wint.chng = sum(pres_change2, na.rm = T),
                  max.wint.bff.pres = max(bff.presence))) 

spring_sums = as.data.frame(spring %>%
                   arrange(time) %>%
                   group_by(camp.name) %>%
                   dplyr::summarise(
                   num.spr.sampled = n_distinct(time),
                   spr.sample.dates = n(),
                   sum.spr.occ.bff = sum(bff.presence, na.rm = T),
                    sum.spr.chng = sum(pres_change2, na.rm = T),
                   max.spr.bff.pres = max(bff.presence))) 

autumn_sums = as.data.frame(autumn %>%
                    arrange(time) %>%
                    group_by(camp.name) %>%
                    dplyr::summarise(
                    num.autm.sampled = n_distinct(time),
                    autm.sample.dates = n(),
                    sum.autm.occ.bff = sum(bff.presence, na.rm = T), 
                    sum.atumn.chng = sum(pres_change2, na.rm = T),
                    max.autm.bff.pres = max(bff.presence))) 


summer_sums = as.data.frame(summer %>%
                      arrange(time) %>%
                      group_by(camp.name) %>%
                      dplyr::summarise(
                       num.summr.sampled = n_distinct(time),
                       summr.sample.dates = n(),
                       sum.summr.occ.bff = sum(bff.presence, na.rm = T), 
                       sum.summr.chng = sum(pres_change2, na.rm = T),
                       max.sum.bff.pres = max(bff.presence))) 

#merge all the dataframes together
ssn_list <- list(summer_sums, autumn_sums, winter_sums, spring_sums)      

#merge all data frames together
ssns<- ssn_list %>% reduce(full_join, by='camp.name')

ssns$sum.ssn.occ.chngs<- rowSums(ssns[ ,c(5,10,15,20)], na.rm=TRUE)

#reorder variables
ssns2<- ssns[,c(1, 5,10,15,20,17:19,21,2:4,6,7:9,11:14,16,22)]

#subset roosts that are sampled a minimum of 3 of each season
ssns_min3<- subset(ssns2,  num.summr.sampled >=3 & num.autm.sampled >=3 & num.wint.sampled >=3 &num.spr.sampled >=3  )
ssns_min3$num.times.occ<- rowSums(ssns_min3[ ,c(8,12,16,20)], na.rm=TRUE)
#ssns_min3$num.times.occ<- rowSums(ssns_min3[ ,c(9,12,15,18)], na.rm=TRUE)

##STOP
winter_split<- split(winter, f=list(winter$year,winter$month2))
write.csv(winter, "allwinter_observations_20072019_20230305.csv", row.names = FALSE )

path = "/Users/kelsee/Desktop/Research/BFF_roostoccupancy_20220510/data/roost_selection/monthly_obs/winter_"
for(i in 1:length(winter_split)){
  write.csv(data.frame(winter_split[[i]]), file = paste0(path, names(winter_split)[i], '.csv'))
}

t<-winter %>%
  group_by(month, month2, year, presence_change ) %>%
reframe(sum_changes= sum(presence_change, na.rm = T))

####
# 
# camp_summaries_time = as.data.frame(qld_recent %>%
#                       arrange(time) %>%
#                        group_by(camp.name, lat, long, time, season, ssn_num) %>%
#                                  dplyr::summarise(sample.dates = n(),
#                                  sum.occ.bff = sum(bff.presence, na.rm = T), 
#                                  sum.occ.ghff = sum(ghff.presence, na.rm = T),
#                                  sum.occ.lrff = sum(lrff.presence, na.rm = T), 
#                                  sum.count.bff = sum(bff.count, na.rm = T),
#                                  sum.count.ghff = sum(ghff.count, na.rm = T), 
#                                  sum.count.lrff = sum(lrff.count, na.rm = T), 
#                                  min.count.bff = min(bff.count), 
#                                  max.count.bff = max(bff.count), 
#                                  mean.count.bff = mean(bff.count, na.rm = T),
#                                  ffpresence = max(ff.presence),
#                                  bffpresence = max(bff.presence),
#                                  time.occ.chngs = sum(pres_change2, na.rm = T),
#                                  max.ssn.btwn.samp = max(ssn_diff_lastsamp))) 

####### month exploration 
camp_summaries_month = as.data.frame(qld_recent %>%
                        arrange(time) %>%
                        group_by(camp.name, lat, long, yearmon, year, month, month2, season, ssn_num) %>%
                        dplyr::summarise(sample.dates = n(),
                        sum.occ.bff = sum(bff.presence, na.rm = T), 
                        sum.occ.ghff = sum(ghff.presence, na.rm = T),
                        sum.occ.lrff = sum(lrff.presence, na.rm = T), 
                        sum.count.bff = sum(bff.count, na.rm = T),
                        sum.count.ghff = sum(ghff.count, na.rm = T), 
                        sum.count.lrff = sum(lrff.count, na.rm = T), 
                        min.count.bff = min(bff.count), 
                        max.count.bff = max(bff.count), 
                        mean.count.bff = mean(bff.count, na.rm = T),
                        ffpresence = max(ff.presence),
                        bffpresence = max(bff.presence),
                        ghffpresence = max(ghff.presence),
                        time.occ.chngs = sum(pres_change2, na.rm = T),
                        max.ssn.btwn.samp = max(ssn_diff_lastsamp))) 



#make binary columns for seasonal occupancy
camp_summaries_month$pres_summr<- ifelse(camp_summaries_month$season == "summer" & camp_summaries_month$sum.occ.bff > 0, 1, 0 )
camp_summaries_month$pres_spr<- ifelse(camp_summaries_month$season == "spring" & camp_summaries_month$sum.occ.bff > 0, 1, 0 )
camp_summaries_month$pres_atmn<- ifelse(camp_summaries_month$season == "autumn" & camp_summaries_month$sum.occ.bff > 0, 1, 0 )
camp_summaries_month$pres_winter<- ifelse(camp_summaries_month$season == "winter" & camp_summaries_month$sum.occ.bff > 0, 1, 0 )

#collapse rows to show binary seasonal presence
camp_summaries_month$num_ssn_occ<- rowSums(camp_summaries_month[ ,c(25:28)])

#make a dataframe with the roost and seasons and binary occupancy factors
#binary_presencedf<- camp_summaries_month[,c(1:5, 22:26)]

##take out the binary season columns 
camp_summaries_month[,c(25:28)]<- NULL



# #sum up the seasons occupied across all 60 
camp_summaries_month<- camp_summaries_month %>%
  group_by(camp.name) %>%
  dplyr::mutate(total_ssns_occ= sum(bffpresence))

#camp_summaries_month$time <- reorder.factor(camp_summaries_month$time, new.order=levels2) #manually set time levels


####merge 
allmin3_sums<- merge(camp_summaries_month, ssns_min3, by = "camp.name", all.y = T)

levels3<- c("summer", "autumn", "winter", "spring")
allmin3_sums$season <- reorder.factor(allmin3_sums$season, new.order=levels3)

#make abbrev month column for xaxis display consistent with all weather/climate data 
# allmin3_sums$mon <-month.abb[allmin3_sums$month]
levels<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# allmin3_sums$mon <- reorder.factor(allmin3_sums$mon, new.order=levels)

allmin3_sums$yearmon= paste(allmin3_sums$year, sep = "-", allmin3_sums$month2 )
#allmin3_sums$yearmon2= paste(allmin3_sums$year, sep = " ", allmin3_sums$mon )



allmin3_sumst<- merge(allmin3_sums, times, by = c("yearmon", "year", "month", "month2"), all = T)
#allmin3_sums_dates<-merge()
allmin3_sumst$mon <-month.abb[allmin3_sumst$month]
allmin3_sumst$mon <- reorder.factor(allmin3_sumst$mon, new.order=levels)

#allmin3_sums$yearmon= paste(allmin3_sums$year, sep = "-", allmin3_sums$month )
allmin3_sumst$yearmon2= paste(allmin3_sumst$year, sep = " ", allmin3_sumst$mon )
#allmin3_sumst$camp.name = allmin3_sumst$camp.name %>% replace_na("Worongary, Worongary Road")


##BOOM 
allmin3_sumst %>%
  filter(camp.name == "Tin Can Bay, Snapper Point" | camp.name == "Warwick, Grafton Street" 
  | camp.name == "Gayndah, Barambah Ck" | camp.name == "Sunshine Acres, Black Swamp Creek" 
  #| camp.name == "Dundowran Beach, Ocean Park Drive, Petersons Park" 
  # | camp.name == "Oakey - Campbell St" | camp.name == "Sandgate, Curlew Park" 
   | camp.name == "Worongary, Worongary Road"
  | camp.name == "Dundowran Beach, Ocean Park Drive, Petersons Park") %>%
  ggplot() +
  #geom_line(aes(x = yearmon, y = bffpresence, color = season, group = camp.name), size =1.5) +
  geom_point(aes(x = yearmon, y = ghffpresence, color = season), size =3) +
  theme_bw(base_size = 9) +
  scale_y_continuous(breaks = c(0,1))+
  scale_color_manual(values = c( "red", "orange", "blue", "forestgreen")) +
  facet_wrap( facets = ~reorder(camp.name, -lat), drop = FALSE, ncol =1) +
  scale_x_discrete(drop = FALSE, breaks = allmin3_sumst$yearmon, labels = allmin3_sumst$yearmon2) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 85, vjust =0.90,  hjust=0.90 )) 

###plot GHFF  presence 
allmin3_sumst %>%
  filter(  
    # camp.name == "Redcliffe Botanic Gardens"|
    #  camp.name ==  "Southport, Akes Avenue,"|
    # camp.name ==    "Oakey - Campbell St" |
    #  camp.name ==   "Finch Hatton Gorge" |
    #  camp.name ==   "Currumbin Creek" |
    #  camp.name ==   "Noosaville, Wallace Drive" |
    #  camp.name == "Broadbeach, Cascade Gardens"
    camp.name == "Woodend" |
    camp.name == "Bundamba, Paice St" |
     camp.name == "Maroochydore, Stella Maris CS" |
     camp.name == "Sandstone Point, Bestman Rd" |
     camp.name == "Horton, Station Road" |
     camp.name == "Redbank (Pan Pacific Peace Garden)" |
     camp.name == "Regents Park, Emerald Drive" |
    camp.name == "Bargara, Larder Street"|
     camp.name == "Miami, Pizzey Drive") %>%
  ggplot() +
  #geom_line(aes(x = yearmon, y = bffpresence, color = season, group = camp.name), size =1.5) +
  geom_point(aes(x = yearmon, y = ghffpresence, color = season), size =3) +
  theme_bw(base_size = 9) +
  scale_y_continuous(breaks = c(0,1))+
  scale_color_manual(values = c( "red", "orange", "blue", "forestgreen")) +
  facet_wrap( facets = ~reorder(camp.name, -lat), drop = FALSE, ncol =1) +
  scale_x_discrete(drop = FALSE, breaks = allmin3_sumst$yearmon, labels = allmin3_sumst$yearmon2) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 85, vjust =0.90,  hjust=0.90 )) 









allmin3_sumst %>%
  filter(
    wint.sample.dates > 15
  #  season == "winter"
      # camp.name == "Dundowran Beach, Ocean Park Drive, Petersons Park"|
      # camp.name ==   "Sunshine Acres, Black Swamp Cree"  |
      # camp.name ==  "Gayndah, Barambah Ck"|
      # camp.name == "Tin Can Bay, Snapper Point" |
      # camp.name ==   "Warwick, Grafton Street"
        # camp.name == "Redcliffe Botanic Gardens"|
        # camp.name ==  "Southport, Akes Avenue,"|
        # camp.name ==    "Oakey - Campbell St" |
        # camp.name ==   "Finch Hatton Gorge" |
        # camp.name ==   "Currumbin Creek" |
        # camp.name ==   "Noosaville, Wallace Drive"
        # camp.name == "Broadbeach, Cascade Gardens" |
     # camp.name == "Woodend" |
     # camp.name == "Bundamba, Paice St" |
     # camp.name == "Maroochydore, Stella Maris CS" |
     # camp.name == "Sandstone Point, Bestman Rd" |
     # camp.name == "Horton, Station Road" |
     # camp.name == "Redbank (Pan Pacific Peace Garden)" |
     # camp.name == "Regents Park, Emerald Drive" |
     # camp.name == "Bargara, Larder Street"|
     # camp.name == "Miami, Pizzey Drive"
  ) %>%
ggplot() +
  geom_boxplot(aes(x = reorder(camp.name, -max.count.bff), y = max.count.bff, color = season)) +
  geom_point(aes(x = camp.name, y = max.count.bff, color = season), position=position_jitterdodge()) +
  #geom_point(aes(x = yearmon, y = sum.count.bff, color = season), size =3) +
  theme_bw(base_size = 9) +
  #ylim(0,50000)+
  scale_color_manual(values = c("red", "orange", "blue" , "forestgreen")) +
  #scale_color_manual(values = c("blue" )) +
  
  #facet_wrap(facets = ~reorder(camp.name, -lat), ncol =1) +
  #facet_wrap(~year, ncol = 1) + 
  #scale_x_discrete(drop = FALSE, breaks = allmin3_sumst$yearmon, labels = allmin3_sumst$yearmon2) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 85, vjust =0.90,  hjust=0.90 )) 


#ggsave("Roosts_neverwintocc_4yrdata_mnthlypres2007_2021_mostdates.eps", plot = last_plot() , width=10,  height = 8, units = c("in"), dpi = 300)


##

t<- unique( allmin3_sums$camp.name)

###see if this is even necessary anymore 

#make binary columns for seasonal occupancy
camp_summaries_time$pres_summr<- ifelse(camp_summaries_time$season == "summer" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_spr<- ifelse(camp_summaries_time$season == "spring" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_atmn<- ifelse(camp_summaries_time$season == "autumn" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_winter<- ifelse(camp_summaries_time$season == "winter" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )

#collapse rows to show binary seasonal presence
camp_summaries_time$num_ssn_occ<- rowSums(camp_summaries_time[ ,c(21:24)])

#make a dataframe with the roost and seasons and binary occupancy factors
#binary_presencedf<- camp_summaries_time[,c(1:5, 22:26)]

##take out the binary season columns 
camp_summaries_time[,c(12,13,21:24)]<- NULL

 

# #sum up the seasons occupied across all 60 
camp_summaries_time<- camp_summaries_time %>%
                      group_by(camp.name) %>%
                      dplyr::mutate(total_ssns_occ= sum(bffpresence))

camp_summaries_time$time <- reorder.factor(camp_summaries_time$time, new.order=levels2) #manually set time levels


####merge 
allmin3_sums<- merge(camp_summaries_time, ssns_min3, by = "camp.name", all.y = T)

levels3<- c("summer", "autumn", "winter", "spring")
allmin3_sums$season <- reorder.factor(allmin3_sums$season, new.order=levels3)

winter3min_sums<- subset(allmin3_sums, season == "winter")
winter3min_sums <- droplevels(winter3min_sums)


allmin3_sums %>%
  filter(sum.ssn.occ.chngs <= 36 ) %>%
ggplot() +
  #geom_line(aes(x = time, y = bffpresence, color = season, group = camp.name), size =1.5) +
  geom_point(aes(x = time, y = bffpresence, color = season), size =1) +
  theme_bw(base_size = 9) +
  scale_y_continuous(breaks = c(0,1))+
  scale_color_manual(values = c( "red", "orange", "blue", "forestgreen")) +
  facet_wrap(facets = ~reorder(camp.name, num.wint.sampled), ncol = 4) +
  scale_x_discrete(drop = FALSE, labels = levels(camp_summaries_time$time)[c(T, rep(F, 1))],
                   breaks = levels(camp_summaries_time$time)[c(T, rep(F, 1))])  +
  theme(legend.position = "none", axis.text.x = element_text(angle = 85, vjust =0.90,  hjust=0.90 )) 


ggsave("allroosts_allseasonswinter_occupancy_230128.eps", plot = last_plot() , width=14,  height =22 , units = c("in"), dpi = 300)

quantile(allmin3_sums$sum.ssn.occ.chngs, probs = seq(0, 1, 1/4))

###############################################

###summarize the sampling by camp names
count_roosts <- as.data.frame(camp_summaries_time %>%
                              arrange(time) %>%
                               group_by(camp.name) %>%
                               dplyr::summarise(
                               num.ssns.occ = max(total_ssns_occ),
                               num.ssns.sampled = n_distinct(time),
                               #ssn.prop.occ = num.ssns.occ/num.ssns.sampled,
                               season.span.surveys = last(ssn_num) - first(ssn_num) + 1,
                               first.ssn = first(time),
                               last.ssn = last(time),
                               num.surveys = sum(sample.dates),
                               total.sum.occ.bff = sum(sum.occ.bff, na.rm = T),
                               sum.occ.chngs = sum(time.occ.chngs, na.rm = T)))




count_roosts$prop.srvy.span= count_roosts$num.ssns.sampled/count_roosts$season.span.surveys
count_roosts$bffprop.occ.survs<-  count_roosts$total.sum.occ.bff/count_roosts$num.surveys

allmin3_roosts<- merge(count_roosts, ssns_min3, by = "camp.name", all.y = T)


##############
###THIS ONE! identify roosts taht have at least 3 seasons of each season with surveys
# roosts_min_3ssns<- merge(frequencies_long, min_three_ssns, by = "camp.name", all.y=T)
#roosts_min_3ssns<- merge(frequencies_long, min_three_ssns, by = "camp.name", all.y=T)

####plotting 
# ggplot(roosts_min_3ssns) +
#   geom_bar(aes(x = reorder(camp.name, ssns_surveyed), y=ssn.occ, fill = season), stat = "identity", position = "dodge") +
#   scale_fill_manual(values = c( "red", "orange", "blue", "forestgreen")) +
#   #geom_vline(xintercept =  lines, linetype="dotted", 
#             # color = "tomato2", size=1.5) +
#   labs(t = "Proportion of Seasonal Occupancy", x = "") +
#   theme_bw(base_size = 12)
# 
# #subset where occupancy in all seasons is 1 
# alwaysocc<- subset(roosts_min_3ssns, min.ssn.occ ==1)

middle<- subset(roosts_min_3ssns, min.ssn.occ <1 & min.ssn.occ)
## still dont think I did the seasonal change right, come bak to it tomorrow or sunday 




allmin3_roosts$prop.summr.surv.occ<- allmin3_roosts$sum.summr.occ.bff/allmin3_roosts$summr.sample.dates
allmin3_roosts$prop.spring.surv.occ<- allmin3_roosts$sum.spr.occ.bff/allmin3_roosts$spr.sample.dates
allmin3_roosts$prop.wint.surv.occ<- allmin3_roosts$sum.wint.occ.bff/allmin3_roosts$wint.sample.dates
allmin3_roosts$prop.autm.surv.occ<- allmin3_roosts$sum.autm.occ.bff/allmin3_roosts$autm.sample.dates


min3roosts_propocc<- allmin3_roosts[,c(1,2, 3,9, 30:33)]
xy<-as.data.frame(unique(allroosts[,c(2:4)]))
min3roosts_propocc2<- left_join(min3roosts_propocc, xy, by = "camp.name", all.x = T)

#write.csv(min3roosts_propocc2, "roostsmin3ssn_propoccssn_20230128.csv", row.names = F)


#calculate the seasonal proportion of occupancy based on number of surveys
ssn_roosts$prop.summer.surv.occ= ssn_roosts$numsurv.occ.summer/ssn_roosts$total_summer_surveys
ssn_roosts$prop.spring.surv.occ<- ssn_roosts$numsurv.occ.spring/ssn_roosts$total_spring_surveys
ssn_roosts$prop.autumn.surv.occ<- ssn_roosts$numsurv.occ.autumn/ssn_roosts$total_autumn_surveys
ssn_roosts$prop.winter.surv.occ<- ssn_roosts$numsurv.occ.winter/ssn_roosts$total_winter_surveys

##### calculate number of annual seasons occupied
ssn_roosts$numssns_occ<- rowSums(ssn_roosts[c("numsurv.occ.summer", "numsurv.occ.autumn", "numsurv.occ.winter", "numsurv.occ.spring")] > 0)

#calculate the seasonal proportion of occupancy based on number of surveys
ssn_roosts$prop.ssns.occ<- ssn_roosts$num.ssns.occ/ssn_roosts$num.ssns.sampled


############33    START HERE1!!!!!!! ####### fix ssn change column as well 
### ssn_roosts needs to be reordered and cut, but its the mega dataframe 



#don't know if this is the right metric but want something to describe number of changes relative to times surveyed
#ssn_roosts$propchng.persurv<- ssn_roosts$sum.occ.chng/ssn_roosts$num.surveys

#this is a good dataframe!! All cleaned!! 
ssn_roosts_cl<- ssn_roosts[,c(1,2,3,28,4:8,13,14, 9,19,15,23,  11,20,16,25,  12,21,17,26,  10,22,18,24,27)]
ssn_roosts_cl2<- ssn_roosts[,c(1,2,3,28,4:8, 13,14, 9,19,15,  11,20,16,  12,21,17,  10,22,18, 23,25,26,24,27)]
################################################################################
###find the roosts that have at least 3 seasons in all seasons
three_ssns<-subset(ssn_roosts_cl2, numsurv.occ.summer >=3 & numsurv.occ.autumn >=3 & numsurv.occ.winter >=3 & numsurv.occ.spring >=3 )

quantile(three_ssns$prop.ssns.occ, probs = seq(0, 1, 1/4))
#   70%     75%    80%       90%       
# 0.875    0.904  0.9341   0.9683




##########################################################################################
#######now identify the ones that are sampled over 75% of the time during the sampling duration
ssn_roosts_10ssn_surv75<-  subset(ssn_roosts_10ssn, prop.srvy.span >= 0.75)
hist(ssn_roosts_10ssn_surv75$num.surveys)


##get the quantiles
quantile(ssn_roosts_10ssn_surv75$prop.srvy.span, probs = seq(0, 1, 1/4))
#     70%         75%       80%          90%       
#  0.9038462  0.9130435   0.9234286   0.9583333
lines = c(0.904, 0.923, 0.958)
ggplot(ssn_roosts_10ssn_surv75) +
  geom_histogram(aes(prop.srvy.span), bins = 25) +
  geom_vline(xintercept =  lines, linetype="dotted", 
             color = "tomato2", size=1.5) +
  labs(x = "Proportion the roost was surveyed within the duration of sampling at roost", y = "Number of Roosts") +
  theme_bw(base_size = 12)







#####################
time_summary<- as.data.frame(camp_summaries_time %>%
                               group_by(time) %>%
                               dplyr::summarise(num_unq_roosts = n(),
                                 total.samp.times = sum(sample.dates),
                                 total.sum.occ.bff = sum(sum.occ.bff, na.rm = T),
                                 total.sum.occ.ghff = sum(sum.occ.ghff, na.rm = T),
                                 total.sum.occ.lrff = sum(sum.occ.lrff, na.rm = T)))
time_summary$total.occ.bff.prop= time_summary$total.sum.occ.bff/time_summary$total.samp.times



ggplot(time_summary)  +
  #geom_line( aes(x = time, y = total.samp.times), group = "year", color = "black") +
  geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  #geom_line(data = nvrocc_yr, aes(x = year, y = num_roosts), group = "year", color = "black") +
  labs(x = "year", y= "Number of roosts") +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 12)




############## heat map of sampling and seasonal occupancy 
ggplot(camp_summaries_time, aes(x = time, y = reorder(x=camp.name,  bffpropocc) , fill =as.factor(prop_change2))) +
  geom_tile()  +
  #geom_text(aes(label=prop_change2)) +
  labs(y = 'camp names', fill = "Number of occupancy changes") +
  scale_y_discrete(drop = FALSE, labels = levels(camp_summaries_time$camp.name)[c(T, rep(F, 1))],
                   breaks = levels(camp_summaries_time$camp.name)[c(T, rep(F, 1))])  +
  scale_x_discrete(drop = FALSE, labels = levels(camp_summaries_time$time)[c(T, rep(F, 1))],
                   breaks = levels(camp_summaries_time$time)[c(T, rep(F, 1))])  +
  theme_classic()+
  scale_fill_manual(values = c('grey73', 'red')) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10, angle = 80, hjust = 0.6, vjust = 0.5 )) 
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) 
#theme_bw()

############## heat map of sampling and seasonal occupancy 
ggplot(time_sums, aes(x = year, y = reorder(x=camp.name, bffpropocc) , fill = prop_change2)) +
  geom_tile() +
  labs(fill = "Proportion occupancy changed from prior season")
  scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) 
#theme_bw()


#####################################################################################################################
