##########################################################
setwd(".")
# libraries
library(dplyr) #manipulating dfs
library(plyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(reshape2)
library(gridExtra)
library(lubridate)
library(data.table)


##########################################################
#importing data for QLD roost locations 
qld<- read.csv("cleaned_qld_ff_data_Apr03_Mar22_kb_20221129.csv", header = T, stringsAsFactors = F)


# #only consider roosts after 2013
qld_recent<- subset(qld, qld$date > "2006-12-30" & date < "2022-01-01")  #11323

table(qld_recent$year)
# 2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021
#  320   462   765   978  1130  1334  1423  1198   533   484   434   351   564   660   687

table(qld_recent$season)
# autumn spring summer winter 
# 2591    3138    2991   2790

#pull out winter observations
#winter<- subset(qld_recent, season == "winter") # 2790 records in winter 
qld_recent$time<- paste(qld_recent$season, qld_recent$year)



##############################################################################################################
#################################################### TIME ###################################################################

camp_summaries_time = as.data.frame(qld_recent %>%
                                group_by(camp.name, lat, long, time, season, year) %>%
                                 dplyr::summarise(sample.dates = n(),
                                 sum.occ.bff = sum(bff.presence, na.rm = T), 
                                 sum.occ.ghff = sum(ghff.presence, na.rm = T),
                                 sum.occ.lrff = sum(lrff.presence, na.rm = T), 
                                 sum.count.bff = sum(bff.count, na.rm = T),
                                 sum.count.ghff = sum(ghff.count, na.rm = T), 
                                 sum.count.lrff = sum(lrff.count, na.rm = T), 
                                 min.count.bff = min(bff.count, na.rm = T), 
                                 max.count.bff = max(bff.count, na.rm = T), 
                                 mean.count.bff = mean(bff.count, na.rm = T),
                                 ffpresence = max(ff.presence, na.rm = T))) %>% 
                                as.data.frame()

#make binary columns for seasonal occupancy
camp_summaries_time$pres_summr<- ifelse(camp_summaries_time$season == "summer" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_spr<- ifelse(camp_summaries_time$season == "spring" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_atmn<- ifelse(camp_summaries_time$season == "autumn" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_winter<- ifelse(camp_summaries_time$season == "winter" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$num_ssn_occ<- rowSums(camp_summaries_time[ , c(18:21)], na.rm=TRUE)
camp_summaries_time[,c(12,13)]<- NULL

#sum up the seasons occupied in one year 
camp_summaries_time<- camp_summaries_time %>%
                             group_by(camp.name, year) %>%
                             dplyr::mutate(total_ssn_occ= sum(num_ssn_occ))

table(camp_summaries_time$total_ssn_occ)
#ssns   0    1     2     3     4 
#obs   1776  1182  1282  1199  1644
 
#find times they're not there and calc prop occupancy
camp_summaries_time$t_bffnotocc<- camp_summaries_time$sample.dates - camp_summaries_time$sum.occ.bff
camp_summaries_time$bffpropocc<-  camp_summaries_time$sum.occ.bff/camp_summaries_time$sample.dates

#manually set time order
levels2<- c("summer 2007", "autumn 2007", "winter 2007", "spring 2007", 
            "summer 2008", "autumn 2008", "winter 2008", "spring 2008", 
            "summer 2009", "autumn 2009", "winter 2009", "spring 2009", 
            "summer 2010", "autumn 2010", "winter 2010", "spring 2010", 
            "summer 2011", "autumn 2011", "winter 2011", "spring 2011", 
            "summer 2012", "autumn 2012", "winter 2012", "spring 2012", 
            "summer 2013", "autumn 2013", "winter 2013", "spring 2013", 
            "summer 2014", "autumn 2014", "winter 2014", "spring 2014", 
            "summer 2015", "autumn 2015", "winter 2015", "spring 2015", 
            "summer 2016", "autumn 2016", "winter 2016", "spring 2016",  
            "summer 2017", "autumn 2017", "winter 2017", "spring 2017", 
            "summer 2018", "autumn 2018", "winter 2018", "spring 2018", 
            "summer 2019", "autumn 2019", "winter 2019", "spring 2019", 
            "summer 2020", "autumn 2020", "winter 2020", "spring 2020", 
            "summer 2021", "autumn 2021", "winter 2021", "spring 2021") 
require(gdata)
camp_summaries_time$time <- reorder.factor(camp_summaries_time$time, new.order=levels2)

#assign the seasons sampled with a number to account for time between surveys at a roost 
camp_summaries_time<- camp_summaries_time %>%
        arrange(time) %>% 
            mutate(ssn_num = rleid(season))





#clculate the change in time and change in occupancy 
camp_summaries_time<- camp_summaries_time %>% 
  arrange(time) %>% 
  group_by(camp.name) %>%
  dplyr::mutate(ssn_diff_lastsamp = ssn_num-lag(ssn_num),
                yrs_diff_last_samp= year- lag(year),
                prop_change= bffpropocc- lag(bffpropocc)) %>% 
  as.data.frame()
camp_summaries_time$prop_change2<- ifelse(camp_summaries_time$prop_change == 0, 0, 1)

 



hist(camp_summaries_time$ssn_diff_lastsamp, breaks = 60)
table(camp_summaries_time$ssn_diff_lastsamp)

#subset out roost observations that weren't sampled within the last 4 seasons, so no data on seasonal trends 
camps_1ssn_lastsamp<- subset(camp_summaries_time, ssn_diff_lastsamp == 1 ) 
#5323/7083 (75%) seasonal observations were continually sampled in the NFFMP 

camps_4ssn_lastsamp<- subset(camp_summaries_time, ssn_diff_lastsamp <= 4 )
#6292/7083 (89%) camp ~ seasonal observations are sampled within the 4 seasons of each other 

##### subset out the obervations that are sampled very  

###########################






#cast<- dcast(camp_summaries_time, camp.name~time, value.var = "bffpropocc" )



###summarize the sampling by camp names
count_roosts<- as.data.frame(camp_summaries_time %>%
                               group_by(camp.name) %>%
                               dplyr::summarise(
                               num.ssns.sampled = n_distinct(time),
                               season.span.surveys = last(ssn_num) - first(ssn_num) +1,
                               num.surveys = sum(sample.dates),
                               total.sum.occ.bff = sum(sum.occ.bff, na.rm = T),
                               first.ssn = first(time),
                               last.ssn = last(time),
                               sum.summer.occ = sum(pres_summr),
                               sum.spring.occ = sum(pres_spr),
                               sum.autumn.occ = sum(pres_atmn),
                               sum.winter.occ = sum(pres_winter),
                               #min_num_ssns_occ = min(total_ssn_occ),
                               #max_num_ssns_occ = max(total_ssn_occ),
                               sum.occ.chng = sum(prop_change2,  na.rm = T), 
                               max.ssn.btwn.samp = max(ssn_diff_lastsamp, na.rm = T),
                               unq.seasons= n_distinct(season),
                               unq.years= n_distinct(year)))

#count_roosts$total_prop_srvyd60= count_roosts$num_ssns_sampled/60
count_roosts$prop.srvy.span= count_roosts$num.ssns.sampled/count_roosts$season.span.surveys
count_roosts$bffpropocc<-  count_roosts$total.sum.occ.bff/count_roosts$num.surveys

#find roosts sampled at least 10 seasons in 
count_roosts_10ssn<- subset(count_roosts, num.ssns.sampled >= 10 )

                
ggplot(count_roosts_10ssn) +
  geom_histogram(aes(prop.srvy.span), bins = 80) +
  labs(x = "Proportion the roost was surveyed within the duration of sampling at roost", y = "Number of Roosts") +
  theme_bw(base_size = 12)

count_roosts_10ssn_surv75<-  subset(count_roosts_10ssn, prop.srvy.span >= 0.75)
hist(count_roosts_10ssn_surv75$num.surveys)

ggplot(count_roosts_10ssn_surv75) +
  geom_histogram(aes(prop.srvy,span), bins = 80) +
  labs(x = "Proportion the roost was surveyed within the duration of sampling at roost", y = "Number of Roosts") +
  theme_bw(base_size = 12)


##########################3
test<- as.data.frame(camp_summaries_time %>%
                       group_by(camp.name) %>%
                       dplyr::summarise(
                         count(season),
                         sum.summer.occ = sum(pres_summr),
                         sum.spring.occ = sum(pres_spr),
                         sum.autumn.occ = sum(pres_atmn),
                         sum.winter.occ = sum(pres_winter),
                         unq.seasons= n_distinct(season),
                         unq.years= n_distinct(year)))
colnames(test)[2]<- "season"

#make binary columns for seasonal occupancy
test$seasonal_occ<- ifelse(test$season == "summer", test$sum.summer.occ/test$freq,
                    ifelse(test$season == "spring", test$sum.spring.occ/test$freq,
                    ifelse(test$season == "autumn", test$sum.autumn.occ/test$freq,
                    ifelse(test$season == "winter", test$sum.winter.occ/test$freq, NA))))













ggplot(count_roosts)  +
  geom_point( aes(x = all_ssn_prop, y = bffpropocc, color = sum.occ.chng), size = 4) +
  #geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  labs(x = "Proportion of all seasons sampled", y= "Proportion BFF Occupancy", color = "Number of occupancy changes") +
  scale_color_gradient(high = "red", low = "grey") +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 13) + 
  theme(legend.position = "bottom")

##alwyas and never occupied 
alwaysocc<- subset(count_roosts, bffpropocc == 1)
neverocc<- subset(count_roosts, bffpropocc == 0)


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
#find how many unique roosts are sampled and occupied all 4 seasons in a year 

occ_allssn<- subset(camp_summaries_time,total_ssn_occ == 4 ) #this removes roosts that aren't checked in all seasons
n_distinct(occ_allssn$camp.name) #141
allocc_yr <- as.data.frame(occ_allssn %>%
                   group_by(year) %>%
                   dplyr::summarise(num_roosts_bffocc_allssn= n_distinct(camp.name)))
allocc_yr<- left_join(allocc_yr, yr_summary ,by = "year")


occ_3ssn<- subset(camp_summaries_time,total_ssn_occ == 3 ) #this removes roosts that aren't checked in all seasons
# t<- as.data.frame(unique(occ_3ssn$camp.name)) #162
# colnames(t)[1]<- "camp.name"
# t$occssn= "3"
# t2<- as.data.frame(unique(occ_allssn$camp.name)) #162
# colnames(t2)[1]<- "camp.name"
# t2$occssn= "4"
# f<- rbind(t,t2)

#find how many unique roosts are sampled and occupied at least 3 seasons in a year 
occ_grt2ssn<- subset(camp_summaries_time,total_ssn_occ > 2 ) #2,843 obs
n_distinct(occ_grt2ssn$camp.name) #195

grt2occ_yr <- as.data.frame(occ_grt2ssn %>%
                              group_by(year) %>%
                              dplyr::summarise(num_roosts_bffocc_grt2ssn= n_distinct(camp.name)))
grt2occ_yr<- left_join(grt2occ_yr, yr_summary ,by = "year")
grt2occ_yr$prop_roost_occ= grt2occ_yr$num_roosts_bffocc_grt2ssn/grt2occ_yr$num_unq_roosts


#find how many unique roosts are sampled and occupied at less than 2 seasons in a year 
occ_lss2ssn<- subset(camp_summaries_time,total_ssn_occ < 2 ) #2,920 obs
n_distinct(occ_lss2ssn$camp.name) #408

#find how many unique roosts are sampled and occupied 1 seasons in a year 
occ_onessn<- subset(camp_summaries_time,total_ssn_occ == 1 ) #1203 obs
one_ssnocc_yr <- as.data.frame(occ_onessn %>%
                              group_by(year) %>%
                              dplyr::summarise(num_roosts= n_distinct(camp.name)))

#find how many unique roosts are sampled and never occupied in a year 
nvr_allssn<- subset(camp_summaries_time,total_ssn_occ == 0 ) #1717 obs
nvrocc_yr <- as.data.frame(nvr_allssn %>%
                              group_by(year) %>%
                              dplyr::summarise(num_roosts= n_distinct(camp.name)))


# winter<- subset(camp_summaries_time, season == "winter") #1708
# 
# winter_grt2ssn<- subset(camp_summaries_time, total_ssn_occ > 2 & season == "winter")






ggplot()  +
  geom_line(data = grt2occ_yr, aes(x = year, y = num_roosts_bffocc_grt2ssn), group = "year", color = "black") +
  geom_line(data = allocc_yr, aes(x = year, y = num_roosts_bffocc_allssn), group = "year", color = "red") +
  #geom_line(data = nvrocc_yr, aes(x = year, y = num_roosts), group = "year", color = "black") +
  labs(x = "year", y= "Number of roosts") +
  scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 12)


################################
table(camp_summaries_time$time)

write.csv(occ_grt2ssn, "bffroosts_occgrtr2ssn_2007_2021.csv", row.names = F)
# 
# occ_all_ssns<- subset(cast,autumn_bffocc >0  & spring_bffocc >0 &summer_samp_times >0  & winter_samp_times>0 )
# alwysocc_all_ssns<- subset(occ_all_ssns, autumn_bffocc == 1  & spring_bffocc == 1 & summer_bffocc == 1  & winter_bffocc == 1 )
# nvrocc_all_ssns<- subset(cast, autumn_bffocc == 0  & spring_bffocc == 0 & summer_bffocc == 0  & winter_bffocc== 0 )
#rarly_occ_all_ssns<- subset(occ_all_ssns, autumn_bffocc < 0.5  & spring_bffocc < 0.5 & summer_bffocc < 0.5  & winter_bffocc< 0.5 )


alwyocc_8ssnsamp<- subset(occ_all_ssns, autumn_samp_times > 7  & spring_samp_times >7 & summer_samp_times > 7  & winter_samp_times > 7 )


yr_summary<- as.data.frame(camp_summaries_yr %>%
                             group_by(year) %>%
                             dplyr::summarise(num_unq_roosts = n(),
                                              total.sum.occ.bff = sum(sum.occ.bff, na.rm = T),
                                              total.sum.occ.ghff = sum(sum.occ.ghff, na.rm = T),
                                              total.sum.occ.lrff = sum(sum.occ.lrff, na.rm = T)))


#########################################################################################################
############# Select for roost sampled at least 3 or more seasons  #######################################



###summarize the number of occupancy 
count_roosts2<- as.data.frame(camp_summaries_time %>%
                             group_by(camp.name) %>%
                             dplyr::summarise(num_ssns_sampled = n(),
                             total.sum.occ.bff = sum(sum.occ.bff, na.rm = T),
                             sum.occ.chng = sum(prop_change2,  na.rm = T), 
                             sum.yr.chng = sum(yr_change, na.rm = T),
                             year_unq = n_distinct(year)))
count_roosts2$yr_diff<- count_roosts2$sum.yr.chng - count_roosts2$year_unq
