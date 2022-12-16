##########################################################
setwd(".")
# libraries
library(plyr)
library(dplyr) #manipulating dfs
library(ggplot2)
library(stringr)
library(tidyr)
library(reshape2)
library(gridExtra)
library(lubridate)
library(gdata)
library(data.table)


##########################################################
#importing data for QLD roost locations 
qld<- read.csv("cleaned_qld_ff_data_Apr03_Mar22_kb_20221129.csv", header = T, stringsAsFactors = F)

#specify date format and add factor year column
qld$date <- as.Date(qld$date, format = '%Y-%m-%d')
qld$month <- format(qld$date, "%m") 

#pull out summer observations to get wrap around season 
summer_roosts<- subset(qld, season == "summer")

summer_roosts$year2<- ifelse(summer_roosts$month == "01", summer_roosts$year - 1,
                      ifelse(summer_roosts$month == "02", summer_roosts$year - 1,
                      ifelse(summer_roosts$month == "12", (summer_roosts$year + 1),
                      NA)))

summer_roosts$time<- ifelse(summer_roosts$month == "01", paste(summer_roosts$year2, summer_roosts$year, summer_roosts$season),
                     ifelse(summer_roosts$month == "02", paste(summer_roosts$year2, summer_roosts$year, summer_roosts$season),
                     ifelse(summer_roosts$month == "12", paste(summer_roosts$year, summer_roosts$year2, summer_roosts$season),
                     NA)))

summer_roosts$time<- as.factor(summer_roosts$time)
summer_roosts[23]<- NULL

#pull out other seasons that don't cross over years and make time column 
other_roosts<- subset(qld, season != "summer")
other_roosts$time<- as.factor(paste(other_roosts$year, other_roosts$season))

allroosts<- rbind(other_roosts, summer_roosts)
str(allroosts)


##########################################
# #only consider roosts after 2013
qld_recent<- subset(allroosts, allroosts$date > "2006-12-30" & date < "2022-03-01")  #11500

table(qld_recent$year)
# 2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021 2022
#  320   462   765   978  1130  1334  1423  1198   533   484   434   351   564   660   687  175

table(qld_recent$season)
# autumn spring summer winter 
# 2598    3128    2991   2790

#manually set time order, surveys span from 2007-04-20 to 2022-02-24
levels2<- c("2007 autumn", "2007 winter", "2007 spring", "2007 2008 summer", 
            "2008 autumn", "2008 winter", "2008 spring", "2008 2009 summer", 
            "2009 autumn", "2009 winter", "2009 spring", "2009 2010 summer",
            "2010 autumn", "2010 winter", "2010 spring", "2010 2011 summer",
            "2011 autumn", "2011 winter", "2011 spring", "2011 2012 summer",
            "2012 autumn", "2012 winter", "2012 spring", "2012 2013 summer", 
            "2013 autumn", "2013 winter", "2013 spring", "2013 2014 summer", 
            "2014 autumn", "2014 winter", "2014 spring", "2014 2015 summer",
            "2015 autumn", "2015 winter", "2015 spring", "2015 2016 summer",
            "2016 autumn", "2016 winter", "2016 spring", "2016 2017 summer",
            "2017 autumn", "2017 winter", "2017 spring", "2017 2018 summer", 
            "2018 autumn", "2018 winter", "2018 spring", "2018 2019 summer", 
            "2019 autumn", "2019 winter", "2019 spring", "2019 2020 summer",
            "2020 autumn", "2020 winter", "2020 spring", "2020 2021 summer",
            "2021 autumn", "2021 winter", "2021 spring", "2021 2022 summer")
qld_recent$time <- reorder.factor(qld_recent$time, new.order=levels2) #manually set time levels

##assign season numbers 
qld_recent<- qld_recent %>%
  arrange(time) %>% 
  mutate(ssn_num = rleid(season))

#calculate the change in time and change in occupancy 
qld_recent<- qld_recent %>% 
  arrange(time) %>% 
  group_by(camp.name) %>%
  dplyr::mutate(ssn_diff_lastsamp = ssn_num-lag(ssn_num),
                presence_change= bff.presence- lag(bff.presence)) %>% 
  as.data.frame()
qld_recent$pres_change2<- ifelse(qld_recent$presence_change != 0, 1, 0)

##fix the NA presence columns for bff
qld_recent$bff.presence <- qld_recent$bff.presence %>% replace_na(0)


############### summarise the change in occupancy by season
#calculate the change in time and change in occupancy 
qld_recent<- qld_recent %>% 
  arrange(time) %>% 
  group_by(camp.name, season) %>%
  dplyr::mutate(ssn_pres_change= bff.presence- lag(bff.presence)) %>% 
  as.data.frame()
qld_recent$ssn_pres_change2<- ifelse(qld_recent$ssn_pres_change!= 0, 1, 0)


##############################################################################################################
#################################################### TIME ###################################################################

camp_summaries_time = as.data.frame(qld_recent %>%
                       group_by(camp.name, lat, long, time, season, ssn_num) %>%
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
                                 sum.occ.chng = sum(pres_change2), 
                                 sum.ssn.chngs = sum(ssn_pres_change2),
                                 max.ssn.btwn.samp = max(ssn_diff_lastsamp))) 

#sum up the seasons occupied in one year



#make binary columns for seasonal occupancy
camp_summaries_time$pres_summr<- ifelse(camp_summaries_time$season == "summer" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_spr<- ifelse(camp_summaries_time$season == "spring" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_atmn<- ifelse(camp_summaries_time$season == "autumn" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_winter<- ifelse(camp_summaries_time$season == "winter" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )

#collapse rows to show binary seasonal presence
camp_summaries_time$num_ssn_occ<- rowSums(camp_summaries_time[ ,c(22:25)])

#make a dataframe with the roost and seasons and binary occupancy factors
binary_presencedf<- camp_summaries_time[,c(1:5, 22:26)]

##take out the binary season columns 
camp_summaries_time[,c(12,13,22:25)]<- NULL

#now do it and get the proportion occupancy in that season
camp_summaries_time$pres_summr<- ifelse(camp_summaries_time$season == "summer" , camp_summaries_time$sum.occ.bff, 0 )
camp_summaries_time$pres_spr<- ifelse(camp_summaries_time$season == "spring" , camp_summaries_time$sum.occ.bff, 0 )
camp_summaries_time$pres_atmn<- ifelse(camp_summaries_time$season == "autumn", camp_summaries_time$sum.occ.bff, 0 )
camp_summaries_time$pres_winter<- ifelse(camp_summaries_time$season == "winter" , camp_summaries_time$sum.occ.bff, 0 )
#make binary columns for seasonal occupancy
  


# #sum up the seasons occupied across all 60 
camp_summaries_time<- camp_summaries_time %>%
                      group_by(camp.name) %>%
                      dplyr::mutate(total_ssns_occ= sum(bffpresence))

 
#   
#find times they're not there and calc prop occupancy
#camp_summaries_time$bffprop.occ.visits<- camp_summaries_time$sum.occ.bff/camp_summaries_time$sample.dates



#cast<- dcast(camp_summaries_time, camp.name~time, value.var = "bffpropocc" )


############################################################################
#now calculate change in proportion from last year of same season 
# camp_summaries_time<- camp_summaries_time %>% 
#   arrange(time) %>% 
#   group_by(season) %>%
#   dplyr::mutate(prop_change_smssn= bffpropocc- lag(bffpropocc)) %>% 
#   as.data.frame()
# 
# camp_summaries_time$prop_chang_ssn<- ifelse(camp_summaries_time$prop_change_smssn== 0, 0, 1)


# hist(camp_summaries_time$ssn_diff_lastsamp, breaks = 60)
# table(camp_summaries_time$ssn_diff_lastsamp)


camp_summaries_time$time <- reorder.factor(camp_summaries_time$time, new.order=levels2) #manually set time levels


###############################################

###summarize the sampling by camp names
count_roosts <- as.data.frame(camp_summaries_time %>%
                              arrange(time) %>%
                               group_by(camp.name) %>%
                               dplyr::summarise(
                               num.ssns.occ = max(total_ssns_occ),
                               sum.occ.tchanges = sum(sum.occ.chng,  na.rm = T),
                               sum.occ.schanges = sum(sum.ssn.chngs,  na.rm = T),
                               num.ssns.sampled = n_distinct(time),
                               #ssn.prop.occ = num.ssns.occ/num.ssns.sampled,
                               season.span.surveys = last(ssn_num) - first(ssn_num) + 1,
                               first.ssn = first(time),
                               last.ssn = last(time),
                               num.surveys = sum(sample.dates),
                               total.sum.occ.bff = sum(sum.occ.bff, na.rm = T),
                               # max.ssn.occ= max(total_ssn_occ, na.rm = T),
                               # min.ssn.occ= min(total_ssn_occ, na.rm = T),
                               numsurv.occ.summer = sum(pres_summr),
                               numsurv.occ.spring = sum(pres_spr),
                               numsurv.occ.autumn = sum(pres_atmn),
                               numsurv.occ.winter = sum(pres_winter)))




count_roosts$prop.srvy.span= count_roosts$num.ssns.sampled/count_roosts$season.span.surveys
count_roosts$bffprop.occ.survs<-  count_roosts$total.sum.occ.bff/count_roosts$num.surveys

###################################################################################################
###################################################################################################
########################## quantify how many times 

ssn_freq_long<- as.data.frame(camp_summaries_time %>%
                             group_by(camp.name) %>%
                             count(season))


colnames(ssn_freq_long)[2]<- "season"
colnames(ssn_freq_long)[3]<- "ssns_surveyed"

#manually set time order
levels3<- c("summer", "autumn", "winter", "spring")
ssn_freq_long$season <- reorder.factor(ssn_freq_long$season, new.order=levels3)


ssn_freq_short<- reshape2::dcast(ssn_freq_long, camp.name~season, value.var = "ssns_surveyed" )
colnames(ssn_freq_short) = c("camp.name", "num_summers_visited", "num_autumns_visited", "num_winters_visited", "num_springs_visited")

######################
#now summarize it by sampling dates
count_freq_long<- as.data.frame(camp_summaries_time %>%
                             group_by(camp.name, season) %>%
                             dplyr::summarise(
                             surveys= sum(sample.dates)))


count_freq_long$season <- reorder.factor(count_freq_long$season, new.order=levels3)
count_freq_short<- reshape2::dcast(count_freq_long, camp.name~season, value.var = "surveys" )
colnames(count_freq_short) = c("camp.name", "total_summer_surveys", "total_autumn_surveys", "total_winter_surveys", "total_spring_surveys")


frequencies_long<- left_join(ssn_freq_long,count_freq_long, by = c('camp.name', "season"))
frequencies_short<- left_join(ssn_freq_short,count_freq_short, by = 'camp.name')
ssn_roosts<- left_join(count_roosts, frequencies, by = 'camp.name')

#calculate the seasonal proportion of occupancy based on number of surveys
ssn_roosts$prop.summer.surv.occ= ssn_roosts$numsurv.occ.summer/ssn_roosts$total_summer_surveys
ssn_roosts$prop.spring.surv.occ<- ssn_roosts$numsurv.occ.spring/ssn_roosts$total_spring_surveys
ssn_roosts$prop.autumn.surv.occ<- ssn_roosts$numsurv.occ.autumn/ssn_roosts$total_autumn_surveys
ssn_roosts$prop.winter.surv.occ<- ssn_roosts$numsurv.occ.winter/ssn_roosts$total_winter_surveys

##### calculate number of annual seasons occupied
ssn_roosts$numssns_occ<- rowSums(ssn_roosts[c("numsurv.occ.summer", "numsurv.occ.autumn", "numsurv.occ.winter", "numsurv.occ.spring")] > 0)

#calculate the seasonal proportion of occupancy based on number of surveys
ssn_roosts$prop.ssns.occ<- ssn_roosts$num.ssns.occ/ssn_roosts$num.ssns.sampled


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



#find roosts sampled at least 10 seasons in 
ssn_roosts_10ssnocc<- subset(ssn_roosts, prop.ssns.occ <= 0.15 & num.ssns.sampled >= 11)

              
quantile(ssn_roosts_10ssn$prop.srvy.span, probs = seq(0, 1, 1/4)) #change to 1/10
#     70%         75%       80%       90%       
# 0.7964846  0.8181818  0.8508511   0.9130435 

lines = c(0.796, 0.851, 0.913)
ggplot(ssn_roosts_10ssn) +
  geom_histogram(aes(prop.srvy.span), bins = 80) +
  geom_vline(xintercept =  lines, linetype="dotted", 
             color = "tomato2", size=1.5) +
  labs(x = "Proportion the roost was surveyed within the duration of sampling at roost", y = "Number of Roosts") +
  theme_bw(base_size = 12)


quantile(ssn_roosts_10ssn$bffpropocc, probs = seq(0, 1, 1/10))
#     70%        80%        90%       
# 0.77777778  0.87363636   0.94183007

lines = c(0.777, 0.874, 0.942)
ggplot(ssn_roosts_10ssn) +
  geom_histogram(aes(bffpropocc), bins = 100) +
  geom_vline(xintercept =  lines, linetype="dotted", 
             color = "tomato2", size=1.5) +
  labs(x = "Proportion of BFF occupancy", y = "Number of Roosts") +
  theme_bw(base_size = 12)


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




###bff prop occ now 

quantile(ssn_roosts_10ssn_surv75$bffpropocc, probs = seq(0, 1, 1/10)) #also change this to 1/10
#     70%        75%        80%          90%       
# 0.7680463  0.8076923   0.8842105   0.9528123

lines = c(0.768, 0.884, 0.953)

ggplot(ssn_roosts_10ssn_surv75) +
  geom_histogram(aes(bffpropocc), bins = 100) +
  geom_vline(xintercept =  lines, linetype="dotted", 
             color = "tomato2", size=1.5) +
  labs(x = "Proportion of BFF occupancy", y = "Number of Roosts") +
  theme_bw(base_size = 12)


quantile(ssn_roosts_10ssn_surv75$propchng.persurv, probs = seq(0, 1, 1/4)) #also change this to 1/10
#      0%       25%       50%       75%      100% 
# 0.0000000 0.1038961 0.2054795 0.2873563 0.4545455 

##play with cutoff criteria 
highocc_lowchng<- subset(ssn_roosts_10ssn_surv75, bffpropocc >= 0.88 & sum.occ.chng < 5) #11
lowocc_lowchng<- subset(ssn_roosts_10ssn_surv75, bffpropocc <= 0.20 & sum.occ.chng < 5) #11
someocc<- subset(ssn_roosts_10ssn_surv75, bffpropocc > 0.20 & bffpropocc < 0.88 ) #55

lowocc<- subset(ssn_roosts_10ssn_surv75, bffpropocc <= 0.20) #16


##test tuesday
ggplot(ssn_roosts_10ssn_surv75)  +
  geom_point( aes(x = prop.srvy.span, y = bffpropocc, color = sum.occ.chng), size=3) +
  #geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  labs(x = "Proportion the roost was surveyed within the duration of sampling at roost", y= "Proportion BFF Occupancy all Surveys", color = "Number of occupancy changes") +
  scale_color_gradient(high = "red", low = "grey64") +
  geom_hline(yintercept = 0.88) +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 13) + 
  theme(legend.position = "bottom")

lowchng<- subset(ssn_roosts_10ssn_surv75, sum.occ.chng < 5) #24
ggplot(lowchng)  +
  geom_point( aes(x = prop.srvy.span, y = bffpropocc, color = sum.occ.chng), size=3) +
  #geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  labs(x = "Proportion the roost was surveyed within the duration of sampling at roost", y= "Proportion BFF Occupancy all Surveys", color = "Number of occupancy changes") +
  scale_color_gradient(high = "grey23", low = "grey64") +
  geom_hline(yintercept = 0.88) +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 13) + 
  theme(legend.position = "bottom")





##this one!! 
ggplot(ssn_roosts_10ssn_surv75)  +
  geom_point( aes(x = num.ssns.sampled, y = bffpropocc, color = sum.occ.chng, shape = as.factor(numssns_occ)), size = 5) +
  #geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  labs(x = "Number of seasons sampled", y= "Proportion BFF Occupancy all Surveys", color = "Number of occupancy changes", shape = "Number of seasons occupied") +
  scale_color_gradient(high = "red", low = "grey64") +
  scale_shape_manual(values=c(1,16, 17, 18, 15, 8)) +
  geom_hline(yintercept = c(0.2, 0.88)) +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 13) + 
  theme(legend.position = "bottom")


#need to do these above steps all again going by season  START HERE 1!! 

## seasonal stack of occupancy grid arrange 
a<- ggplot(ssn_roosts_10ssn_surv75)  +
  geom_point( aes(x = num.surveys, y = bffpropocc, color = summer_occ), size = 3) +
  #geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  labs(x = "Number of surveys", y= "Proportion BFF Occupancy", color = "BFF summer occupancy", shape = "Number of seasons occupied") +
  scale_color_gradient(high = "tomato3", low = "grey64") +
  geom_hline(yintercept = c(0.2, 0.88)) +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 11) + 
  theme(legend.position = "right")
b<- ggplot(ssn_roosts_10ssn_surv75)  +
  geom_point( aes(x =num.surveys, y = bffpropocc, color = autumn_occ), size = 3) +
  #geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  labs(x = "Number of surveys", y= "Proportion BFF Occupancy", color = "BFF autumn occupancy", shape = "Number of seasons occupied") +
  scale_color_gradient(high = "orange", low = "grey64") +
  geom_hline(yintercept = c(0.2, 0.88)) +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 11) + 
  #expand_limits(x=30) +
  theme(legend.position = "right")
c<- ggplot(ssn_roosts_10ssn_surv75)  +
  geom_point( aes(x = num.surveys, y = bffpropocc, color = winter_occ), size = 3) +
  #geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  labs(x = "Number of surveys", y= "Proportion BFF Occupancy", color = "BFF winter occupancy", shape = "Number of seasons occupied") +
  scale_color_gradient(high = "blue", low = "grey64") +
  geom_hline(yintercept = c(0.2, 0.88)) +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 11) + 
  #expand_limits(x=30) +
  theme(legend.position = "right")
d<- ggplot(ssn_roosts_10ssn_surv75)  +
  geom_point( aes(x = num.surveys, y = bffpropocc, color = spring_occ), size = 3) +
  #geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  labs(x = "Number of surveys", y= "Proportion BFF Occupancy", color = "BFF spring occupancy", shape = "Number of seasons occupied") +
  scale_color_gradient(high = "forestgreen", low = "grey64") +
  geom_hline(yintercept = c(0.2, 0.88)) +
  #expand_limits(x=30) +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 11) + 
  theme(legend.position = "right")

require(gridExtra)
grid.arrange(a,b,c,d, ncol=1 )


highocc_freq<- merge(ssn_freq, highocc_lowchng, by = "camp.name", all.y =  T)
lowocc_freq<- merge(ssn_freq, lowocc_lowchng, by = "camp.name", all.y =  T)
#### not this one.. this is crazy 
p<- ggplot(highocc_freq)  +
  geom_bar( aes(x = camp.name, y = freq, fill= season), stat = "identity", position = "dodge", width = 0.8) +
  #geom_line( aes(x = camp.name, y = freq, fill= season), group = "camp.name") +
  labs(x = "camp.name", y= "Number of Seasonal Surveys") +
  scale_fill_manual(values = c( "red", "orange", "blue", "forestgreen")) +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 13) + 
  #facet_wrap(~season, ncol =1)
  theme(legend.position = "bottom")


p+ theme(axis.text.x = element_text(angle = 55, vjust =0.90,  hjust=0.90 ),
         legend.key.size = unit(1, 'cm'))   
#@##################
someocc_freq<- merge(ssn_freq, someocc, by = "camp.name", all.y =  T)

p<- ggplot(someocc_freq)  +
  geom_bar( aes(x = reorder(camp.name, bffpropocc) , y = freq, fill= season), stat = "identity", position = "dodge", width = 0.8) +
  #geom_line( aes(x = camp.name, y = freq, fill= season), group = "camp.name") +
  labs(x = "camp.name", y= "Seasonal Surveys with BFF Present") +
  scale_fill_manual(values = c( "red", "orange", "blue", "forestgreen")) +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 13) + 
  #facet_wrap(~season, ncol =1)
  theme(legend.position = "bottom")
p+ theme(axis.text.x = element_text(angle = 55, vjust =0.90,  hjust=0.90 ),
         legend.key.size = unit(1, 'cm')) 


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
