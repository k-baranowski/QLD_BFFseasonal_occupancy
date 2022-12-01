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

##by season, one row per camp 
camp_summaries_yr = as.data.frame(qld_recent %>%
                  group_by(camp.name, lat, long,  year) %>%
                  dplyr::summarise(sample.dates = n(),
                  sum.occ.bff = sum(bff.presence, na.rm = T), 
                  sum.occ.ghff = sum(ghff.presence, na.rm = T),
                  sum.occ.lrff = sum(lrff.presence, na.rm = T), 
                  sum.count.bff = sum(bff.count, na.rm = T),
                  sum.count.ghff = sum(ghff.count, na.rm = T), 
                  sum.count.lrff = sum(lrff.count, na.rm = T), 
                  min.count.bff = min(bff.count, na.rm = T), 
                  max.count.bff = max(bff.count, na.rm = T), 
                 mean.count.bff = as.integer(mean(bff.count, na.rm = T)),
                 ffpresence = max(ff.presence, na.rm = T)))

table(camp_summaries_yr$year)

yr_summary<- as.data.frame(camp_summaries_yr %>%
                             group_by(year) %>%
                             dplyr::summarise(num_unq_roosts = n(),
                                    total.samp.times = sum(sample.dates),
                                    total.sum.occ.bff = sum(sum.occ.bff, na.rm = T),
                                    total.sum.occ.ghff = sum(sum.occ.ghff, na.rm = T),
                                    total.sum.occ.lrff = sum(sum.occ.lrff, na.rm = T)))
yr_summary$total.occ.bff.prop= yr_summary$total.sum.occ.bff/yr_summary$total.samp.times


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
                                 ffpresence = max(ff.presence, na.rm = T)))


camp_summaries_time$pres_summr<- ifelse(camp_summaries_time$season == "summer" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_spr<- ifelse(camp_summaries_time$season == "spring" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_atmn<- ifelse(camp_summaries_time$season == "autumn" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$pres_winter<- ifelse(camp_summaries_time$season == "winter" & camp_summaries_time$sum.occ.bff > 0, 1, 0 )
camp_summaries_time$num_ssn_occ<- rowSums(camp_summaries_time[ , c(18:21)], na.rm=TRUE)
camp_summaries_time[,c(12,13,18:21)]<- NULL

camp_summaries_time<- camp_summaries_time %>%
                             group_by(camp.name, year) %>%
                             dplyr::mutate(total_ssn_occ= sum(num_ssn_occ))

table(camp_summaries_time$total_ssn_occ)
#ssns   0    1     2     3     4 
#obs   1776  1182  1282  1199  1644
 
camp_summaries_time$t_bffnotocc<- camp_summaries_time$sample.dates - camp_summaries_time$sum.occ.bff
camp_summaries_time$bffpropocc<-  camp_summaries_time$sum.occ.bff/camp_summaries_time$sample.dates

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

camp_summaries_time<- camp_summaries_time %>% 
  arrange( time) %>% 
  group_by(camp.name ) %>%
  dplyr::mutate(prop_change= bffpropocc- lag(bffpropocc)) %>% 
  as.data.frame()
camp_summaries_time$prop_change2<- ifelse(camp_summaries_time$prop_change == 0, 0, 1)

camp_summaries_time<- camp_summaries_time %>% 
  arrange( time) %>% 
  group_by(camp.name ) %>%
  dplyr::mutate(yr_change= year- lag(year)) %>% 
  as.data.frame()

###summarize the number of occupancy 
count_roosts<- as.data.frame(camp_summaries_time %>%
                               group_by(camp.name) %>%
                               dplyr::summarise(num_ssns_sampled = n(),
                                total.sum.occ.bff = sum(sum.occ.bff, na.rm = T),
                                 sum.occ.chng = sum(prop_change2,  na.rm = T), 
                                 sum.yr.chng = sum(yr_change, na.rm = T)))
ggplot(count_roosts)  +
  geom_point( aes(x = sum.occ.chng, y = num_ssns_sampled, color = total.sum.occ.bff), size = 4) +
  #geom_line( aes(x = time, y = total.occ.bff.prop), group = "year", color = "red") +
  labs(x = "Number of occupancy changes", y= "Number of seasons sampled", color = "Times BFF were present") +
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_bw(base_size = 12)



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
ggplot(camp_summaries_time, aes(x = time, y = reorder(x=camp.name,  lat) , fill =as.factor(prop_change2))) +
  geom_tile()  +
  labs(y = 'camp names', fill = "Number of occupancy changes") +
  scale_y_discrete(drop = FALSE, labels = levels(camp_summaries_time$camp.name)[c(T, rep(F, 1))],
                   breaks = levels(camp_summaries_time$camp.name)[c(T, rep(F, 1))])  +
  scale_x_discrete(drop = FALSE, labels = levels(camp_summaries_time$time)[c(T, rep(F, 1))],
                   breaks = levels(camp_summaries_time$time)[c(T, rep(F, 1))])  +
  theme_classic()+
  scale_fill_manual(values = c('grey73', 'red')) +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 10, angle = 80, hjust = 0.6, vjust = 0.5 )) 
  #scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) 
#theme_bw()

############## heat map of sampling and seasonal occupancy 
ggplot(time_sums, aes(x = year, y = reorder(x=camp.name, bffpropocc) , fill = prop_change2)) +
  geom_tile() +
  labs(fill = "Proportion occupancy changed from prior season")
  scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)) 
#theme_bw()

###########################
cast<- dcast(camp_summaries_time, camp.name~time, value.var = "bffpropocc" )


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


############################ do summary of all species occupation across time ########################
#summarize bff presence at roosts by year 
ff_allsummary_yr = as.data.frame(qld_recent %>%
                                   group_by(camp.name, lat, long, year) %>%
                                   dplyr::summarise(yrsample.dates = n(),
                                    all.occ.bff = sum(bff.presence, na.rm = T),
                                    all.occ.ghff = sum(ghff.presence, na.rm = T), 
                                    all.occ.lrff = sum(lrff.presence, na.rm = T)))

#find the num of sample times not occupied and the pct occupied by year 
ff_allsummary_yr$t_bff_notocc_yr<- (ff_allsummary_yr$yrsample.dates - ff_allsummary_yr$all.occ.bff) 
ff_allsummary_yr$pct_bff_occ_yr<- (ff_allsummarry_yr$all.occ.bff/ff_allsummary_yr$yrsample.dates) 

ff_allsummary_yr$t_ghff_notocc_yr<- (ff_allsummary_yr$yrsample.dates - ff_allsummary_yr$all.occ.ghff) 
ff_allsummary_yr$pct_ghff_occ_yr<- (ff_allsummary_yr$all.occ.ghff/ff_allsummary_yr$yrsample.dates) 

ff_allsummary_yr$t_lrff_notocc_yr<- (ff_allsummary_yr$yrsample.dates - ff_allsummary_yr$all.occ.lrff) 
ff_allsummary_yr$pct_lrff_occ_yr<- (ff_allsummary_yr$all.occ.lrff/ff_allsummary_yr$yrsample.dates) 


ff_allsummary<- as.data.frame(ff_allsummary_yr %>%
                                group_by(camp.name, lat, long) %>%
                                dplyr::summarise(num_yrs_allsamp = n(),
                                                 total_times_sampled = sum(yrsample.dates),
                                                 sum.occ.bff = sum(all.occ.bff, na.rm = T),
                                                 sum.notocc.bff = sum(t_bff_notocc_yr), 
                                                 sum.occ.ghff = sum(all.occ.ghff, na.rm = T),
                                                 sum.notocc.ghff = sum(t_ghff_notocc_yr),
                                                 sum.occ.lrff = sum(all.occ.lrff, na.rm = T),
                                                 sum.notocc.lrff = sum(t_lrff_notocc_yr, na.rm = T)))

#summarize bff presence at roosts by year 
# camp_summary_yr = as.data.frame(qld_recent %>%
#                    group_by(camp.name, lat, long, year) %>%
#                    dplyr::summarise(sample.dates = n(),
#                    occ.bff = sum(bff.presence, na.rm = T)))




#calculate across years occupancy percentages
ff_allsummary$bffpct_occ<- ff_allsummary$sum.occ.bff/(ff_allsummary$sum.occ.bff + ff_allsummary$sum.notocc.bff)
ff_allsummary$ghffpct_occ<- ff_allsummary$sum.occ.ghff/(ff_allsummary$sum.occ.ghff + ff_allsummary$sum.notocc.ghff)
ff_allsummary$lrffpct_occ<- ff_allsummary$sum.occ.lrff/(ff_allsummary$sum.occ.lrff + ff_allsummary$sum.notocc.lrff)

##so many digits 
ff_allsummary$bffpct_occ <- round(ff_allsummary$bffpct_occ, 2)
ff_allsummary$ghffpct_occ <- round(ff_allsummary$ghffpct_occ, 2)
ff_allsummary$lrffpct_occ <- round(ff_allsummary$lrffpct_occ, 2)

#########################################
######## pull out the roosts that were sampled at least 5 of these years 
camp_samp4yr<- subset(ff_allsummary, num_yrs_allsamp >=4)
camp_samp3yr<- subset(ff_allsummary, num_yrs_allsamp >=3)

#Check occupancy
hist(camp_samp4yr$bffpct_occ)
hist(camp_samp3yr$bffpct_occ)

ggplot(camp_samp3yr, aes(x =bffpct_occ)) + 
  geom_histogram(bins = 50) +
  theme_bw(base_size = 18) +
  xlab("Proportion of roost occupancy") + 
  ylab("Number of Roosts")


write.csv(ff_allsummary, "ffallsummary_allseasons_221116.csv", row.names = FALSE)

#identify the roosts that are always, never, and mediumly occupied by bff every time they are sampled 
roosts_always_occ<- subset(camp_samp3yr, camp_samp3yr$bffpct_occ == 1) 
roosts_never_occ<- subset(camp_samp3yr, camp_samp3yr$bffpct_occ == 0) 
roosts_more50pct_occ<- subset(camp_samp3yr, camp_samp3yr$bffpct_occ >= 0.5 & camp_samp3yr$bffpct_occ < 1) 
roosts_less50pct_occ<- subset(camp_samp3yr, camp_samp3yr$bffpct_occ <= 0.5) 

roosts_more50pct_occ<- subset(camp_samp4yr, camp_samp4yr$bffpct_occ >= 0.5 & camp_samp4yr$bffpct_occ < 1) 
roosts_less50pct_occ<- subset(camp_samp4yr, camp_samp4yr$bffpct_occ <= 0.5) 

roosts_more50pct_occ<- subset(ff_allsummary, ff_allsummary$bffpct_occ >= 0.5 & ff_allsummary$bffpct_occ < 1) 
roosts_less50pct_occ<- subset(ff_allsummary,ff_allsummary$bffpct_occ <= 0.5) 

roosts_inbtwn_occ<- subset(camp_samp3yr, camp_samp3yr$bffpct_occ >0 & camp_samp3yr$bffpct_occ < 1) 

# write.csv(roosts_more50pct_occ, "roosts_more50pct_bffocc_221115.csv", row.names = FALSE)
# write.csv(roosts_less50pct_occ, "roosts_less50pct_bffocc_221115.csv", row.names = FALSE)
write.csv(roosts_always_occ, "roosts_always_bffocc_221115.csv", row.names = FALSE)
write.csv(roosts_never_occ, "roosts_never_bffocc_221115.csv", row.names = FALSE)
write.csv(roosts_inbtwn_occ, "roosts_sometimes_bffocc_221115.csv", row.names = FALSE)



############################################################################
a<- ggplot(ff_allsummary, aes(x =bffpct_occ)) + 
  geom_histogram(bins = 50) +
  theme_bw(base_size = 18) +
  xlab("") + 
  ylab("Number of Roosts")

b<- ggplot(camp_samp3yr, aes(x =bffpct_occ)) + 
  geom_histogram(bins = 50) +
  theme_bw(base_size = 18) +
  xlab("") + 
  ylab("Number of Roosts")

c<- ggplot(camp_samp4yr, aes(x =bffpct_occ)) + 
  geom_histogram(bins = 50) +
  theme_bw(base_size = 18) +
  xlab("Proportion roost occupancy") + 
  ylab("Number of Roosts")


grid.arrange(a,b,c, ncol = 1)
