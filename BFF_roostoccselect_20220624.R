##########################################################
setwd(".")
# libraries
library(plyr)
library(dplyr) #manipulating dfs
library(ggplot2)
library(stringr)
library(scales)

####################################################################################################################
#importing data for QLD roost locations 
qld<- read.csv("cleaned_qld_ff_data_Apr03_Mar22_kb_20220705.csv", stringsAsFactors = F)

str(qld)
#CFs code introduce some NAs in bff, lrr, spff, & ghff presence and count columns for ~ 72 records, I believe 
# it's from her code handling duplicated surveys (i.e. 2 diff roost counts on same day) in NFFMP data 
qld<- qld %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
 
sum(is.na(qld$bff.count)) #check is 0

#specify date format and add factor year column
qld$date <- as.Date(qld$date, format = '%Y-%m-%d')

table(qld$year)

# 2003 2004 2005 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 
#   29   23   19  320  462  765  978 1130 1334 1423 1198  533  484  434  351  564  660  687  177

table(qld$season)
# autumn spring summer winter 
#   2663   3129   2988   2791 

#select only the subtropical observations, due to lack of info about diet in northern tropical range 
# and only sporadic bff surveys up there
qld_subtrop<- subset(qld, lat < -23.26)
#11,154 obs

#select when annual surveys began, (only eclude 417 records from 4ish seasons prior)
qld_recent<-subset(qld_subtrop, year > 2006) 
#11083 observations

#qld_recent$time<- paste(qld_recent$year,qld_recent$season, sep = "_" )
qld_recent$month<-format(qld_recent$date, "%m") 

###############################################################################
#pull out summer observations to get wrap around season name 
summer_roosts<- subset(qld_recent, season == "summer")

#link the 2nd year based on month 
summer_roosts$year2<- ifelse(summer_roosts$month == "01", summer_roosts$year - 1,
                     ifelse(summer_roosts$month == "02", summer_roosts$year - 1,
                     ifelse(summer_roosts$month == "12", (summer_roosts$year + 1),
                      NA)))
#paste the season together
summer_roosts$time<- ifelse(summer_roosts$month == "01", paste(summer_roosts$year2, summer_roosts$year, summer_roosts$season),
                       ifelse(summer_roosts$month == "02", paste(summer_roosts$year2, summer_roosts$year, summer_roosts$season),
                        ifelse(summer_roosts$month == "12", paste(summer_roosts$year, summer_roosts$year2, summer_roosts$season),
                        NA)))
#make month and time column (season, i.e. 2007 autumn)
summer_roosts$month<-format(summer_roosts$date, "%m") 
summer_roosts$time<- as.factor(summer_roosts$time)

#remove extra year column we don't need 
summer_roosts[,24]<- NULL

#pull out other seasons that don't cross over years and make time column 
other_roosts<- subset(qld_recent, season != "summer")

#paste year and season 
other_roosts$time<- as.factor(paste(other_roosts$year, other_roosts$season))

#paste summer with other seasons 
allroosts<- rbind(other_roosts, summer_roosts) #1500 obs, 24 columns 

#check for missing bff count data here
missing <- allroosts[is.na(allroosts$bff.count),]

###split out observations into the seasonal dfs 
# splits<- split(allroosts, allroosts$time, drop = T)
# 
# #write out csvs so I can turn csvs to points in arc for processing
# path = "~/Desktop/Research/BFF_roostoccupancy_20220510/data/R_outputs/allroosts_season/"
# for(i in 1:length(splits)){
#   write.csv(data.frame(splits[[i]]), file = paste0(path, names(splits)[i], '.csv'))
# }


####################################################################################################################
#summarize bff presence at roosts by year 
# camp_summaries_yr= as.data.frame(allroosts %>%
#                    group_by(camp.name, lat, long, year) %>%
#                    dplyr::summarise(totalsample.dates = n(),
#                    total.occ.bff = sum(bff.presence, na.rm = T), 
#                    total.count.bff = sum(bff.count, na.rm = T), 
#                    min.count.bff = min(bff.count, na.rm = T), 
#                    max.count.bff = max(bff.count, na.rm = T), 
#                    total.occ.ghff = sum(ghff.presence, na.rm = T), 
#                    total.count.ghff = sum(ghff.count, na.rm = T), 
#                    total.occ.lrff = sum(lrff.presence, na.rm = T), 
#                    total.count.lrff = sum(lrff.count, na.rm = T),
#                    ffpresent.max = max(ff.presence)))
# 
# hist(qld_recent$bff.count)

##by season, one row per camp 
camp_summaries_season= as.data.frame(allroosts %>%
                       group_by(camp.name, lat, long,  time) %>%
                        dplyr::summarise(seasonsample.dates = n(),
                        total.occ.bff = sum(bff.presence, na.rm = T), 
                        total.occ.ghff = sum(ghff.presence, na.rm = T),
                        total.occ.lrff = sum(lrff.presence, na.rm = T), 
                        total.count.bff = sum(bff.count, na.rm = T),
                        total.count.ghff = sum(ghff.count, na.rm = T), 
                        total.count.lrff = sum(lrff.count, na.rm = T), 
                        min.count.bff = min(bff.count, na.rm = T), 
                        max.count.bff = max(bff.count, na.rm = T), 
                        mean.count.bff = mean(bff.count, na.rm = T),
                        max.presence = max(ff.presence)))


###split out observations 
splits_ssn_sums<- split(camp_summaries_season, camp_summaries_season$time, drop = T)


# path3 = "~/Desktop/Research/BFF_roostoccupancy_20220510/data/R_outputs/subtrop_roost_season_sums/"
# for(i in 1:length(splits_ssn_sums)){
#   write.csv(data.frame(splits_ssn_sums[[i]]), file = paste0(path3, names(splits_ssn_sums)[i], '.csv'))
# }

#summmarize the previous dataframe to count # years sampled and how many sample times they were occupied 
ff_summary<- as.data.frame(camp_summaries_season %>%
                                      group_by(time) %>%
                                      dplyr::summarise(num_unq_roosts = n(),
                                      season.sum.occ.bff = sum(total.occ.bff, na.rm = T),
                                      season.sum.occ.ghff = sum(total.occ.ghff, na.rm = T),
                                      season.sum.occ.lrff = sum(total.occ.lrff, na.rm = T)))
                            
#################
### now output the csvs for roosts with any ff presence for the roost tessellating 
ffpresent_roosts<- subset(allroosts, ff.presence > 0) #7190 obs

presence_sums = as.data.frame(ffpresent_roosts %>%
                                group_by(time) %>%
                                dplyr::summarise(present_roosts = n_distinct(camp.name)))

ff_summary$present.roosts= presence_sums$present_roosts
ff_summary$proproost_occ = ff_summary$present.roosts/ff_summary$num_unq_roosts

#filter out 2022 autumn bc it is sparsely sampled, actually late surveys from the previous quarter/season 
ff_summary2<- subset(ff_summary, time != "2022 autumn")



#################
### now output the csvs for roosts with any ff presence for the roost tessellating 
bffpresent_roosts<- subset(allroosts, bff.presence > 0) #6670 obs

bffpresence_sums = as.data.frame(bffpresent_roosts %>%
                                group_by(time) %>%
                                dplyr::summarise(bffpresent_roosts = n_distinct(camp.name)))

ff_summary$bffpresent.roosts= bffpresence_sums$bffpresent_roosts
ff_summary$bffproproost_occ = ff_summary$bffpresent.roosts/ff_summary$num_unq_roosts

#filter out 2022 autumn bc it is sparsely sampled, actually late surveys from the previous quarter/season 
ff_summary2<- subset(ff_summary, time != "2022 autumn")
ff_summary2$absentroosts=  ff_summary2$num_unq_roosts - ff_summary2$present.roosts

###split out observations 
#splits_ffpresent<- split(ffpresent_roosts, ffpresent_roosts$time, drop = T)

#write out csvs so I can turn csvs to points in arc for tessellating but still need to weight foraging buff by roost size??!)
# path2 = "~/Desktop/Research/BFF_roostoccupancy_20220510/data/R_outputs/ffpresentroosts_subtrop_season/"
# for(i in 1:length(splits_ffpresent)){
#   write.csv(data.frame(splits_ffpresent[[i]]), file = paste0(path2, names(splits_ffpresent)[i], '.csv'))
# }




#summmarize the seasonal dataframe to have an average population size observed each season, using average
#beacause some roosts get sampled a lot (range 0-13) in one season, can't double count population 
bff_summary_mean<- as.data.frame(camp_summaries_season %>%
                             group_by(time) %>%
                             dplyr::summarise(num_unq_roosts = n(),
                             season.sum_meanpop.bff = sum(mean.count.bff, na.rm = T)))


levels2<- c("2006 2007 summer", "2007 autumn", "2007 winter", "2007 spring", 
            "2007 2008 summer", "2008 autumn", "2008 winter", "2008 spring", 
            "2008 2009 summer", "2009 autumn", "2009 winter", "2009 spring", 
            "2009 2010 summer","2010 autumn", "2010 winter", "2010 spring",
            "2010 2011 summer","2011 autumn", "2011 winter", "2011 spring",
            "2011 2012 summer","2012 autumn", "2012 winter", "2012 spring",
            "2012 2013 summer", "2013 autumn", "2013 winter", "2013 spring",
            "2013 2014 summer", "2014 autumn", "2014 winter", "2014 spring", 
            "2014 2015 summer", "2015 autumn", "2015 winter", "2015 spring", 
            "2015 2016 summer", "2016 autumn", "2016 winter", "2016 spring",  
            "2016 2017 summer", "2017 autumn", "2017 winter", "2017 spring", 
            "2017 2018 summer", "2018 autumn", "2018 winter", "2018 spring", 
            "2018 2019 summer", "2019 autumn", "2019 winter", "2019 spring", 
            "2019 2020 summer","2020 autumn", "2020 winter", "2020 spring",
            "2020 2021 summer","2021 autumn", "2021 winter", "2021 spring",
            "2021 2022 summer", "2022 autumn")

require(gdata)
ff_summary2$time <- reorder.factor(ff_summary2$time, new.order=levels2)
bff_summary_mean$time <- reorder.factor(bff_summary_mean$time, new.order=levels2)
bff_summary_mean2<- subset(bff_summary_mean, time != "2022 autumn")

########  ########  ########  ########  ########  ########  ########  ######## 
# ########  ######## #### DIAGNOSTIC PLOT ########  ########  ########  ######## 
########  ########  ########  ########  ########  ########  ########  ######## 

p<- ggplot(ff_summary2) +
  geom_line(aes(x = time, y = proproost_occ),size =1, color = "mediumorchid",linetype = "solid", group = 1) + 
  geom_line(aes(x = time, y = bffproproost_occ),size =1, color = "black",linetype = "solid", group = 1) +
  theme_bw(base_size = 15) +
  xlab("Time") +
  ylab("Proportion of roost sampled that were occupied by FF") +
  scale_x_discrete(drop = FALSE, labels = levels(bff_summary_mean2$time)[c(T, rep(F, 1))],
                   breaks = levels(bff_summary_mean2$time)[c(T, rep(F, 1))])  
p + theme(axis.text.x = element_text(size = 15, angle = 80, hjust = 0.6, vjust = 0.5))    


#####
p<- ggplot(ff_summary2) +
  geom_line(aes(x = time, y = present.roosts),size =1.5, color = "mediumorchid",linetype = "solid", group = 1) + 
  geom_line(aes(x = time, y = bffpresent.roosts),size =1.5, color = "black",linetype = "solid", group = 1) +
  geom_line(aes(x = time, y = num_unq_roosts),size =1.6, color = "coral3",linetype = "solid", group = 1) +
  theme_bw(base_size = 15) +
  xlab("") +
  ylab("Number of subtropical roosts sampled") +
  scale_y_continuous(labels = c("0","25", "50", "75","100", "125", "150", "175", "200"),
                            breaks =c(0,25, 50, 75, 100, 125, 150, 175, 200) )+
  scale_x_discrete(drop = FALSE, labels = levels(bff_summary_mean2$time)[c(T, rep(F, 1))],
                   breaks = levels(bff_summary_mean2$time)[c(T, rep(F, 1))])  
p + theme(axis.text.x = element_text(size = 15, angle = 80, hjust = 0.6, vjust = 0.5))   

p<- ggplot(ff_summary2) +
  geom_bar(aes(x = time, y = num_unq_roosts), stat = "identity", fill = "coral3") + 
  geom_bar(aes(x = time, y = present.roosts), stat = "identity",fill = "mediumorchid1") + 
  geom_bar(aes(x = time, y = bffpresent.roosts), stat = "identity", fill = "black") +
  theme_bw(base_size = 12) +
  xlab("") +
  ylab("Number of subtropical roosts with black flying foxes") +
  scale_y_continuous(labels = c("0","25", "50", "75","100", "125", "150", "175", "200"),
                     breaks =c(0,25, 50, 75, 100, 125, 150, 175, 200) )+
  scale_x_discrete(drop = FALSE, labels = levels(bff_summary_mean2$time)[c(T, rep(F, 1))],
                   breaks = levels(bff_summary_mean2$time)[c(T, rep(F, 1))])  
p + theme(axis.text.x = element_text(size = 13, angle = 80, hjust = 0.6, vjust = 0.5))


##########
p<- ggplot(ff_summary2) +
  #geom_line(aes(x = time, y = absentroosts),size =1.5, color = "red4",linetype = "solid", group = 1) + 
  geom_line(aes(x = time, y = bffproproost_occ),size =1.5, color = "black",  linetype = "solid", group = 1) + 
  #geom_bar(aes(x = time, y = absentroosts),fill = "red4", stat = "identity") +
  #geom_smooth(aes( x = time, y = absentroosts, group =1), color = "red4") +
  geom_smooth(aes( x = time, y = proproost_occ, group =1), color = "black") +
  theme_bw(base_size = 13) +
  xlab("") +
  ylab("Number subtropical roost sampled with black flying foxes present") +
  #scale_y_continuous(labels = c("0","25", "50", "75","100", "125", "150", "175", "200"),
                    #breaks =c(0,25, 50, 75, 100, 125, 150, 175, 200) )+
  scale_x_discrete(drop = FALSE, labels = levels(bff_summary_mean2$time)[c(T, rep(F, 1))],
                   breaks = levels(bff_summary_mean2$time)[c(T, rep(F, 1))])  
p + theme(axis.text.x = element_text(size = 13, angle = 80, hjust = 0.66, vjust = 0.6)) 

#####
coeff<- max(bff_summary_mean2$season.sum_meanpop.bff)/max(bff_summary_mean2$num_unq_roosts)

p<- ggplot(bff_summary_mean2) +
  geom_line(aes(x = time, y = season.sum_meanpop.bff),size =0.8, color = "black",linetype = "solid", group = 1) + 
  geom_smooth(aes(x = time, y = season.sum_meanpop.bff, size = 0.8), color = "black", method = lm) + 
  geom_line( aes(x = time, y = num_unq_roosts *coeff),size =0.8, color = "blue",linetype = "solid", group = 1 ) +
  #geom_smooth(method=lm, color = "red", aes(x = time, y = season.sum_meanpop.bff, group =1))+
  theme_bw(base_size = 12) +
  xlab("") +
  ylab("Mean Subtropical Population Size of Black Flying Foxes") +
  scale_x_discrete(drop = FALSE, labels = levels(bff_summary_mean2$time)[c(T, rep(F, 1))],
                   breaks = levels(bff_summary_mean2$time)[c(T, rep(F, 1))])  +
  scale_y_continuous(sec.axis = sec_axis( ~ ./coeff, name = "Number of Roosts Observed"), 
                     labels=formattable::comma(seq(0, 400000, by=100000), digits=0, big.mark=","),
                     breaks = seq(0, 400000, by = 100000)) 
p + theme(axis.text.x = element_text(size = 12, angle = 80, hjust = 0.6, vjust = 0.5),
          axis.text.y.right = element_text(color = "blue"),
          axis.title.y.right = element_text(color = "blue"))    


  
ggsave("propbffpresent_allroostsseasons_221129.png", plot = last_plot() , width=9,  height =5 , units = c("in"), dpi = 300)



  ############  ############  ############  ############  ############
  ############### making time series ###############################

ffsummary_dates = as.data.frame(allroosts %>%
                           group_by(date) %>%
                            dplyr::summarise(total.count.bff = sum(bff.count, na.rm = T),
                            total.count.ghff = sum(ghff.count, na.rm = T), 
                            total.count.lrff = sum(lrff.count, na.rm = T)))

bffsum_present<- subset(ffsummary_dates,total.count.bff > 0)

ggplot(bffsum_present ) +
  #geom_line(aes(x=date, y=total.count.ghff), group =1, color = 'blue', linetype = "dashed", size = 0.9) +
  geom_line(aes(x=date, y=total.count.bff), group =1, size = 0.5) +
  
  #geom_line(aes(x=date, y=total.count.lrff), group =1, color = 'red') +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 year")+
  labs(y = "Observed Count of Black Flying Foxes", x = "Date") +
  theme_bw(base_size = 16)

ts<- ts(ffpresent_roosts$bff.count)
plot(ts)


ggsave("subtroproost_bars_20220721.eps", plot = last_plot() , width=12,  height = 6, units = c("in"), dpi = 300)

############
hev<- read.csv("hevspillover_studyareachap4_aft2007.csv", header = T, stringsAsFactors = F)
 hev_small<- hev[,c(6,9,13:14)]
 hev_small$time<- c("2007 winter", "2008 winter", "2009 winter","2010 autumn", "2011 winter",
                                       "2011 winter","2011 winter","2011 winter","2011 winter","2011 winter",
                                        "2011 winter", "2011 spring", "2012 autumn", "2012 winter", "2013 winter", 
                                        "2014 autumn", "2014 winter", "2014 winter", "2017 autumn")
 hev_small$total.cases<- hev_small$cases_confirmed_horse_bq + hev_small$unresolved_incontact_horses_bq
 hev_small2<- as.data.frame(hev_small %>%
                               group_by(time) %>%
                               dplyr::summarise(total.horse.cases = sum(total.cases)))
 