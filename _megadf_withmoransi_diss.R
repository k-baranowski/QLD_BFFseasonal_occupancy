setwd(".")

library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)

#         JUMP IN POINT DOWN BELOW 

##read in the dataframe that has all other environmental variables, with each spillover clock set to 0, 24658 rows, 213 variables
#all_hev2<- read.csv("all_hev2_megadf_everything2_spillclock_20231205.csv", stringsAsFactors = F)

##read in the dataframe that has all other environmental variables, with all months from 2007-2020, total 913506 rows, 213 variables
all_hev2<- read.csv("all2_megadf_withimportant_20231211.csv", stringsAsFactors = F)

window<- read.csv("camp_monthly_rawvals_3mo_movw_preciptemps20002020_20231115.csv", stringsAsFactors = F)
window2<- window[c(1,5,10:13)]


all_hev3<- merge(all_hev2, window2, by = c("camp_name", "yrmon"))
##read in moran's i values
alldiet<- read.csv("alldietuppr8m_moransi_stylB_results_20231205.csv", stringsAsFactors = F)
alldiet[c(5:7)]<- NULL
alldiet<- subset(alldiet, year != 2000)
colnames(alldiet) = c("camp", "year", "morI_alldiettdis", "pval_alldietdis")

allwint<- read.csv("allwintdietuprr8m_moransi_stylB_results_20231121.csv", stringsAsFactors = F)
allwint[c(5:6)]<- NULL
allwint<- subset(allwint, year != 2000)
colnames(allwint) = c("camp", "year", "morI_allwinttdis", "pval_allwintdis")


atypcl<- read.csv("atypclwintuppr8m_moransi_stylB_fixdresults_20231210.csv", stringsAsFactors = F)
atypcl<- subset(atypcl, year != 2000)
colnames(atypcl) = c("camp", "year", "morI_atypclwintdis", "pval_atypclwintdis")

typcl<- read.csv("typclwintuppr8m_moransi_stylB_resultsfixd_20231210.csv", stringsAsFactors = F)
typcl<- typcl[c(1:4)]
typcl<- subset(typcl, year != 2000)
colnames(typcl) = c("camp", "year", "morI_typclwintdis", "pval_typclwintdis")

all<- merge(alldiet, allwint, by = c("camp", "year"), all.x = T )
typs<-  merge(typcl, atypcl, by = c("camp", "year"), all.x = T)

alls<- merge(all,typs, by = c("camp", "year"), all.x = T)

alls<- alls %>%
  dplyr::group_by(camp) %>%
  fill( morI_atypclwintdis, morI_typclwintdis, pval_typclwintdis, pval_atypclwintdis,  .direction = "downup") %>%
  dplyr::ungroup()

mega<- merge(all_hev3, alls, by = c("camp", "year"), all.x = T)




write.csv(mega, "mega2_finaldf_envbatsmorI_20231211.csv", row.names = F)

###############################################################

                         ## JUMP IN HERE 
mega<- read.csv("mega_finaldf_spillclock_envbatsmorI_20231205.csv", stringsAsFactors = F)

mega<- read.csv("mega_finaldf_envbatsmorI_20231205.csv", stringsAsFactors = F)

mega %>%
  filter( dist_to_spill_km < 100 ) %>%
  ggplot() +
  geom_smooth( aes(x = yrmon, y = mean_csum_precip_anom_20kbuff, color= control, group = control )) +
  #  geom_point(aes(x = distance_bin , y =value, color= latbin)) +
  # geom_hline(yintercept=1, color = "black", linetype = "dashed")+
  labs(x ='Distance to Spillover (Km) ', y ='Mean Cumulative Precipitation Anomaly in 20km Roost Buffers',  color = "Latitude bin of roost") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  #scale_color_manual(values = c("grey57", "red"  )) +
  # ylim(-3,3) +
  facet_wrap(~spillover, ncol = 8, scales = "free_x")+
  theme_bw(base_size = 10) +
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 65, vjust =1, hjust =1 ))


mega %>%
  #filter( dist_to_spill_km < 100 ) %>%
  ggplot() +
  geom_point(aes(x = camp_name , y =morI_allwint ), color = "darkblue") +
  geom_point(aes(x = camp_name , y =morI_alldiet ), color = "red") +
  geom_point(aes(x = camp_name , y =morI_typclwintdis ), color = "dodgerblue3") +
  geom_point(aes(x = camp_name , y =morI_atypclwintdis ), color = "turquoise") +
  labs(x ='Camp ', y ='Morans I value',  color = "Diet Type") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  #scale_color_manual(values = c("grey57", "red"  )) +
  # ylim(-3,3) +
 # facet_wrap(~yearm, ncol = 1)+
  theme_bw(base_size = 10) +
  theme(legend.position  = "bottom", axis.text.x = element_text(size = 4,angle = 85, vjust =1, hjust =1 ))

#### pull out data where the NFFMP surveys were recorded
surveys<- mega[!is.na(mega$bff.presence),]

surveys %>%
  filter( year == 2019 ) %>%
  ggplot() +
  geom_point(aes(x = prop_20kbuff_yrobs , y =morI_allwint ), color = "darkblue") +
  geom_point(aes(x = prop_20kbuff_yrobs  , y =morI_alldiet ), color = "red") +
  geom_point(aes(x = prop_20kbuff_yrobs  , y =morI_typclwint ), color = "dodgerblue3") +
  geom_point(aes(x =prop_20kbuff_yrobs  , y =morI_atypclwint ), color = "turquoise") +
  labs(x ='Proportion of Winter Habitat in 20km Buffer  ', y ='Morans I value',  color = "Diet Type") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  #scale_color_manual(values = c("grey57", "red"  )) +
   ylim(-1,-0.2) +
  # facet_wrap(~yearm, ncol = 1)+
  theme_bw(base_size = 12) +
  theme(legend.position  = "bottom", axis.text.x = element_text(size = 12,angle = 0, vjust =1, hjust =1 ))


onezero<- subset(surveys, bff.presence != 0.5)

onezero %>%
  arrange(bff.presence)%>%
 # filter(month_num2 == 6  | month_num2 == 7 | month_num2 == 8  ) %>%
  ggplot() +
# geom_point(aes(x = prop_20kbuff_yrobs , y =morI_allwint, color = as.factor(bff.presence), size = as.factor(bff.presence) )) +
  geom_point(aes(x = prop_20kbuff_yrobs  , y =morI_alldiet, color = as.factor(bff.presence), size = as.factor(bff.presence)  )) +
 #geom_point(aes(x = prop_20kbuff_yrobs  , y =morI_typclwintdis,color = as.factor(bff.presence), size = as.factor(bff.presence)  )) +
 # geom_point(aes(x =prop_20kbuff_yrobs  , y =morI_atypclwint, color = as.factor(bff.presence), size = as.factor(bff.presence) )) +
  #geom_point(aes(x =prop_20kbuff_yrobs  , y =morI_atypclwintdis,color = as.factor(bff.presence), size = as.factor(bff.presence)  )) +
  scale_size_manual(values= c(0.8, 1.4)) +
  labs(x ='Proportion of Winter Habitat in 20km Buffer', y ='Morans I value All  Diet Joined Trees',  color = "BFF Presence") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  scale_color_manual(values = c("grey57", "red"  )) +
   ylim(-1,-0.1) +
  facet_wrap(~year, ncol = 5)+
  theme_bw(base_size = 10) +
  theme(legend.position  = "none", axis.text.x = element_text(size = 10,angle = 0, vjust =1, hjust =1 ))



#wtf<- onezero[is.na(onezero$morI_atypclwint),]



##one on each axis 
mega %>%
   arrange(spill20km) %>%
  filter( dist_to_spill_km < 200 ) %>%
  ggplot() +
   geom_point(aes(x = morI_atypclwintdis, y =morI_typclwintdis,  color= as.factor(spill20km))) +
  geom_abline(slope = 1, intercept = -1) +
  labs(x ='Morans I value atypcal winter diet ', y ='Morans I value typcal winter diet',  color = "Spillover in 20km") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  scale_color_manual(values = c("grey57", "red"  )) +
   ylim(-1,-0.2) +
  xlim(-1,-0.2) +
  facet_wrap(~spillover, ncol = 8)+
  theme_bw(base_size = 10) +
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 65, vjust =1, hjust =1 ))

surveys$month<- factor(surveys$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
surveys %>%
  arrange(yrmon) %>%
  filter( dist_to_spill_km < 200  & bff.presence != 0.5) %>%
  ggplot() +
  # geom_jitter(aes(x = month,  y =morI_alldiet,color = as.factor(bff.presence)), size = 1, alpha = 0.25) +
  # geom_boxplot(aes(x = month,  y =morI_alldiet,color = as.factor(bff.presence)),notch = T, outlier.shape = NULL ) +
 # geom_jitter(aes(x = month,  y =morI_atypclwint,color = as.factor(bff.presence)), size = 1, alpha = 0.25) +
  geom_boxplot(aes(x = month,  y =morI_atypclwintdis,color = as.factor(bff.presence)),notch = T, outlier.shape = NULL ) +
  # geom_jitter(aes(x = month,  y =morI_typclwint,color = as.factor(bff.presence)), size = 1, alpha = 0.25) +
 # geom_boxplot(aes(x = month,  y =morI_typclwint,color = as.factor(bff.presence)),notch = T, outlier.shape = NULL ) +
  # geom_jitter(aes(x = month,  y =morI_allwint,color = as.factor(bff.presence)), size = 1, alpha = 0.25) +
 # geom_boxplot(aes(x = month,  y =morI_allwint,color = as.factor(bff.presence)),notch = T, outlier.shape = NULL ) +
  
  
  labs(x ='Months ', y ='Morans I value All Winter Diet Species 20km buff',  color = "BFF Presence") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  scale_color_manual(values = c("grey57", "red"  )) +
  ylim(-1,0) +
  facet_wrap(~year, ncol = 2)+
  theme_bw(base_size = 9) +
  theme(legend.position  = "none", axis.text.x = element_text(angle = 75, vjust =1, hjust =1 ))


write.csv(surveys, "megadf_surveysonly_formodel_20231210.csv", row.names = F)
write.csv(onezero, "megadf_onezero_only_formodel_20231210.csv", row.names = F)
#################### LOOK AT WINTER ######################## 

#wint<- subset(surveys, season == 'winter') #this is so wrong, figure out wtf is this column 
winter<-  subset(surveys, month == "June" | month == "July"|month == "August")
wint$month<- droplevels(wint$month)
table(wint$month)
table(winter$month)


winter %>%
  arrange(yrmon) %>%
  filter( dist_to_spill_km < 200  & bff.presence != 0.5) %>%
  ggplot() +
  
  geom_jitter(aes(x = month,  y =morI_allwint,color = as.factor(bff.presence)), size = 1, alpha = 0.75) +
  labs(x ='Winter Months ', y ='Morans I value All winter diet',  color = "BFF Presence") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  scale_color_manual(values = c("grey57", "red"  )) +
  ylim(-1,0.2) +
  facet_wrap(~year, ncol = 7)+
  theme_bw(base_size = 12) +
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 65, vjust =1, hjust =1 ))

table(wint$month)


###################### do this step
mega<- mega[c(3,4,2,5:221)]

############raw values 
spillall_precipmini_raw<- mega[,c(1:4, 13:24,   113,140:144,149:151,153,154,213,215,217,219)]
spillall_tempmini_raw<- mega[,c(1:4, 25:60,     113,140:144,149:151,153,154,213,215,217,219)]


##melt the raw dataframes 
spillall_prcp_melt_raw<- reshape2::melt(spillall_precipmini_raw, id = c("camp_name", "year", "yrmon", "month_num2" ,"lat_roost","bff.presence",  "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff",  "dist_to_spill_km", "spillover", "spillover_yrmon", "control",  "latbin_q","morI_alldiet","morI_allwint","morI_atypclwint","morI_typclwint" ))
spillall_temp_melt_raw<- reshape2::melt(spillall_tempmini_raw, id = c("camp_name",  "year", "yrmon", "month_num2" , "lat_roost", "bff.presence",  "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" , "dist_to_spill_km", "spillover", "spillover_yrmon", "control",  "latbin_q","morI_alldiet","morI_allwint","morI_atypclwint","morI_typclwint" ))



############anomalies values 
spillall_precipmini_anom<- mega[,c(1:4, 67:78,   113,140:144,149:151,153,154,213,215,217,219)]
spillall_tempmini_anom<- mega[,c(1:4, 79:102,   113,140:144,149:151,153,154,213,215,217,219)]


##melt the raw dataframes 
spillall_prcp_melt_anom<- reshape2::melt(spillall_precipmini_anom, id = c("camp_name",  "year",  "yrmon", "month_num2" ,"lat_roost","bff.presence",  "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff", "dist_to_spill_km", "spillover", "spillover_yrmon", "control",  "latbin_q","morI_alldiet","morI_allwint","morI_atypclwint","morI_typclwint" ))
spillall_temp_melt_anom<- reshape2::melt(spillall_tempmini_anom, id = c("camp_name",  "year",  "yrmon", "month_num2" ,"lat_roost", "bff.presence",  "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" , "dist_to_spill_km", "spillover", "spillover_yrmon", "control",  "latbin_q","morI_alldiet","morI_allwint","morI_atypclwint","morI_typclwint" ))




#keep all the rows with the change between months 
spillall_precipmini_change <- mega[,c(1,2,3,5,114, 116,140:143, 150:152, 154:170,213,214,216,218,220 )]           
spillall_tempmini_change<-   mega[,c(1,2,3,5,114, 116,14140:143, 150:152, 154:156, 171:214,216,218,220)]

spillall_prcp_melt_change<- reshape2::melt(spillall_precipmini_change, id = c("camp_name", "year", "yrmon","bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff", "latbin", "dist_to_spill_km", "spillover", "spillover_yrmon", "control", "distance_bin" , "latbin_q", "spill20km", "morI_alldiet","morI_allwint","morI_atypclwint","morI_typclwint" ))
spillall_temp_melt_change<- reshape2::melt(spillall_tempmini_change, id = c("camp_name","year", "yrmon",  "bff.presence", "season", "pres","prop_20kbuff_yrobs","spillover_name", "spill20kmbuff" , "latbin" ,"dist_to_spill_km", "spillover", "spillover_yrmon", "control", "distance_bin", "latbin_q", "spill20km","morI_alldiet","morI_allwint","morI_atypclwint","morI_typclwint"   ))
str(spillall_prcp_melt_change)
#change character value to numeric for plotting
spillall_prcp_melt_change$value2<-as.numeric(spillall_prcp_melt_change$value)
t<-spillall_prcp_melt_change[is.na(spillall_prcp_melt_change$value2),]
#make this a yearmon object so we can order by time
spillall_precipmini_change$yrmon2<- zoo::as.yearmon(spillall_precipmini_change$yrmon, "%Y_%m")
#set levels of lagged difference so it doesn't plot out of order 
spillall_precipmini_change$time_maxprcp_chng <- factor(spillall_precipmini_change$time_maxprcp_chng, levels= c("precip_diff0_1mo","precip_diff1_2mo", "precip_diff2_3mo","precip_diff3_4mo","precip_diff4_5mo","precip_diff5_6mo",  "precip_diff6_7mo","precip_diff7_8mo","precip_diff8_9mo","precip_diff9_10mo","precip_diff10_11mo","precip_diff11_12mo"))



###### plots 

length(unique(spillall_prcp_melt_anom$camp_name))

spillall_prcp_melt_anom %>%
  arrange(spill20kmbuff)%>%
  filter( dist_to_spill_km < 100 &
            bff.presence != 0.5 &
     variable != "mean_monthlytemp_20kmbuff" 
    & variable != "mean_mintemp_anom_20kbuff"  ) %>%
  ggplot() +
  #geom_boxplot( aes(x=morI_atypclwint, y =value , color =as.factor(spill20kmbuff) ), notch = F) +
  #geom_jitter(aes(x = bff.presence, y =value, color= morI_alldiet ), size =1.8) +
  geom_jitter( aes( x=morI_atypclwint, y =value , color =as.factor(spill20kmbuff) )) +
  
 # geom_hline(yintercept=1, color = "black", linetype = "dashed")+
  labs(x ='MoransI Value Atypical Winter 20km Buffer ', y ='Mean Cumulative Precipitation Anomaly in 20km Roost Buffers',color = "BFF Presence") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  scale_color_manual(values = c("grey57", "red"  )) +
  theme_bw(base_size = 10) +
  # ylim(-3,3) +
  facet_wrap(~variable, ncol = 6)+
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 65, vjust =1, hjust =1 ))

#ggsave("spill_precip_anomaly_melt_lag_distbinlatbin_2.png", plot = last_plot(), width=12,  height = 8, units = c("in"), dpi = 300)


spillall_temp_melt_anom %>%
  # arrange(dist) %>%
  filter( dist_to_spill_km < 100 &
            bff.presence != 0.5 &
            variable != "mean_csum_precip_anom_20kbuff" & variable != "mean_maxtemp_anom_20kbuff" & variable != "mean_monthlytemp_20kmbuff" 
          & variable != "mean_mintemp_anom_20kbuff"  ) %>%
  ggplot() +
 # geom_boxplot( aes( x=morI_alldiet, y =value , color =as.factor(bff.presence) ), notch =T) +
  geom_jitter( aes( x=morI_alldiet, y =value , color =as.factor(bff.presence) )) +
  # geom_hline(yintercept=0, color = "black", linetype = "dashed")+
  labs(x ='Morans I Value All Diet ', y ='Mean Temp Anomaly in 20km Roost Buffers',  color = "BFF Presence") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  scale_color_manual(values = c("grey57", "red"  )) +
  #  scale_shape_manual(values = c( 16,15,17,9,3)) +
  # scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1 ))+
  theme_bw(base_size = 8) +
  # ylim(-3,3) +
  facet_wrap(~variable, ncol = 4)+
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 65, vjust =1, hjust =1 ))




spillall_prcp_melt_raw %>%
  #arrange(distance_bin) %>%
  filter(
    variable != "mean_csum_precip_20kbuff" & variable != "mean_maxtemp_20kbuff" & variable != "mean_monthlytemp_20kmbuff" 
    & variable != "mean_mintemp_20kbuff"  ) %>%
  ggplot() +
  geom_jitter( aes( x=morI_atypclwint, y =value , color =as.factor(bff.presence) )) +
  
  # geom_hline(yintercept=1, color = "black", linetype = "dashed")+
  labs(x ='MoransI Value Atypical Winter Diet 20km Buffer ', y ='Mean Cumulative Precipitation Anomaly in 20km Roost Buffers',color = "BFF Presence") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  # scale_color_manual(values = c("skyblue", "midnightblue","dodgerblue1" )) +
  
  #scale_color_manual(values = c("red3","grey60", "firebrick4","indianred3" )) +
  #  scale_shape_manual(values = c( 16,15,17,9,3)) +
  #scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1 ))+
  theme_bw(base_size = 8) +
  # ylim(-3,3) +
  facet_wrap(~variable, ncol = 3)+
  theme(legend.position  = "none", axis.text.x = element_text(angle = 65, vjust =1, hjust =1 ))

# ggplot(roosts10_2010spill) +
#   geom_point(x= yr_mon) 
# 

spill_temp_melt_raw %>%
  arrange(distance_bin) %>%
  filter(
    variable != "mean_csum_precip_20kbuff" & variable != "mean_maxtemp_20kbuff" & variable != "mean_monthlytemp_20kmbuff" 
    & variable != "mean_mintemp_20kbuff"  ) %>%
  ggplot() +
  geom_boxplot( aes(x = distance_bin , y =value, color= latbin ), notch =T) +
  
  #  geom_point( aes(x = prop_20kbuff_yrobs , y =value, color = distance_bin), size = 1.2) +
  # geom_hline(yintercept=1, color = "black", linetype = "dashed")+
  labs(x ='Distance to Spillover (Km) ', y ='Mean Temperature Anomaly in 20km Roost Buffers',  color = "Latitude bin of roost") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  # scale_color_manual(values = c("skyblue", "midnightblue","dodgerblue1" )) +
  
  #scale_color_manual(values = c("red3","grey60", "firebrick4","indianred3" )) +
  #  scale_shape_manual(values = c( 16,15,17,9,3)) +
  #scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1 ))+
  theme_bw(base_size = 8) +
  # ylim(-3,3) +
  facet_wrap(~variable, ncol = 3)+
  theme(legend.position  = "none", axis.text.x = element_text(angle = 65, vjust =1, hjust =1 ))


spillall_precipmini_anom %>%
  # arrange(dist) %>%
  filter( dist_to_spill_km < 100 ) %>%
  ggplot() +
  geom_smooth( aes(x = yrmon, y = mean_csum_precip_anom_20kbuff, color= control, group = control )) +
  #  geom_point(aes(x = distance_bin , y =value, color= latbin)) +
  # geom_hline(yintercept=1, color = "black", linetype = "dashed")+
  labs(x ='Distance to Spillover (Km) ', y ='Mean Cumulative Precipitation Anomaly in 20km Roost Buffers',  color = "Latitude bin of roost") +
  # labs(x ='Time', y ='Mean Precipitation Anomaly in 20km Rooost Foraging Buffers', shape = "year", color = "Spillover in Roost Buffer") +
  #scale_color_manual(values = c("grey57", "red"  )) +
  #  scale_shape_manual(values = c( 16,15,17,9,3)) +
  # scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1 ))+
  
  # ylim(-3,3) +
  facet_wrap(~spillover, ncol = 8, scales = "free_x")+
  theme_bw(base_size = 10) +
  theme(legend.position  = "bottom", axis.text.x = element_text(angle = 65, vjust =1, hjust =1 ))



