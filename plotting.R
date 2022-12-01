setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(tidyverse) #tidying 
library(ggplot2) #viz
library(scales)
library(gridExtra)

df<- read.csv("LMBFFtracks_cleanedfixed_fulldf_221019.csv", header = T, stringsAsFactors = F)
mini<- df[,c(2,11,18,19,20,22,23)]
mini<- as.data.frame(unique(mini))
nightID_sums<- read.csv("nightIDsums_cleanedfixed_221019.csv", header = T, stringsAsFactors = F)


nightID_sums2<- left_join(nightID_sums, mini, by = "night_ID", all.x = T)
#there are 4 nights where the animal maps to 2 seasons or years 
##Francis 15, years 2020 and 2021 - same season
#lumiere 25, months feb and march - diff season
#patch26, months may and june - diff season
#roscoe14, years 2020 and 2021 - same season

nightID_sums3 = nightID_sums2[!duplicated(nightID_sums2$night_ID),]

#don't run population mean distance yet, nights are wrong 
###########################################################################################
###########################################################################################
#now do buffer size but aggregate by camp name for some who dip into NSW
buff10km<- read.csv("allcamps_xyalb_10kmbuff_land_int.csv", header = T, stringsAsFactors = F)

##divide buffer area by area of a full 5k/20km buffer to get proportion of buffer over land
buff10km$prop_10kbuff_land<- buff10km$Shape_Area/314159223


#merge the buff proportions togeter and clean
buff10km<- buff10km[,-2]

c1<- unique(buff10km$camp_name)
c2<- unique(nightID_sums3$origin_camp_name)
#attach buffer props to night_ID sums df of return nights only
roost_buffs<- merge(nightID_sums3, buff10km, by.x = "origin_camp_name", by.y = "camp_name", all.x = T)

##just noticing now Wendy 01 night is off, she should actually start at a new roost I named but is still within 900m of the next roost she goes to 
roost_buffs$origin_camp_name <- ifelse(roost_buffs$night_ID== "Wendy_01", "Federal, Skyring Creek", roost_buffs$origin_camp_name)

#don't know how the noosaville goat island snuck in here.. ugh 
roost_buffs$origin_camp_name <- ifelse(roost_buffs$night_ID== "Lucky_10", "Noosaville, Wallace Drive", roost_buffs$origin_camp_name)
#mannually fixing where return pt gets mapped to known roost despite starting at new roosts as identified by me (diff searching distances used) 
roost_buffs$origin_camp_name <- ifelse(roost_buffs$night_ID== "Merryweather_04", "Arcot State Forest, Silver Spur Redgate Road", roost_buffs$origin_camp_name)
#same fix
roost_buffs$origin_camp_name <- ifelse(roost_buffs$night_ID== "Patch_11", "Noosaville, Bushlands Park", roost_buffs$origin_camp_name)

#technically sebastian00 is wrong.. since the tracker come on late it maps to redwood park on night00
roost_buffs$origin_camp_name <- ifelse(roost_buffs$night_ID== "Sebastian_00", "Toowoomba, Spring Street", roost_buffs$origin_camp_name)

table(roost_buffs$rtrn_night_origin)

#recalculate nights with returns
roost_buffs$rtrn_night_origin <- ifelse(roost_buffs$origin_camp_name ==  roost_buffs$return_camp_name,1,0 )
table(roost_buffs$rtrn_night_origin)

#805 return nights, 85.2% of nights are return, only 15% switch 

return_roost_buffs<- subset(roost_buffs, rtrn_night_origin == 1) 

#fix the order of df bc it's bothering me 
return_roost_buffs<- return_roost_buffs[,c(2,3,1,4:14)]

# ## no clue why by kolan river and kilcoy, kilcoy creek just don't match despite being in the joining table and no text/whitespace problems.. ughh manually fixing
#values from the buffer table with the same exact names that won't match for some reason 
##5km         #20km
# Kilcoy, Kilcoy Creek (Anzac Park)
# 1.0000000        
## Kolan River, Avondale 0.885515


return_roost_buffs$prop_10kbuff_land <- as.numeric(ifelse(return_roost_buffs$origin_camp_name == "Kolan River, Avondale" , paste(0.885515),
                                          ifelse(return_roost_buffs$origin_camp_name == "Kilcoy, Kilcoy Creek (Anzac Park)" , paste(1.00),
                                          paste(return_roost_buffs$prop_10kbuff_land ))))

unique(return_roost_buffs$origin_camp_name)
#bin the scaled proportions
return_roost_buffs$bin10km<- cut(return_roost_buffs$prop_10kbuff_land, breaks = c(0, 0.25, 0.5, 0.75, 0.99, 1.1),
                                labels = c("Under 25%","25%-50%", "50%-75%", "75%-99%", "Whole buffer"))



median(return_roost_buffs$max_nght_displacement_km) #[1] 4.42
mean(return_roost_buffs$max_nght_displacement_km) #[1] 6.70 


sex_sums<- return_roost_buffs %>%
  group_by(Sex) %>%
  summarize(median_displ = median(max_nght_displacement_km),
            mean_displ = mean(max_nght_displacement_km),
            median_csum = median(nghtly_cumsumdist_km), 
            mean_csum = mean(nghtly_cumsumdist_km))


################################################################################################################
################################################################################################################
#read in the list of all camps 10km buff intersected with woody veg v5 data to get distribution of
# forest/sparse/non-forest pixels surrounding all roosts used in this tracking dataset
forests<- read.csv( "allcamps_woodyveg10kmbuffint_2yrsums_221110.csv", header = T, stringsAsFactors = F)
colnames(forests)[5]<- "forest_year"
forests$hectares<- forests$Shape_Area*0.0001
forests[,3]<- NULL
forests$forest_year<- as.factor(forests$forest_year)

campsused<- as.data.frame(unique(roost_buffs$origin_camp_name))
colnames(campsused)[1]<- "origin_camp_name"
all_forests<- merge(forests, campsused, by.x = "Name_of_camp", by.y = "origin_camp_name", all.x =  T)

#get the proportion of landcover type for 10km buff 
all_forests$prop<- all_forests$hectares/31415.92

###forest and sparse vs non forest 
all_type_forest_agg<- aggregate(all_forests$hectares, by=list(all_forests$fors_type,  all_forests$forest_year), FUN=sum)
colnames(all_type_forest_agg)<- c("forest_type", "forest_year", "sum_ha")
all_type_forest_agg$prop<- all_type_forest_agg$sum_ha/12837741

###now do the forest vs non forest only 
all_binary_forest_agg<- aggregate(all_forests$hectares, by=list(all_forests$forest, all_forests$forest_year), FUN=sum)
colnames(all_binary_forest_agg)<- c("forest", "forest_year", "sum_ha")
all_binary_forest_agg$prop<- all_binary_forest_agg$sum_ha/12837741



########################################################################################################################
###### now just do the roost with the tracking data 
origin_forests<- merge(forests, campsused, by.x = "Name_of_camp", by.y = "origin_camp_name", all.y =  T)

#get the proportion of landcover type for 10km buff 
origin_forests$prop<- origin_forests$hectares/31415.92
origin_forests$forest<- ifelse(origin_forests$fors_type == "Non-forest", paste("Non-forest"),paste("Forest"))

###forest and sparse vs non forest 
type_forest_agg<- aggregate(origin_forests$hectares, by=list(origin_forests$fors_type,  origin_forests$forest_year), FUN=sum)
colnames(type_forest_agg)<- c("forest_type", "forest_year", "sum_ha")
type_forest_agg$prop<- type_forest_agg$sum_ha/12020927

###now do the forest vs non forest only 
binary_forest_agg<- aggregate(origin_forests$hectares, by=list(origin_forests$forest, origin_forests$forest_year), FUN=sum)
colnames(binary_forest_agg)<- c("forest", "forest_year", "sum_ha")
binary_forest_agg$prop<- binary_forest_agg$sum_ha/12020927

#firstlast<- subset(origin_forests,forest_year == 2007|forest_year == 2020)



##########################  now get the forests of all roosts that have bff presence >1 in nffmp   ########################################
#############################################################################################################################################
# ffmp<- read.csv("cleaned_qld_ff_data_Apr03_Mar22_kb_20220705.csv", header = T, stringsAsFactors = F)
# 
# bffpres<- subset(ffmp, bff.presence > 0)
# 
# bffcamps<- as.data.frame(unique(bffpres$camp.name))
# colnames(bffcamps)[1]<- "Name_of_camp"
# bff_forests<-  merge(forests, bffcamps, by = "Name_of_camp" , all.y =  T)
# bff_forests$prop<- bff_forests$hectares/31415.92
# which(is.na(bff_forests$fors_type))  #ughh some didn't get forest buffers how is that possible 
# 
# bff_forests2<- bff_forests[!is.na(bff_forests$fors_type), ] #some of the roosts are outside my study area

##########################  now look which roosts are always/never occupied ########################################
#############################################################################################################################################
bffsums<- read.csv("bffsummary2_allseasons_221114.csv", header = T, stringsAsFactors = F)
alwaysocc<- subset(bffsums, pct_occ == 1)

alwaysocc_forests<-  merge(forests, alwaysocc, by.x = "Name_of_camp", by.y = "camp.name", all.y = T) 
alwaysocc_forests$prop<-alwaysocc_forests$hectares/31415.92
alwaysocc_forests2<- alwaysocc_forests[!is.na(alwaysocc_forests$fors_type), ]


####################3
nvrocc<- subset(bffsums, pct_occ == 0)

nvrocc_forests<-  merge(forests, nvrocc, by.x = "Name_of_camp", by.y = "camp.name", all.y = T) 
nvrocc_forests$prop<-nvrocc_forests$hectares/31415.92
nvrocc_forests2<- nvrocc_forests[!is.na(nvrocc_forests$fors_type), ]  #some of the roosts are outside my study area


####################
someocc<- subset(bffsums, pct_occ > 0  & pct_occ <1)

someocc_forests<-  merge(forests, someocc, by.x = "Name_of_camp", by.y = "camp.name", all.y = T) 
someocc_forests$prop<-someocc_forests$hectares/31415.92
someocc_forests2<- someocc_forests[!is.na(someocc_forests$fors_type), ]  #some of the roosts are outside my study area



#############################################

### plot landcover type in 10km buffers of roosts tracked individuals went to
####get distribution of just central 95% data

#manually set viridis palette and replace the yellow thats hard to see 
#vir_pal2<- c("goldenrod1" , "#9FDA3AFF","#4AC16DFF",  "#1FA187FF", "#277F8EFF", "#365C8DFF", "#46337EFF","darkorchid4" )
origin_forests %>%
  filter(forest_year == 2007 |forest_year == 2013 |forest_year == 2020) %>%
 ggplot(aes(x= prop,  color = forest_year)) +
  geom_density(size = 0.65) +
  #scale_fill_brewer( palette = "PRGn") +
  #scale_fill_manual(values = c("#00BA38", "grey57", "#619CFF")) +
  scale_y_continuous(labels = comma) +
  labs(y= " ", x = "", fill ="Forest year", color  = "Forest year") +
  facet_wrap(~ fors_type, ncol = 3, scales = "free_y") +
  #xlim(0,40) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")


vir_pal<- c("goldenrod1" , "#9FDA3AFF","#4AC16DFF",  "#1FA187FF", "#277F8EFF", "#365C8DFF", "slateblue2","purple4" )

### plot landcover type in 10km buffers of roosts tracked individuals went to
a<- ggplot(origin_forests, aes(x= prop,  color = forest_year)) +
  geom_density(size = 0.65) +
  scale_color_manual(values = vir_pal) +
  scale_y_continuous(labels = comma) +
  labs(y= "Density of Landcover type in 10km buffer", x = "", fill ="Forest year", color  = "Forest year") +
  facet_wrap(~ fors_type, ncol = 3, scales = "free_y") +
 # xlim(0,0.75) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")
a
b<- ggplot(alwaysocc_forests2, aes(x= prop,  color = forest_year)) +
  geom_density(size = 0.65) +
  scale_color_manual(values = vir_pal) +
  scale_y_continuous(labels = comma) +
  labs(y= "", x = "", fill ="Forest year", color  = "Forest year") +
  facet_wrap(~ fors_type, ncol = 3, scales = "free_y") +
  #xlim(0,0.75) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  expand_limits(y = c(0, 2))
b
c<- ggplot(nvrocc_forests2, aes(x= prop,  color = forest_year)) +
  geom_density(size = 0.65) +
  scale_color_manual(values = vir_pal) +
  scale_y_continuous( breaks = c(0,5.0,10), limits = c(0,12.5)) +
  labs(y= "Density of Landcover type in 10km buffer", x = "", fill ="Forest year", color  = "Forest year") +
  facet_wrap(~ fors_type, ncol = 3, scales = "free_y") +
  #xlim(0,0.75) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") 
c

d<- ggplot(someocc_forests2, aes(x= prop,  color = forest_year)) +
  geom_density(size = 0.65) +
  scale_color_manual(values = vir_pal) +
  scale_y_continuous(labels = comma) +
  labs(y= "", x = "", fill ="Forest year", color  = "Forest year") +
  facet_wrap(~ fors_type, ncol = 3, scales = "free_y") +
  #xlim(0,0.75) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  expand_limits(y = c(0, 2))
d

e<- ggplot(all_forests, aes(x= prop,  color = forest_year)) +
  geom_density(size = 0.65) +
  scale_color_manual(values = vir_pal) +
  scale_y_continuous(labels = comma) +
  labs(y= " ", x = "Proportion of Landcover", fill ="Forest year", color  = "Forest year") +
  facet_wrap(~ fors_type, ncol = 3, scales = "free_y") +
 # xlim(0,0.75) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")
e


t<- grid.arrange(b,d,c, nrow = 3)
grid.arrange(b,c, nrow = 2)

############################################
ggsave("forest_nonforest_sparse_3gridocc_ovrtime_yaxisfix_221116.eps",  plot = t ,width=11,  height =9, units = c("in"), dpi = 300)
#ggsave("forest_nonforest_sparseyaxis_fix_221116.eps",  plot = e , width=6,  height =7, units = c("in"), dpi = 300)
ggsave("forest_yr_legend_221116.eps",  plot = e , width=6,  height =7, units = c("in"), dpi = 300)



vir_pal2<- c("goldenrod1" ,  "#1FA187FF", "purple4" )

##firsstlast
f<- alwaysocc_forests2 %>%
  filter(forest_year == 2007 |forest_year == 2013 |forest_year == 2020) %>%
  ggplot(aes(x= prop,  color = forest_year)) +
  geom_density(size = 0.65) +
  scale_color_manual(values = vir_pal2) +
  scale_y_continuous( breaks = c(0,5.0,10), limits = c(0,12.5)) +
  labs(y= "", x = "", fill ="Forest year", color  = "Forest year") +
  facet_wrap(~ fors_type, ncol = 3, scales = "free_y") +
  #xlim(0,0.75) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  expand_limits(y = c(0, 12.5))
f


g<- nvrocc_forests2 %>%
  filter(forest_year == 2007 |forest_year == 2013 |forest_year == 2020) %>%
  ggplot(aes(x= prop,  color = forest_year)) +
  geom_density(size = 0.65) +
  scale_color_manual(values = vir_pal2) +
  scale_y_continuous( breaks = c(0,5.0,10), limits = c(0,12.5)) +
  labs(y= "", x = "", fill ="Forest year", color  = "Forest year") +
  facet_wrap(~ fors_type, ncol = 3, scales = "free_y") +
  #xlim(0,0.75) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") + 
  expand_limits(y=c(0,12.5))
g

h<- someocc_forests2 %>%
  filter(forest_year == 2007 |forest_year == 2013 |forest_year == 2020) %>%
  ggplot(aes(x= prop,  color = forest_year)) +
  geom_density(size = 0.65) +
  scale_color_manual(values = vir_pal2) +
  scale_y_continuous( breaks = c(0,5.0,10), limits = c(0,12.5)) +
  #scale_y_continuous(labels = comma) +
  labs(y= "Density of Landcover type in 10km buffer", x = "", fill ="Forest year", color  = "Forest year") +
  facet_wrap(~ fors_type, ncol = 3, scales = "free_y") +
  #xlim(0,0.75) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  expand_limits(y = c(0, 12.5))
h

t2<- grid.arrange(f,h,g, nrow = 3)

ggsave("forest_nonforest_sparse_3gridocc_3yr_yaxisfix_221116.eps",  plot = t2 ,width=11,  height =9, units = c("in"), dpi = 300)







ggplot(both, aes(y= prop_10kbuff_land , x = forest_year,  fill = forest)) +
  geom_bar(stat = "identity", position = position_stack()) +
  #scale_fill_brewer( palette = "Blues") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#00BA38", "grey57", "#619CFF")) +
  labs(y= "Hectares of landcover in  10km buffers", x = "Year", fill = "Forest year") +
  facet_wrap(~Name_of_camp) +
  #xlim(0,40) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom")

#proportion histogram
ggplot(both, aes(x=forest_year, y= prop ,  fill = forest)) +
  geom_bar(stat = "identity", position = "dodge") +
  #scale_fill_brewer( palette = "Blues") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#00BA38", "grey57", "#619CFF")) +
  labs(y= "Proportions of landcover in 10km roost buffers", x = "Year", fill = "Landcover type") +
  facet_wrap(~Name_of_camp) +
  #xlim(0,40) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom")

ggplot(origin_forests, aes(x=Name_of_camp, y= prop ,  fill = forest_year)) +
  geom_bar(stat = "identity", position = "dodge") +
  #scale_fill_brewer( palette = "Blues") +
  scale_y_continuous(labels = comma) +
  #scale_fill_manual(values = c("#00BA38", "grey57", "#619CFF")) +
  labs(y= "Proportions of landcover in 10km roost buffers", x = "Year", fill = "Landcover type") +
  facet_wrap(~forest, ncol = 1) +
  #xlim(0,40) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom")


### plot landcover type in 10km buffers of roosts tracked individuals went to
ggplot(origin_forests, aes(x= prop, fill = fors_type)) +
  geom_density( alpha = 0.35) +
  #scale_fill_brewer( palette = "Blues") +
  scale_fill_manual(values = c("#00BA38", "black" , "#619CFF")) +
  scale_y_continuous(labels = comma) +
  labs(y= "Density of landcover in 10km buffer", x = "Proportion of Landcover", fill = "Forest type") +
  facet_wrap(~ forest_year, ncol = 1) +
  #xlim(0,40) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

ggplot(firstlast, aes(x= prop, fill = forest)) +
  geom_density( alpha = 0.35) +
  #scale_fill_brewer( palette = "Blues") +
  scale_fill_manual(values = c("#00BA38", "black", "#619CFF")) +
  scale_y_continuous(labels = comma) +
  labs(y= "Density of landcover in 10km buffer", x = "Proportion of Landcover", fill = "Forest type") +
  facet_wrap(~ forest_year, ncol = 2) +
  #xlim(0,40) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

################################################################
#put the quantile distances in the df 
q = c(0.05, 0.95)
 quants<-  return_roost_buffs %>%
  group_by(Sex, season) %>%
  summarize(sxsnmaxd_q05 = quantile(max_nght_displacement_km, probs = q[1]), 
            sxsnmaxd_q95 = quantile(max_nght_displacement_km, probs = q[2]),
            sxsncsum_q05 = quantile(nghtly_cumsumdist_km, probs = q[1]),
            sxsncsum_q95 = quantile(nghtly_cumsumdist_km, probs = q[2]),
            animals = n_distinct(ID),
            nights_tracked = n_distinct(night_ID))



 returns<- left_join(return_roost_buffs, quants, by = c( "season", "Sex"))
 returns$season<- factor(returns$season, levels = c("winter", "spring", "summer", "autumn" ))

####### scale female nightly distances by 3 since they are underrepresented in the data 3:1
returns$max_disp_scaled<- ifelse(returns$Sex == "Female", (returns$max_nght_displacement_km *3), returns$max_nght_displacement_km) 

median(returns$max_disp_scaled) #[1] 5.215364
mean(returns$max_disp_scaled) #[1] 11.011


test3<-  returns %>%
  group_by(Sex) %>%
  summarize(median_displ = median(max_disp_scaled),
            mean_displ = mean(max_disp_scaled))


test4 <-  returns %>%
  group_by(Sex, season) %>%
  summarize(sxsnmaxd_q05_scale = quantile(max_disp_scaled, probs = q[1]), 
            sxsnmaxd_q95_scale = quantile(max_disp_scaled, probs = q[2]))



quants2<- returns %>%
  group_by( bin5km) %>%
  summarize(mxdisp5km_05 = quantile(max_nght_displacement_km, probs = q[1]), 
            mxdisp5km_95 = quantile(max_nght_displacement_km, probs = q[2]))


quants3<- returns %>%
  group_by( bin20km) %>%
  summarize(mxdisp20km_05 = quantile(max_nght_displacement_km, probs = q[1]), 
            mxdisp20km_95 = quantile(max_nght_displacement_km, probs = q[2]))


quants4<- returns %>%
  group_by( bin5km, Sex) %>%
  summarize(mxdisp5km_05 = quantile(max_nght_displacement_km, probs = q[1]), 
            mxdisp5km_95 = quantile(max_nght_displacement_km, probs = q[2]))


quants5<- returns %>%
  group_by( bin20km, Sex) %>%
  summarize(mxdisp20km_05 = quantile(max_nght_displacement_km, probs = q[1]), 
            mxdisp20km_95 = quantile(max_nght_displacement_km, probs = q[2]))


##now check nighlty cummulative sum distances 
quants6<- returns %>%
  group_by( bin5km) %>%
  summarize(csumdist5km_05 = quantile(nghtly_cumsumdist_km, probs = q[1]), 
            csumdist5km_95 = quantile(nghtly_cumsumdist_km, probs = q[2]))


quants7<- returns %>%
  group_by( bin20km) %>%
  summarize(csumdist20km_05 = quantile(nghtly_cumsumdist_km, probs = q[1]), 
            csumdist20km_95 = quantile(nghtly_cumsumdist_km, probs = q[2]))


quants8<- returns %>%
  group_by( bin5km, Sex) %>%
  summarize(csumdist5km_05 = quantile(nghtly_cumsumdist_km, probs = q[1]), 
            csumdist5km_95 = quantile(nghtly_cumsumdist_km, probs = q[2]))

quants9<- returns %>%
  group_by( bin20km, Sex) %>%
  summarize(csumdist20km_05 = quantile(nghtly_cumsumdist_km, probs = q[1]), 
            csumdist20km_95 = quantile(nghtly_cumsumdist_km, probs = q[2]))



table(returns$bin20km)
####### find out the number of roosts/animals behind each quantile thing   #######

### distances by season and sex
ggplot(returns, aes(x= max_nght_displacement_km, fill = season)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c( "dodgerblue1","forestgreen", "red" ,"darkorange1"))+
  scale_color_manual(values = c( "dodgerblue1","forestgreen", "red" ,"darkorange1"))+
  theme_bw(base_size = 13) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,40,50,80),
                     labels = c(0,5,10,15,20,25,30,40,50,80)) +
  labs(y= "Density of Return Nights Tracked", x = "Maximum Displacement KM", fill = "season") +
  geom_vline(data = returns,aes(xintercept = sxsnmaxd_q05, color = season), size =1, linetype = "dashed") +
  geom_vline(data = returns,aes(xintercept = sxsnmaxd_q95, color = season), size =1,  linetype = "dashed") +
  facet_wrap(~ Sex, ncol = 1) +
  xlim(0,40) +
  theme(legend.position = "bottom")

###color by 5km bin
ggplot(return_roost_buffs, aes(x= max_nght_displacement_km, fill = bin10km)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c(  "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))+
  # scale_fill_manual(values = c( "dodgerblue1","forestgreen", "red" ,"darkorange1"))+
  # scale_color_manual(values = c( "dodgerblue1","forestgreen", "red" ,"darkorange1"))+
  theme_bw(base_size = 13) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,40,50,80),
                     labels = c(0,5,10,15,20,25,30,40,50,80)) +
  labs(y= "Density of Return Nights Tracked", x = "Maximum Displacement KM", fill = "Pct buffer over land") +
  # geom_vline(data = returns,aes(xintercept = quant05, color = bin5km), size =1, linetype = "dashed") +
  # geom_vline(data = returns,aes(xintercept = quant95, color = bin5km), size =1,  linetype = "dashed") +
  facet_wrap(~ Sex, ncol = 1) +
  xlim(0,25) +
  theme(legend.position = c(0.85,0.85))


###color by 20km bin
ggplot(returns, aes(x= max_nght_displacement_km, fill = bin20km)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c( "#F8766D" ,"#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))+
  # scale_fill_manual(values = c( "dodgerblue1","forestgreen", "red" ,"darkorange1"))+
  # scale_color_manual(values = c( "dodgerblue1","forestgreen", "red" ,"darkorange1"))+
  theme_bw(base_size = 13) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,40,50,80),
                     labels = c(0,5,10,15,20,25,30,40,50,80)) +
  labs(y= "Density of Return Nights Tracked", x = "Maximum Displacement KM", fill = "Prop buffer over land") +
  # geom_vline(data = returns,aes(xintercept = quant05, color = bin5km), size =1, linetype = "dashed") +
  # geom_vline(data = returns,aes(xintercept = quant95, color = bin5km), size =1,  linetype = "dashed") +
  facet_wrap(~ Sex, ncol = 1) +
  #xlim(0,20) +
  theme(legend.position = c(0.85,0.79))



##########################  Cumulative distance flown with retur   ########################################
###################################################################################################
### distances by season and sex
ggplot(returns, aes(x= nghtly_cumsumdist_km  , fill = season)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c( "dodgerblue1","forestgreen", "red" ,"darkorange1"))+
  scale_color_manual(values = c( "dodgerblue1","forestgreen", "red" ,"darkorange1"))+
  theme_bw(base_size = 13) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,40,50,80, 100),
                     labels = c(0,5,10,15,20,25,30,40,50,80, 100)) +
  labs(y= "Density of Return Nights Tracked", x = "Cumulative Distance Flown in a Night (KM)", fill = "season") +
  geom_vline(data = returns,aes(xintercept = sxsncsum_q05 , color = season), size =1, linetype = "dashed") +
  geom_vline(data = returns,aes(xintercept = sxsncsum_q95 , color = season), size =1,  linetype = "dashed") +
  facet_wrap(~ Sex, ncol = 1) +
  #xlim(0,50) +
  theme(legend.position = "bottom")

###color by 5km bin
ggplot(returns, aes(x= nghtly_cumsumdist_km , fill = bin5km)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c(  "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))+
  theme_bw(base_size = 13) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,40,50,80),
                     labels = c(0,5,10,15,20,25,30,40,50,80)) +
  labs(y= "Density of Return Nights Tracked", x = "Cumulative Distance Flown in a Night (KM)", fill = "Prop buffer over land") +
  # geom_vline(data = returns,aes(xintercept = quant05, color = bin5km), size =1, linetype = "dashed") +
  # geom_vline(data = returns,aes(xintercept = quant95, color = bin5km), size =1,  linetype = "dashed") +
  facet_wrap(~ Sex, ncol = 1) +
  xlim(0,70) +
  theme(legend.position = c(0.85,0.79))


###color by 20km bin
ggplot(returns, aes(x=nghtly_cumsumdist_km, fill = bin20km)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c( "#F8766D" ,"#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))+
  theme_bw(base_size = 13) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,40,50,80),
                     labels = c(0,5,10,15,20,25,30,40,50,80)) +
  labs(y= "Density of Return Nights Tracked", x = "Cumulative Distance Flown in a Night (KM)", fill = "Prop buffer over land") +
  # geom_vline(data = returns,aes(xintercept = quant05, color = bin5km), size =1, linetype = "dashed") +
  # geom_vline(data = returns,aes(xintercept = quant95, color = bin5km), size =1,  linetype = "dashed") +
  facet_wrap(~ Sex, ncol = 1) +
  #xlim(0,50) +
  theme(legend.position = c(0.85,0.79))



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


