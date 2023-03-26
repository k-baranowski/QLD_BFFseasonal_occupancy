setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(tidyverse) #tidying 
library(ggplot2) #viz
library(data.table)
library(plotly)


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#tomorrow, assign species as winter or  not, go back to old matching code and write out winter list from R 
winter<-read.csv("possiblewinterdiet_allsp_20230320.csv", header = T)
winter$diet = NA
winter$diet<- ifelse(winter$species == "Eucalyptus tereticornis" | winter$species == "E. tereticornis" | winter$species == "Eucalyptus tereticornis subsp. tereticornis" | winter$species == "E. tereticornis subsp. tereticornis" |
                       winter$species == "Eucalyptus robusta" | winter$species == "E. robusta" | 
                       winter$species == "Melaleuca quinquenervia" | winter$species == "M. quinquenervia" |
                       winter$species == "Banksia integrifolia" | winter$species == "B. integrifolia" | 
                       winter$species == "Eucalyptus siderphloia"| winter$species == "E.s siderphloia",  print("annual"), print("non-annual") )



#readfiles
#rem1750<- read.csv("RE1750_allwintroost_10kmbuffint.csv")
rem1997<- read.csv("RE1997_allwintroost_10kmbuffint.csv")
rem2007<- read.csv("RE2007_allwintroost_10kmbuffint.csv") 
rem2009<- read.csv("RE2009_allwintroost_10kmbuffint.csv")
rem2011<- read.csv("RE2011_allwintroost_10kmbuffint.csv")
rem2013<- read.csv("RE2013_allwintroost_10kmbuffint.csv")
rem2015<- read.csv("RE2015_allwintroost_10kmbuffint.csv")
rem2017<- read.csv("RE2017_allwintroost_10kmbuffint.csv")
rem2019<- read.csv("RE2019_allwintroost_10kmbuffint.csv")

#rem1750$year  = "1750"
rem1997$year  = "1997"
rem2007$year  = "2007"
rem2009$year  = "2009"
rem2011$year  = "2011"
rem2013$year  = "2013"
rem2015$year  = "2015"
rem2017$year  = "2017"
rem2019$year  = "2019"


veg<-rbind(rem2007,rem2009,rem2011,rem2013,rem2015,rem2017,rem2019)
veg[,c(1,14:29,31:36)]<- NULL
rem1997[1]<- NULL

veg<- rbind(rem1997, veg)
veg<- veg[c(13,16,1:12,14:15)]


########

#species<- read.csv("regecos_expandedspdf_V12_10sp_valid_20230129.csv", header = T, stringsAsFactors = F)
species<- read.csv("regecos_expandedspdf_V12_10sp_valid_20230322.csv", header = T, stringsAsFactors = F)



##fill in the  species information now 
#######################################################
vegRE1<- merge(veg, species, by.x = "RE1",by.y = "re_id", all.x = T )
#scale and convert shape area of regional ecosystems for RE1
vegRE1$scaled_ha<- ((vegRE1$Shape_Area*(vegRE1$PC1*0.01))*0.0001)
colnames(vegRE1)[1]<- "re_id"
vegRE1<- vegRE1[,-c(4:14)] 


vegRE2<- subset(veg, PC2 > 0)
vegRE2<- merge(vegRE2, species, by.x = "RE2",by.y = "re_id", all.x = T )
#scale and convert shape area of regional ecosystems for RE1
vegRE2$scaled_ha<- ((vegRE2$Shape_Area*(vegRE2$PC2*0.01))*0.0001)
colnames(vegRE2)[1]<- "re_id"
vegRE2<- vegRE2[,-c(4:14)] 
sum(is.na(vegRE2$veg_description2))


vegRE3<- subset(veg, PC3 > 0)
vegRE3<- merge(vegRE3, species, by.x = "RE3",by.y = "re_id", all.x = T )
#scale and convert shape area of regional ecosystems for RE3
vegRE3$scaled_ha<- ((vegRE3$Shape_Area*(vegRE3$PC3*0.01))*0.0001)
colnames(vegRE3)[1]<- "re_id"
vegRE3<- vegRE3[,-c(4:14)] 
sum(is.na(vegRE3$veg_description2))

vegRE4<- subset(veg, PC4 > 0)
vegRE4<- merge(vegRE4, species, by.x = "RE4",by.y = "re_id", all.x = T )
#scale and convert shape area of regional ecosystems for RE4
vegRE4$scaled_ha<- ((vegRE4$Shape_Area*(vegRE4$PC4*0.01))*0.0001)
colnames(vegRE4)[1]<- "re_id"
vegRE4<- vegRE4[,-c(4:14)] 
sum(is.na(vegRE4$veg_description2))

vegRE5<- subset(veg, PC5 > 0)
vegRE5<- merge(vegRE5, species, by.x = "RE5",by.y = "re_id", all.x = T )
#scale and convert shape area of regional ecosystems for RE4
vegRE5$scaled_ha<- ((vegRE5$Shape_Area*(vegRE5$PC5*0.01))*0.0001)
colnames(vegRE5)[1]<- "re_id"
vegRE5<- vegRE5[,-c(4:14)] 
sum(is.na(vegRE5$veg_description2))


#bind  
veg_allre<- rbind(vegRE1, vegRE2, vegRE3, vegRE4, vegRE5)


sum(is.na(veg_allre$veg_description2))
#sum the scaled area of each patch to not double count patches and get one unique total area of winter veg area 

veg_sums<- veg_allre %>%
  group_by(camp_name, year, re_id, veg_description2, X1, X2, X3, X4, X5, X6, X7) %>%
  dplyr::summarise(
    num_patches = n(),
    sum_scaled_ha = sum(scaled_ha),
    prop_total_buffer= sum_scaled_ha/31420,
    max_patch_size_ha= max(scaled_ha),
    min_patch_size_ha= min(scaled_ha),
    mean_patch_size_ha = mean(scaled_ha),
    median_patch_size_ha = median(scaled_ha))

###look for winter species in the reg ecos  while they're still att patch level
sp<- winter$species

veg_sums <- veg_sums %>%
  mutate(
    wintx1 = case_when(X1 %in% sp ~ 1), 
    wintx2 = case_when(X2 %in% sp ~ 1), 
    wintx3 = case_when(X3 %in% sp ~ 1), 
    wintx4 = case_when(X4 %in% sp ~ 1), 
    wintx5 = case_when(X5 %in% sp ~ 1), 
    wintx6 = case_when(X6 %in% sp ~ 1), 
    )

#replace 0  where no winter diet species was matched 
veg_sums  <- veg_sums %>% 
  mutate_at(c(19:24), ~replace_na(.,0))

veg_sums$num_wintsp<- rowSums(veg_sums[ ,c(19:24)], na.rm=TRUE)
veg_sums[,c(19:24)]<-NULL


table(veg_sums$num_wintsp)
#    0      1      2       3     4 
# 40526  19599  17691  12674   4857 

veg_sums$winter_present<- ifelse(veg_sums$num_wintsp >  0, 1, 0)
veg_sums$winter_present<- ifelse(veg_sums$re_id =="non-remnant" , 9, veg_sums$winter_present)
veg_sums$num_wintsp<- ifelse(veg_sums$re_id =="non-remnant" , 9, veg_sums$num_wintsp)

###############
yrlycamp_sumpatches<- veg_sums %>%
  group_by(camp_name, year, num_wintsp) %>%
  dplyr::summarise(
   total_patches = sum(num_patches),
   sum_total_area_ha = sum(sum_scaled_ha))
   #sum_max_size_ha = sum(max_patch_size_ha),
   #sum_median_ha = sum(median_patch_size_ha



yrlycamp_sumpatches<- yrlycamp_sumpatches %>%                        # Add lagged column of rate of loss
  group_by(camp_name, num_wintsp) %>%
  dplyr::mutate(change_area_since97_ha = sum_total_area_ha - first(sum_total_area_ha),
                 change_patch_num_since97 = total_patches - first(total_patches),
                prop_area_change_since97  = (sum_total_area_ha /first(sum_total_area_ha) -1))   %>%
  as.data.frame()

yrlycamp_sumpatches$change_area_since97_ha<-round(yrlycamp_sumpatches$change_area_since97_ha, digits = 3)

#yrlycamp_sumpatches$


yrlycamp_sumpatches <- yrlycamp_sumpatches %>% mutate_if(is.numeric, ~round(., 4))


write.csv(yrlycamp_sumpatches,"yrlycamp_patchestype_sumslosses_20230324.csv",row.names  = F)

################ 
################ read in here to skip all the steps above,  I  renamed it  
################ 
################ 

yrlycamp_sumpatches<- read.csv("allcamps_remveg_10kmbuff_sum_20230324.csv", header =T,  stringsAsFactors = F) 


#forest_rshp<- reshape::reshape()
veg_rshp<- reshape(yrlycamp_sumpatches, idvar = c("camp_name", "year"), timevar = "num_wintsp", direction = "wide")
colnames(veg_rshp)<- c("camp_name", "year", 
                       "total.patches.0wintsp",  "total.areaha.0wintsp",  "chang.area.since97.0wintsp",   "chang.patchn.since97.0wintsp",   "chang.prop.area.since97.0wintsp",
                       "total.patches.1wintsp",  "total.areaha1wintsp",  "chang.area.since97.1wintsp",   "chang.patchn.since97.1wintsp",   "chang.prop.area.since97.1wintsp",
                       "total.patches.2wintsp",  "total.areaha.2wintsp",  "chang.area.since97.2wintsp",   "chang.patchn.since97.2wintsp",   "chang.prop.area.since97.2wintsp",
                       "total.patches.3wintsp",  "total.areaha.3wintsp",  "chang.area.since97.0wintsp",   "chang.patchn.since97.3wintsp",   "chang.prop.area.since97.3wintsp",
                       "total.patches.4wintsp",  "total.areaha.4wintsp",  "chang.area.since97.4wintsp",   "chang.patchn.since97.4wintsp",   "chang.prop.area.since97.4wintsp",
                       "total.patches.nonrem",  "total.areaha.nonrem",  "chang.area.since97.nonrem",   "chang.patchn.since97.nonrem",   "chang.prop.area.since97.nonrem"
                       )


write.csv(veg_rshp, "allcamps_remvegrshp_10kmbuff_bywintsp_20230325.csv", row.names  = F)


########
yr07<- subset(yrlycamp_sumpatches, year ==  2007)
yr13<- subset(yrlycamp_sumpatches, year ==  2013)
yr15<- subset(yrlycamp_sumpatches, year ==  2015)
yr19<- subset(yrlycamp_sumpatches, year ==  2019)

##intnerractive plotly 
fig <- plot_ly(
  yr07, x = ~jitter(num_wintsp) , y = ~change_in_area_ha , type = "scatter", mode = "markers",marker = list(size = 9),
  text = ~paste("Camp: ", camp_name, 
                "<br>Number of winter sp in patch:", num_wintsp,
                '<br>Total num patches in buffer in 1997:',  total_patches, 
                '<br>Change in area since 1997 (ha):',  change_in_area_ha,
                '<br>Change in patches since 1997:',  change_patch_num),
  color = ~change_patch_num)
fig

##2007
fig <- plot_ly(
  yr07, x = ~jitter(num_wintsp) , y = ~ change_patch_num,  type = "scatter", mode = "markers",marker = list(size = 9),
  text = ~paste("Camp: ", camp_name, 
                "<br>Number of winter sp in patch:", num_wintsp,
                '<br>Total num patches in buffer in 2007:',  total_patches, 
                '<br>Change in area since 1997 (ha):',  change_in_area_ha,
                '<br>Change in patches since 1997:',  change_patch_num),
  color = ~change_in_area_ha)
fig

##2019

fig <- plot_ly(
  yr19, x = ~jitter(num_wintsp) , y = ~ prop_area_change_since97 ,  type = "scatter", mode = "markers",marker = list(size = 9),
  text = ~paste("Camp: ", camp_name, 
                "<br>Number of winter sp in patch:", num_wintsp,
                '<br>Total num patches in buffer in 2019:',  total_patches, 
                '<br>Change in area since 2017 (ha):',  change_area_since97_ha,
                '<br>Change in patches since 2017:',  change_patch_num_since97),
  color = ~camp_name)
fig



######################################################
occ<-read.csv("allcamps_seasonalocc_allssnsums_20230320.csv", header = T, stringsAsFactors = F)

camps_occ<-merge(yrlycamp_sumpatches, occ, by.x = "camp_name",  by.y = "camp.name", all.x = T)





#write.csv(veg, "allcamps_rem_vegveg10kmbuffint_veg_230319.csv", row.names = F)
camps_occ %>%
  filter(year == 2019 &  num.wint.sampled >3 )%>%
ggplot() +
  geom_bar(aes(x= reorder(camp_name,  prop.wint.surv.occ)  , y = sum_total_area_ha, fill= as.factor(num_wintsp)),  position = "stack", stat = "identity")+
  theme_bw(base_size = 5) +
  labs(y = "Area of Patches  in 2019 (ha)",x  =  "camp name", fill  = "Number of Winter Diet Sp") +
  scale_fill_manual(values = c("grey34", "palegreen2",  "limegreen", "springgreen4", "darkgreen", "grey87")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 85, vjust =1, hjust = 1))



####inntreactive plot 

fig <- plot_ly(
  yrlycamp_sumpatches, x = ~jitter(winter_present) , y = ~ total_patches , type = "scatter", mode = "markers",marker = list(size = 9),
  text = ~paste("Camp: ", camp_name, 
                '<br>Sum total of scaled area:',  sum_total_area_ha),
  color = ~camp_name)
fig


####  ABANDON THIS FOR NOW, TOO SPECIFIC, STAY AT THE PATCH LEVEL 
##calculate the frequncy  the species names  appear 
freqX1<- veg_allre %>% count(camp_name, year,X1) 
freqX2<- veg_allre %>% count(camp_name, year,X2) 
freqX3<- veg_allre %>% count(camp_name, year,X3) 
freqX4<- veg_allre %>% count(camp_name, year,X4)
freqX5<- veg_allre %>% count(camp_name, year,X5)
freqX6<- veg_allre %>% count(camp_name, year,X6)
freqX7<- veg_allre %>% count(camp_name, year,X7)
freqX8<- veg_allre %>% count(camp_name, year,X8)

colnames(freqX1)[3] = "species" 
colnames(freqX2)[3] = "species" 
colnames(freqX3)[3] = "species" 
colnames(freqX4)[3] = "species" 
colnames(freqX5)[3] = "species" 
colnames(freqX6)[3] = "species" 
colnames(freqX7)[3] = "species" 
colnames(freqX8)[3] = "species" 


freqs<- rbind(freqX1, freqX2, freqX3, freqX4, freqX5, freqX6, freqX7, freqX8)
freqs<- subset(freqs, species != "")

#gather up the totals  by species 
freqs<-freqs %>%
  group_by(camp_name, year, species) %>%
  dplyr::summarise(
  sum_word_freq = sum(n))


freqs<- freqs %>%
  group_by(camp_name, year) %>%
  dplyr::mutate(
   total_num_sp= n_distinct(species)
  )





#take out nonvegetated classes, non-remnant trees are acccounted for with different data 
freqs_veg<- subset(freqs,  species != "water"& species != "ocaen"& species != "canal"& species != "estuary"& species != "sand" & species !="shallow")



#merge them with roost list 
veg_species<- merge(freqs_veg, winter, by = "species", all.x = T)
veg_species$diet<- veg_species$diet %>% replace_na("nonwinter")

veg_species<-veg_species[,c(2,3,1,6,4,5)]

camp_veg_species<- veg_species  %>%
  group_by(camp_name, year, diet) %>%
  dplyr::summarise(
    species_freq = sum(sum_freq)) %>%
  as.data.frame()


  
camp_rshp<- reshape(camp_veg_species, idvar = c("camp_name", "year"), timevar = "diet", direction = "wide")
camp_rshp$species_freq.reliable <-camp_rshp$species_freq.reliable %>% replace_na(0)
camp_rshp$totalsp<- camp_rshp$species_freq.reliable +  camp_rshp$species_freq.unreliable + camp_rshp$species_freq.nonwinter
camp_rshp$wint_reliable_prop_total <- camp_rshp$species_freq.reliable/camp_rshp$totalsp
camp_rshp$wint_unreliable_prop_total <- camp_rshp$species_freq.unreliable/camp_rshp$totalsp
camp_rshp$nonwint_prop_total <- camp_rshp$species_freq.nonwinter/camp_rshp$totalsp
camp_rshp$reliab_prop_winter  <- camp_rshp$species_freq.reliable /(camp_rshp$species_freq.unreliable +  camp_rshp$species_freq.reliable)

# 
# camp_rshp$wint_nonwint_ratio  <- ((camp_rshp$species_freq.reliable +  camp_rshp$species_freq.unreliable)/camp_rshp$species_freq.nonwinter)

#write.csv(camp_rshp, "allroosts_remveg_speciesratios_230320.csv" ,row.names = F)
#sum(is.na(camp_rshp$species_freq.winter))

######################################################
occ<-read.csv("allcamps_seasonalocc_allssnsums_20230320.csv", header = T, stringsAsFactors = F)

camps_occ<-merge(camp_rshp, occ, by.x = "camp_name",  by.y = "camp.name", all.x = T)
camps_occ2<- camps_occ %>% filter(!is.na(camps_occ$num.wint.sampled))

#work with only roosts that have  been sampled at  least 3  winters and aren't in 1750
min3ssn<-subset(camps_occ2, wint.sample.dates  >= 3 &  year != 1750) 
#


#write.csv(camps_occ2,"allcamp_vegcomm_ratios17502019_20230321.csv" , row.names = F)
#######

fig <- plot_ly(
  camps_occ2, x = ~wint_unreliable_prop_total , y = ~ wint_reliable_prop_total , type = "scatter", mode = "markers",marker = list(size = 9),
  text = ~paste("Camp: ", camp_name, 
                '<br>Freq. nonwinter species:',  species_freq.nonwinter, 
                '<br>Num winters surveyed:',  wint.sample.dates,
                '<br>Num winters occ:',  sum.wint.occ.bff,
                '<br>Prop. winter occ:',  prop.wint.surv.occ,
                '<br>Prop. spring occ:',  prop.spring.surv.occ,
                '<br>Prop. summer occ:',  prop.summr.surv.occ,
                '<br>Prop. autumn occ:',  prop.autm.surv.occ),
  color = ~prop.wint.surv.occ)%>%
add_segments(x = 0, xend = 0.25, y = 0, yend = 0.25, line = list(dash = "dash", color = 'black'),inherit = FALSE,showlegend = FALSE)

fig



fig <- plot_ly(
  camps_occ2, x = ~wint_unreliable_prop_total , y = ~ wint_reliable_prop_total , type = "scatter", mode = "markers",marker = list(size = 9),
  text = ~paste("Camp: ", camp_name, 
                '<br>Freq. nonwinter species:',  species_freq.nonwinter, 
                '<br>Num winters surveyed:',  wint.sample.dates,
                '<br>Num winters occ:',  sum.wint.occ.bff,
                '<br>Prop. winter occ:',  prop.wint.surv.occ,
                '<br>Prop. spring occ:',  prop.spring.surv.occ,
                '<br>Prop. summer occ:',  prop.summr.surv.occ,
                '<br>Prop. autumn occ:',  prop.autm.surv.occ),
  color = ~prop.wint.surv.occ)%>%
  add_segments(x = 0, xend = 0.25, y = 0, yend = 0.25, line = list(dash = "dash", color = 'black'),inherit = FALSE,showlegend = FALSE)

fig









###### ternary plot 
axis <- function(title) {
  list(
    title = title,
    titlefont = list(
      size = 20
    ),
    tickfont = list(
      size = 15
    ),
    tickcolor = 'rgba(0,0,0,0)',
    ticklen = 5
  )
}


fig2 <- min3ssn %>% plot_ly()
fig2 <- fig2 %>% add_trace (
  type = "scatterternary",
  mode = "markers",
  color =~prop.wint.surv.occ,
  a = ~wint_reliable_prop_total, 
  b = ~wint_unreliable_prop_total ,
  c = ~nonwint_prop_total,
  text = ~paste("Camp: ", camp_name,
                '<br>Num winters occ:',  sum.wint.occ.bff,
                '<br>Num winters surveyed:',  wint.sample.dates,
                '<br>Prop. winter occ:',  prop.wint.surv.occ),
  marker = list( 
    symbol = 100,
    #color = ~year,
    size = 9,
    line = list('width' = 3)
  )
)
fig2 <- fig %>% layout(
  ternary = list(
    sum = 1,
    aaxis = list(title = 'Prop. reliable winter sp'),
    baxis = list(title = 'Prop. unreliable winter sp'),
    caxis = list(title = 'Prop. non-winter sp')
  )
)

fig2

