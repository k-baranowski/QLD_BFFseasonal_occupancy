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

#read in raw dataframe from Liam sent Sept 1, 2022
bats<- read.csv("mcguirelab_export2022.csv", header = T, stringsAsFactors = F, numerals = "no.loss")

#make a posixct date column and make new column pushing observations into Brisbane time zone 
bats$recorded_at2<- as.POSIXct(bats$recorded_at, format = "%Y-%m-%d  %H:%M:%S", tz =  "UTC")


# bats$time2 <- format(df$Time, format="%H:%M:%S")
# bats$recorded_aus<- with_tz(bats$recorded_at, "Australia/Brisbane" ) #set tz but doesn't actually work 
# tz(bats$recorded_aus) <- "Australia/Brisbane"

#forcing/changing time zones is not working so trying to add 10 hours onto posixct object
hrs <- hours(10)
bats$recorded_at2<- bats$recorded_at2 + hrs 

#make month and year column 
bats$month<- as.factor(format(bats$recorded_at2,'%m'))
bats$year<- as.factor(format(bats$recorded_at2,'%Y'))

sum(is.na(bats$recorded_at2)) #0 check
sum(is.na(bats$year)) #0

#serial numbers for 5 animals don't exist in raw data, have to only use last 4 numbers to ID observations
bats$serial_end<- str_sub(bats$serial, start= -4)

####################### #read in the serial numbers + animal IDs provided by Liam ###########3
#read in serials with on/off times 
serials<- read.csv("tracking_serials.csv", header = T, stringsAsFactors = F,numerals = "no.loss")
serials$serial_end<- str_sub(serials$serial, start= -4)
t<- unique(serials$serial_end) #72, however 78 tags so some last 4 digits get repeated 

#check if last 4 digits of serial numbers are on in the same month
dubs<- serials[duplicated(serials$serial_end)|duplicated(serials$serial_end, fromLast=TRUE),]
#okay we got lucky, no repeated serials with same last 4 digits recorded in the same month 

#fix the date formats into 1 
dates_fix1<- parse_date_time(x = serials$first_point_bat, orders = c( "%m/%d/%y  %H:%M", "m/d/y"))
dates_fix2<- parse_date_time(x = serials$LastPoint, orders = c( "%m/%d/%y  %H:%M", "m/d/y"))
serials$firstpt = dates_fix1
serials$lastpt = dates_fix2

#make month column so observations can be matched serial number and month! important bc tags get used twice 
serials$month<- as.factor(format(serials$firstpt,'%m'))
serials$month2<- as.factor(format(serials$lastpt,'%m'))

######################## merge the raw gps data with serials and animal IDs ######################## 
# df<- merge(bats, serials, by  = c("serial_end", "month"), all = T)
# missing <- df[is.na(df$ID),] #check out obs that didn't match to ann animal 
# sum(!is.na(df$ID)) #261,690, now 259,484

#some bats tracked over 2 different months, need to match observations for both and combine them together
#match to first month and serial number
animals_m1<- merge(bats, serials, by  = c("serial_end", "month"), all.y = T)
#match to second month and serial number
animals_m2<- merge(bats, serials, by.x  = c("serial_end", "month"), by.y =  c("serial_end", "month2"), all.y = T)

#find the unique observations from second month merge and add them together
animals2_unq<- anti_join(animals_m2, animals_m1)  #anti-join: what observations are in x that are not in y 
colnames(animals2_unq)[38] = "month2"

#combine the first and second month merged df 
df_combined<-rbind(animals_m1, animals2_unq) #286,357 observations, 284420
table(df_combined$ID) #check obs by animal 

#check if time of observation is between on and off time for each tag
df_combined$deployed<- ifelse(df_combined$recorded_at2 > df_combined$firstpt & df_combined$recorded_at2 < df_combined$lastpt, "T", "F")

table(df_combined$deployed)
deployed<-subset(df_combined, deployed == "T") #134,201 observations while deployed but its wrong 

#clean some up and reorder df so animal, date, time, lat, long all at the beginning 
deployed_cl<- deployed[, -c(1,3:5, 9:15, 17:23,27,32:34)]
deployed_cl<- deployed_cl[,c(8,6,3,4,5,1,16,7,9,10,11,12,13,14,15,17)]
sum(is.na(deployed_cl$recorded_at2))

write.csv(deployed_cl, "deployed_clean_tzfix_20220909.csv", row.names = F )

