setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(ggplot2) #viz
library(stringr) #viz
library(scales)
library(RColorBrewer)
library(reshape)
library(data.table)


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### PURPOSE OF ANALYSIS ######## ######### ######### ######## ######## 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#show seasonal characteristics of BFF foraging from GPS tagged individuals in 2019 and 2020

## importing raw data
files = list.files(pattern="*.csv$") # file list of all .csv files
seasons = lapply(files,read.csv, sep = ",") #gives out warnings about the last line not being complete but reads in fine

# read file name
dec_filenames <- files %>%
  basename() %>%
  as.list()

# combine file content list and file name list
dec_lists <- mapply(c, seasons, dec_filenames, SIMPLIFY = FALSE)

# unlist all lists and change column name
all_seasons <- rbindlist(dec_lists, fill = T)


# change column name
names(all_seasons)[10] <- "File.Path"
seasons<- data.frame(do.call('rbind', strsplit(as.character(all_seasons$File.Path ),'_',fixed=TRUE)))
seasons$sd<-  str_sub(seasons$X3, start= 1, end = 3)
seasons<- seasons[c(1,4)]
colnames(seasons)[1] = "season"

all_seasons<- cbind(all_seasons, seasons)
all_seasons[,10]<- NULL
#all_seasons<- do.call("rbind", seasons)

#add hectares and cln up
all_seasons$hectares<- all_seasons$Shape_Area*0.0001
table(all_seasons$season)
#colnames(all_seasons)[7]<- "season"
all_seasons$season <- factor(all_seasons$season, levels = c("winter2019", "summer", "autumn", "winter2020", "spring"))

#get a look at distrbtn 
ggplot(all_seasons, aes( x = hectares, fill=season))+
  labs(x = "standard deviation ellipse", y = "area of ellipse (ha)") +
  scale_fill_manual(values = c( "dodgerblue1","red", "darkorange1", "blue3" ,"forestgreen"))+
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  #facet_wrap(~season, ncol = 1 )
  theme(legend.position = c(0.87,0.87))


table(all_seasons$season)
# Summer Autumn Winter Spring  
# 10     24     16     12 
#so they are 5 summer roosts, 12 autumn roosts, 8 winter roosts and 6 spring


ggplot(all_seasons, aes(x = season, y = hectares, fill = season))+
  geom_dotplot( binaxis = "y", stackdir = "center") +
  labs(x = "standard deviation ellipse", y = "area of ellipse (ha)") +
  scale_fill_manual(values = c("red", "darkorange1", "dodgerblue1", "blue2" ,"forestgreen"))+
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  facet_wrap(~sd, ncol = 1 )
theme(legend.position = c(0.07,0.87))

sd2<- subset(all_seasons, sd == "2sd")
sd2$season <- factor(sd2$season, levels = c("winter2019","summer", "autumn",  "winter2020", "spring"))
variances<- sd2 %>% 
  group_by(season) %>%
  summarise_if(is.numeric, var)

ggplot(all_seasons, aes(x = season, y = hectares, fill = season)) +
  geom_violin() +
  labs(x = "second standard deviation ellipse", y = "variance of seasonal ellipse area (ha)") +
  scale_fill_manual(values = c("dodgerblue1","red", "darkorange1",  "blue2" ,"forestgreen"))+
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  #facet_wrap(~sd, ncol = 1 )
theme(legend.position = "none")

ggplot(sd2) +
  geom_point(aes(x = season, y = hectares, color= Rotation)) +
               #binaxis = "y", stackdir = "center") +
  scale_color_gradient(low="pink", high='red') +
  #scale_fill_brewer(palette="Reds") +
  labs(x = "Season", y = "Area of s.d. ellipse (ha)") +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18)

r<- ggplot(sd2) +
  geom_point(aes(x = Rotation, y = hectares, color= season), size = 5, alpha = 0.4) +
  scale_fill_manual(values = c("dodgerblue1", "red", "darkorange1",  "blue4" ,"forestgreen"))+
  labs(x = "Ellipse Rotation Angle", y = "Area of s.d. ellipse (ha)") +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "bottom")
r + coord_polar(theta = "x", direction=1)

#tst
ggplot(sd2, aes(x=Rotation, y=hectares, color = season)) + 
  coord_polar(theta= "x", direction=1) + geom_point( size = 5, alpha =0.5) + 
  scale_x_continuous(breaks=seq(0, 360, by=90), expand=c(0,0), lim=c(0, 360)) +
  scale_color_manual(values = c("dodgerblue1","red", "darkorange1",  "blue4" ,"forestgreen"))+
  labs(x = "Ellipse Rotation Angle", y = "Area of 2nd s.d. foraging ellipse (ha)") +
  #scale_y_continuous(labels = comma) + 
  ylim(0,2000)+
  facet_wrap(~season, ncol=3)+
  theme_bw(base_size = 16) +
  theme(legend.position = "none")


sd2_sums<-  as.data.frame(sd2 %>%
            group_by(season) %>%
            dplyr::summarise(
            largest_ellipse = max(hectares), 
            smallest_ellipse = min(hectares),
            median_area = median(hectares),
            mean_area = mean(hectares),
            roosts = n_distinct(ORIGIN_CAMP_NAME)))

ggplot(sd2_sums, aes(x=season, y=mean_area, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("red", "darkorange1", "dodgerblue1", "blue4" ,"forestgreen"))+
  geom_text(aes(label = roosts), vjust = 0, size = 5) +
 labs(x = "Season", y = "Mean area of s.d. ellipse (ha)") +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "none")



ggplot(sd2_sums, aes(x=season, y=median_area, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("red", "darkorange1", "dodgerblue1", "blue4" ,"forestgreen"))+
  geom_text(aes(label = roosts), vjust = 0, size = 5) +
  labs(x = "Season", y = "Median area of s.d. ellipse (ha)") +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

ggplot(sd2_sums, aes(x=season, y=largest_ellipse, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("red", "darkorange1", "dodgerblue1", "blue4" ,"forestgreen"))+
  geom_text(aes(label = roosts), vjust = 0, size = 5) +
  labs(x = "Season", y = "Largest area of s.d. ellipse (ha)") +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "none")


write.csv(all_seasons,"allseasons_allroosts_sdellipses_20220913.csv", row.names = F)
