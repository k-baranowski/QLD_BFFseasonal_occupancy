setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(ggplot2) #viz
library(stringr) #viz
library(scales)
library(RColorBrewer)
library(reshape)


######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ######## 
######## ######### ######### PURPOSE OF ANALYSIS ######## ######### ######### ######## ######## 
######## ######### ######### ######## ######### ######### ######## ######### ######### ######## ########
#show seasonal characteristics of BFF foraging from GPS tagged individuals in 2019 and 2020

## importing raw data
files = list.files(pattern="*.csv$") # file list of all .csv files
seasons = lapply(files,read.csv, sep = ",") #gives out warnings about the last line not being complete but reads in fine

all_seasons<- do.call("rbind", seasons)

#add hectares and cln up
all_seasons$hectares<- all_seasons$Shape_Area*0.0001


colnames(all_seasons)[7]<- "season"
all_seasons$season <- factor(all_seasons$season, levels = c("Summer", "Autumn", "Winter", "Spring"))

#get a look at distrbtn 
ggplot(all_seasons, aes( x = hectares, fill=season))+
  labs(x = "standard deviation ellipse", y = "area of ellipse (ha)") +
  scale_fill_manual(values = c("red", "darkorange1", "blue", "forestgreen"))+
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  facet_wrap(~season, ncol = 1 )
  theme(legend.position = c(0.87,0.87))


table(all_seasons$season)
# Summer Autumn Winter Spring  
# 10     24     16     12 
#so they are 5 summer roosts, 12 autumn roosts, 8 winter roosts and 6 spring


ggplot(all_seasons, aes(x = season, y = hectares, fill = season))+
  geom_dotplot( binaxis = "y", stackdir = "center") +
  labs(x = "standard deviation ellipse", y = "area of ellipse (ha)") +
  scale_fill_manual(values = c("red", "darkorange1", "blue", "forestgreen"))+
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  facet_wrap(~sd, ncol = 1 )
theme(legend.position = c(0.07,0.87))

sd2<- subset(all_seasons, sd == 2)

ggplot(sd2) +
  geom_point(aes(x = season, y = hectares, color= Rotation)) +
               #binaxis = "y", stackdir = "center") +
  scale_color_gradient(low="pink", high='red') +
  #scale_fill_brewer(palette="Reds") +
  labs(x = "Season", y = "Area of s.d. ellipse (ha)") +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18)

r<- ggplot(sd2) +
  geom_point(aes(x = Rotation, y = hectares, color= season), size = 5, alpha = 0.5) +
  scale_color_manual(values = c("red", "darkorange1", "blue", "forestgreen"))+
  labs(x = "Ellipse Rotation Angle", y = "Area of s.d. ellipse (ha)") +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "bottom")
r + coord_polar(theta = "x", direction=1)

#tst
ggplot(sd2, aes(x=Rotation, y=hectares, color = season)) + 
  coord_polar(theta= "x", direction=1) + geom_point( size = 5, alpha =0.5) + 
  scale_x_continuous(breaks=seq(0, 360, by=90), expand=c(0,0), lim=c(0, 360)) +
  scale_color_manual(values = c("red", "darkorange1", "blue", "forestgreen"))+
  labs(x = "Ellipse Rotation Angle", y = "Area of 2nd s.d. foraging ellipse (ha)") +
  #scale_y_continuous(labels = comma) + 
  ylim(0,2000)+
  #facet_wrap(~season, ncol=2)+
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")


sd2_sums<-  as.data.frame(sd2 %>%
            group_by(season) %>%
            dplyr::summarise(
            largest_ellipse = max(hectares), 
            smallest_ellipse = min(hectares),
            median_area = median(hectares),
            mean_area = mean(hectares),
            roosts = n_distinct(NAME_OF_CAMP)))

ggplot(sd2_sums, aes(x=season, y=mean_area, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("red", "darkorange1", "blue", "forestgreen")) +
  geom_text(aes(label = roosts), vjust = 0, size = 5) +
 labs(x = "Season", y = "Mean area of s.d. ellipse (ha)") +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "none")



ggplot(sd2_sums, aes(x=season, y=median_area, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("red", "darkorange1", "blue", "forestgreen")) +
  geom_text(aes(label = roosts), vjust = 0, size = 5) +
  labs(x = "Season", y = "Median area of s.d. ellipse (ha)") +
  scale_y_continuous(labels = comma) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

############################# Central 95% data of ellipse areas in hectares ####################
q = c(.05,.95)
qs<- all_seasons %>%
  group_by(season, sd) %>%
  summarize(quant05 = quantile(hectares, probs = q[1]),
            quant95 = quantile(hectares, probs = q[2]),) %>%
  as.data.frame()
qs$qauntrange<-qs$quant95-qs$quant05

ggplot(qs, aes( qauntrange,season, color = as.factor(sd))) +
  geom_point(size = 5) +
  #geom_line(size = 2, aes(group = interaction(sd, season))) +
  #geom_line( ) +
  labs(y = "Season", x = "Range of the Central 95% Area of Foraging Ellipses (ha)", color = "s.d.") +
  theme_bw(base_size = 18) +
  theme(legend.position = "bottom")



#melt it around
qs_melt<- melt(qs, id=c("season","sd"), variable_name = "quantile")
qs_melt$sd<- as.factor(qs_melt$sd)

qs_melt$sd<- factor(qs_melt$sd, levels = c("1", "2"))
dodge <- position_dodge(width=0.6)  
###this works 
ggplot(qs_melt, aes(y=value, x=season, group = interaction(sd, season),  color = sd)) +
  geom_point(position = dodge, size = 4) +
  geom_line(size = 2, position = position_dodge(width = 0.6)) +
  labs(x = "Season", y = "Central 95% Area of Foraging Ellipses (ha)") +
  theme_bw(base_size = 18) +
  scale_y_continuous(labels = comma) + 
  coord_flip()+
  #facet_wrap(~sd, ncol = 1)
  theme(legend.position = "bottom")
