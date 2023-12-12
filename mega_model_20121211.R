
setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr)
library(data.table)

library(ggplot2) #viz
library(plotly)

onezero<- read.csv("megadf_onezero_only_formodel_20231210.csv", stringsAsFactors = F)
onezero[c(143, 149:151, 153:156, 215, 217, 219, 221,227, 229)]<- NULL
onezero<- onezero[c(1:6, 104:123, 7:103, 124:215)]


#separate out northern and southern roosts, lets focus on southern roosts now 
# nr<-subset(onezero, lat_roost < -22)
# sr<-subset(onezero, lat_roost >= -22)

df<- as.data.frame(unique(onezero))
winter<- subset(df, month_num2 == 6 |  month_num2 == 7 | month_num2 == 8 )
winter<- subset(df, season == "winter" )

model<- df[c(3, 7,9,11,13:62, 64, 66, 68:104, 108, 113:116,142, 146, 149:160, 163:174, 177:188, 191:202, 210:213,206, 207,  214, 215)]
model.scld<- model

################################# Make sure to rescale variables #######################
### rescale important variables to be between 0 and 1, scales packaged produced negative values when there wasn't any
normalized <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
model.scld[, c(4:56, 59:70, 103:154)] <- lapply(model.scld[, c(4:56, 59:70, 103:154)], normalized) 

model.scld$camp_name<- as.factor(model.scld$camp_name)
#model.scld$bff.presence <- as.factor(model.scld$bff.presence)
model.scld$ghff.presence <- as.factor(model.scld$ghff.presence)
model.scld$lrff.presence <- as.factor(model.scld$lrff.presence)


complete<-model.scld[complete.cases(model.scld),]
t<-model.scld[!complete.cases(model.scld),] 


################### optimize lambda 
#set lambdas... go from 0 to 10^5, in 10 log steps
lambda <- 10^seq(-3,5, length=10)


#dummy vectors of model fit values for each lambda: BIC, AIC, prediction error
BIC_vec <- rep(Inf, length(lambda))
AIC_vec <- rep(Inf, length(lambda))
Devianz_ma<-NULL
Coeff_ma<-NULL


family = gaussian(link = "logit")

j<-1
for (j in 1:length(BIC_vec)){
  print(paste("Iteration ", j, sep=""))
  
  glm1 <- try( #i write out all  possible interaction effects... glmmLasso syntax doesn't allow for "*" notation
    glmmLasso(bff.presence ~ ghff.presence  +  lrff.presence 
              + prop_20kbuff_yrobs + morI_alldiet + morI_allwint + morI_typclwintdis + morI_atypclwintdis 
              + mean_csum_precip_20kbuff  + mean_maxtemp_20kbuff + mean_mintemp_20kbuff + mean_meantemp_20kbuff  
              + mean_prcp_3mo_w + mean_tmax_3mo_w + mean_tmin_3mo_w + mean_tmean_3mo_w
              ### Monthly raw weather values lagged 1-12 mo
              + prcp_lag1mo + prcp_lag2mo +  prcp_lag3mo +   prcp_lag4mo   +  prcp_lag5mo +    prcp_lag6mo
              + prcp_lag7mo +  prcp_lag8mo +prcp_lag9mo  + prcp_lag10mo + prcp_lag11mo+  prcp_lag12mo
              + tmax_lag1mo + tmax_lag2mo + tmax_lag3mo + tmax_lag4mo + tmax_lag5mo + tmax_lag6mo
              + tmax_lag7mo + tmax_lag8mo+ tmax_lag9mo + tmax_lag10mo + tmax_lag11mo +   tmax_lag12mo
              + tmin_lag1mo + tmin_lag2mo +tmin_lag3mo +  tmin_lag4mo  + tmin_lag5mo +  tmin_lag6mo
              + tmin_lag7mo  + tmin_lag8mo +tmin_lag9mo + tmin_lag10mo + tmin_lag11mo + tmin_lag12mo
              # Monthly anomaly weather values lagged 1-12 mo
              + prcpanom_lag1mo + prcpanom_lag2mo +  prcpanom_lag3mo +   prcpanom_lag4mo   +  prcpanom_lag5mo +    prcpanom_lag6mo
              + prcpanom_lag7mo +  prcpanom_lag8mo +prcpanom_lag9mo  + prcpanom_lag10mo + prcpanom_lag11mo+  prcpanom_lag12mo
              + tmaxanom_lag1mo + tmaxanom_lag2mo + tmaxanom_lag3mo + tmaxanom_lag4mo + tmaxanom_lag5mo + tmaxanom_lag6mo
              + tmaxanom_lag7mo + tmaxanom_lag8mo+ tmaxanom_lag9mo + tmaxanom_lag10mo + tmaxanom_lag11mo +   tmaxanom_lag12mo
              + tminanom_lag1mo + tminanom_lag2mo +tminanom_lag3mo +  tminanom_lag4mo  + tminanom_lag5mo +  tminanom_lag6mo
              + tminanom_lag7mo + tminanom_lag8mo +tminanom_lag9mo+ tminanom_lag10mo + tminanom_lag11mo + tminanom_lag12mo
              # Monthly changes in weather values sliding window 1-12 months
              + precip_diff0_1mo + precip_diff1_2mo +  precip_diff2_3mo +   precip_diff3_4mo   +  precip_diff4_5mo +    precip_diff5_6mo
              + precip_diff6_7mo +  precip_diff7_8mo +precip_diff8_9mo  + precip_diff9_10mo + precip_diff10_11mo+  precip_diff11_12mo
              + tmax_diff0_1mo + tmax_diff1_2mo + tmax_diff2_3mo + tmax_diff3_4mo + tmax_diff4_5mo + tmax_diff5_6mo
              + tmax_diff6_7mo + tmax_diff7_8mo+ tmax_diff8_9mo + tmax_diff9_10mo + tmax_diff10_11mo +   tmax_diff11_12mo
              + tmin_diff0_1mo +  tmin_diff1_2mo +tmin_diff2_3mo +  tmin_diff3_4mo  + tmin_diff4_5mo +  tmin_diff5_6mo
              + tmin_diff6_7mo  + tmin_diff7_8mo +tmin_diff8_9mo+ tmin_diff9_10mo + tmin_diff10_11mo + tmin_diff11_12mo
              + tmean_diff0_1mo +  tmean_diff1_2mo +tmean_diff2_3mo +  tmean_diff3_4mo  + tmean_diff4_5mo +  tmean_diff5_6mo
              + tmean_diff6_7mo + tmean_diff7_8mo +tmean_diff8_9mo+ tmean_diff9_10mo + tmean_diff10_11mo + tmean_diff11_12mo
              + ##########  LAGGED  PRECIP ANOMALIES
                + prop_20kbuff_yrobs:prcp_lag1mo
              + prop_20kbuff_yrobs:prcp_lag2mo
              + prop_20kbuff_yrobs:prcp_lag3mo
              + prop_20kbuff_yrobs:prcp_lag4mo
              + prop_20kbuff_yrobs:prcp_lag5mo
              + prop_20kbuff_yrobs:prcp_lag6mo
              + prop_20kbuff_yrobs:prcp_lag7mo
              + prop_20kbuff_yrobs:prcp_lag8mo
              + prop_20kbuff_yrobs:prcp_lag9mo
              + prop_20kbuff_yrobs:prcp_lag10mo
              + prop_20kbuff_yrobs:prcp_lag11mo
              + prop_20kbuff_yrobs:prcp_lag12mo


              ##########  LAGGED RAW PRECIPITATION VALUES
              + prop_20kbuff_yrobs:prcpanom_lag1mo
              + prop_20kbuff_yrobs:prcpanom_lag2mo
              + prop_20kbuff_yrobs:prcpanom_lag3mo
              + prop_20kbuff_yrobs:prcpanom_lag4mo
              + prop_20kbuff_yrobs:prcpanom_lag5mo
              + prop_20kbuff_yrobs:prcpanom_lag6mo
              + prop_20kbuff_yrobs:prcpanom_lag7mo
              + prop_20kbuff_yrobs:prcpanom_lag8mo
              + prop_20kbuff_yrobs:prcpanom_lag9mo
              + prop_20kbuff_yrobs:prcpanom_lag10mo
              + prop_20kbuff_yrobs:prcpanom_lag11mo
              + prop_20kbuff_yrobs:prcpanom_lag12mo



              ##########  LAGGED  PRECIP DIFFERENCES
              + prop_20kbuff_yrobs:precip_diff0_1mo
              + prop_20kbuff_yrobs:precip_diff1_2mo
              + prop_20kbuff_yrobs:precip_diff2_3mo
              + prop_20kbuff_yrobs:precip_diff3_4mo
              + prop_20kbuff_yrobs:precip_diff4_5mo
              + prop_20kbuff_yrobs:precip_diff5_6mo
              + prop_20kbuff_yrobs:precip_diff6_7mo
              + prop_20kbuff_yrobs:precip_diff7_8mo
              + prop_20kbuff_yrobs:precip_diff8_9mo
              + prop_20kbuff_yrobs:precip_diff9_10mo
              + prop_20kbuff_yrobs:precip_diff10_11mo
              + prop_20kbuff_yrobs:precip_diff11_12mo

              ########## 3 MONTH MOVING AVERAGE WINDOW OF PRECIPITATION #########
              + prop_20kbuff_yrobs:mean_prcp_3mo_w


              ##########  LAGGED TMAX RAW VALUES
              +  prop_20kbuff_yrobs:tmax_lag1mo
              +  prop_20kbuff_yrobs:tmax_lag2mo
              +  prop_20kbuff_yrobs:tmax_lag3mo
              +  prop_20kbuff_yrobs:tmax_lag4mo
              +  prop_20kbuff_yrobs:tmax_lag5mo
              +  prop_20kbuff_yrobs:tmax_lag6mo
              +  prop_20kbuff_yrobs:tmax_lag7mo
              +  prop_20kbuff_yrobs:tmax_lag8mo
              +  prop_20kbuff_yrobs:tmax_lag9mo
              +  prop_20kbuff_yrobs:tmax_lag10mo
              + prop_20kbuff_yrobs:tmax_lag11mo
              + prop_20kbuff_yrobs:tmax_lag12mo


              ##########  LAGGED TMAX ANOMALY VALUES
              +  prop_20kbuff_yrobs:tmaxanom_lag1mo
              +  prop_20kbuff_yrobs:tmaxanom_lag2mo
              +  prop_20kbuff_yrobs:tmaxanom_lag3mo
              +  prop_20kbuff_yrobs:tmaxanom_lag4mo
              +  prop_20kbuff_yrobs:tmaxanom_lag5mo
              +  prop_20kbuff_yrobs:tmaxanom_lag6mo
              +  prop_20kbuff_yrobs:tmaxanom_lag7mo
              +  prop_20kbuff_yrobs:tmaxanom_lag8mo
              +  prop_20kbuff_yrobs:tmaxanom_lag9mo
              +  prop_20kbuff_yrobs:tmaxanom_lag10mo
              + prop_20kbuff_yrobs:tmaxanom_lag11mo
              + prop_20kbuff_yrobs:tmaxanom_lag12mo

              ##########  LAGGED TMAX DIFFERENCES
              +  prop_20kbuff_yrobs:tmax_diff0_1mo
              +  prop_20kbuff_yrobs:tmax_diff1_2mo
              +  prop_20kbuff_yrobs:tmax_diff2_3mo
              +  prop_20kbuff_yrobs:tmax_diff3_4mo
              +  prop_20kbuff_yrobs:tmax_diff4_5mo
              +  prop_20kbuff_yrobs:tmax_diff5_6mo
              +  prop_20kbuff_yrobs:tmax_diff6_7mo
              +  prop_20kbuff_yrobs:tmax_diff7_8mo
              +  prop_20kbuff_yrobs:tmax_diff8_9mo
              +  prop_20kbuff_yrobs:tmax_diff9_10mo
              +  prop_20kbuff_yrobs:tmax_diff10_11mo
              +  prop_20kbuff_yrobs:tmax_diff11_12mo

              ##########  LAGGED  TMAX ANOMALIES AND 3mo MOVING WINDOW
              +  prop_20kbuff_yrobs:mean_tmax_3mo_w


              #######################################
              ##########  LAGGED tmin RAW VALUES
              +  prop_20kbuff_yrobs:tmin_lag1mo
              +  prop_20kbuff_yrobs:tmin_lag2mo
              +  prop_20kbuff_yrobs:tmin_lag3mo
              +  prop_20kbuff_yrobs:tmin_lag4mo
              +  prop_20kbuff_yrobs:tmin_lag5mo
              +  prop_20kbuff_yrobs:tmin_lag6mo
              +  prop_20kbuff_yrobs:tmin_lag7mo
              +  prop_20kbuff_yrobs:tmin_lag8mo
              +  prop_20kbuff_yrobs:tmin_lag9mo
              +  prop_20kbuff_yrobs:tmin_lag10mo
              + prop_20kbuff_yrobs:tmin_lag11mo
              + prop_20kbuff_yrobs:tmin_lag12mo


              ##########  LAGGED tmin ANOMALY VALUES
              +  prop_20kbuff_yrobs:tminanom_lag1mo
              +  prop_20kbuff_yrobs:tminanom_lag2mo
              +  prop_20kbuff_yrobs:tminanom_lag3mo
              +  prop_20kbuff_yrobs:tminanom_lag4mo
              +  prop_20kbuff_yrobs:tminanom_lag5mo
              +  prop_20kbuff_yrobs:tminanom_lag6mo
              +  prop_20kbuff_yrobs:tminanom_lag7mo
              +  prop_20kbuff_yrobs:tminanom_lag8mo
              +  prop_20kbuff_yrobs:tminanom_lag9mo
              +  prop_20kbuff_yrobs:tminanom_lag10mo
              + prop_20kbuff_yrobs:tminanom_lag11mo
              + prop_20kbuff_yrobs:tminanom_lag12mo

              ##########  LAGGED tmin DIFFERENCES
              +  prop_20kbuff_yrobs:tmin_diff0_1mo
              +  prop_20kbuff_yrobs:tmin_diff1_2mo
              +  prop_20kbuff_yrobs:tmin_diff2_3mo
              +  prop_20kbuff_yrobs:tmin_diff3_4mo
              +  prop_20kbuff_yrobs:tmin_diff4_5mo
              +  prop_20kbuff_yrobs:tmin_diff5_6mo
              +  prop_20kbuff_yrobs:tmin_diff6_7mo
              +  prop_20kbuff_yrobs:tmin_diff7_8mo
              +  prop_20kbuff_yrobs:tmin_diff8_9mo
              +  prop_20kbuff_yrobs:tmin_diff9_10mo
              +  prop_20kbuff_yrobs:tmin_diff10_11mo
              +  prop_20kbuff_yrobs:tmin_diff11_12mo

              ##########  LAGGED  tmin ANOMALIES AND 3mo MOVING WINDOW
              +  prop_20kbuff_yrobs:mean_tmin_3mo_w



              ###############
              ##########  LAGGED tmean RAW VALUES
              +  prop_20kbuff_yrobs:tmean_lag1mo
              +  prop_20kbuff_yrobs:tmean_lag2mo
              +  prop_20kbuff_yrobs:tmean_lag3mo
              +  prop_20kbuff_yrobs:tmean_lag4mo
              +  prop_20kbuff_yrobs:tmean_lag5mo
              +  prop_20kbuff_yrobs:tmean_lag6mo
              +  prop_20kbuff_yrobs:tmean_lag7mo
              +  prop_20kbuff_yrobs:tmean_lag8mo
              +  prop_20kbuff_yrobs:tmean_lag9mo
              +  prop_20kbuff_yrobs:tmean_lag10mo
              + prop_20kbuff_yrobs:tmean_lag11mo
              + prop_20kbuff_yrobs:tmean_lag12mo


              ##########  LAGGED tmean DIFFERENCES
              +  prop_20kbuff_yrobs:tmean_diff0_1mo
              +  prop_20kbuff_yrobs:tmean_diff1_2mo
              +  prop_20kbuff_yrobs:tmean_diff2_3mo
              +  prop_20kbuff_yrobs:tmean_diff3_4mo
              +  prop_20kbuff_yrobs:tmean_diff4_5mo
              +  prop_20kbuff_yrobs:tmean_diff5_6mo
              +  prop_20kbuff_yrobs:tmean_diff6_7mo
              +  prop_20kbuff_yrobs:tmean_diff7_8mo
              +  prop_20kbuff_yrobs:tmean_diff8_9mo
              +  prop_20kbuff_yrobs:tmean_diff9_10mo
              +  prop_20kbuff_yrobs:tmean_diff10_11mo
              +  prop_20kbuff_yrobs:tmean_diff11_12mo

              ##########  LAGGED  tmean ANOMALIES AND 3mo MOVING WINDOW
              +  prop_20kbuff_yrobs:mean_tmean_3mo_w,

              rnd = list(camp_name=~1), data = complete, 
              family = gaussian(link = "logit"),
              lambda = lambda[j],
              switch.NR = TRUE,
              final.re = TRUE),
    silent = TRUE)
  

  # code to make it continue anyway if an error occurs
  if(class(glm1)!="try-error")
  {
    
    #save BIC, AIC
    BIC_vec[j]<-glm1$bic
    AIC_vec[j]<-glm1$aic
    
    #save coefficient outputs
    Coeff_ma<-cbind(Coeff_ma,glm1$coefficients)
    
    #save error (deviance) values
    y.hat<-predict(glm1,complete)
    Devianz_ma[j]<-sum(family$dev.resids(complete$bff.presence, y.hat,wt=rep(1,length(y.hat))))
    
  }
}

lambda[which.min(BIC_vec)] #start at 507pm, itteration takes 1 in 

## [1] 27.8

lambda[which.min(AIC_vec)]

## [1] 0.0007

lambda[which.min(Devianz_ma)]
options(max.print = 100000)
final_lambda<-lambda[which.min(BIC_vec)] 
summary(glm1)











##################################################################################################
################################### M O D E L I N G ##############################################
##################################################################################################


library(glmmLasso)
##glmmLasso attempt 12/11/23

las1<- glmmLasso(bff.presence ~ ghff.presence  +  lrff.presence 
                 + prop_20kbuff_yrobs + morI_alldiet + morI_allwint + morI_typclwintdis + morI_atypclwintdis 
                 + mean_csum_precip_20kbuff  + mean_maxtemp_20kbuff + mean_mintemp_20kbuff + mean_meantemp_20kbuff  
                 + mean_prcp_3mo_w + mean_tmax_3mo_w + mean_tmin_3mo_w + mean_tmean_3mo_w
                 ### Monthly raw weather values lagged 1-12 mo
                 + prcp_lag1mo + prcp_lag2mo +  prcp_lag3mo +   prcp_lag4mo   +  prcp_lag5mo +    prcp_lag6mo
                 + prcp_lag7mo +  prcp_lag8mo +prcp_lag9mo  + prcp_lag10mo + prcp_lag11mo+  prcp_lag12mo
                 + tmax_lag1mo + tmax_lag2mo + tmax_lag3mo + tmax_lag4mo + tmax_lag5mo + tmax_lag6mo
                 + tmax_lag7mo + tmax_lag8mo+ tmax_lag9mo + tmax_lag10mo + tmax_lag11mo +   tmax_lag12mo
                 + tmin_lag1mo + tmin_lag2mo +tmin_lag3mo +  tmin_lag4mo  + tmin_lag5mo +  tmin_lag6mo
                 + tmin_lag7mo  + tmin_lag8mo +tmin_lag9mo + tmin_lag10mo + tmin_lag11mo + tmin_lag12mo
                 # Monthly anomaly weather values lagged 1-12 mo
                 + prcpanom_lag1mo + prcpanom_lag2mo +  prcpanom_lag3mo +   prcpanom_lag4mo   +  prcpanom_lag5mo +    prcpanom_lag6mo
                 + prcpanom_lag7mo +  prcpanom_lag8mo +prcpanom_lag9mo  + prcpanom_lag10mo + prcpanom_lag11mo+  prcpanom_lag12mo
                 + tmaxanom_lag1mo + tmaxanom_lag2mo + tmaxanom_lag3mo + tmaxanom_lag4mo + tmaxanom_lag5mo + tmaxanom_lag6mo
                 + tmaxanom_lag7mo + tmaxanom_lag8mo+ tmaxanom_lag9mo + tmaxanom_lag10mo + tmaxanom_lag11mo +   tmaxanom_lag12mo
                 + tminanom_lag1mo + tminanom_lag2mo +tminanom_lag3mo +  tminanom_lag4mo  + tminanom_lag5mo +  tminanom_lag6mo
                 + tminanom_lag7mo + tminanom_lag8mo +tminanom_lag9mo+ tminanom_lag10mo + tminanom_lag11mo + tminanom_lag12mo
                 # Monthly changes in weather values sliding window 1-12 months
                 + precip_diff0_1mo + precip_diff1_2mo +  precip_diff2_3mo +   precip_diff3_4mo   +  precip_diff4_5mo +    precip_diff5_6mo
                 + precip_diff6_7mo +  precip_diff7_8mo +precip_diff8_9mo  + precip_diff9_10mo + precip_diff10_11mo+  precip_diff11_12mo
                 + tmax_diff0_1mo + tmax_diff1_2mo + tmax_diff2_3mo + tmax_diff3_4mo + tmax_diff4_5mo + tmax_diff5_6mo
                 + tmax_diff6_7mo + tmax_diff7_8mo+ tmax_diff8_9mo + tmax_diff9_10mo + tmax_diff10_11mo +   tmax_diff11_12mo
                 + tmin_diff0_1mo +  tmin_diff1_2mo +tmin_diff2_3mo +  tmin_diff3_4mo  + tmin_diff4_5mo +  tmin_diff5_6mo
                 + tmin_diff6_7mo  + tmin_diff7_8mo +tmin_diff8_9mo+ tmin_diff9_10mo + tmin_diff10_11mo + tmin_diff11_12mo
                 + tmean_diff0_1mo +  tmean_diff1_2mo +tmean_diff2_3mo +  tmean_diff3_4mo  + tmean_diff4_5mo +  tmean_diff5_6mo
                 + tmean_diff6_7mo + tmean_diff7_8mo +tmean_diff8_9mo+ tmean_diff9_10mo + tmean_diff10_11mo + tmean_diff11_12mo
                 + ##########  LAGGED  PRECIP ANOMALIES
                   + prop_20kbuff_yrobs:prcp_lag1mo
                 + prop_20kbuff_yrobs:prcp_lag2mo
                 + prop_20kbuff_yrobs:prcp_lag3mo
                 + prop_20kbuff_yrobs:prcp_lag4mo
                 + prop_20kbuff_yrobs:prcp_lag5mo
                 + prop_20kbuff_yrobs:prcp_lag6mo
                 + prop_20kbuff_yrobs:prcp_lag7mo
                 + prop_20kbuff_yrobs:prcp_lag8mo
                 + prop_20kbuff_yrobs:prcp_lag9mo
                 + prop_20kbuff_yrobs:prcp_lag10mo
                 + prop_20kbuff_yrobs:prcp_lag11mo
                 + prop_20kbuff_yrobs:prcp_lag12mo
                 
                 
                 ##########  LAGGED RAW PRECIPITATION VALUES
                 + prop_20kbuff_yrobs:prcpanom_lag1mo
                 + prop_20kbuff_yrobs:prcpanom_lag2mo
                 + prop_20kbuff_yrobs:prcpanom_lag3mo
                 + prop_20kbuff_yrobs:prcpanom_lag4mo
                 + prop_20kbuff_yrobs:prcpanom_lag5mo
                 + prop_20kbuff_yrobs:prcpanom_lag6mo
                 + prop_20kbuff_yrobs:prcpanom_lag7mo
                 + prop_20kbuff_yrobs:prcpanom_lag8mo
                 + prop_20kbuff_yrobs:prcpanom_lag9mo
                 + prop_20kbuff_yrobs:prcpanom_lag10mo
                 + prop_20kbuff_yrobs:prcpanom_lag11mo
                 + prop_20kbuff_yrobs:prcpanom_lag12mo
                 
                 
                 
                 ##########  LAGGED  PRECIP DIFFERENCES
                 + prop_20kbuff_yrobs:precip_diff0_1mo
                 + prop_20kbuff_yrobs:precip_diff1_2mo
                 + prop_20kbuff_yrobs:precip_diff2_3mo
                 + prop_20kbuff_yrobs:precip_diff3_4mo
                 + prop_20kbuff_yrobs:precip_diff4_5mo
                 + prop_20kbuff_yrobs:precip_diff5_6mo
                 + prop_20kbuff_yrobs:precip_diff6_7mo
                 + prop_20kbuff_yrobs:precip_diff7_8mo
                 + prop_20kbuff_yrobs:precip_diff8_9mo
                 + prop_20kbuff_yrobs:precip_diff9_10mo
                 + prop_20kbuff_yrobs:precip_diff10_11mo
                 + prop_20kbuff_yrobs:precip_diff11_12mo
                 
                 ########## 3 MONTH MOVING AVERAGE WINDOW OF PRECIPITATION #########
                 + prop_20kbuff_yrobs:mean_prcp_3mo_w
                 
                 
                 ##########  LAGGED TMAX RAW VALUES
                 +  prop_20kbuff_yrobs:tmax_lag1mo
                 +  prop_20kbuff_yrobs:tmax_lag2mo
                 +  prop_20kbuff_yrobs:tmax_lag3mo
                 +  prop_20kbuff_yrobs:tmax_lag4mo
                 +  prop_20kbuff_yrobs:tmax_lag5mo
                 +  prop_20kbuff_yrobs:tmax_lag6mo
                 +  prop_20kbuff_yrobs:tmax_lag7mo
                 +  prop_20kbuff_yrobs:tmax_lag8mo
                 +  prop_20kbuff_yrobs:tmax_lag9mo
                 +  prop_20kbuff_yrobs:tmax_lag10mo
                 + prop_20kbuff_yrobs:tmax_lag11mo
                 + prop_20kbuff_yrobs:tmax_lag12mo
                 
                 
                 ##########  LAGGED TMAX ANOMALY VALUES
                 +  prop_20kbuff_yrobs:tmaxanom_lag1mo
                 +  prop_20kbuff_yrobs:tmaxanom_lag2mo
                 +  prop_20kbuff_yrobs:tmaxanom_lag3mo
                 +  prop_20kbuff_yrobs:tmaxanom_lag4mo
                 +  prop_20kbuff_yrobs:tmaxanom_lag5mo
                 +  prop_20kbuff_yrobs:tmaxanom_lag6mo
                 +  prop_20kbuff_yrobs:tmaxanom_lag7mo
                 +  prop_20kbuff_yrobs:tmaxanom_lag8mo
                 +  prop_20kbuff_yrobs:tmaxanom_lag9mo
                 +  prop_20kbuff_yrobs:tmaxanom_lag10mo
                 + prop_20kbuff_yrobs:tmaxanom_lag11mo
                 + prop_20kbuff_yrobs:tmaxanom_lag12mo
                 
                 ##########  LAGGED TMAX DIFFERENCES
                 +  prop_20kbuff_yrobs:tmax_diff0_1mo
                 +  prop_20kbuff_yrobs:tmax_diff1_2mo
                 +  prop_20kbuff_yrobs:tmax_diff2_3mo
                 +  prop_20kbuff_yrobs:tmax_diff3_4mo
                 +  prop_20kbuff_yrobs:tmax_diff4_5mo
                 +  prop_20kbuff_yrobs:tmax_diff5_6mo
                 +  prop_20kbuff_yrobs:tmax_diff6_7mo
                 +  prop_20kbuff_yrobs:tmax_diff7_8mo
                 +  prop_20kbuff_yrobs:tmax_diff8_9mo
                 +  prop_20kbuff_yrobs:tmax_diff9_10mo
                 +  prop_20kbuff_yrobs:tmax_diff10_11mo
                 +  prop_20kbuff_yrobs:tmax_diff11_12mo
                 
                 ##########  LAGGED  TMAX ANOMALIES AND 3mo MOVING WINDOW
                 +  prop_20kbuff_yrobs:mean_tmax_3mo_w
                 
                 
                 #######################################
                 ##########  LAGGED tmin RAW VALUES
                 +  prop_20kbuff_yrobs:tmin_lag1mo
                 +  prop_20kbuff_yrobs:tmin_lag2mo
                 +  prop_20kbuff_yrobs:tmin_lag3mo
                 +  prop_20kbuff_yrobs:tmin_lag4mo
                 +  prop_20kbuff_yrobs:tmin_lag5mo
                 +  prop_20kbuff_yrobs:tmin_lag6mo
                 +  prop_20kbuff_yrobs:tmin_lag7mo
                 +  prop_20kbuff_yrobs:tmin_lag8mo
                 +  prop_20kbuff_yrobs:tmin_lag9mo
                 +  prop_20kbuff_yrobs:tmin_lag10mo
                 + prop_20kbuff_yrobs:tmin_lag11mo
                 + prop_20kbuff_yrobs:tmin_lag12mo
                 
                 
                 ##########  LAGGED tmin ANOMALY VALUES
                 +  prop_20kbuff_yrobs:tminanom_lag1mo
                 +  prop_20kbuff_yrobs:tminanom_lag2mo
                 +  prop_20kbuff_yrobs:tminanom_lag3mo
                 +  prop_20kbuff_yrobs:tminanom_lag4mo
                 +  prop_20kbuff_yrobs:tminanom_lag5mo
                 +  prop_20kbuff_yrobs:tminanom_lag6mo
                 +  prop_20kbuff_yrobs:tminanom_lag7mo
                 +  prop_20kbuff_yrobs:tminanom_lag8mo
                 +  prop_20kbuff_yrobs:tminanom_lag9mo
                 +  prop_20kbuff_yrobs:tminanom_lag10mo
                 + prop_20kbuff_yrobs:tminanom_lag11mo
                 + prop_20kbuff_yrobs:tminanom_lag12mo
                 
                 ##########  LAGGED tmin DIFFERENCES
                 +  prop_20kbuff_yrobs:tmin_diff0_1mo
                 +  prop_20kbuff_yrobs:tmin_diff1_2mo
                 +  prop_20kbuff_yrobs:tmin_diff2_3mo
                 +  prop_20kbuff_yrobs:tmin_diff3_4mo
                 +  prop_20kbuff_yrobs:tmin_diff4_5mo
                 +  prop_20kbuff_yrobs:tmin_diff5_6mo
                 +  prop_20kbuff_yrobs:tmin_diff6_7mo
                 +  prop_20kbuff_yrobs:tmin_diff7_8mo
                 +  prop_20kbuff_yrobs:tmin_diff8_9mo
                 +  prop_20kbuff_yrobs:tmin_diff9_10mo
                 +  prop_20kbuff_yrobs:tmin_diff10_11mo
                 +  prop_20kbuff_yrobs:tmin_diff11_12mo
                 
                 ##########  LAGGED  tmin ANOMALIES AND 3mo MOVING WINDOW
                 +  prop_20kbuff_yrobs:mean_tmin_3mo_w
                 
                 
                 
                 ###############
                 ##########  LAGGED tmean RAW VALUES
                 +  prop_20kbuff_yrobs:tmean_lag1mo
                 +  prop_20kbuff_yrobs:tmean_lag2mo
                 +  prop_20kbuff_yrobs:tmean_lag3mo
                 +  prop_20kbuff_yrobs:tmean_lag4mo
                 +  prop_20kbuff_yrobs:tmean_lag5mo
                 +  prop_20kbuff_yrobs:tmean_lag6mo
                 +  prop_20kbuff_yrobs:tmean_lag7mo
                 +  prop_20kbuff_yrobs:tmean_lag8mo
                 +  prop_20kbuff_yrobs:tmean_lag9mo
                 +  prop_20kbuff_yrobs:tmean_lag10mo
                 + prop_20kbuff_yrobs:tmean_lag11mo
                 + prop_20kbuff_yrobs:tmean_lag12mo
                 
                 
                 ##########  LAGGED tmean DIFFERENCES
                 +  prop_20kbuff_yrobs:tmean_diff0_1mo
                 +  prop_20kbuff_yrobs:tmean_diff1_2mo
                 +  prop_20kbuff_yrobs:tmean_diff2_3mo
                 +  prop_20kbuff_yrobs:tmean_diff3_4mo
                 +  prop_20kbuff_yrobs:tmean_diff4_5mo
                 +  prop_20kbuff_yrobs:tmean_diff5_6mo
                 +  prop_20kbuff_yrobs:tmean_diff6_7mo
                 +  prop_20kbuff_yrobs:tmean_diff7_8mo
                 +  prop_20kbuff_yrobs:tmean_diff8_9mo
                 +  prop_20kbuff_yrobs:tmean_diff9_10mo
                 +  prop_20kbuff_yrobs:tmean_diff10_11mo
                 +  prop_20kbuff_yrobs:tmean_diff11_12mo
                 
                 ##########  LAGGED  tmean ANOMALIES AND 3mo MOVING WINDOW
                 +  prop_20kbuff_yrobs:mean_tmean_3mo_w,
                 rnd = list(camp_name=~1), data =complete, 
                 lambda =final_lambda, family = binomial(link="logit"), final.re=TRUE)
summary(las1)

##################################### LAMBDA #########################################
