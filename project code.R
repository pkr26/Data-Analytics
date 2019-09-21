setwd('C:/Users/DILIP/Downloads')
library(tidyverse)
library(ggplot2)
library(e1071)
library(reshape2)
library(lubridate)
library(fitdistrplus)
#TASK-1
#IMPORTING THE DATA SET
#CLEANING THE DATA SET
# CHOOSING THE TARGET VARIABLE
#FINDING THE CORRELATIONS
housing_df<-read.csv("cal.csv",col.names = c('longitude','latitude','housing_median_age',
                                          'total_rooms','total_bedrooms','population',
                                          'households','median_income','median_house_value'),sep = ',')
str(housing_df)
head(housing_df)
View(housing_df)
summary(housing_df)
#CLEANING THE DATA WHICH MEANS REPLACING THE NULL VALUES WITH MEAN VALUES OF THE COLUMN OR ZERO DEPENDS ON THE MODEL 
checking_null<-is.na(housing_df)
View(checking_null)#we dont have any null values
View(cor(housing_df))
# SCATTERING PLOT OF LATITUDE AND LONGITUDE TO INDENTIFY THE LOCATION
ggplot(data=housing_df)+
  geom_point(mapping = aes(x=longitude,y=latitude),color='blue')
#TASK-2 UNDERSTANDING THE DATA
#VISUALISATION OF ALL THE COLUMNS
#HISTOGRAMS OF ALL THE COLUMNS
#1) HOUSING_MEDIAN_AGE
ggplot(data=housing_df)+
  geom_histogram(mapping = aes(x=housing_median_age),binwidth=1,color='black')
#2)TOTAL_ROOMS
ggplot(data=housing_df)+
  geom_histogram(mapping = aes(x=total_rooms),binwidth=300,color='black')
#3)TOTAL_BED_ROOMS
ggplot(data=housing_df)+
  geom_histogram(mapping = aes(x=total_bedrooms),binwidth=50,color='black')
#4)POPULATION
ggplot(data=housing_df)+
  geom_histogram(mapping = aes(x=population),binwidth=250,color='black')
#5)HOUSEHOLDS
ggplot(data=housing_df)+
  geom_histogram(mapping = aes(x=households),binwidth=50,color='black')
#6)TOTAL_BEDROOMS
ggplot(data=housing_df)+
  geom_histogram(mapping = aes(x=total_bedrooms),binwidth=50,color='black')
#7)MEDIAN_INCOME
ggplot(data=housing_df)+
  geom_histogram(mapping = aes(x=median_income),binwidth=0.3,color='black')
#8)MEDIAN_HOUSE_VALUE
ggplot(data=housing_df)+
  geom_histogram(mapping = aes(x=median_house_value),binwidth=9000,color='black')

#GROUPING THE DATASET BY LONGITUDE AND LATITUDE
grouping<-group_by(housing_df,longitude,latitude)
View(grouping)
#FINDING THE MEAN OF EACH COLUMN CORRESPONDING TO LATITUDE AND LONGITUDES
grouped_mean<-aggregate(grouping[,3:9],by=list(housing_df$longitude,housing_df$latitude),FUN=mean)
View(grouped_mean)
#GRAPH REPRESENTING THE RELATION BETWEEN MEDIAN_INCOME AND MEDIAN_HOUSE_VALUE 
#FOR A PARTICULAR LATITUDE AND LONGITUDE
ggplot(data=grouped_mean)+
  geom_point(mapping = aes(x=median_income,y=median_house_value),color='black')+
  xlab("median_income in 1k dollars pm")+
  ylab("median_house_value in millions")
#GRAPH REPRESENTING THE RELATION BETWEEN MEDIAN_HOUSE_VALUE AND TOTAL_ROOMS_PER_HOUSEHOLD
ggplot(data=grouped_mean)+
  geom_point(mapping = aes(x=median_house_value,y=(total_rooms/households)),color='black')+
  xlab("median_house_value in millions")+
  ylab("total_rooms PER HOUSEHOLDS")

#GRAPH REPRESENTING THE RELATION BETWEEN MEDIAN_HOUSE_VALUE AND TOTAL_BED_ROOMS

ggplot(data=grouped_mean)+
  geom_point(mapping = aes(x=median_house_value,y=round((total_bedrooms/households),digits=0)),color='black')+
  xlab("median_house_value in millions")+
  ylab("total_bedrooms PER  HOUSEHOLDS")
ggplot(data=grouped_mean)+
  geom_point(mapping = aes(x=median_house_value,y=(total_bedrooms/households)),color='black')+
  xlab("median_house_value in millions")+
  ylab("total_bedrooms PER HOUSEHOLDS")
#GRAPH REPRESENTING median_house_value and housing_median_age
ggplot(data=grouped_mean)+
  geom_point(mapping = aes(x=median_house_value,y=housing_median_age),color='black')+
  xlab("median_house_value in millions")+
  ylab("housing_median_age")
#TASK-3
#SAMPLING
#Housing_Median_age
pop_mean_age<-mean(housing_df$housing_median_age)
pop_mean_age
pop_sd_age<-sd(housing_df$housing_median_age)
pop_sd_age
a<-sample(housing_df[1:1000,1],size=100)
mean_sample1<-numeric(length = length(a))

for(i in 1:300){
  sam=sample(housing_df$housing_median_age[1:20639], size=100)
  y<-mean(sam)
  mean_sample1[i]<-y
}
mean_sample1
mean_means_age1<-mean(mean_sample1)
mean_means_age1
variance_sample1<-var(mean_sample1)
variance_sample1

mean_of_3001.df <- as.data.frame(mean_sample1)
str(mean_of_3001.df)
x1<-1:300
ggplot(data=mean_of_3001.df, aes(x=x1)) +
  geom_point(aes(y=mean_sample1,color="brown"))+
  geom_line(aes(y=pop_mean_age,color='green'))

#median_house_value

pop_mean_mhv<-mean(housing_df$median_house_value)
pop_mean_mhv
pop_var_mhv<-var(housing_df$median_house_value)
pop_var_mhv
a1<-sample(housing_df[1:10000,1],size=100)
mean_sample_hv<-numeric(length = length(a1))
for(i in 1:500){
  sam_hv=sample(housing_df$median_house_value[1:20639], size=100)
  y2<-mean(sam_hv)
  mean_sample_hv[i]<-y2
}
mean_sample_hv
mean_means_hv1<-mean(mean_sample_hv)
mean_means_hv1
variance_sample_hv1<-var(mean_sample_hv)
variance_sample_hv1

mean_of_5001.df <- as.data.frame(mean_sample_hv)
str(mean_of_5001.df)
x2<-1:500
ggplot(data=mean_of_5001.df, aes(x=x2)) +
  geom_point(aes(y=mean_sample_hv,color="brown"))+
  geom_line(aes(y=pop_mean_mhv,color='green'))



#Task4

#HOUSING_MEDIAN_AGE
#DESCRITIVE STATISTICS(DISTRIBUTION FIT)
ds3<-descdist(housing_df$housing_median_age)
ds3
fit3_nor<-fitdist(housing_df$housing_median_age, "norm")
summary(fit3_nor)
#GOODNESS-OF-FIT
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit3_nor), legendtext = plot.legend, xlab = 'housing_df$housing_median_age', xlegend = 'topleft')
cdfcomp (list(fit3_nor), legendtext = plot.legend, xlab = 'housing_df$housing_median_age')
qqcomp  (list(fit3_nor), legendtext = plot.legend, xlab = 'housing_df$housing_median_age')
ppcomp  (list(fit3_nor), legendtext = plot.legend, xlab = 'housing_df$housing_median_age')
# CONFIDENCE INTERVAL FOR HOUSING_MEDIAN_AGE
pop_sd_median_age<-sd(housing_df$housing_median_age)
pop_sd_median_age
sample_size<-c(100)
error<-((qnorm(0.97)*pop_sd_median_age)/sqrt(100))
error
left<-mean_means_age1-error
left
right<-mean_means_age1+error
right
#TOTAL_ROOMS
#DESCRITIVE STATISTICS(DISTRIBUTION FIT)
ds4<-descdist(housing_df$total_rooms)
ds4
fit4_ln<- fitdist(housing_df$total_rooms, "lnorm")
summary(fit4_ln)
#GOODNESS-OF-FIT
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit4_ln), legendtext = plot.legend, xlab = 'housing_df$total_rooms', xlegend = 'topleft')
cdfcomp (list(fit4_ln), legendtext = plot.legend, xlab = 'housing_df$total_rooms')
qqcomp  (list(fit4_ln), legendtext = plot.legend, xlab = 'housing_df$total_rooms')
ppcomp  (list(fit4_ln), legendtext = plot.legend, xlab = 'housing_df$total_rooms')
#TOTAL_BEDROOMS
ds5<-descdist(housing_df$total_bedrooms)
ds5
fit5_ln<- fitdist(housing_df$total_bedrooms, "lnorm")
summary(fit5_ln)
#GOODNESS-OF-FIT
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit5_ln), legendtext = plot.legend, xlab = 'housing_df$total_bedrooms', xlegend = 'topleft')
cdfcomp (list(fit5_ln), legendtext = plot.legend, xlab = 'housing_df$total_bedrooms')
qqcomp  (list(fit5_ln), legendtext = plot.legend, xlab = 'housing_df$total_bedrooms')
ppcomp  (list(fit5_ln), legendtext = plot.legend, xlab = 'housing_df$total_bedrooms')
#POPULATION
ds6<-descdist(housing_df$population)
ds6
fit6_ln<- fitdist(housing_df$population, "lnorm")
summary(fit6_ln)
#GOODNESS-OF-FIT
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit6_ln), legendtext = plot.legend, xlab = 'housing_df$population', xlegend = 'topleft')
cdfcomp (list(fit6_ln), legendtext = plot.legend, xlab = 'housing_df$population')
qqcomp  (list(fit6_ln), legendtext = plot.legend, xlab = 'housing_df$population')
ppcomp  (list(fit6_ln), legendtext = plot.legend, xlab = 'housing_df$population')
#HOUSEHOLDS
ds7<-descdist(housing_df$households)
ds7
fit7_ln<- fitdist(housing_df$households, "lnorm")
summary(fit7_ln)
#GOODNESS-OF-FIT
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit7_ln), legendtext = plot.legend, xlab = 'housing_df$households', xlegend = 'topleft')
cdfcomp (list(fit7_ln), legendtext = plot.legend, xlab = 'housing_df$households')
qqcomp  (list(fit7_ln), legendtext = plot.legend, xlab = 'housing_df$households')
ppcomp  (list(fit7_ln), legendtext = plot.legend, xlab = 'housing_df$households')
#median_income
ds8<-descdist(housing_df$median_income)
ds8
fit8_ln<- fitdist(housing_df$median_income, "lnorm")
summary(fit8_ln)
#GOODNESS-OF-FIT
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit8_ln), legendtext = plot.legend, xlab = 'housing_df$median_income', xlegend = 'topleft')
cdfcomp (list(fit8_ln), legendtext = plot.legend, xlab = 'housing_df$median_income')
qqcomp  (list(fit8_ln), legendtext = plot.legend, xlab = 'housing_df$median_income')
ppcomp  (list(fit8_ln), legendtext = plot.legend, xlab = 'housing_df$median_income')
#MEDIAN_HOUSE_VALUE
ds9<-descdist(housing_df$median_house_value)
ds9
fit9_ln<- fitdist(housing_df$median_house_value, "lnorm")
summary(fit9_ln)
#GOODNESS-OF-FIT
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit8_ln), legendtext = plot.legend, xlab = 'housing_df$median_house_value', xlegend = 'topleft')
cdfcomp (list(fit8_ln), legendtext = plot.legend, xlab = 'housing_df$median_house_value')
qqcomp  (list(fit8_ln), legendtext = plot.legend, xlab = 'housing_df$median_house_value')
ppcomp  (list(fit8_ln), legendtext = plot.legend, xlab = 'housing_df$median_house_value')

###TASK--5
## Hypothesis Testing
#one sample z-test is used when the population is normally distributed, and the population is known
#defining Hypothesis
#Null Hypothesis(H0): µ = µ0
#Alternate Hypothesis(Ha): µ!= µ0
# iam selecting my columns as house median_age which is normally distributed
# step 1: creating a sample
#step 2: Considering the significance value is alpha=0.01
#step 3: calculating
z_test<- (((mean_means_age1)-(pop_mean_age))/((pop_sd_age)/(sqrt(100))))
z_test
#Since the z-value lies within the range [-1.96, 1.96], we thus fail to reject null hypothesis 
#conclusion is that there is no significant difference between sample housing_median_age and population housing_median_age