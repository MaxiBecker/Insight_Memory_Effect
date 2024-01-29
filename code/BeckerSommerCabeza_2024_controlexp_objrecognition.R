#### Code for Manuscript Becker, Sommer & Cabeza 2023 
#### Neural Mechanisms of Creative Problem Solving: From Representational Change to Memory Formation
#### Control analysis: Estimate object recognition in Mooney images (for gray scaled images)  
#### (c) almaxi@gmail.com
#### last update: 22/Jan/2024

rm(list= ls())

## load libraries ##############################################################
#library(sjPlot)
#library(sjmisc)
#library(knitr)
library(magrittr)
#library(sjlabelled)      
#library(sjstats) 
library(ggeffects)
library(tidyverse)
library(ggplot2)
#library(ggpubr)

### Functions ########

## summary function
data_summary <- function(data, varname, groupnames,n){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = (sd(x[[col]], na.rm =TRUE))/sqrt(n) ) 
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)}

## plotting function for hi vs lo insight
plot_multi_histogram_neu <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_density(alpha=0.6) + theme_classic()+
    geom_histogram(alpha=0.2, position="identity", aes(y = ..density..), color="gray") +
    labs(x=feature, y = "Density") +scale_fill_manual( values = c( "#ADEFD1FF","#00203FFF"))
  plt + guides(fill=guide_legend(title=label_column))
} 

#### load main data #####################################################
setwd('C:/Users/Maxi/Google Drive/studies/Maxi/IN_REVISION/PVI/GitHub_4_publication/REVISION1/data/control_experiments/') 
load("BeckerSommerCabeza_2024_objectrecog.Rda")
data[data$cor1 != 1 & !is.na(data$cor1),]$RT = NA

unique(data$subject)
gender = data%>% group_by(subject) %>% dplyr::count(sex, age)
alter=mean(gender$age, na.rm = T)
alter_sd = sd(gender$age, na.rm = T)
n= nrow(gender)
gender[gender$sex == 2 & !is.na(gender$sex),]$sex=1
geschlecht=sum(gender$sex, na.rm = T)

unique(data$subject)

##### Data analysis

# plot response time distribution for supplement
alldata1 <- data[ , c('RT','subject', 'moonie', 'vec_Gray') ]
agg_data_all1 <- alldata1 %>% group_by(moonie, vec_Gray) %>% dplyr::summarise(across(c("RT"), ~mean(., na.rm=T))) 
agg_data_all1 <- na.omit(agg_data_all1)

FigS3_RT<- plot_multi_histogram_neu(agg_data_all1[!is.na(agg_data_all1$RT) ,], 'RT',  'vec_Gray')+
  labs(x = "Mean response time in ms",y = "Density")+
  geom_vline(aes(xintercept=median(agg_data_all1$RT, na.rm =T)), color="black", linetype="longdash", size=.8)

#FigS3_RT_st<- plot_multi_histogram_neu(data[!is.na(data$RT) ,], 'RT', 'vec_Gray')+
#  labs(x = "Single trial: Response time in ms (all images)",y = "Density")+
#  geom_vline(aes(xintercept=median(data$RT, na.rm =T)), color="black", linetype="longdash", size=.8)

Fig <- ggarrange(FigS3_RT, 
                 common.legend = TRUE, legend = "none",
                 ncol = 1, nrow = 1)
Fig

# Median und Standabweichung - Threshold berechnen
RT_gray=  median(agg_data_all1$RT, na.rm =T)
RT_gray_sd=  sd(agg_data_all1$RT, na.rm =T)
RT_5sd=  RT_gray+5*RT_gray_sd
  
print(sprintf("Es nahmen n=%s subjects dran teil: Frauen n=%s,age=%s (SD=%s) median RT to identify the gray scaled images is: %s (SD=%s) and 3*SD for this object recognition time is %s", 
      n,  round(geschlecht, digits = 4), round(alter,2), round(alter_sd,2), RT_gray, round(RT_gray_sd, 1),  RT_5sd ))
  
  