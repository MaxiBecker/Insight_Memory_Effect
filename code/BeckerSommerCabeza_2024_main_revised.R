
#### Code for Manuscript Becker, Sommer & Cabeza 2023 
#### Neural Mechanisms of Creative Problem Solving: From Representational Change to Memory Formation
#### (c) almaxi@gmail.com
#### last update: 22/Jan/2024

    ## Insight Dimensions
    ## Auf einer Skala von 1 - 4:, (sehr unsicher) 1 <--  --> 4 (sehr sicher)
    ## Auf einer Skala von 1 - 4:,  (graduelle Loesung) 1 <-- --> 4 (ploetzliche Loesung)
    ## Auf einer Skala von 1 - 4:  emotion  (kein AHA) 1 <-- --> 4 (starker AHA)

    # "Aufgabe im MRT GESEHEN? 
    #	   1 = definitiv gesehen#
    #	   2 = vermutlich gesehen
    #	   3 = weiß ich nicht mehr
    #	   4 = vermutlich NICHT gesehen
    #	   5 = definitiv NICHT gesehen
    
    # Aufgabe im MRT GELÖST? 
    #    1 = definitiv gelöst
    #    2 = vermutlich gelöst
    #    3 = weiß ich nicht mehr
    #    4 = vermutlich NICHT gelöst
    #    5 = definitiv NICHT gelöst

rm(list= ls())
sessionInfo()

## load libraries ##############################################################
library(lme4)
library(lmerTest)
library(sjPlot)
library(sjmisc)
library(knitr)
library(magrittr)
library(sjlabelled)      
library(sjstats) 
library(ggeffects)
library(performance)
library(tidyverse)
library(glmmTMB)
library(effectsize)
library(ggplot2)
library(dplyr)
library(emmeans)
library(ggpattern)                           

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
        labs(x=feature, y = "Density") +scale_fill_manual( values = c("#f5d6e2", "#733e54"))
      plt + guides(fill=guide_legend(title=label_column))
    } 

#### load main data #####################################################
setwd('C:/Users/Maxi/Google Drive/studies/Maxi/IN_REVISION/PVI/GitHub_4_publication/REVISION1/data') 
load('BeckerSommerCabeza_2024_analyses.Rda') 

### 1) Behavioral data analysis  #############################################
    PPSS$RT_log = log(PPSS$RT)
    subjects = unique(PPSS$ID)
    behave_data = PPSS[PPSS$tp_fac == 1 & PPSS$ROI== "l_Amygdala",]
    
    #### filter MOONEY IDENTIFICATION PERFORMANCE MEASURES  
    
    behave_data$RT_correct = NA # response time for correctly solved trials
    behave_data[behave_data$cor2 == 1,]$RT_correct = behave_data[behave_data$cor2 == 1,]$RT
    behave_data$RT_incorrect = NA # response time for incorrectly solved trials
    behave_data[behave_data$cor2 == 0,]$RT_incorrect = behave_data[behave_data$cor2 == 0,]$RT
   
    behave_data$HII_ms = NA # all high insight trials
    behave_data[behave_data$insight_mediansplit == "HI-I"  ,]$HII_ms= 1
    
    behave_data$LOI_ms = NA # all low insight trials
    behave_data[behave_data$insight_mediansplit == "LO-I",]$LOI_ms= 1

    behave_data$HII_ms_correct = NA #  correctly solved high insight trials
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1 ,]$HII_ms_correct = 1

    behave_data$HII_ms_correct1 = NA 
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1 ,]$HII_ms_correct1 = 1
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1 ,]$HII_ms_correct1 = 0
    
    behave_data$LOI_ms_correct = NA #  correctly solved low insight trials
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1 ,]$LOI_ms_correct = 1

    behave_data$LOI_ms_correct1 = NA 
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1 ,]$LOI_ms_correct1 = 1
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1 ,]$LOI_ms_correct1 = 0
    
    behave_data$HII_ms_incorrect = NA #  incorrectly solved high insight trials
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 0 ,]$HII_ms_incorrect = 1

    behave_data$HII_ms_incorrect1 = NA
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 0 ,]$HII_ms_incorrect1 = 1
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 0 ,]$HII_ms_incorrect1 = 0
    
    behave_data$LOI_ms_incorrect = NA #  incorrectly solved low insight trials
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 0 ,]$LOI_ms_incorrect = 1

    behave_data$LOI_ms_incorrect1 = NA
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 0 ,]$LOI_ms_incorrect1 = 1
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 0 ,]$LOI_ms_incorrect1 = 0
    
    behave_data$HII_ms_RTcorrect = NA
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1,]$HII_ms_RTcorrect = behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1,]$RT
    
    behave_data$LOI_ms_RTcorrect = NA
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1,]$LOI_ms_RTcorrect = behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1,]$RT
    
    behave_data$HII_ms_RTincorrect = NA
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 0,]$HII_ms_RTincorrect = behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 0,]$RT
    
    behave_data$LOI_ms_RTincorrect = NA
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 0,]$LOI_ms_RTincorrect = behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 0,]$RT
    
    #### filter  SUBSEQUENT MEMORY MEASURES 
    
    see_thresh=2
    solve_thresh=2
    
    #### general image recognition
    behave_data$SME_rec_all1 = 0
    behave_data[ behave_data$SME_certain_see<=see_thresh & !is.na(behave_data$SME_certain_see),]$SME_rec_all1 =1
    behave_data[ behave_data$SME_certain_see>see_thresh & !is.na(behave_data$SME_certain_see) ,]$SME_rec_all1 =0
    
    #### Correct image recognition 
    behave_data$SME_rec_cor2_all1 = 0
    behave_data[ behave_data$cor2 == 1 & !is.na(behave_data$cor2) & behave_data$SME_certain_see<=see_thresh & !is.na(behave_data$SME_certain_see),]$SME_rec_cor2_all1 =1
    behave_data[ behave_data$cor2 == 1 & !is.na(behave_data$cor2) & behave_data$SME_certain_see>see_thresh & !is.na(behave_data$SME_certain_see) ,]$SME_rec_cor2_all1 =0
    
      ### just for Figure S5
      behave_data$SME_rec_cor2_HII=NA
      behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1 & !is.na(behave_data$cor2) & behave_data$SME_certain_see<=see_thresh & !is.na(behave_data$SME_certain_see) ,]$SME_rec_cor2_HII =1
      
      behave_data$SME_rec_cor2_LOI= NA 
      behave_data[behave_data$insight_mediansplit == "LO-I" &  behave_data$cor2 == 1 & !is.na(behave_data$cor2) & behave_data$SME_certain_see<=see_thresh & !is.na(behave_data$SME_certain_see) ,]$SME_rec_cor2_LOI =1
    
    #### Correct solution recall -> remember to correctyl having solved it
    behave_data$SME_rec_s_cor2_all = NA
    behave_data[ behave_data$cor2 == 1 & !is.na(behave_data$cor2) & behave_data$SME_certain_solve <=solve_thresh & !is.na(behave_data$SME_certain_solve )  ,]$SME_rec_s_cor2_all =1
    
      ### just for Figure S5
      behave_data$SME_rec_s_cor2_HII=NA
      behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1 & !is.na(behave_data$cor2) & behave_data$SME_certain_solve <=solve_thresh & !is.na(behave_data$SME_certain_solve )  ,]$SME_rec_s_cor2_HII =1
      
      behave_data$SME_rec_s_cor2_LOI=NA
      behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1 & !is.na(behave_data$cor2) & behave_data$SME_certain_solve <=solve_thresh & !is.na(behave_data$SME_certain_solve )  ,]$SME_rec_s_cor2_LOI =1
      
    
    ### correct identity recall
    behave_data$SME_solve_cor2 = NA
    behave_data[ behave_data$cor2 == 1 & !is.na(behave_data$cor2) &  behave_data$SME_certain_solve <=solve_thresh & !is.na(behave_data$SME_certain_solve) & behave_data$SME_solve == "Remembered" & !is.na(behave_data$SME_solve),]$SME_solve_cor2 =1

    behave_data$SME_solve1_cor2 = 0
    behave_data[ behave_data$cor2 == 1 & !is.na(behave_data$cor2)  & behave_data$SME_solve == "Remembered" & !is.na(behave_data$SME_solve),]$SME_solve1_cor2 =1
    #behave_data[ behave_data$cor2 == 1 & !is.na(behave_data$cor2)  & behave_data$SME_solve != "Remembered" & !is.na(behave_data$SME_solve),]$SME_solve1_cor2 =0
    
    behave_data$SME_nosolve_cor2 = NA
    behave_data[ behave_data$cor2 == 1 & !is.na(behave_data$cor2)  & behave_data$SME_solve != "Remembered" & !is.na(behave_data$SME_solve),]$SME_nosolve_cor2 =1
    
    
    ### just for Figure S5
    behave_data$SME_rem_cor2_HII = NA
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1 & !is.na(behave_data$cor2)  & behave_data$SME_solve == "Remembered" & !is.na(behave_data$SME_solve),]$SME_rem_cor2_HII =1
    
    behave_data$SME_forget_cor2_HII = NA
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1 & !is.na(behave_data$cor2)  & behave_data$SME_solve == "Forgotten" & !is.na(behave_data$SME_solve),]$SME_forget_cor2_HII =1
    
    behave_data$SME_rem_cor2_LOI = NA
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1 & !is.na(behave_data$cor2)  & behave_data$SME_solve == "Remembered" & !is.na(behave_data$SME_solve),]$SME_rem_cor2_LOI =1
    
    behave_data$SME_forget_cor2_LOI = NA
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1 & !is.na(behave_data$cor2)  & behave_data$SME_solve == "Forgotten" & !is.na(behave_data$SME_solve),]$SME_forget_cor2_LOI =1
    
    
  #### Fig.S4 Response time distribution divided by high and low insight solutions ####
  alldata1 <- behave_data[ , c('RT','RT_correct','ID','insight_mediansplit', "cor2") ]
  agg_data_all1 <- alldata1 %>% group_by(insight_mediansplit,ID) %>% dplyr::summarise(across(c("RT","RT_correct"), ~mean(., na.rm=T))) 
  agg_data_all1 <- na.omit(agg_data_all1)
  
  FigS3_RT<- plot_multi_histogram_neu(agg_data_all1[!is.na(agg_data_all1$RT),], 'RT', 'insight_mediansplit')+labs(x = "Solution time in sec (all)",y = "Density", fill = "Insight")+
    geom_vline(aes(xintercept=median(agg_data_all1[agg_data_all1$insight_mediansplit == "HI-I",]$RT, na.rm =T)), color="black", linetype="longdash", size=.8) +
    geom_vline(aes(xintercept=median(agg_data_all1[agg_data_all1$insight_mediansplit == "LO-I",]$RT, na.rm =T)), color="black", linetype="solid", size=.8)
 
  FigS3_RT_cor<- plot_multi_histogram_neu(agg_data_all1[!is.na(agg_data_all1$RT_correct),], 'RT_correct', 'insight_mediansplit')+labs(x = "Solution time in sec (correct)",y = "Density", fill = "Insight")+
    geom_vline(aes(xintercept=median(agg_data_all1[agg_data_all1$insight_mediansplit == "HI-I" ,]$RT_correct, na.rm =T)), color="black", linetype="longdash", size=.8) +
    geom_vline(aes(xintercept=median(agg_data_all1[agg_data_all1$insight_mediansplit == "LO-I" ,]$RT_correct, na.rm =T)), color="black", linetype="solid", size=.8)
 
  FigS3_RT_st<- plot_multi_histogram_neu(alldata1[!is.na(alldata1$RT),], 'RT', 'insight_mediansplit')+
                                labs(x = "Single trial: Solution time in sec (all)",y = "Density", fill = "Insight")+
    geom_vline(aes(xintercept=median(alldata1[alldata1$insight_mediansplit == "HI-I" ,]$RT, na.rm =T)), color="black", linetype="longdash", size=.8) +
    geom_vline(aes(xintercept=median(alldata1[alldata1$insight_mediansplit == "LO-I",]$RT, na.rm =T)), color="black", linetype="solid", size=.8)
  
  FigS3_RT_cor_st<- plot_multi_histogram_neu(alldata1[!is.na(alldata1$RT)  & alldata1$cor2==1,], 'RT', 'insight_mediansplit')+
    labs(x = "Single trial: Solution time in sec (correct)",y = "Density", fill = "Insight")+
    geom_vline(aes(xintercept=median(alldata1[alldata1$insight_mediansplit == "HI-I" & alldata1$cor2==1 ,]$RT, na.rm =T)), color="black", linetype="longdash", size=.8) +
    geom_vline(aes(xintercept=median(alldata1[alldata1$insight_mediansplit == "LO-I" & alldata1$cor2==1,]$RT, na.rm =T)), color="black", linetype="solid", size=.8)
  
#### Measurement Model: AHA Experience #####

  library(lavaan)
  insightfac <-   'Insight  =~ Certain + Aha  + Sudden 
                   Insight ~~ 1*Insight
                  '
  fit <- lavaan(insightfac, data=behave_data[behave_data$cor2 == 1 & (behave_data$RT >=1.5 & behave_data$RT <=9.5) ,]   , #& behave_data$I_RTcorrected==1
               auto.var=TRUE, auto.fix.first=F,
               auto.cov.lv.x=TRUE)
  summary(fit, fit.measures=TRUE)
 

  # create aggregated data per subject 
  behave_data0 = behave_data[ ,c('ID', 'cor2', 'cor1', 'RT', 'RT_correct', 'RT_incorrect',  'age', 'sex',  'Aha', 'Sudden', 'Certain', 
                                'HII_ms', 'HII_ms_correct', 'HII_ms_correct1','HII_ms_incorrect','HII_ms_incorrect1','LOI_ms', 
                                'LOI_ms_correct','LOI_ms_correct1','LOI_ms_incorrect', 'LOI_ms_incorrect1',
                              'HII_ms_RTcorrect','LOI_ms_RTcorrect', 'HII_ms_RTincorrect','LOI_ms_RTincorrect' ,
                              
                              'SME_rem_cor2_HII','SME_forget_cor2_HII','SME_rem_cor2_LOI','SME_forget_cor2_LOI',
                              'SME_rec_cor2_HII', 'SME_rec_cor2_LOI', 'SME_rec_s_cor2_HII','SME_rec_s_cor2_LOI'
                              )]
  behave_data_conditioncount1 <- behave_data0 %>% group_by(ID) %>% summarise(HI_I = sum(!is.na(HII_ms)),
                                                                             HI_I_cor = sum(!is.na(HII_ms_correct)),
                                                                             HI_I_false = sum(!is.na(HII_ms_incorrect)),
                                                                             LO_I = sum(!is.na(LOI_ms)),
                                                                             LO_I_cor = sum(!is.na(LOI_ms_correct)),
                                                                             LO_I_false = sum(!is.na(LOI_ms_incorrect)))
  
  behave_data_conditioncount2 <-  behave_data0[(behave_data0$ID!= 45 & behave_data0$ID!= 36),] %>% group_by(ID) %>% summarise(
                                                                              HII_rec_cor = sum(!is.na(SME_rec_cor2_HII)),
                                                                              LOI_rec_cor = sum(!is.na(SME_rec_cor2_LOI)),
                                                                              
                                                                              HII_rec_s_cor = sum(!is.na(SME_rec_s_cor2_HII)),
                                                                              LOI_rec_s_cor = sum(!is.na(SME_rec_s_cor2_LOI)),
                                                                              
                                                                              HII_rem_cor = sum(!is.na(SME_rem_cor2_HII)),
                                                                              HII_norem_cor = sum(!is.na(SME_forget_cor2_HII)),
                                                                              LOI_rem_cor = sum(!is.na(SME_rem_cor2_LOI)),
                                                                              LOI_norem_cor = sum(!is.na(SME_forget_cor2_LOI))
                                                                              )

  # depict frequencies of all conditions 
  library(data.table)
  behave_data_conditioncount_long1 <- as.data.frame(melt(setDT(behave_data_conditioncount1), id.vars = c("ID"), variable.name = "condition"))
  behave_data_conditioncount_long2 <- as.data.frame(melt(setDT(behave_data_conditioncount2), id.vars = c("ID"), variable.name = "condition"))
 
  #### Fig S5: Amount of trials for each insight and insight memory condition ####
  p1 <- ggplot(data = behave_data_conditioncount_long1,mapping = aes(x = condition, y = value, fill = condition))
  p1a <- p1+ geom_bar( position = 'dodge', stat = 'summary', fun.y = "mean") + 
    geom_errorbar(stat = 'summary', position = 'dodge',  width = 0.2, size = .95) + #,
    geom_jitter(position = position_jitter(width = 0.1, height = 0.1), color = "#949393")+
    scale_fill_manual(breaks = c('HI_I', 'HI_I_cor','HI_I_false','LO_I','LO_I_cor','LO_I_false' ),
                      values=c("#ba2864","#733e54","#d0cccc",  "#ff378a" ,"#f5d6e2","#d0cccc"))+ 
                      labs(x = "conditions",y = "absolute amount")+theme_classic() +
    scale_y_continuous(breaks = round(seq(min(behave_data_conditioncount_long1$value), max(behave_data_conditioncount_long1$value), by = 5),1))

  p2 <- ggplot(data = behave_data_conditioncount_long2,mapping = aes(x = condition, y = value, fill = condition))
  p2a <- p2+ geom_bar( position = 'dodge', stat = 'summary', fun.y = "mean") + 
    geom_errorbar(stat = 'summary', position = 'dodge',  width = 0.2, size = .95) + #,
    geom_jitter(position = position_jitter(width = 0.1, height = 0.1), color = "#949393")+
    scale_fill_manual(breaks = c('HII_rec_cor','LOI_rec_cor','HII_rec_s_cor','LOI_rec_s_cor', 'HII_rem_cor','HII_norem_cor','LOI_rem_cor','LOI_norem_cor'), ##'HI_I_rem', 'HI_I_norem','LO_I_rem','LO_I_norem',
                      values=c( "#733e54","#f5d6e2","#733e54","#f5d6e2" ,"#733e54","#d0cccc","#f5d6e2" ,"#d0cccc"))+ ##"#ba2864","#d0cccc","#ff378a" ,"#d0cccc",
    scale_y_continuous(breaks = round(seq(min(behave_data_conditioncount_long2$value), max(behave_data_conditioncount_long2$value), by = 5),1))+
    labs(x = "subsequent memory conditions",y = "absolute amount")+theme_classic() 
  
  
  
  #### Demographics ####
  behave_data1 = behave_data %>% group_by(ID) %>% summarise_all(funs(mean), na.rm =T)  
  
  b <- behave_data1 %>%dplyr::count( ID,sex); sum(b$sex)

  max(2021-behave_data1$age)#older participant
  min(2021-behave_data1$age)#youngest participant

  mean(2021-behave_data1[behave_data1$sex == 1,]$age)#average female age
  mean(2021-behave_data1[behave_data1$sex == 0,]$age)#average male age
  
  #### Behavioral Results - descriptives ####
  
        #insight measures
        mean(behave_data1$Certain, na.rm =T)
        sd(behave_data1$Certain, na.rm =T)
        
        mean(behave_data1$Sudden, na.rm =T)
        sd(behave_data1$Sudden, na.rm =T)
        
        mean(behave_data1$Aha, na.rm =T)
        sd(behave_data1$Aha, na.rm =T)
        
        #performance measures
        mean(behave_data1$cor1)
        sd(behave_data1$cor1)
        
        mean(behave_data1$cor2)*0.831 #chance correct
        sd(behave_data1$cor2)*0.831 #chance correct
        
        mean(behave_data1$HII_ms_correct1) 
        sd(behave_data1$HII_ms_correct1) 
        
        mean(behave_data1$LOI_ms_correct1)
        sd(behave_data1$LOI_ms_correct1)
        
        mean(behave_data1$HII_ms_incorrect1)
        sd(behave_data1$HII_ms_incorrect1)
        
        mean(behave_data1$LOI_ms_incorrect1)
        sd(behave_data1$LOI_ms_incorrect1)
        
        median(behave_data1$RT_correct)
        sd(behave_data1$RT_correct)
        
        median(behave_data1$HII_ms_RTcorrect)
        sd(behave_data1$HII_ms_RTcorrect)
        
        median(behave_data1$LOI_ms_RTcorrect, na.rm =T)
        sd(behave_data1$LOI_ms_RTcorrect, na.rm =T)
        
        median(behave_data1$HII_ms_RTincorrect, na.rm =T)
        sd(behave_data1$HII_ms_RTincorrect, na.rm =T)
        
        median(behave_data1$LOI_ms_RTincorrect, na.rm =T)
        sd(behave_data1$LOI_ms_RTincorrect, na.rm =T)
        
        # memory recognition
        mean(behave_data1[behave_data1$ID != 45,]$SME_rec_all1)
        sd(behave_data1[behave_data1$ID != 45,]$SME_rec_all1)
        
        mean(behave_data1[behave_data1$ID != 45,]$SME_rec_cor2_all1)*0.831 #chance correct
        sd(behave_data1[behave_data1$ID != 45,]$SME_rec_cor2_all1)*0.831 #chance correct
        
        # memory solution recall
        mean(behave_data1[behave_data1$ID != 45,]$SME_rec_s_cor2_all1)*0.831 #chance correct
        sd(behave_data1[behave_data1$ID != 45,]$SME_rec_s_cor2_all1)*0.831 #chance correct
        
        # memory identity 
        mean(behave_data1[behave_data1$ID != 45,]$SME_solve1_cor2)*0.831 #chance correct 
        sd(behave_data1[behave_data1$ID != 45,]$SME_solve1_cor2)*0.831 #chance correct
        
        
    ####correlation between accuracy and insight  ####
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1 & (PPSS$RT>=1.5 & PPSS$RT<=9.5),]
    M0_cor2 <- glmer(cor2 ~             +sessionblock  + (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_cor2 <- glmer(cor2 ~ insight_sum +sessionblock  + (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    anova(M0_cor2, M1_cor2)
    tab_model(M1_cor2, show.std = T)

    ####correlation between solution time and insight ####
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1 &  (PPSS$RT>=1.5 & PPSS$RT<=9.5) & PPSS$cor2 == 1,]
    M0_RT <- lmer(log(RT) ~                      sessionblock+(1|ID) + (1|Item),data= data_new, na.action  = na.omit)
    M1_RT <- lmer(log(RT) ~ insight_sum +sessionblock+(1|ID) + (1|Item),data= data_new, na.action  = na.omit)
    hist(resid(M1_RT))
    anova(M0_RT, M1_RT)
    tab_model(M1_RT, show.std  = T)

    #### Fig2-A: overall amount of solved trials divided by condition: HI-L LO-I and not solved ####
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1 & (PPSS$cor2 !=3 | is.na(PPSS$RT)),] #&  PPSS$I_RTcorrected==1
    behave_data_no_solve = data_new[data_new$insight_mediansplit == "not solved",] %>% group_by(ID) %>% dplyr::count(insight_mediansplit)
    behave_data_HI_I = data_new[data_new$insight_mediansplit == "HI-I",] %>% group_by(ID) %>% dplyr::count(insight_mediansplit)
    behave_data_LO_I = data_new[data_new$insight_mediansplit == "LO-I",] %>% group_by(ID) %>% dplyr::count(insight_mediansplit)
    behave_data_HI_I$n =  100*(behave_data_HI_I$n/120)
    behave_data_no_solve$n =  100*(behave_data_no_solve$n/120)
    behave_data_LO_I$n =  100*(behave_data_LO_I$n/120)
    descriptives = data.frame(behave_data_HI_I$ID, behave_data_HI_I$n, behave_data_no_solve$n, behave_data_LO_I$n) 
    descriptives_long <- descriptives %>% pivot_longer(cols = 'behave_data_HI_I.n':'behave_data_LO_I.n', names_to='condition', values_to="amount")
    descriptives_long$condition <- gsub('behave_data_', '', as.character(descriptives_long$condition)) 
    descriptives_long$condition <- gsub('.n', '', as.character(descriptives_long$condition)) 
    descriptives_long$condition <- gsub('HI_I', 'HI-I', as.character(descriptives_long$condition)) 
    descriptives_long$condition <- gsub('LO_I', 'LO-I', as.character(descriptives_long$condition)) 
    descriptives_long$condition <- gsub('no_solve', 'not solved', as.character(descriptives_long$condition)) 
    descriptives_long$condition_ord = ordered(descriptives_long$condition, levels=c("not solved" ,"LO-I","HI-I"  ),
                                                  labels=c("not solved" ,"LO-I","HI-I"))
    
    p <- ggplot(descriptives_long, aes(condition_ord,amount,fill = condition))
    AHA_amount_plot <- p+ geom_bar( position = 'dodge', stat = 'summary', fun.y = 'mean') + #, color = "black"
      geom_errorbar(stat = 'summary', position = 'dodge', width = 0.2, size = .95) +
      geom_jitter(position = position_jitter(width = 0.1, height = 0.1), color = "#949393")+ #+ shape = 21, 
      labs(title = "during fMRI",x = "Insight (categorical)",y = "overall amount in %")+theme_classic()+scale_y_continuous(limits = c(0, 70))+
      scale_fill_manual(breaks = c('not solved','LO-I', 'HI-I'),values=c("#d0cccc", "#f5d6e2", "#733e54"))
                         
    # False Alarm Rate for new pictures
    Cor.Rejection = c(48,	52,	53,	24,	42,	60,	46,	53,	34,	55,	50,	58,	55,	51,	53,	51,	43,	48,	33,	39,	57,	56,	47,	53,	51,	53,	59,	43,	46,	53,	35) #Anzahl der richtig rejecteten neuen Mooney Bilder (es gab 60 Neue) 
    Cor.Rejection_perc = (100*Cor.Rejection)/60
    FA = 100-Cor.Rejection_perc
    mean(FA); sd(FA)
    FA = as.data.frame(FA)
    FA$n =100-Cor.Rejection_perc 
    
    # behavioral generation and insight memory effect not controlled for RT
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1 & ((PPSS$RT>=1.5 & PPSS$RT<=9.5 & PPSS$cor2 == 1)|is.na(PPSS$RT) ) & PPSS$ID != 45,]
    M0_IME <- glmer(SME_solve_fac ~ scale(sessionblock)+                        (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_IME <- glmer(SME_solve_fac ~ scale(sessionblock)+insight_mediansplit_fac+ (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    anova(M0_IME, M1_IME)
    tab_model(M1_IME)
    emmeans(M1_IME, list(pairwise ~ insight_mediansplit_fac  ), adjust = "tukey")      

    #  Fig.2-B for display purproses (without RT)
     IME_ggpredict =ggpredict(M1_IME , c(  'insight_mediansplit_fac'))
     IME_ggpredict_plot = ggplot(IME_ggpredict, aes(x= x, y = predicted*100 , fill=  x)) +
      geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
      geom_errorbar(aes(ymin=conf.low*100 , ymax=conf.high*100 ), width=.2,size = .95,position=position_dodge(.9)) +
                      theme_classic() +labs(title = "5 days after fMRI",x = "Insight (categorical)",y = "Mooney solution recall in %")+
                      scale_y_continuous(limits = c(0, 90))+
                      scale_fill_manual(breaks = c('not solved','LO-I','HI-I'),values=c('#babcba', "#f5d6e2", "#733e54"))

    # behavioral insight memory effect additionally controlled for RT (note, has no "not solved" condition anymore)
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1 & (PPSS$RT>=1.5 & PPSS$RT<=9.5) & (PPSS$cor2 == 1) & PPSS$ID != 45,]
    M0_IME_RT <- glmer(SME_solve_fac ~  sessionblock + scale(RT)                      +(1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_IME_RT <- glmer(SME_solve_fac ~  sessionblock + scale(RT)+ insight_sum+(1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    anova(M0_IME_RT, M1_IME_RT)
    tab_model(M1_IME_RT)

    # Fig.2-C
    library(ggeffects)
    IME_RT_ggplot = ggpredict(M1_IME_RT , c('insight_sum'))  
    IME_RT_finalPlot= ggplot(IME_RT_ggplot, aes(x= x, y = predicted, fill = predicted)) + #
      scale_fill_gradient(low="#f5d6e2",high="#733e54")+
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T) + 
      geom_errorbar(aes(ymin=predicted-std.error , ymax=predicted+std.error ), width=.2, size = 1.1, position=position_dodge(.9))+ 
      theme_classic() +
      labs(title = "5 days after fMRI", x = "Insight (continuous)", y = "Mooney solution recall in %", fill = "Solution style")+
      scale_y_continuous(labels = scales::percent)+ theme(legend.position = "none")
    
    #IME_RT_finalPlot+plot(IME_RT_ggplot, add.data = TRUE, jitter = 0.1, dot.alpha = .1)  
    
    # Control analysis: behavioral insight memory effect additionally adjusted for RT (RT difference between HI-I & LO-I, p>.2)
    PPSS$I_RTcorrected1 = 0
    PPSS[PPSS$insight_mediansplit == "HI-I" & ((PPSS$RT >=2.2 &  PPSS$RT<=10) | is.na(PPSS$RT) ) ,]$I_RTcorrected1 =1 #
    PPSS[PPSS$insight_mediansplit == "LO-I" & ((PPSS$RT >=1.5& PPSS$RT<=6.1) | is.na(PPSS$RT) ) ,]$I_RTcorrected1 =1 #6.8
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1 & PPSS$I_RTcorrected1==1 & (PPSS$cor2 == 1) & PPSS$ID != 45,]
    M0_RT_adj <- lmer(log(RT) ~              sessionblock+(1|ID) + (1|Item),data= data_new, na.action  = na.omit)
    M1_RT_adj <- lmer(log(RT) ~ insight_sum +sessionblock+(1|ID) + (1|Item),data= data_new, na.action  = na.omit)
    anova(M0_RT_adj, M1_RT_adj)
    
    M0_IME_RT_adj <- glmer(SME_solve_fac ~  sessionblock +             +(1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_IME_RT_adj <- glmer(SME_solve_fac ~  sessionblock + insight_sum +(1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    anova(M0_IME_RT_adj, M1_IME_RT_adj)
    tab_model(M1_IME_RT_adj)
    
#### Association with verbal insight tasks: Anagrams  #######
    ## check if insight in Mooney images correlates with insight in anagrams
    load("BeckerSommerCabeza_2024_anagram_mooney_cor.Rda")
    
    library(reshape2)
    data_wide =  reshape(anagrams1, v.names=c('insight_sum', 'insight_mediansplit' ), timevar=c("PV"), 
                         idvar=c('cor2', 'cor1', 'RT','ID', 'Item', 'sessionblock'),direction="wide") 
    data_wide = data_wide[!is.na(data_wide$Item),]
    
    AM_subjdata <-  data_wide %>% group_by(ID) %>%  dplyr::summarise(across(c("insight_sum.V","insight_sum.P"), ~mean(., na.rm=T)))
    
    cor.test(AM_subjdata$insight_sum.P, AM_subjdata$insight_sum.V)
    
    #Fig.2-D
    AM_plot = ggplot(AM_subjdata, aes(x= insight_sum.P, y = insight_sum.V )) + geom_point()+ theme_classic()+ geom_smooth(method=lm) +
      labs( x = "Mooney images: Insight (continuous) ",y = 'Anagrams: Insight (continuous)' ) 
    

### 2) Univariate ROI analysis in Amy & HC #################

##### 2a) Amy & HC: Insight*Memory: with single trials betas (standard space) #####  
    newdata = PPSS[ 
     ( PPSS$ROI == "r_Amygdala"  |
        PPSS$ROI == "l_Amygdala"  |
        PPSS$ROI == "l_aHC"  |
        PPSS$ROI == "r_aHC"  |
        PPSS$ROI == "l_pHC" |
        PPSS$ROI == "r_pHC"),] 
    
    newdata$ROI_bilat = NA
    newdata[newdata$ROI == "r_Amygdala",]$ROI_bilat = 'Amy'
    newdata[newdata$ROI == "l_Amygdala",]$ROI_bilat = 'Amy'
    newdata[newdata$ROI == "l_aHC",]$ROI_bilat = 'aHC'
    newdata[newdata$ROI == "r_aHC",]$ROI_bilat = 'aHC'
    newdata[newdata$ROI == "r_pHC",]$ROI_bilat = 'pHC'
    newdata[newdata$ROI == "l_pHC",]$ROI_bilat = 'pHC'

    # Amy & HC Insight solution time as covariate
    Activity_data = newdata[ newdata$tp == 1 & newdata$cor2 == 1 & (!newdata$RT<1.5 & !newdata$RT>9.5) ,] #!is.na(newdata$insight_mediansplit)  &
    Activity_data$xRT = scale(Activity_data$RT)
    Activity_data$ROI_bilat_ord = ordered(Activity_data$ROI_bilat, levels=c("Amy" ,"aHC","pHC"),labels=c("Amy" ,"aHC","pHC"))

    Activity_M1 <- lmer(Beta_Buttonpress ~ sessionblock +xRT+           +ROI_bilat_ord+(1|ID) + (1|Item),data= Activity_data ,na.action  = na.omit)
    Activity_M2 <- lmer(Beta_Buttonpress ~ sessionblock +xRT+insight_sum+ROI_bilat_ord+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M3 <- lmer(Beta_Buttonpress ~ sessionblock +xRT+insight_sum*ROI_bilat_ord+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)

    hist(residuals(Activity_M3)) # residuals are normally distributed -> model is adecuate
    anova( Activity_M1,Activity_M2,Activity_M3)
    plot(check_collinearity(Activity_M2))
    tab_model(Activity_M2, show.std = T)
    #PostHoc Test
    library("modelbased")
    estimate_slopes(Activity_M3, trend = "insight_sum", at = "ROI_bilat_ord")#,
    
    ##### Fig.4: Amy & HC activity during solution are associated with insight ####
    Amy_HC_ggplot = ggpredict(Activity_M3 , c('ROI_bilat_ord' ,'insight_sum'))#%>%plot()+ggplot2::theme_classic()
    Amy_HC_finalPlot= ggplot(Amy_HC_ggplot, aes(x= x, y = predicted, fill = group )) +#scale_fill_gradient(low="#37c0c6",high="#f98578")+#+ scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))+
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +#coord_cartesian(ylim=c(0.71,.93))+
      theme_classic() +labs( fill = "Insight", x = "",y ="Beta estimate during solution"  ) +
      scale_fill_manual(breaks = c('6.9', '9','11.2'),values=c("#f5d6e2", "#cd7397","#733e54" ))+
      geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,position=position_dodge(.9))  # + #+color = "black"
    
    #SUPPL: adjusted for solution time (throw out fast HII and slow LOI until: p>.2)
    Activity_data$I_RTcorrected1 = 0
    Activity_data[ Activity_data$insight_mediansplit == "HI-I" & Activity_data$RT >=2.206 & Activity_data$RT <=9.5 & !is.na(Activity_data$RT) & Activity_data$cor2 == 1 ,]$I_RTcorrected1 =1 
    Activity_data[ Activity_data$insight_mediansplit == "LO-I" & Activity_data$RT >=1.5 & Activity_data$RT <=5.9 & !is.na(Activity_data$RT) & Activity_data$cor2 == 1 ,]$I_RTcorrected1 = 1 #6.2
    summary(lmer( log(RT) ~ insight_sum+ (1|ID) + (1|Item),data= Activity_data[Activity_data$I_RTcorrected1==1 & !is.na(Activity_data$RT),], na.action = na.omit))
    
    Activity_data1 = Activity_data[Activity_data$I_RTcorrected1==1 & Activity_data$cor2 ==1 & !is.na(Activity_data$SME_solve_fac)  ,]
    Activity_M1_adj <- lmer(Beta_Buttonpress ~sessionblock +ROI_bilat+            (1|ID) + (1|Item),data= Activity_data1 ,na.action  = na.omit)
    Activity_M2_adj <- lmer(Beta_Buttonpress ~sessionblock +ROI_bilat+insight_sum+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M3_adj <-lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat*insight_sum+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)

    hist(residuals(Activity_M3_adj)) # residuals are normally distributed -> model is adecuate
    anova( Activity_M1_adj,Activity_M2_adj,Activity_M3_adj)
    plot(check_collinearity(Activity_M2_adj))
    tab_model(Activity_M2_adj, show.std =T)

#####################################################################################-
    ##### 2b) Amy & HC: Control analysis - Parametric modulation #####################
    
    ###### Insight parametrical modulation in subject space
    r_Amy    <- read.table("univariate_results_parametric_modulation/s2_ALL_AHA_cont_RT_SubjSp_BP_r_Amy_22-Dec-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_Amy    <- read.table("univariate_results_parametric_modulation/s2_ALL_AHA_cont_RT_SubjSp_BP_l_Amy_22-Dec-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    r_aHC    <- read.table("univariate_results_parametric_modulation/s2_ALL_AHA_cont_RT_SubjSp_BP_r_aHC_22-Dec-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_aHC    <- read.table("univariate_results_parametric_modulation/s2_ALL_AHA_cont_RT_SubjSp_BP_l_aHC_22-Dec-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    r_pHC    <- read.table("univariate_results_parametric_modulation/s2_ALL_AHA_cont_RT_SubjSp_BP_r_pHC_22-Dec-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_pHC    <- read.table("univariate_results_parametric_modulation/s2_ALL_AHA_cont_RT_SubjSp_BP_l_pHC_22-Dec-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    
    Amy= data.frame(matrix(nrow = 31, ncol = 1))
    Amy$ID = l_Amy$ID
    Amy$vis_AHA_1 <- (l_Amy$vis_AHA_bl.1sek_1   + r_Amy$vis_AHA_bl.1sek_1)/2
    Amy$vis_AHA_2 <- (l_Amy$vis_AHA_bl.1sek_2   + r_Amy$vis_AHA_bl.1sek_2)/2
    Amy$vis_AHA_3 <- (l_Amy$vis_AHA_bl.1sek_3   + r_Amy$vis_AHA_bl.1sek_3)/2
    Amy$vis_AHA_4 <- (l_Amy$vis_AHA_bl.1sek_4   + r_Amy$vis_AHA_bl.1sek_4)/2
    
    aHC= data.frame(matrix(nrow = 31, ncol = 1))
    aHC$ID = l_aHC$ID
    aHC$vis_AHA_1 <- (l_aHC$vis_AHA_bl.1sek_1   + r_aHC$vis_AHA_bl.1sek_1)/2
    aHC$vis_AHA_2 <- (l_aHC$vis_AHA_bl.1sek_2   + r_aHC$vis_AHA_bl.1sek_2)/2
    aHC$vis_AHA_3 <- (l_aHC$vis_AHA_bl.1sek_3   + r_aHC$vis_AHA_bl.1sek_3)/2
    aHC$vis_AHA_4 <- (l_aHC$vis_AHA_bl.1sek_4   + r_aHC$vis_AHA_bl.1sek_4)/2
    
    pHC= data.frame(matrix(nrow = 31, ncol = 1))
    pHC$ID = l_pHC$ID
    pHC$vis_AHA_1 <- (l_pHC$vis_AHA_bl.1sek_1   + r_pHC$vis_AHA_bl.1sek_1)/2
    pHC$vis_AHA_2 <- (l_pHC$vis_AHA_bl.1sek_2   + r_pHC$vis_AHA_bl.1sek_2)/2
    pHC$vis_AHA_3 <- (l_pHC$vis_AHA_bl.1sek_3   + r_pHC$vis_AHA_bl.1sek_3)/2
    pHC$vis_AHA_4 <- (l_pHC$vis_AHA_bl.1sek_4   + r_pHC$vis_AHA_bl.1sek_4)/2
    
    data_HILOI <- rbind(Amy[,-1],aHC[,-1],pHC[,-1])#  
    data_HILOI$roi=  rep(c('Amy','aHC','pHC'), each=31)# 
    library(tidyr)
    data_AHA_long <- data_HILOI %>% pivot_longer(cols = 'vis_AHA_1':'vis_AHA_4', names_to='condition', values_to="betas")
    data_AHA_long$session = rep(c(1,2,3,4), 1*31*3) #'1*conds*31subj*3ROIs'
    data_AHA_long$condition <- gsub('vis_', '', as.character(data_AHA_long$condition)) 
    data_AHA_long$condition <- gsub('_1', '', as.character(data_AHA_long$condition)) 
    data_AHA_long$condition <- gsub('_2', '', as.character(data_AHA_long$condition)) 
    data_AHA_long$condition <- gsub('_3', '', as.character(data_AHA_long$condition)) 
    data_AHA_long$condition <- gsub('_4', '', as.character(data_AHA_long$condition)) 
    
    data_AHA_long$roi_ord = ordered(data_AHA_long$roi, levels=c("Amy" ,"aHC","pHC"  ),labels=c("Amy" ,"aHC","pHC"))
    
    ### test for amygdala and hippocampal activity during (correct) solution as a function of Insight
    AHA_m0_AmyHC <- lmer(betas ~     session +(1|ID) ,data= data_AHA_long, na.action  = na.omit) #[data_AHA_long$roi == "Amy",]
    AHA_m1_AmyHC <- lmer(betas ~ roi+session +(1|ID) ,data= data_AHA_long, na.action  = na.omit) 
    hist(residuals(AHA_m0_AmyHC)) # residuals are normally distributed -> model is adecuate
    summary(AHA_m1_AmyHC)
    anova(AHA_m0_AmyHC,AHA_m1_AmyHC)
    tab_model(AHA_m0_AmyHC, show.std = T)

    ### PostHoc: test for individual amygdala OR hippocampal activity during (correct) solution as a function of Insight
    AHA_m0_Amy <- lmer(betas ~ session +(1|ID) ,data= data_AHA_long[data_AHA_long$roi == "aHC",], na.action  = na.omit) #aHC, pHC, Amy
    hist(residuals(AHA_m0_Amy)) # residuals are normally distributed -> model is adecuate
    summary(AHA_m0_Amy)
    tab_model(AHA_m0_Amy)

######## Exploratory: which dimension is driving the univariate insight effect? (standard space, single trial analysis) ######
    newdata = PPSS[  PPSS$ROI_bilat == "Amy",] #aHC, pHC
    Activity_data = newdata[ newdata$tp == 1 & newdata$cor2 == 1 & (newdata$RT>=1.5 & newdata$RT<=9.5)  ,] 
    Activity_data$xRT = scale(Activity_data$RT)
    Activity_M4 <- lmer(Beta_Buttonpress ~ sessionblock+xRT+Aha+Sudden+Certain+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)

    plot(check_collinearity(Activity_M4))
    hist(residuals(Activity_M4)) # residuals are normally distributed -> model is adecuate
    summary(Activity_M4);    
    tab_model(Activity_M4, show.std =T) #get effectsize

    ##### 2c) Amy & HC Insight Memory #####
    newdata = PPSS[  PPSS$ROI_bilat == "Amy" | PPSS$ROI_bilat == "aHC" | PPSS$ROI_bilat == "pHC",] #aHC, pHC
    Activity_data = newdata[ (newdata$RT >=1.5 & newdata$RT <=9.5) & newdata$tp == 1 & newdata$ID != 45 & newdata$cor2 == 1 & !is.na(newdata$insight_sum) & !is.na(newdata$SME_solve_fac) ,] 
    Activity_data$xRT = scale(Activity_data$RT)
    Activity_data$ROI_bilat_ord = ordered(Activity_data$ROI_bilat, levels=c("Amy" ,"aHC","pHC"),labels=c("Amy" ,"aHC","pHC"))

    Activity_M5 <- lmer(Beta_Buttonpress ~ xRT +sessionblock +                          ROI_bilat_ord+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M6 <- lmer(Beta_Buttonpress ~ xRT +sessionblock +insight_sum+              ROI_bilat_ord+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M7 <- lmer(Beta_Buttonpress ~ xRT +sessionblock +insight_sum+SME_solve_fac+ROI_bilat_ord+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M8 <- lmer(Beta_Buttonpress ~ xRT +sessionblock +insight_sum*SME_solve_fac+ROI_bilat_ord+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M9 <- lmer(Beta_Buttonpress ~ xRT +sessionblock +insight_sum*SME_solve_fac*ROI_bilat_ord+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)

    anova( Activity_M5, Activity_M6,Activity_M7, Activity_M8, Activity_M9)#
    plot(check_collinearity(Activity_M7))
    tab_model(Activity_M7, show.std =T)
    tab_model(Activity_M8, show.std =T)
    tab_model(Activity_M9, show.std =T)
    
    #PostHoc (note: substitute pHC for Amy or aHC)
    Activity_M10 <- lmer(Beta_Buttonpress ~ xRT +sessionblock +insight_sum+SME_solve_fac+(1|ID) + (1|Item),data= Activity_data[Activity_data$ROI_bilat== "pHC",], na.action  = na.omit)#Amy,aHC, pHC
    Activity_M11 <- lmer(Beta_Buttonpress ~ xRT +sessionblock +insight_sum*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data[Activity_data$ROI_bilat== "pHC",], na.action  = na.omit)#Amy,aHC, pHC
    anova( Activity_M10, Activity_M11)
    tab_model(Activity_M11, show.std =T)
           
           
    #Fig.6-C
    IME_Amy_HC_ggplot = ggpredict(Activity_M9 , c('SME_solve_fac','insight_sum','ROI_bilat_ord' ))  #%>%plot()+ggplot2::theme_classic()
    IME_Amy_HC_ggplot_finalPlot=ggplot(IME_Amy_HC_ggplot, aes(x= x, y = predicted , fill= group)) +
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  #coord_cartesian(ylim=c(0,.13)) + #+color = "black"
      geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                    position=position_dodge(.9)) + theme_classic()  +labs( fill = "Insight", #title = "",
                      x = "Subsequent Memory",y = 'Beta estimate during solution' ) +  
      scale_fill_manual(breaks = c('6.9', '9.1','11.2'),values=c("#f5d6e2", "#cd7397","#733e54" ))+facet_wrap(~ facet, nrow=1)# 
    IME_Amy_HC_ggplot_finalPlot
    
######### Exploratory: test which subdimensions drives the insight memory effect! ####
    Activity_data = PPSS[ PPSS$ROI_bilat == "aHC" & PPSS$tp == 1 & PPSS$cor2 == 1 & (PPSS$RT>=1.5 & PPSS$RT<=9.5) ,] 

    Activity_M12 <- lmer(Beta_Buttonpress ~ scale(RT) +sessionblock +Aha*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
       tab_model(Activity_M12, show.std =T)
       plot(check_collinearity(Activity_M12))
       summary(Activity_M12)
    Activity_M13 <- lmer(Beta_Buttonpress ~ scale(RT) +sessionblock +Certain*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
       tab_model(Activity_M13, show.std =T)
    Activity_M14 <- lmer(Beta_Buttonpress ~ scale(RT) +sessionblock +Sudden*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
       tab_model(Activity_M14, show.std =T)
       plot(check_collinearity(Activity_M14))
       summary(Activity_M14)
    
    #insight*memory adjusted for solution time (resampled)
    Activity_data = newdata[ newdata$tp == 1 & newdata$ID != 45 & newdata$cor2 == 1 & !is.na(newdata$insight_sum) & !is.na(newdata$SME_solve_fac) ,] # newdata$I_RTcorrected==1 &  & newdata$RT>2& newdata$ROI_bilat == "xaHC"
    Activity_data$I_RTcorrected1 = 0
    Activity_data[ Activity_data$ID != 45 & Activity_data$insight_mediansplit == "HI-I" & Activity_data$RT >=2.206 & Activity_data$RT <=9.5 & !is.na(Activity_data$RT) & Activity_data$cor2 == 1 ,]$I_RTcorrected1 =1 
    Activity_data[ Activity_data$ID != 45 & Activity_data$insight_mediansplit == "LO-I" & Activity_data$RT >=1.5 & Activity_data$RT <=5.9 & !is.na(Activity_data$RT) & Activity_data$cor2 == 1 ,]$I_RTcorrected1 = 1 #6.2
    summary(lmer( log(RT) ~ insight_sum+ (1|ID) + (1|Item),data= Activity_data[Activity_data$I_RTcorrected1==1 & !is.na(Activity_data$RT),], na.action = na.omit))
    
    Activity_data1 = Activity_data[Activity_data$I_RTcorrected1==1 & Activity_data$cor2 ==1 & !is.na(Activity_data$SME_solve_fac)  ,]
    Activity_M4_adj <- lmer(Beta_Buttonpress ~  sessionblock +ROI_bilat+insight_sum+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M5_adj <- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat+insight_sum+SME_solve_fac+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M6_adj <- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat+insight_sum*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M7_adj <- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat*insight_sum*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    
    hist(residuals(Activity_M6_adj)) # residuals are normally distributed -> model is adecuate
    anova( Activity_M4_adj,Activity_M5_adj,Activity_M6_adj,Activity_M7_adj)
    plot(check_collinearity(Activity_M4_adj))
    ggpredict(Activity_M6_adj , c('SME_solve_fac','insight_sum'))%>%plot()+ggplot2::theme_classic()
    tab_model(Activity_M6_adj, show.std =T)

    library("modelbased")
    estimate_slopes(Activity_M6_adj, trend = "insight_sum", at = "SME_solve_fac")#,lmerTest.limit = 11556)#, pbkrtest.limit = 19536)

###### SUPPL: control analysis - Check for univariate activity effect in VOTC-RC Areas ########

    Activity_data = PPSS[(PPSS$RT >=1.5 & PPSS$RT <=9.5)&  PPSS$cor2 == 1 &
                   (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC") &  PPSS$tp ==1 &
                   !is.na(PPSS$insight_sum)  ,] 

    Activity_data$xRT = scale(Activity_data$RT)
    Activity_VS_V4 <- lmer(Beta_Buttonpress ~ xRT+sessionblock +ROI_bilat+        +(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_VS_V5 <- lmer(Beta_Buttonpress ~ xRT+sessionblock +ROI_bilat+insight_sum+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)#insight_mediansplit_fac
    Activity_VS_V6 <- lmer(Beta_Buttonpress ~ xRT+sessionblock +ROI_bilat*insight_sum+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)#insight_mediansplit_fac

    plot(check_collinearity(Activity_VS_V5))
    anova( Activity_VS_V4, Activity_VS_V5,Activity_VS_V6)
    tab_model(Activity_VS_V5, show.std =T)
    tab_model(Activity_VS_V6, show.std =T)
    library("modelbased")
    estimate_slopes(Activity_VS_V6, trend = "insight_sum", at = "ROI_bilat")
    
    # Fig.S6 Relationship between univariate BOLD activity in RC VOTC areas and insight. 
    VOTC_ggplot = ggpredict(Activity_VS_V6 , c( 'ROI_bilat','insight_sum'))
    VOTC_ggplot_finalPlot=ggplot(VOTC_ggplot, aes(x= x, y = predicted, fill = group )) +
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +
      theme_classic() +labs( fill = "Insight", x = "",y ="Beta estimate during solution"  ) +
      scale_fill_manual(breaks = c('6.9', '9','11.2'),values=c("#f5d6e2", "#cd7397","#733e54" ))+
      geom_errorbar(aes(ymin= conf.low, ymax=conf.high ), width=.2, size = .95,position=position_dodge(.9))  
    
    # checking for time-on-task effects on BOLD signal of VOTC-RC areas
    Activity_VS_V7 <- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat+xRT+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_VS_V8 <- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat*xRT+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    plot(check_collinearity(Activity_VS_V8))
    anova( Activity_VS_V7, Activity_VS_V8)
    tab_model(Activity_VS_V8, show.std =T)
    ggpredict(Activity_VS_V8 , c('ROI_bilat','xRT'))%>%plot()+ggplot2::theme_classic()
    library("modelbased")
    estimate_slopes(Activity_VS_V8, trend = "xRT", at = "ROI_bilat")
    
### 3) multivariate ROI analysis - Brain areas showing representational change during insight #####

    newdata = PPSS[ 
     (  PPSS$ROI_bilat == "pFusG"  |
        PPSS$ROI_bilat == "aFusG"  |
        PPSS$ROI_bilat == "pITG"  |
        PPSS$ROI_bilat == "aITG"  |
        PPSS$ROI_bilat == "mITG"  |
        PPSS$ROI_bilat == "iLOC" )
        & PPSS$cor2 ==1 & PPSS$tp == 1 &  (PPSS$RT >=2 & PPSS$RT <=9.5),]
    
    newdata$ROI_bilat_fac = as.factor(newdata$ROI_bilat)

######## 3a) RSA1-Insight - Multivoxel Pattern Similarity ######################################################
  ERSdata = newdata#[newdata$ROI_bilat == "iLOC" | newdata$ROI_bilat == "pFusG" ,]#
  ERSdata$ERS_PrePost_R_delta = 1-ERSdata$ERS_PrePost_R 
  ERSdata$xRT = scale(ERSdata$RT)

  ERS_M0 <- lmer(ERS_PrePost_R_delta ~     ROI_bilat_fac          +sessionblock+(1|ID) + (1|Item), data= ERSdata, na.action  = na.omit) 
  ERS_M1 <- lmer(ERS_PrePost_R_delta ~ xRT+ROI_bilat_fac+         +sessionblock+(1|ID) + (1|Item), data= ERSdata, na.action  = na.omit)
  ERS_M2 <- lmer(ERS_PrePost_R_delta ~ xRT+ROI_bilat_fac+insight_sum+sessionblock+(1|ID) + (1|Item), data= ERSdata, na.action  = na.omit)
  ERS_M3 <- lmer(ERS_PrePost_R_delta ~ xRT+ROI_bilat_fac*insight_sum+sessionblock+(1|ID) + (1|Item), data= ERSdata, na.action  = na.omit)

  anova( ERS_M0,ERS_M1, ERS_M2, ERS_M3)#
  tab_model(ERS_M2, show.std  = T)
  tab_model(ERS_M3, show.std  = T)
  library("modelbased")
  slopes <- estimate_slopes(ERS_M3, trend = "insight_sum", at = "ROI_bilat_fac")#, pbkrtest.limit = 19536)
  
  # Fig.3-A RC from pre to post solution: Multivoxel pattern similarity
  ERS_ggpredict = ggpredict(ERS_M3 , c('ROI_bilat_fac' ,'insight_sum'))
  ERS_final=   ggplot(ERS_ggpredict, aes(x= x, y = predicted, fill = group ))+ scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))+
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +coord_cartesian(ylim=c(0.71,.93))+
      theme_classic() +labs( fill = "Insight", #title = "",
       x = "",y = expression("Δ MVPS: 1-r"[post - pre])  ) +
       scale_fill_manual(breaks = c('6.5', '8.7','10.9'),values=c("#f5d6e2", "#cd7397","#733e54" ))+
    geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,position=position_dodge(.9))  # + #+color = "black"
    
  
#### 3a1) RSA1-Insight*Memory Multivoxel Pattern Similarity ############  

newdata = PPSS[ 
    (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC" )&
      (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 & PPSS$tp==1 & PPSS$ID != 45  
  ,] 
  newdata$ROI_bilat_fac = as.factor(newdata$ROI_bilat)

  ERSdata = newdata[!is.na(newdata$SME_solve),]
  ERSdata$ERS_PrePost_R_delta = 1-ERSdata$ERS_PrePost_R 
  ERSdata$xRT = scale(ERSdata$RT)
  
  ERS_M0_IME <- lmer(ERS_PrePost_R_delta ~ xRT+insight_sum+              ROI_bilat+sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit) #cor2_fac
  ERS_M1_IME <- lmer(ERS_PrePost_R_delta ~ xRT+insight_sum+SME_solve_fac+ROI_bilat+sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit) #cor2_fac
  ERS_M2_IME <- lmer(ERS_PrePost_R_delta ~ xRT+insight_sum*SME_solve_fac+ROI_bilat+sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit) #cor2_fac
  ERS_M3_IME <- lmer(ERS_PrePost_R_delta ~ xRT+insight_sum*SME_solve_fac*ROI_bilat+sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit) #cor2_fac

  plot(check_collinearity(ERS_M1_IME))
  hist(residuals(ERS_M3_IME)) # residuals are normally distributed -> model is adecuate
  anova(ERS_M0_IME, ERS_M1_IME, ERS_M2_IME,ERS_M3_IME)#
  tab_model(ERS_M1_IME, show.std = T)
  tab_model(ERS_M2_IME, show.std = T)
  tab_model(ERS_M3_IME, show.std = T)
  
  #Fig.6-A
  ERS_Mem_ggpredict = ggpredict(ERS_M3_IME , c( 'SME_solve_fac','insight_sum', 'ROI_bilat')) #%>%plot()+ggplot2::theme_classic()
  ERS_Mem_finalPlot= ggplot(ERS_Mem_ggpredict, aes(x= x, y = predicted , fill= group)) +
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  coord_cartesian(ylim=c(0.70,.82)) + #+color = "black"
    geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs(fill = "Insight", #title = "", 
                                                                       x = "Subsequent Memory",y = expression("Δ MVPS: 1-r"[post - pre])  ) + facet_wrap(~ facet, nrow=1) + #scale_fill_gradient(low="#37c0c6",high="#f98578")+
    scale_fill_manual(breaks = c('6.6', '8.7','10.9'),values=c("#f5d6e2", "#cd7397","#733e54" ))
  
  #PostHoc (note, choose either iLOC or pFusG)
  ERS_M0_IME_posthoc <- lmer(ERS_PrePost_R ~ xRT+insight_sum+sessionblock+(1|ID)+ (1|Item), 
                             data= ERSdata[ERSdata$ROI_bilat== "iLOC",], na.action  = na.omit) 
  ERS_M1_IME_posthoc <- lmer(ERS_PrePost_R ~ xRT+insight_sum+SME_solve_fac+sessionblock+(1|ID)+ (1|Item), 
                             data= ERSdata[ERSdata$ROI_bilat== "iLOC",], na.action  = na.omit) 
  ERS_M2_IME_posthoc <- lmer(ERS_PrePost_R ~ xRT+insight_sum*SME_solve_fac+sessionblock+(1|ID)+ (1|Item),
                             data= ERSdata[ERSdata$ROI_bilat== "iLOC",], na.action  = na.omit) 
  anova(ERS_M0_IME_posthoc,ERS_M1_IME_posthoc, ERS_M2_IME_posthoc)#
  tab_model(ERS_M2_IME_posthoc, show.std = T)

########  3b) RSA2-Insight  ####################################################
  newdata = PPSS[ 
    (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC" )&
      (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 ,]
 
#### 3b1) RSA2-Insight using AlexNet8 ######
  newdata$xRT = scale(newdata$RT)
  RSAdata = newdata[   newdata$RSA_AlexNet8_realpix_control_corr_n_vis > 10 & !is.na(newdata$insight_sum),] #!is.na(newdata$insight_mediansplit) &

  M0AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ tp_fac+            ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
  M1AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ tp_fac+insight_sum+ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata) #cor2_fac +RT
  M2AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ tp_fac*insight_sum+ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
  M3AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ tp_fac*insight_sum*ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)

  hist(residuals(M2AN)) # residuals are normally distributed -> model is adecuate
  plot(check_collinearity(M1AN))
  anova(M0AN, M1AN, M2AN, M3AN)
  tab_model(M2AN, show.std = T)

  #Posthoc (note, substitute pFusG or iLOC)
  M1AN_posthoc <- lmer(RSA_AlexNet8_realpix_R_vis ~ tp_fac+insight_sum+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata[RSAdata$ROI_bilat=="iLOC",])
  M2AN_posthoc <- lmer(RSA_AlexNet8_realpix_R_vis ~ tp_fac*insight_sum+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata[RSAdata$ROI_bilat=="iLOC",])
  anova(M1AN_posthoc,M2AN_posthoc)
  tab_model(M2AN_posthoc, show.std = T)
  #estimate_slopes(M2AN, trend = "insight_sum", at = "tp_fac")
  
  #Fig.3: plot RSA2-Insight results for pFusG
  RSAN8_ggpredict = ggpredict(M2AN , c('tp_fac','insight_sum', 'ROI_bilat'))
  RSAAN8_finalPlot_all= ggplot(RSAN8_ggpredict, aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2, size =.95,
                  position=position_dodge(.9)) + theme_classic() +labs( fill = "condition", 
                    x = "",y = "Rep-Str (AlexNet)")+scale_x_discrete(labels=c('Pre', 'Post'))+ facet_wrap(~ facet, nrow=1)+#
                    scale_fill_manual(breaks = c('6.5', '8.7','10.9'),values=c("#f5d6e2", "#cd7397","#733e54" ))

#### 3b1a) RSA2-IME using AlexNet8: Insight memory effect for representations #####

  newdata = PPSS[ (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC" )&
      (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 & PPSS$ID != 45 ,]     
  
  RSAdata = newdata[ !is.na(newdata$SME_solve_fac)  & newdata$RSA_AlexNet8_realpix_control_corr_n_vis > 10 ,]
  RSAdata$xRT = scale(RSAdata$RT)

  M0AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~xRT+sessionblock+tp_fac+insight_sum             +ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit) #cor2_fac + RT
  M1AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~xRT+sessionblock+tp_fac+insight_sum+SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2aAN_mem <-lmer(RSA_AlexNet8_realpix_R_vis ~xRT+sessionblock+tp_fac*insight_sum+SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2bAN_mem <-lmer(RSA_AlexNet8_realpix_R_vis ~xRT+sessionblock+tp_fac*SME_solve_fac+tp_fac*insight_sum+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2cAN_mem <-lmer(RSA_AlexNet8_realpix_R_vis ~xRT+sessionblock+tp_fac*SME_solve_fac+tp_fac*insight_sum+insight_sum*SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2dAN_mem <-lmer(RSA_AlexNet8_realpix_R_vis ~xRT+sessionblock+tp_fac*insight_sum*SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M3AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~xRT+sessionblock+tp_fac*insight_sum*SME_solve_fac*ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)

  hist(residuals(M3AN_mem)) # residuals are normally distributed -> model is adecuate
  plot(check_collinearity(M1AN_mem))
  anova(M0AN_mem, M1AN_mem,M2aAN_mem,M2bAN_mem, M2cAN_mem,M2dAN_mem, M3AN_mem)
  tab_model(M1AN_mem  , show.std =T)
  tab_model(M2bAN_mem , show.std =T)
  tab_model(M2dAN_mem , show.std =T)
  tab_model(M3AN_mem  , show.std =T)
  
  #Fig. 6-B
  AN08_Mem_ggpredict = ggpredict(M2dAN_mem , c('tp_fac','insight_sum','SME_solve_fac'))#%>%plot()+ggplot2::theme_classic()
  AN08_Mem_finalPlot=ggplot(AN08_Mem_ggpredict, aes(x= x, y = predicted , fill= group)) +
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  coord_cartesian(ylim=c(0,.061)) + #+color = "black"
    geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs(fill = "Insight", #title = "Stimulus Onset", 
                   x = "Subsequent Memory",y = 'Rep.Str (AN08) ' ) +  scale_x_discrete(labels=c('Pre', 'Post'))+
    scale_fill_manual(breaks = c('6.6', '8.7','10.9'),values=c("#f5d6e2", "#cd7397","#733e54" )) + facet_wrap(~ facet, nrow=1)#"#fdfd00", 
  AN08_Mem_finalPlot
  
  
#### 3b2) RSA2-Insight using W2V (word2vec) ################

  newdata = PPSS[ 
    (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC" )&
      (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 ,]
  
  RSAdata = newdata[ !is.na(newdata$insight_sum)  & newdata$RSA_W2V_control_corr_n_vis > 10 ,] 
  RSAdata$xRT = scale(RSAdata$RT)#
  
  M0W2V <- lmer(RSA_W2V_R_vis ~ tp_fac            +ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit) 
  M1W2V <- lmer(RSA_W2V_R_vis ~ tp_fac+insight_sum+ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2W2V <- lmer(RSA_W2V_R_vis ~ tp_fac*insight_sum+ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M3W2V <- lmer(RSA_W2V_R_vis ~ tp_fac*insight_sum*ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)

  plot(check_collinearity(M1W2V))
  hist(residuals(M3W2V)) # residuals are normally distributed -> model is adecuate
  anova(M0W2V, M1W2V, M2W2V,M3W2V)
  tab_model(M2W2V, show.std = T)
 
  #Posthoc (note, substitute iLOC or pFusG)
  M1W2V_posthoc <- lmer(RSA_W2V_R_vis ~ tp_fac+insight_sum+sessionblock  +(1|ID)+ (1|Item),data= RSAdata[RSAdata$ROI_bilat=="iLOC",], na.action  = na.omit)
  M2W2V_posthoc <- lmer(RSA_W2V_R_vis ~ tp_fac*insight_sum+sessionblock  +(1|ID)+ (1|Item),data= RSAdata[RSAdata$ROI_bilat=="iLOC",], na.action  = na.omit)
  anova(M1W2V_posthoc,M2W2V_posthoc)
  tab_model(M2W2V_posthoc, show.std = T)
  #estimate_slopes(M2W2V, trend = "insight_sum", at = "tp_fac")#, pbkrtest.limit = 19536)
  
  #Fig.3: plot W2V-Insight results for pFusG/ilOC
  W2V_ggpredict = ggpredict(M3W2V , c('tp_fac','insight_sum', 'ROI_bilat'))
  W2V_finalPlot_all= ggplot(W2V_ggpredict, aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2, size =.95,
                  position=position_dodge(.9)) + theme_classic() +labs( fill = "condition", x = "" , #title = "pFusG", 
                  y = "Rep-Str (W2V)")+scale_x_discrete(labels=c('Pre', 'Post'))+ facet_wrap(~ facet, nrow=1)+#
    scale_fill_manual(breaks = c('6.5', '8.7','10.9'),values=c("#f5d6e2", "#cd7397","#733e54" ))


#### 3b2a) RSA2-Insight*Memory using W2V: Insight memory effect for representations  #####
  
  newdata = PPSS[ (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC" )&
                    (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 & PPSS$ID != 45 ,]
  
 RSAdata = newdata[ !is.na(newdata$SME_solve_fac) & newdata$RSA_W2V_control_corr_n_vis > 10  ,]
 RSAdata$xRT = scale(RSAdata$RT)

    M1W2V_mem <- lmer(RSA_W2V_R_vis ~sessionblock+tp_fac+insight_sum              +ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M2W2V_mem <- lmer(RSA_W2V_R_vis ~sessionblock+tp_fac+insight_sum+SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M3W2V_mem <- lmer(RSA_W2V_R_vis ~sessionblock+tp_fac*SME_solve_fac+insight_sum+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M4W2V_mem <- lmer(RSA_W2V_R_vis ~sessionblock+tp_fac*insight_sum+tp_fac*SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M5W2V_mem <- lmer(RSA_W2V_R_vis ~sessionblock+tp_fac*insight_sum*SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M6W2V_mem <- lmer(RSA_W2V_R_vis ~sessionblock+tp_fac*insight_sum*SME_solve_fac*ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  
    hist(residuals(M3W2V_mem)) # residuals are normally distributed -> model is adecuate
    plot(check_collinearity(M2W2V_mem))
    anova( M1W2V_mem,M2W2V_mem,M3W2V_mem,M4W2V_mem,M5W2V_mem,M6W2V_mem)#,
    tab_model(M2W2V_mem, show.std =T)
    tab_model(M3W2V_mem, show.std =T)
    tab_model(M5W2V_mem, show.std =T)
    #lstrends(M5W2V_mem, ~ SME_solve_fac | tp_fac, var = "insight_sum")#, pbkrtest.limit = 25152)

    # Fig.6-D
    W2V_Mem_ggpredict = ggpredict(M5W2V_mem , c('tp_fac' ,'insight_sum','SME_solve_fac' ))#%>%plot()+ggplot2::theme_classic()
    W2V_Mem_finalPlot=ggplot(W2V_Mem_ggpredict, aes(x= x, y = predicted , fill= group)) +
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  coord_cartesian(ylim=c(0,.05)) + #+color = "black"
      geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                    position=position_dodge(.9)) + theme_classic() +labs(fill = "Insight", #title = "Stimulus Onset", 
                     x = "Time",y = 'Rep.Str (W2V) ' )   + facet_wrap(~ facet, nrow=1)+
    scale_fill_manual(breaks = c('6.6', '8.7','10.9'),values=c("#f5d6e2", "#cd7397","#733e54" ))+scale_x_discrete(labels=c('Pre', 'Post'))
    W2V_Mem_finalPlot
    

######################################################################################  -
### 4) Plotting all results together #####################     
    library(ggpubr)
  
    ### Figure 2: Behavioral Insight Memory Effect.
    Fig2 <- ggarrange( AHA_amount_plot,IME_ggpredict_plot,IME_RT_finalPlot,AM_plot,
                                              common.legend = T, legend = "none",
                                              ncol = 2, nrow = 2,labels = c("A", "B", "C","D")) 
    
    ##Figure S5 Amount of trials for each insight and insight memory condition
    FigS5 <- ggarrange( p1a,p2a,common.legend = T, legend = "bottom",
                         ncol = 2, nrow = 1,labels = c("A", "B")) 
    
    ### Figure 3-B
    RSA_AN8_W2V_ggarrangeplot <- ggarrange( RSAAN8_finalPlot_all, W2V_finalPlot_all,   
                                            common.legend = T, legend = "top",
                                            ncol = 1, nrow = 2) #                               
    ### Figure 3-AB
    Fig3 <- ggarrange( ERS_final,RSA_AN8_W2V_ggarrangeplot,
                       common.legend = T, legend = "bottom",
                       ncol = 2, nrow = 1, labels = c("A", "B"))
                        
    ### Figure 6: IME for MVPS, Rep.Strength, Amy activity and FC
    Fig6 <- ggarrange(ERS_Mem_finalPlot, AN08_Mem_finalPlot,IME_Amy_HC_ggplot_finalPlot, W2V_Mem_finalPlot,
                                    common.legend = TRUE, legend = "bottom",
                                    labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
    
    ### Figure S3
    FigS4 <- ggarrange( FigS3_RT,FigS3_RT_cor,FigS3_RT_st,FigS3_RT_cor_st, ncol = 2, nrow = 2, labels = c("A","", "B"),legend = "top",common.legend = TRUE)

    ### Figure S5
    FigS5 <- ggarrange(  p1a, p2a, common.legend = F, ncol = 2, nrow = 1, labels = c("A", "B"), legend= "none")
    
    ### Figure S6
    FigS6 <- ggarrange(  VOTC_ggplot_finalPlot, common.legend = T, legend = "bottom",ncol = 1, nrow = 1)
    