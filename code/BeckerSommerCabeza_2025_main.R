
#### Code for Manuscript Becker, Sommer & Cabeza 2025 (Nature Communications)
#### Neural Mechanisms of Creative Problem Solving: From Representational Change to Memory Formation
#### (c) almaxi@gmail.com
#### last update: 18/Feb/2025

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

# for simulations: 
# predictmeans -> permlmer
library(predictmeans)

#set seed for reproducibility
set.seed(123)

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
setwd('I:/Meine Ablage/uni/_studies/IN_REVISION/PVI/GitHub_4_publication/REVISION1/data') 
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
  
#### SUPPLEMENT: Measurement Model: AHA Experience #####

  library(lavaan)
  insightfac <-   'Insight  =~ Certain + Aha  + Sudden 
                   Insight ~~ 1*Insight
                  '
  fit <- lavaan(insightfac, data=behave_data[behave_data$cor2 == 1 & (behave_data$RT >=1.5 & behave_data$RT <=9.5) ,]   , #& behave_data$I_RTcorrected==1
               auto.var=TRUE, auto.fix.first=F,
               auto.cov.lv.x=TRUE)
  summary(fit, fit.measures=TRUE)
  modindices(fit, sort = TRUE)
  inspect(fit,what="std")$lambda

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
    M0_RT <- lmer(log(RT) ~              sessionblock+(1|ID) + (1|Item),data= data_new, na.action  = na.omit)
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
      geom_jitter(position = position_jitter(width = 0.1, height = 0.1), color = "#949393")+ #+ shape = 21, 
      geom_errorbar(stat = 'summary', fun.data = mean_se, position = 'dodge', width = 0.2, size = .95) +
      labs(title = "during fMRI",x = "Insight (categorical)",y = "overall amount in %")+theme_classic()+scale_y_continuous(limits = c(0, 70))+
      scale_fill_manual(breaks = c('not solved','LO-I', 'HI-I'),values=c("#d0cccc", "#f5d6e2", "#733e54"))+
      theme(legend.position = "none")       # Remove the legend for insight_sum_fac
      
                         
    # False Alarm Rate for new pictures
    Cor.Rejection = c(48,	52,	53,	24,	42,	60,	46,	53,	34,	55,	50,	58,	55,	51,	53,	51,	43,	48,	33,	39,	57,	56,	47,	53,	51,	53,	59,	43,	46,	53,	35) #Anzahl der richtig rejecteten neuen Mooney Bilder (es gab 60 Neue) 
    Cor.Rejection_perc = (100*Cor.Rejection)/60
    FA = 100-Cor.Rejection_perc
    mean(FA); sd(FA)
    FA = as.data.frame(FA)
    FA$n =100-Cor.Rejection_perc 
    
    # behavioral generation and insight memory effect not controlled for RT
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1 & ((PPSS$RT>=1.5 & PPSS$RT<=9.5 & PPSS$cor2 == 1)|is.na(PPSS$RT) ) & PPSS$ID != 45,]
    data_new$insight_ms = factor(data_new$insight_mediansplit, ordered = F)
    M0_IME <- glmer(SME_solve_fac ~ scale(sessionblock)+            (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_IME <- glmer(SME_solve_fac ~ scale(sessionblock)+insight_ms+ (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    anova(M0_IME, M1_IME)
    tab_model(M1_IME)
    #emmeans(M1_IME, list(pairwise ~ insight_ms  ), adjust = "tukey")      
    
  #####bootstrapping data for memory analysis
    library(boot)
    # Set custom contrasts to compare "HI-I" against "LO-I"
    data_new$insight_ms <- factor(data_new$insight_ms, levels = c("not solved", "LO-I", "HI-I"))
    contrasts(data_new$insight_ms) <- cbind("LO-I_vs_nS" = c(-1, 1, 0))
    
    boot_function_nS <- function(data1, indices) {
      d <- data1[indices, ]
      fit <- glmer(SME_solve_fac ~ sessionblock + insight_ms + (1 | ID) + (1 | Item), 
                   data = d,  family = binomial(link = "logit"), na.action  = na.omit)
        return(fixef(fit)["insight_msLO-I_vs_nS "]) # insight_msLO-I_vs_nS Extract the custom contrast
       }
    
    # Perform bootstrapping with 1000 replications
    set.seed(123)  # Set seed for reproducibility
    results_insight_fac2 <- boot(data = data_new, statistic = boot_function_nS,  R = 50, stype = "i",  
                                 sim = "ordinary", parallel= c("multicore"), ncpus=7)
    print(results_insight_fac2); hist(results_insight_fac2$t) #t1* 1.909704 0.142256  0.09935347
    boot.ci(results_insight_fac2, conf= c(.95), type=c("perc")) #95%   ( 1.859,  2.254 )
    mean(results_insight_fac2$t)  #<= 0)  # One-tailed test

    ####  Fig.2-B for display purproses (without RT) #####
    data_new1 = data_new[!is.na(data_new$insight_ms) & !is.na(data_new$SME_solve_fac),]
    #data_new1 <- data_new1 %>%group_by(insight_ms) %>%filter(n() >= 2) %>%ungroup()
    M1_IME <- glmer(SME_solve_fac ~ scale(sessionblock)+insight_ms+ (1|ID) + (1|Item),data= data_new1,family = binomial(link ="logit"))#, na.action  = na.omit)
    
    IME_ggpredict <- ggpredict(M1_IME, terms = "insight_ms")
    data_new1 <- data_new1 %>%
      mutate(predicted = predict(M1_IME, type = "response") * 100)
    
    # Create the plot:
    IME_ggpredict_plot=ggplot(data_new1, aes(x = insight_ms, y = predicted, fill = insight_ms)) +
      # Violin plot for the raw data distribution
      geom_violin(trim = FALSE, alpha = 0.6, width = 1.8,adjust = 0.5) +
      theme_classic() +
      labs(title = "5 days after fMRI",
           x = "Insight (categorical)",
           y = "Mooney solution recall in %") +
      scale_y_continuous(limits = c(0, 100)) +
      theme(legend.position = "none") +      
      scale_fill_manual(breaks = c('not solved','LO-I','HI-I'),
                        values = c('#babcba', "#f5d6e2", "#733e54"))+
      # Estimated marginal means (EMMs) inside the violin plot
      geom_point(data = IME_ggpredict, 
                 aes(x = x, y = predicted * 100), inherit.aes = FALSE,
                 color = "black", size = 3, shape = 16) +  # Black dots for EMMs
      # Overlay the model's error bars (from ggpredict) as a separate data layer
      geom_errorbar(data = IME_ggpredict,
                    aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100), inherit.aes = FALSE,
                    width = 0.2, size = 0.95, position = position_dodge(0.9)) 
    
     
    ######################## 
    # behavioral insight memory effect additionally controlled for RT (note, has no "not solved" condition anymore)
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1 & (PPSS$RT>=1.5 & PPSS$RT<=9.5) & (PPSS$cor2 == 1) & PPSS$ID != 45,]
    data_new$RT_scale = scale(data_new$RT)
    M0_IME_RT <- glmer(SME_solve_fac ~  sessionblock + RT_scale                      +(1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_IME_RT <- glmer(SME_solve_fac ~  sessionblock + RT_scale+ insight_sum+(1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    anova(M0_IME_RT, M1_IME_RT)
    #tab_model(M1_IME_RT)

    ###### Bootstrap data for memory analysis 
    #library(boot)
    # Define a function for bootstrapping
    boot_func_insightsum <- function(data1, indices) {
      # Resample the data based on indices
      d <- data1[indices, ]
      # Refit the GLMM to the resampled data
      fit <- glmer(SME_solve_fac ~ sessionblock + RT_scale + insight_sum + (1|ID) + (1|Item),data= d,family = binomial(link ="logit"), na.action  = na.omit)
      # Return the coefficient of interest (e.g., for 'insight_sum')
      return(fixef(fit)["insight_sum"])}
    
    # Perform bootstrapping with 1000 replications
    set.seed(123)  # Set seed for reproducibility
    results_boot_insightsum <- boot(data = data_new, statistic = boot_func_insightsum, R = 1000, stype = "i",  sim = "ordinary", parallel= c("multicore"), ncpus=7)
    print(results_boot_insightsum)
    exp(mean(results_boot_insightsum$t))  # average log-odds -> converted to odds ratio
    boot.ci(results_boot_insightsum, conf= c(.95), type=c("perc"))#95%   
    
    #### Fig.2-C #####
    library(ggeffects)
    data_new1 = data_new[!is.na(data_new$insight_sum) & !is.na(data_new$SME_solve_fac),]
    M1_IME_RT <- glmer(SME_solve_fac ~  sessionblock + RT_scale+ insight_sum+(1|ID) + (1|Item),data= data_new1,family = binomial(link ="logit"), na.action  = na.omit)
    IME_RT_ggplot = ggpredict(M1_IME_RT , c('insight_sum'))  
    data_new1 <- data_new1 %>%mutate(predicted = predict(M1_IME_RT, type = "response") * 100)
    data_new1$insight_sum_fac = as.factor(data_new1$insight_sum)
    
    n_levels <- length(unique(data_new1$insight_sum_fac))
    gradient_palette <- colorRampPalette(c("#f5d6e2", "#733e54"))(n_levels)
    
    IME_RT_finalPlot=ggplot(data_new1, aes(x = insight_sum, y = predicted, fill = insight_sum_fac)) +
      geom_violin(trim = FALSE, alpha = 0.6, width = 1.8, adjust = 0.5) +
      theme_classic() +
      labs(title = "5 days after fMRI",
           x = "Insight (continuous)",
           y = "Mooney solution recall in %") +
      scale_y_continuous(limits = c(0, 115)) +
      scale_x_continuous(breaks = 3:12) +   # Set x-axis ticks to 3, 4, 5, ... 12
      scale_fill_manual(values = gradient_palette) +
      theme(legend.position = "none") +      # Remove the legend for insight_sum_fac
      geom_point(data = IME_RT_ggplot, 
                 aes(x = x, y = predicted * 100), inherit.aes = FALSE,
                 color = "black", size = 3, shape = 16) +  # Black dots for EMMs
      geom_errorbar(data = IME_RT_ggplot,
                    aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100), inherit.aes = FALSE,
                    width = 0.2, size = 0.95, position = position_dodge(0.9)) 
    
    # SUPPLEMENT: Control analysis: behavioral insight memory effect additionally adjusted for RT (RT difference between HI-I & LO-I, p>.2)
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

    #library(boot)
    # Define a function for bootstrapping
    boot_func_insightsum <- function(data1, indices) {
      # Resample the data based on indices
      d <- data1[indices, ]
      fit <-  glmer(SME_solve_fac ~  sessionblock + insight_sum +(1|ID) + (1|Item),data= d,family = binomial(link ="logit"), na.action  = na.omit)
      return(fixef(fit)["insight_sum"])}
    
    # Perform bootstrapping with 1000 replications
    set.seed(123)  # Set seed for reproducibility
    results_boot_insightsum_RTadj <- boot(data = data_new, statistic = boot_func_insightsum, R = 1000, stype = "i",  sim = "ordinary", parallel= c("multicore"), ncpus=7)
    print(results_boot_insightsum_RTadj)
    exp(mean(results_boot_insightsum_RTadj$t))  # average log-odds -> converted to odds ratio
    boot.ci(results_boot_insightsum_RTadj, conf= c(.95), type=c("perc")) #95%   
    
#### Association with verbal insight tasks: Anagrams  #######
    ## check if insight in Mooney images correlates with insight in anagrams
    load("BeckerSommerCabeza_2024_anagram_mooney_cor.Rda")
    
    library(reshape2)
    data_wide =  reshape(anagrams1, v.names=c('insight_sum', 'insight_mediansplit' ), timevar=c("PV"), 
                         idvar=c('cor2', 'cor1', 'RT','ID', 'Item', 'sessionblock'),direction="wide") 
    data_wide = data_wide[!is.na(data_wide$Item),]
    
    AM_subjdata <-  data_wide %>% group_by(ID) %>%  dplyr::summarise(across(c("insight_sum.V","insight_sum.P"), ~mean(., na.rm=T)))
    
    #outliers?
    boxplot(AM_subjdata$insight_sum.P)
    boxplot(AM_subjdata$insight_sum.P)
    
    cor.test(AM_subjdata$insight_sum.P, AM_subjdata$insight_sum.V, method = "spearman")
    # Compute Spearman correlation with bootstrapped CI
    library(RVAideMemoire)
    spearman.ci(AM_subjdata$insight_sum.P, AM_subjdata$insight_sum.V, nrep = 1000, conf.level = 0.95)
    
    #### Fig.2-D  #####
    AM_plot = ggplot(AM_subjdata, aes(x= insight_sum.P, y = insight_sum.V )) + geom_point()+ theme_classic()+ geom_smooth(method=lm) +
      labs( x = "Mooney images: Insight (continuous) ",y = 'Anagrams: Insight (continuous)' ) 
    
### 2) UNIVARIATE ROI ANALYSIS in AMY & HC #################

##### 2a) Amy & HC: Insight: with single trials betas (standard space) #####  
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

    Activity_M1 <- lmer(Beta_Buttonpress ~ sessionblock +xRT+           +ROI_bilat+(1|ID) + (1|Item),data= Activity_data ,na.action  = na.omit)
    Activity_M2 <- lmer(Beta_Buttonpress ~ sessionblock +xRT+insight_sum+ROI_bilat+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M3 <- lmer(Beta_Buttonpress ~ sessionblock +xRT+insight_sum*ROI_bilat+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    
    hist(residuals(Activity_M3)) # residuals are normally distributed -> model is adecuate
    anova( Activity_M1,Activity_M2,Activity_M3)
    plot(check_collinearity(Activity_M2))
    tab_model(Activity_M2, show.std = T)
    tab_model(Activity_M3, show.std = T)
    
    #PostHoc Test
    library("modelbased")
    estimate_slopes(Activity_M3, trend = "insight_sum", at = "ROI_bilat", seed = 123, pbkrtest.limit = 9432)#,
    
    ##### Fig.4: Amy & HC activity during solution are associated with insight ####
    Activity_data$predicted <- predict(Activity_M3, newdata = Activity_data)
    Activity_data$insight_bin <- cut(Activity_data$insight_sum, 
                                     breaks = quantile(Activity_data$insight_sum, probs = c(0, 1/3, .5, 1), na.rm = TRUE), 
                                     include.lowest = TRUE, 
                                     labels = c("Low", "Medium", "High"))
    Amy_HC_ggplot = as.data.frame(ggpredict(Activity_M3 , c('insight_sum', 'ROI_bilat')))
    Amy_HC_ggplot$ROI_bilat_ord = as.factor(Amy_HC_ggplot$group)
    Amy_HC_ggplot$insight_bin=  Amy_HC_ggplot$x
    Amy_HC_ggplot$insight_bin <- cut(Amy_HC_ggplot$x, 
                                     breaks = quantile(Amy_HC_ggplot$x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                                     include.lowest = TRUE, 
                                     labels = c("Low", "Medium", "High"))
    Amy_HC_ggplot_summary <- Amy_HC_ggplot %>%
      group_by(ROI_bilat_ord, insight_bin) %>% 
      summarise( mean_predicted = mean(predicted),
        mean_std_error = mean(std.error),
        mean_conf_low  = mean(conf.low), mean_conf_high = mean(conf.high), .groups = "drop")
    
    Amy_HC_ggplot_finalPlot = ggplot(Activity_data, aes(x = insight_bin, y = predicted, fill = insight_bin)) +
      geom_violin(alpha = 0.8, show.legend = F,adjust = 0.6, trim =F,  position = position_dodge(width = 0.8)) +
      geom_point(data = Amy_HC_ggplot_summary, 
                 aes(x = insight_bin, y = mean_predicted, color = insight_bin), inherit.aes = FALSE,
                 position = position_dodge(width = 0.8), size = 1.7) +
      geom_errorbar(data = Amy_HC_ggplot_summary, 
                    aes(x = insight_bin, ymin = mean_predicted - mean_std_error , ymax = mean_predicted+ mean_std_error , color = insight_bin), inherit.aes = FALSE,
                    position = position_dodge(width = 0.8),  width = 0.4) +
      facet_wrap(~ ROI_bilat_ord, nrow = 1) +
      theme_minimal() +
      labs(x = "Insight", y = "Beta estimate during solution")+
      scale_fill_manual(values = c("Low" = "#f5d6e2", 
                                   "Medium" = "#cd7397", 
                                   "High" = "#733e54")) +
      scale_color_manual(values = c("Low" = "black", 
                                    "Medium" = "black", 
                                    "High" = "black"), guide = "none")+
      theme(axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),  
            axis.text.x  = element_text(size = 11), axis.text.y  = element_text(size = 11),
            strip.text   = element_text(size = 14, face = "bold")) 
    
  #SUPPLEMEMT: adjusted for solution time (throw out fast HII and slow LOI until: p>.2)
    #insight
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
    tab_model(Activity_M3_adj, show.std =T)
    
    #insight memory - single trial (standard space)
    Activity_data1 = Activity_data[Activity_data$I_RTcorrected1==1 & Activity_data$cor2 ==1 & !is.na(Activity_data$SME_solve_fac) & Activity_data$ID != 45 ,]
    Activity_M4_adj <- lmer(Beta_Buttonpress ~sessionblock +ROI_bilat+insight_sum+SME_solve_fac+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M5_adj <-lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat+insight_sum*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M6_adj <-lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat*insight_sum*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    hist(residuals(Activity_M6_adj)) # residuals are normally distributed -> model is adecuate     plot(check_collinearity(Activity_M2_adj))
    anova( Activity_M4_adj,Activity_M5_adj,Activity_M6_adj)
    #permlmer(Activity_M4_adj,Activity_M5_adj, plot = T, seed = 123)
    tab_model(Activity_M5_adj, show.std =T)
    
    #PostHoc Test
    library("modelbased")
    estimate_slopes(Activity_M5_adj, trend = "insight_sum", at = "SME_solve_fac", seed = 123)#, pbkrtest.limit = 9432)#,
    
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
    summary(AHA_m0_AmyHC)
    tab_model(AHA_m0_AmyHC)
    anova(AHA_m0_AmyHC,AHA_m1_AmyHC)
 
    ### PostHoc: test for individual amygdala OR hippocampal activity during (correct) solution as a function of Insight
    AHA_m0_Amy <- lmer(betas ~ session +(1|ID) ,data= data_AHA_long[data_AHA_long$roi == "Amy",], na.action  = na.omit) #aHC, pHC, Amy
    hist(residuals(AHA_m0_Amy)) # residuals are normally distributed -> model is adecuate
    summary(AHA_m0_Amy)
    tab_model(AHA_m0_Amy)

######## Exploratory: which dimension is driving the univariate insight effect? (standard space, single trial analysis) ######
    newdata = PPSS[  PPSS$ROI_bilat == "pHC",] #aHC, pHC, Amy
    Activity_data = newdata[ newdata$tp == 1 & newdata$cor2 == 1 & (newdata$RT>=1.5 & newdata$RT<=9.5)  ,] 
    Activity_data$xRT = scale(Activity_data$RT)
    Activity_M4 <- lmer(Beta_Buttonpress ~ sessionblock+xRT+Aha+Sudden+Certain+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)

    plot(check_collinearity(Activity_M4))
    hist(residuals(Activity_M4)) # residuals are normally distributed -> model is adecuate
    summary(Activity_M4);    
    tab_model(Activity_M4, show.std =T) #get effectsize

    ##### 2c) Amy & HC Insight Memory ##### standard space
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
    
    library(predictmeans)
    Mperm_BBall0a = permlmer(Activity_M6,Activity_M7, plot = T, seed = 123)
    Mperm_BBall0b = permlmer(Activity_M7,Activity_M8, plot = T, seed = 123)
    Mperm_BBall1 = permlmer(Activity_M8,Activity_M9, plot = T, seed = 123)
    #Mperm_BBall2= permmodels(Activity_M9, type = 2, seed = 123)
    
    #PostHoc (note: substitute pHC for Amy or aHC)
    set.seed(123)
    Activity_M10a <- lmer(Beta_Buttonpress ~ xRT +sessionblock +insight_sum+SME_solve_fac+(1|ID) + (1|Item),
                          data= Activity_data[Activity_data$ROI_bilat== "pHC",], na.action  = na.omit)#Amy,aHC, pHC
    Activity_M11a <- lmer(Beta_Buttonpress ~ xRT +sessionblock +insight_sum*SME_solve_fac+(1|ID) + (1|Item),
                          data= Activity_data[Activity_data$ROI_bilat== "pHC",], na.action  = na.omit)#Amy,aHC, pHC
    anova( Activity_M10a, Activity_M11a)
    tab_model(Activity_M11a, show.std =T)
      # permutation test
      Mperm_aHC1 = permlmer(Activity_M10a,Activity_M11a, plot = T, seed = 123)
      #Mperm_aHC2= permmodels(Activity_M11a, type = 2, seed = 123)
  
    #Fig.6-C
    Activity_data$predicted <- predict(Activity_M9, newdata = Activity_data)
      Activity_data$insight_bin <- cut(Activity_data$insight_sum, 
                               breaks = quantile(Activity_data$insight_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                               include.lowest = TRUE, 
                               labels = c("Low", "Medium", "High"))
    IME_Amy_HC_ggplot = as.data.frame(ggpredict(Activity_M9 , c('SME_solve_fac','insight_sum', 'ROI_bilat_ord')))
    IME_Amy_HC_ggplot$ROI_bilat_ord = as.factor(IME_Amy_HC_ggplot$facet)
    IME_Amy_HC_ggplot$insight_bin=  as.factor(IME_Amy_HC_ggplot$group)
    IME_Amy_HC_ggplot$SME_solve_fac =as.factor(IME_Amy_HC_ggplot$x)
    IME_Amy_HC_ggplot$insight_bin <- factor(IME_Amy_HC_ggplot$insight_bin,
                                            levels = c("6.9", "9.1", "11.2"),
                                            labels = c("Low", "Medium", "High"))
    IME_Amy_HC_ggplot_finalPlot = ggplot(Activity_data, aes(x = SME_solve_fac, y = predicted, fill = insight_bin)) +
      geom_violin(aes(group = interaction(SME_solve_fac, insight_bin)),
                  position = position_dodge(width = 0.8),
                  trim = FALSE, alpha = 0.8, adjust = .6) +
      geom_point(data = IME_Amy_HC_ggplot, 
                 aes(x = SME_solve_fac, y = predicted, color = insight_bin), inherit.aes = FALSE,
                 position = position_dodge(width = 0.8), 
                 size = 1.3) +
      geom_errorbar(data = IME_Amy_HC_ggplot, 
                    aes(x = SME_solve_fac, ymin = predicted - std.error, ymax = predicted+ std.error, color = insight_bin), inherit.aes = FALSE,
                    position = position_dodge(width = 0.8), 
                    width = 0.4) +
      facet_wrap(~ ROI_bilat_ord, nrow = 1) +
      theme_minimal() +
      #scale_y_continuous(limits = c(-.11, .18)) +
      labs(x = "Subsequent Memory", y = "Beta estimate during solution", fill = "Insight") +
      scale_fill_manual(values = c("Low" = "#f5d6e2", 
                                   "Medium" = "#cd7397", 
                                   "High" = "#733e54")) +
      scale_color_manual(values = c("Low" = "black", 
                                    "Medium" = "black", 
                                    "High" = "black"), guide = "none")+
      theme(axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),  
            axis.text.x  = element_text(size = 11), axis.text.y  = element_text(size = 11),
            strip.text   = element_text(size = 14, face = "bold")) 
    
######### Exploratory: test which subdimensions drives the insight memory effect! ####
    Activity_data = PPSS[ PPSS$ROI_bilat == "aHC" & PPSS$tp == 1 & PPSS$cor2 == 1 & (PPSS$RT>=1.5 & PPSS$RT<=9.5) ,] 

    Activity_M12 <- lmer(Beta_Buttonpress ~ scale(RT) +sessionblock +Aha*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
       tab_model(Activity_M12, show.std =T)
       plot(check_collinearity(Activity_M12))
       # permutation test
       permAct_M12 = permmodels(Activity_M12,  seed = 123)
       
    Activity_M13 <- lmer(Beta_Buttonpress ~ scale(RT) +sessionblock +Certain*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
       tab_model(Activity_M13, show.std =T)
       # permutation test
       permAct_M13 =permmodels(Activity_M13 ,  seed = 123)
       
    Activity_M14 <- lmer(Beta_Buttonpress ~ scale(RT) +sessionblock +Sudden*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
       tab_model(Activity_M14, show.std =T)
       plot(check_collinearity(Activity_M14))
       # permutation test
       permAct_M14 = permmodels(Activity_M14,  seed = 123)
       #permAct_M14a = permlmer(Activity_M14,  plot = T, seed = 123)
       
    #insight*memory adjusted for solution time (resampled)
    Activity_data = newdata[ newdata$tp == 1 & newdata$ID != 45 & newdata$cor2 == 1 & !is.na(newdata$insight_sum) & !is.na(newdata$SME_solve_fac) ,] # newdata$I_RTcorrected==1 &  & newdata$RT>2& newdata$ROI_bilat == "xaHC"
    Activity_data$I_RTcorrected1 = 0
    Activity_data[ Activity_data$ID != 45 & Activity_data$insight_mediansplit == "HI-I" & Activity_data$RT >=2.206 & Activity_data$RT <=9.5 & !is.na(Activity_data$RT) & Activity_data$cor2 == 1 ,]$I_RTcorrected1 =1 
    Activity_data[ Activity_data$ID != 45 & Activity_data$insight_mediansplit == "LO-I" & Activity_data$RT >=1.5 & Activity_data$RT <=5.9 & !is.na(Activity_data$RT) & Activity_data$cor2 == 1 ,]$I_RTcorrected1 = 1 #6.2
    summary(lmer( log(RT) ~ insight_sum+ (1|ID) + (1|Item),data= Activity_data[Activity_data$I_RTcorrected1==1 & !is.na(Activity_data$RT),], na.action = na.omit))
    
    Activity_data1 = Activity_data[Activity_data$I_RTcorrected1==1 & Activity_data$cor2 ==1 & !is.na(Activity_data$SME_solve_fac)  ,]
    Activity_M4a_adj<- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat+                          (1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M4_adj <- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat+insight_sum+              (1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M4b_adj <-lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat*insight_sum+              (1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M5_adj <- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat+insight_sum+SME_solve_fac+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M6_adj <- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat+insight_sum*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    Activity_M7_adj <- lmer(Beta_Buttonpress ~ sessionblock +ROI_bilat*insight_sum*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data1, na.action  = na.omit)
    
    hist(residuals(Activity_M6_adj)) # residuals are normally distributed -> model is adecuate
    anova( Activity_M4_adj,Activity_M5_adj,Activity_M6_adj,Activity_M7_adj)
    plot(check_collinearity(Activity_M4_adj))
    ggpredict(Activity_M6_adj , c('SME_solve_fac','insight_sum'))%>%plot()+ggplot2::theme_classic()
    tab_model(Activity_M4_adj, show.std =T)

    # permutation test
      permAct_M4_adj =permlmer(Activity_M4a_adj,Activity_M4_adj, plot = T, seed = 123)
      permAct_M4b_adj=permlmer(Activity_M4_adj,Activity_M4b_adj, plot = T, seed = 123)
      permAct_M5_adj =permlmer(Activity_M4_adj,Activity_M5_adj, plot = T, seed = 123)
      permAct_M6_adj =permlmer(Activity_M5_adj,Activity_M6_adj, plot = T, seed = 123)
      permAct_M7_adj =permlmer(Activity_M6_adj,Activity_M7_adj, plot = T, seed = 123)
      
      library("modelbased")
      estimate_slopes(Activity_M6_adj, trend = "insight_sum", at = "SME_solve_fac", seed = 123)#,lmerTest.limit = 11556)#, pbkrtest.limit = 19536)
      permAct_M6_adj1 =permmodels(Activity_M6_adj ,  seed = 123)
    
###### SUPPLEMENT: control analysis - Check for univariate activity effect in VOTC-RC Areas ########
    Activity_data = PPSS[(PPSS$RT >=1.5 & PPSS$RT <=9.5)&  PPSS$cor2 == 1 &
                   (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC") &  PPSS$tp ==1 &
                   !is.na(PPSS$insight_sum)  ,] 

    Activity_data$xRT = scale(Activity_data$RT)
    Activity_data$xsessionblock = scale(Activity_data$sessionblock)
    Activity_data$xinsight_sum = scale(Activity_data$insight_sum ) 
    Activity_VS_V4 <- lmer(Beta_Buttonpress ~ xRT+xsessionblock +ROI_bilat+        +(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_VS_V5 <- lmer(Beta_Buttonpress ~ xRT+xsessionblock +ROI_bilat+xinsight_sum+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_VS_V6 <- lmer(Beta_Buttonpress ~ xRT+xsessionblock +ROI_bilat*xinsight_sum+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)

    plot(check_collinearity(Activity_VS_V5))
    anova( Activity_VS_V4, Activity_VS_V5,Activity_VS_V6)
    tab_model(Activity_VS_V5, show.std =T)
    tab_model(Activity_VS_V6, show.std =T)
    library("modelbased")
    estimate_slopes(Activity_VS_V6, trend = "xinsight_sum", at = "ROI_bilat",seed = 123)
    
    #### Fig.S6 Relationship between univariate BOLD activity in RC VOTC areas and insight ####
    VOTC_ggplot <- ggpredict(Activity_VS_V6, terms = c('xinsight_sum', 'ROI_bilat'))
    VOTC_VS_ggplot <- ggplot(Activity_data, aes(x = xinsight_sum, y = Beta_Buttonpress, color = xinsight_sum)) +
      geom_jitter(width = 0.2, height = 0.1, alpha = 0.8, size = .5) +
      geom_line(data = VOTC_ggplot, aes(x = x, y = predicted, group = group), 
                linetype = "solid", color = "black", size = .8) +
      geom_ribbon(data = VOTC_ggplot, 
                  aes(x = x, ymin = conf.low, ymax = conf.high, group = group), 
                  alpha = 0.2, fill = "black", inherit.aes = FALSE) +
      scale_color_gradient(low = "#f5d6e2", high = "#733e54") +
      facet_wrap(~ group) +   theme_minimal() + 
      labs(x = "Insight (continuous)", y = "Beta Estimate During Solution" ) + 
      theme(axis.title.x = element_text(size = 14),  
            axis.title.y = element_text(size = 14),  
            axis.text.x  = element_text(size = 11), 
            axis.text.y  = element_text(size = 11),
            strip.text   = element_text(size = 14), 
            legend.position = "none") + scale_x_continuous(breaks = 3:12)    # Set x-axis ticks to 3, 4, 5, ... 12
    
    # CONTROL ANALYSIS: checking for time-on-task effects on BOLD signal of VOTC-RC areas
    Activity_VS_V7 <- lmer(Beta_Buttonpress ~ xsessionblock +ROI_bilat+xRT+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_VS_V8 <- lmer(Beta_Buttonpress ~ xsessionblock +ROI_bilat*xRT+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    plot(check_collinearity(Activity_VS_V8))
    anova( Activity_VS_V7, Activity_VS_V8)
    tab_model(Activity_VS_V8, show.std =T)
    ggpredict(Activity_VS_V8 , c('ROI_bilat','xRT'))%>%plot()+ggplot2::theme_classic()
    
    library("modelbased")
    estimate_slopes(Activity_VS_V8, trend = "xRT", at = "ROI_bilat",seed = 123)

    ### 3) MULTIVARIATE ROI ANALYSIS - Brain areas showing representational change during insight #####

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
  slopes <- estimate_slopes(ERS_M3, trend = "insight_sum", at = "ROI_bilat_fac", seed = 123) #, pbkrtest.limit = 19536
  
######### Fig.3-A RC from pre to post solution: Multivoxel pattern similarity  #####
    ERSdata$insight_cat <- ifelse(ERSdata$insight_sum <= median(ERSdata$insight_sum, na.rm = TRUE),  "Low", "High")
    ERSdata$insight_cat <- factor(ERSdata$insight_cat, levels = c("Low", "High"))
    ERSdata$xRT = scale(ERSdata$RT)
    ERS_M3 <- lmer(ERS_PrePost_R_delta ~ xRT+ROI_bilat*insight_cat+sessionblock+(1|ID) + (1|Item), data= ERSdata, na.action  = na.omit)
    
    predicted_values <- ggpredict(ERS_M3, terms = c('insight_cat', 'ROI_bilat'))
    predicted_values <- predicted_values %>%  rename(ROI_bilat = group)
    predicted_values$insight_cat <- factor(predicted_values$x, levels = c("Low", "High"))
    
    ERS_final =  ggplot(ERSdata, aes(x = insight_cat, y = ERS_PrePost_R_delta, fill = insight_cat)) +
      geom_violin(alpha = 0.8, show.legend = F,adjust = 0.5) +
      scale_fill_manual(breaks = c('Low','High'),values=c("#f5d6e2", "#733e54"))+
      geom_point(data = predicted_values, 
                 aes(x = insight_cat, y = predicted), 
                 color = "black", size = 3, shape = 16) +  # Black dots for EMMs
      geom_line(data = predicted_values, 
                aes(x = insight_cat, y = predicted, group = ROI_bilat),
                linetype = "dashed", color = "black", size = .8) +
      geom_errorbar(data = predicted_values, 
                    aes(x = insight_cat,  ymin = conf.low, ymax = conf.high),inherit.aes = FALSE,
                    width = 0.4,  size = .8, color = "black") + 
      facet_wrap(~ ROI_bilat, nrow = 1) +  
      labs( fill = "Insight", x = "Insight",y = expression("Δ MVPS: 1-r"[post - pre])  ) +
      theme_minimal()+  theme(legend.position = "none")  +
      theme(legend.position = "none",
            axis.title.x = element_text(size = 22),  axis.title.y = element_text(size = 22),  
            axis.text.x  = element_text(size = 14), axis.text.y  = element_text(size = 14),
            strip.text   = element_text(size = 22, face = "bold"))    # facet labels (upper names) font size)
  
  #Supplement: time-on-task control (p.9f)
  ERS_M1s <- lmer(ERS_PrePost_R ~ Beta_Buttonpress+ROI_bilat_fac+            sessionblock+(1|ID) + (1|Item), data= ERSdata, na.action  = na.omit)
  ERS_M2s <- lmer(ERS_PrePost_R ~ Beta_Buttonpress+ROI_bilat_fac+insight_sum+sessionblock+(1|ID) + (1|Item), data= ERSdata, na.action  = na.omit)
  anova( ERS_M1s, ERS_M2s)#
  tab_model(ERS_M2s, show.std  = T)
    #post-hoc
    ERSdata_BP = ERSdata[newdata$ROI_bilat == "iLOC" ,]# pFusG
    ERS_M1s_ph <- lmer(ERS_PrePost_R ~ Beta_Buttonpress+            sessionblock+(1|ID) + (1|Item), data= ERSdata_BP, na.action  = na.omit)
    ERS_M2s_ph <- lmer(ERS_PrePost_R ~ Beta_Buttonpress+insight_sum+sessionblock+(1|ID) + (1|Item), data= ERSdata_BP, na.action  = na.omit)
    anova( ERS_M1s_ph, ERS_M2s_ph)#
    tab_model(ERS_M2s_ph, show.std  = T)
    
#### 3a1) RSA1-Insight*Memory Multivoxel Pattern Similarity ############  

newdata = PPSS[ 
    (PPSS$ROI_bilat == "pFusG"|PPSS$ROI_bilat == "iLOC" )&
      (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 & PPSS$tp==1 & PPSS$ID != 45  
  ,] 
  newdata$ROI_bilat_fac = as.factor(newdata$ROI_bilat)

  ERSdata = newdata[!is.na(newdata$SME_solve),]
  ERSdata$ERS_PrePost_R_delta = 1-ERSdata$ERS_PrePost_R 
  ERSdata$xRT = scale(ERSdata$RT)
  
  ERS_M0_IME <- lmer(ERS_PrePost_R ~ xRT+insight_sum+              ROI_bilat+sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit) #cor2_fac
  ERS_M1_IME <- lmer(ERS_PrePost_R ~ xRT+insight_sum+SME_solve_fac+ROI_bilat+sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit) #cor2_fac
  ERS_M2_IME <- lmer(ERS_PrePost_R ~ xRT+insight_sum*SME_solve_fac+ROI_bilat+sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit) #cor2_fac
  ERS_M3_IME <- lmer(ERS_PrePost_R ~ xRT+insight_sum*SME_solve_fac*ROI_bilat+sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit) #cor2_fac

  plot(check_collinearity(ERS_M1_IME))
  hist(residuals(ERS_M3_IME)) # residuals are normally distributed -> model is adecuate
  anova(ERS_M0_IME, ERS_M1_IME, ERS_M2_IME,ERS_M3_IME)#
  tab_model(ERS_M1_IME, show.std = T)
  tab_model(ERS_M2_IME, show.std = T)
  tab_model(ERS_M3_IME, show.std = T)
  
  # permutation test for model comparison to get p-values  library(predictmeans)
  permERS_M1_IME = permlmer(ERS_M0_IME,ERS_M1_IME, plot = T, seed = 123)
  permERS_M2_IME = permlmer(ERS_M1_IME,ERS_M2_IME, plot = T, seed = 123)
  permERS_M3_IME = permlmer(ERS_M2_IME,ERS_M3_IME, plot = T, seed = 123)
  #permERS_M2_IMEa = permmodels(ERS_M2_IME, type = 2, seed = 123)
  
  #### Fig.6-A ####  -> note use with this variable: ERS_PrePost_R_delta!!! (inverse)
        ERSdata$predicted <- predict(ERS_M3_IME, newdata = ERSdata)
        ERSdata$insight_bin <- cut(ERSdata$insight_sum, 
                                   breaks = quantile(ERSdata$insight_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                                   include.lowest = TRUE, 
                                   labels = c("Low", "Medium", "High"))
        ERS_Mem_ggpredict = as.data.frame(ggpredict(ERS_M3_IME , c('SME_solve_fac','insight_sum', 'ROI_bilat')))
        ERS_Mem_ggpredict$ROI_bilat = as.factor(ERS_Mem_ggpredict$facet)
        ERS_Mem_ggpredict$insight_bin=  as.factor(ERS_Mem_ggpredict$group)
        ERS_Mem_ggpredict$SME_solve_fac =as.factor(ERS_Mem_ggpredict$x)
        ERS_Mem_ggpredict$insight_bin <- factor(ERS_Mem_ggpredict$insight_bin,
                                              levels = c("6.6", "8.7", "10.9"),
                                              labels = c("Low", "Medium", "High"))
        
        ERS_Mem_finalPlot =  ggplot(ERSdata, aes(x = SME_solve_fac, y = predicted, fill = insight_bin)) +
          geom_violin(aes(group = interaction(SME_solve_fac, insight_bin)),
                      position = position_dodge(width = 0.8),
                      trim = FALSE, alpha = 0.8, adjust = .6) +
          geom_point(data = ERS_Mem_ggpredict, 
                     aes(x = SME_solve_fac, y = predicted, color = insight_bin), inherit.aes = FALSE,
                     position = position_dodge(width = 0.8), 
                     size = 1.3) +
          geom_errorbar(data = ERS_Mem_ggpredict, 
                        aes(x = SME_solve_fac, ymin = predicted - std.error, ymax = predicted+ std.error, color = insight_bin), inherit.aes = FALSE,
                        position = position_dodge(width = 0.8), 
                        width = 0.4) +
          facet_wrap(~ ROI_bilat, nrow = 1) +
          theme_minimal() +
          #scale_y_continuous(limits = c(.35, 1.2)) +
          labs( x = "Subsequent Memory",y = expression("Δ MVPS: 1-r"[post - pre]) , fill = "Insight") +
          scale_fill_manual(values = c("Low" = "#f5d6e2", 
                                       "Medium" = "#cd7397", 
                                       "High" = "#733e54")) +
          scale_color_manual(values = c("Low" = "black", 
                                        "Medium" = "black", 
                                        "High" = "black"), guide = "none")+
          theme(axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),  
                axis.text.x  = element_text(size = 11), axis.text.y  = element_text(size = 11),
                strip.text   = element_text(size = 14, face = "bold"))  

  #PostHoc (note, choose either iLOC or pFusG)
  ERS_M0_IME_posthoc <- lmer(ERS_PrePost_R ~ xRT+insight_sum+              sessionblock+(1|ID)+ (1|Item),data= ERSdata[ERSdata$ROI_bilat== "iLOC",], na.action  = na.omit) 
  ERS_M1_IME_posthoc <- lmer(ERS_PrePost_R ~ xRT+insight_sum+SME_solve_fac+sessionblock+(1|ID)+ (1|Item),data= ERSdata[ERSdata$ROI_bilat== "iLOC",], na.action  = na.omit) 
  ERS_M2_IME_posthoc <- lmer(ERS_PrePost_R ~ xRT+insight_sum*SME_solve_fac+sessionblock+(1|ID)+ (1|Item),data= ERSdata[ERSdata$ROI_bilat== "iLOC",], na.action  = na.omit) 
  anova(ERS_M0_IME_posthoc,ERS_M1_IME_posthoc, ERS_M2_IME_posthoc)#
  tab_model(ERS_M2_IME_posthoc, show.std = T)
  
    #permutation test for model comparison
    permERS_M1_IME_posthoc = permlmer(ERS_M0_IME_posthoc,ERS_M1_IME_posthoc, plot = T, seed = 123)
    permERS_M2_IME_posthoc = permlmer(ERS_M1_IME_posthoc,ERS_M2_IME_posthoc, plot = T, seed = 123)
    
########  3b) RSA2-INSIGHT  ####################################################
  newdata = PPSS[ 
    (PPSS$ROI_bilat == "pFusG" | PPSS$ROI_bilat == "iLOC") &
      (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 ,]
 
#### 3b1) RSA2-Insight using AlexNet8 ######
  newdata$xRT = scale(newdata$RT)
  RSAdata = newdata[   newdata$RSA_AlexNet8_realpix_control_corr_n_vis > 10 & !is.na(newdata$insight_sum),] #!is.na(newdata$insight_mediansplit) &

  M0aAN <- lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+tp_fac+            ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
  M0bAN <- lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+tp_fac+insight_sum+ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata) #cor2_fac +RT
  M1AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+tp_fac*insight_sum+ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
  M2AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+tp_fac*insight_sum*ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)

  hist(residuals(M2AN)) # residuals are normally distributed -> model is adecuate
  plot(check_collinearity(M1AN))
  anova(M0aAN, M0bAN, M1AN, M2AN)
  tab_model(M1AN, show.std = T)
  tab_model(M2AN, show.std = T)
  
  #Posthoc (note, substitute pFusG or iLOC)
  M1AN_posthoc <- lmer(RSA_AlexNet8_realpix_R_vis ~ tp_fac+insight_sum+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata[RSAdata$ROI_bilat=="iLOC",])
  M2AN_posthoc <- lmer(RSA_AlexNet8_realpix_R_vis ~ tp_fac*insight_sum+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata[RSAdata$ROI_bilat=="iLOC",])
  anova(M1AN_posthoc,M2AN_posthoc)
  tab_model(M2AN_posthoc, show.std = T)
  #
  
  #### Fig.3b: plot RSA2-Insight results for pFusG ####
      RSAdata$predicted <- predict(M2AN, newdata = RSAdata)
      RSAdata$insight_bin <- cut(RSAdata$insight_sum, 
                                 breaks = quantile(RSAdata$insight_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                                 include.lowest = TRUE, 
                                 labels = c("Low", "Medium", "High"))
      RSAN8_ggpredict = as.data.frame(ggpredict(M2AN , c('tp_fac','insight_sum', 'ROI_bilat')))
      RSAN8_ggpredict$ROI_bilat = as.factor(RSAN8_ggpredict$facet)
      RSAN8_ggpredict$insight_bin=  as.factor(RSAN8_ggpredict$group)
      RSAN8_ggpredict$tp_fac =as.factor(RSAN8_ggpredict$x)
      RSAN8_ggpredict$insight_bin <- factor(RSAN8_ggpredict$insight_bin,
                                            levels = c("6.5", "8.7", "10.9"),
                                            labels = c("Low", "Medium", "High"))
      
      RSAAN8_finalPlot_all = ggplot(RSAdata, aes(x = tp_fac, y = predicted, fill = insight_bin)) +
        geom_violin(aes(group = interaction(tp_fac, insight_bin)),
                    position = position_dodge(width = 0.8),
                    trim = FALSE, alpha = 0.8, adjust = .6) +
        geom_point(data = RSAN8_ggpredict, 
                   aes(x = tp_fac, y = predicted, color = insight_bin), inherit.aes = FALSE,
                   position = position_dodge(width = 0.8), 
                   size = 1.5) +
        geom_errorbar(data = RSAN8_ggpredict, 
                      aes(x = tp_fac, ymin = predicted - std.error, ymax = predicted+ std.error, color = insight_bin), inherit.aes = FALSE,
                      position = position_dodge(width = 0.8), 
                      width = 0.4) +
        facet_wrap(~ ROI_bilat, nrow = 1) +
        theme_minimal() +
        scale_y_continuous(limits = c(-.11, .18)) +
        labs(x = "Time", y = "Rep-Str (AlexNet)", fill = "Insight") +
        scale_x_discrete(labels = c("Pre", "Post")) +
        scale_fill_manual(values = c("Low" = "#f5d6e2", 
                                     "Medium" = "#cd7397", 
                                     "High" = "#733e54")) +
        scale_color_manual(values = c("Low" = "black", 
                                      "Medium" = "black", 
                                      "High" = "black"), guide = "none")+
        theme(axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),  
              axis.text.x  = element_text(size = 11), axis.text.y  = element_text(size = 11),
              strip.text   = element_text(size = 14, face = "bold"))  
  
  #Supplement: time-on-task control (p.9f)
      RSA_M1s <- lmer(RSA_AlexNet8_realpix_R_vis ~ Beta_Buttonpress+ROI_bilat+tp_fac+insight_sum+sessionblock+(1|ID) + (1|Item), data= RSAdata, na.action  = na.omit)
      RSA_M2s <- lmer(RSA_AlexNet8_realpix_R_vis ~ Beta_Buttonpress+ROI_bilat+tp_fac*insight_sum+sessionblock+(1|ID) + (1|Item), data= RSAdata, na.action  = na.omit)
      RSA_M3s <- lmer(RSA_AlexNet8_realpix_R_vis ~ Beta_Buttonpress+ROI_bilat*tp_fac*insight_sum+sessionblock+(1|ID) + (1|Item), data= RSAdata, na.action  = na.omit)
      anova( RSA_M1s, RSA_M2s,RSA_M3s)#
      tab_model(RSA_M2s, show.std  = T)
      tab_model(RSA_M3s, show.std  = T)
      
      #post-hoc    #library("modelbased")
      estimate_slopes(RSA_M2s, trend = "insight_sum", at = "tp_fac", seed = 123)

#### 3b1a) RSA2-IME using AlexNet8: Insight memory effect for representations #####

  newdata = PPSS[ (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC" )&
      (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 & PPSS$ID != 45 ,]     
  
  RSAdata = newdata[ !is.na(newdata$SME_solve_fac)  & newdata$RSA_AlexNet8_realpix_control_corr_n_vis > 10 ,]
  RSAdata$xRT = scale(RSAdata$RT)

  #interchange Beta_Buttonpress for xRT for analysis in Supplementary Material p.9
  M0AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+sessionblock+tp_fac+insight_sum             +ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit) #cor2_fac + RT
  M1AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+sessionblock+tp_fac+insight_sum+SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2aAN_mem <-lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+sessionblock+tp_fac*SME_solve_fac+insight_sum+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2aaAN_mem<-lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+sessionblock+tp_fac*insight_sum+SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2bAN_mem <-lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+sessionblock+tp_fac*SME_solve_fac+tp_fac*insight_sum+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2cAN_mem <-lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+sessionblock+tp_fac*SME_solve_fac+tp_fac*insight_sum+insight_sum*SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2dAN_mem <-lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+sessionblock+tp_fac*insight_sum*SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M3AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ xRT+sessionblock+tp_fac*insight_sum*SME_solve_fac*ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)

  hist(residuals(M3AN_mem)) # residuals are normally distributed -> model is adecuate
  plot(check_collinearity(M1AN_mem))
  anova(M0AN_mem, M1AN_mem,M2aAN_mem,M2bAN_mem, M2cAN_mem,M2dAN_mem, M3AN_mem)
  tab_model(M1AN_mem  , show.std =T)
  tab_model(M2aAN_mem , show.std =T)
  tab_model(M2dAN_mem , show.std =T)
  tab_model(M3AN_mem  , show.std =T)
  
  # permutation test for model comparison   library(predictmeans)
  permAN_M1_IME = permlmer(M0AN_mem,M1AN_mem, plot = T, seed = 123)
  permAN_M2_IME = permlmer(M1AN_mem,M2aAN_mem, plot = T, seed = 123)
  permAN_M5_IMEa = permlmer(M2cAN_mem,M2dAN_mem, plot = T, seed = 123)
  permAN_M6_IME = permlmer(M2dAN_mem,M3AN_mem, plot = T, seed = 123)
  
  #Fig. 6-B
      RSAdata$predicted <- predict(M2dAN_mem, newdata = RSAdata)
      RSAdata$insight_bin <- cut(RSAdata$insight_sum, 
                                 breaks = quantile(RSAdata$insight_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                                 include.lowest = TRUE, 
                                 labels = c("Low", "Medium", "High"))
      AN08_Mem_ggpredict = as.data.frame(ggpredict(M2dAN_mem , c('tp_fac','insight_sum', 'SME_solve_fac')))
      AN08_Mem_ggpredict$SME_solve_fac = as.factor(AN08_Mem_ggpredict$facet)
      AN08_Mem_ggpredict$insight_bin=  as.factor(AN08_Mem_ggpredict$group)
      AN08_Mem_ggpredict$tp_fac =as.factor(AN08_Mem_ggpredict$x)
      AN08_Mem_ggpredict$insight_bin <- factor(AN08_Mem_ggpredict$insight_bin,
                                            levels = c("6.6", "8.7", "10.9"),
                                            labels = c("Low", "Medium", "High"))
      
      AN08_Mem_finalPlot = ggplot(RSAdata, aes(x = tp_fac, y = predicted, fill = insight_bin)) +
        geom_violin(aes(group = interaction(tp_fac, insight_bin)),
                    position = position_dodge(width = 0.8),
                    trim = FALSE, alpha = 0.8, adjust = .6) +
        geom_point(data = AN08_Mem_ggpredict, 
                   aes(x = tp_fac, y = predicted, color = insight_bin), inherit.aes = FALSE,
                   position = position_dodge(width = 0.8), 
                   size = 1.3) +
        geom_errorbar(data = AN08_Mem_ggpredict, 
                      aes(x = tp_fac, ymin = predicted - std.error, ymax = predicted+ std.error, color = insight_bin), inherit.aes = FALSE,
                      position = position_dodge(width = 0.8), 
                      width = 0.4) +
        facet_wrap(~ SME_solve_fac, nrow = 1) +
        theme_minimal() +
        #scale_y_continuous(limits = c(-.11, .18)) +
        labs(x = "Time", y = "Rep-Str (AlexNet)", fill = "Insight") +
        scale_x_discrete(labels = c("Pre", "Post")) +
        scale_fill_manual(values = c("Low" = "#f5d6e2", 
                                     "Medium" = "#cd7397", 
                                     "High" = "#733e54")) +
        scale_color_manual(values = c("Low" = "black", 
                                      "Medium" = "black", 
                                      "High" = "black"), guide = "none")+
        theme(axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),  
              axis.text.x  = element_text(size = 11), axis.text.y  = element_text(size = 11),
              strip.text   = element_text(size = 14, face = "bold")) 
      

#### 3b2) RSA2-Insight using W2V (word2vec) ################

  newdata = PPSS[  (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC" )& (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 ,]
  
  RSAdata = newdata[ !is.na(newdata$insight_sum)  & newdata$RSA_W2V_control_corr_n_vis > 10 ,] 
  RSAdata$xRT = scale(RSAdata$RT)#
  
  # for Supplement change xRT to Beta_Buttonpress (p.10)
  M0W2V <- lmer(RSA_W2V_R_vis ~ xRT+tp_fac            +ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit) 
  M1W2V <- lmer(RSA_W2V_R_vis ~ xRT+tp_fac+insight_sum+ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2W2V <- lmer(RSA_W2V_R_vis ~ xRT+tp_fac*insight_sum+ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M3W2V <- lmer(RSA_W2V_R_vis ~ xRT+tp_fac*insight_sum*ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)

  plot(check_collinearity(M1W2V))
  hist(residuals(M3W2V)) # residuals are normally distributed -> model is adecuate
  anova(M0W2V, M1W2V, M2W2V,M3W2V)
  tab_model(M2W2V, show.std = T)
 
  #Posthoc (note, substitute iLOC or pFusG)
  M1W2V_posthoc <- lmer(RSA_W2V_R_vis ~ xRT+tp_fac+insight_sum+sessionblock  +(1|ID)+ (1|Item),data= RSAdata[RSAdata$ROI_bilat=="iLOC",], na.action  = na.omit)
  M2W2V_posthoc <- lmer(RSA_W2V_R_vis ~ xRT+tp_fac*insight_sum+sessionblock  +(1|ID)+ (1|Item),data= RSAdata[RSAdata$ROI_bilat=="iLOC",], na.action  = na.omit)
  anova(M1W2V_posthoc,M2W2V_posthoc)
  tab_model(M2W2V_posthoc, show.std = T)
  
  # Supplement p.9 -> when Beta_Buttonpress instead of xRT is modelled   #library("modelbased")
  M2W2V_suppl <- lmer(RSA_W2V_R_vis ~ Beta_Buttonpress+tp_fac*insight_sum+ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  estimate_slopes(M2W2V_suppl, trend = "insight_sum", at = "tp_fac", seed = 123,lmerTest.limit = 14652)
  
  ####Fig.3b2: plot W2V-Insight results for pFusG/ilOC  #####
      RSAdata$predicted <- predict(M2W2V, newdata = RSAdata)
      RSAdata$insight_bin <- cut(RSAdata$insight_sum, 
                                 breaks = quantile(RSAdata$insight_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                                 include.lowest = TRUE, 
                                 labels = c("Low", "Medium", "High"))
      W2V_ggpredict = as.data.frame(ggpredict(M2W2V , c('tp_fac','insight_sum', 'ROI_bilat')))
      W2V_ggpredict$ROI_bilat = as.factor(W2V_ggpredict$facet)
      W2V_ggpredict$insight_bin=  as.factor(W2V_ggpredict$group)
      W2V_ggpredict$tp_fac =as.factor(W2V_ggpredict$x)
      W2V_ggpredict$insight_bin <- factor(W2V_ggpredict$insight_bin,
                                            levels = c("6.5", "8.7", "10.9"),
                                            labels = c("Low", "Medium", "High"))
      
      W2V_finalPlot_all = ggplot(RSAdata, aes(x = tp_fac, y = predicted, fill = insight_bin)) +
        geom_violin(aes(group = interaction(tp_fac, insight_bin)),
                    position = position_dodge(width = 0.8),
                    trim = FALSE, alpha = 0.8, adjust = .6) +
        geom_point(data = W2V_ggpredict, 
                   aes(x = tp_fac, y = predicted, color = insight_bin), inherit.aes = FALSE,
                   position = position_dodge(width = 0.8), 
                   size = 1.5) +
        geom_errorbar(data = W2V_ggpredict, 
                      aes(x = tp_fac, ymin = predicted - std.error, ymax = predicted+ std.error, color = insight_bin), inherit.aes = FALSE,
                      position = position_dodge(width = 0.8), 
                      width = 0.4) +
        facet_wrap(~ ROI_bilat, nrow = 1, labeller = as_labeller(function(x) "")) +
        theme_minimal() +
        scale_y_continuous(limits = c(-.11, .18)) +
        labs(x = "Time", y = "Rep-Str (W2V)", fill = "Insight") +
        scale_x_discrete(labels = c("Pre", "Post")) +
        scale_fill_manual(values = c("Low" = "#f5d6e2", 
                                     "Medium" = "#cd7397", 
                                     "High" = "#733e54")) +
        scale_color_manual(values = c("Low" = "black", 
                                      "Medium" = "black", 
                                      "High" = "black"), guide = "none")+
        theme(axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),  
              axis.text.x  = element_text(size = 11), axis.text.y  = element_text(size = 11))
      

#### 3b2a) RSA2-Insight*Memory using W2V: Insight memory effect for representations  #####
  
  newdata = PPSS[ (PPSS$ROI_bilat == "pFusG" |PPSS$ROI_bilat == "iLOC" )&
                    (PPSS$RT >=2 & PPSS$RT <=9.5) & PPSS$cor2 ==1 & PPSS$ID != 45 ,]
  
 RSAdata = newdata[ !is.na(newdata$SME_solve_fac) & newdata$RSA_W2V_control_corr_n_vis > 10  ,]
 RSAdata$xRT = scale(RSAdata$RT)

    # for Supplement change xRT to Beta_Buttonpress (p.10)
    M1W2V_mem <- lmer(RSA_W2V_R_vis ~xRT+sessionblock+tp_fac+insight_sum              +ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M2W2V_mem <- lmer(RSA_W2V_R_vis ~xRT+sessionblock+tp_fac+insight_sum+SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M3W2V_mem <- lmer(RSA_W2V_R_vis ~xRT+sessionblock+tp_fac*SME_solve_fac+insight_sum+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M3Waa2V_mem<-lmer(RSA_W2V_R_vis ~xRT+sessionblock+tp_fac*insight_sum+SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M3aW2V_mem<- lmer(RSA_W2V_R_vis ~xRT+sessionblock+tp_fac+insight_sum+SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M4W2V_mem <- lmer(RSA_W2V_R_vis ~xRT+sessionblock+tp_fac*insight_sum+tp_fac*SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M5W2V_mem <- lmer(RSA_W2V_R_vis ~xRT+sessionblock+tp_fac*insight_sum*SME_solve_fac+ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M6W2V_mem <- lmer(RSA_W2V_R_vis ~xRT+sessionblock+tp_fac*insight_sum*SME_solve_fac*ROI_bilat+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  
    hist(residuals(M3W2V_mem)) # residuals are normally distributed -> model is adecuate
    plot(check_collinearity(M2W2V_mem))
    anova( M1W2V_mem,M2W2V_mem,M3W2V_mem,M4W2V_mem,M5W2V_mem,M6W2V_mem)#,
    tab_model(M2W2V_mem, show.std =T)
    tab_model(M3W2V_mem, show.std =T)
    tab_model(M5W2V_mem, show.std =T)

    # permutation test for model comparison   library(predictmeans)
    permW2V_M2_IME = permlmer(M1W2V_mem,M2W2V_mem, plot = T, seed = 123)
    permW2V_M3_IME = permlmer(M2W2V_mem,M3W2V_mem, plot = T, seed = 123)
    permW2V_M5_IME = permlmer(M4W2V_mem,M5W2V_mem, plot = T, seed = 123)  
    permW2V_M6_IME = permlmer(M5W2V_mem,M6W2V_mem, plot = T, seed = 123)
    
    # Fig.6-D
          RSAdata$predicted <- predict(M5W2V_mem, newdata = RSAdata)
          RSAdata$insight_bin <- cut(RSAdata$insight_sum, 
                                     breaks = quantile(RSAdata$insight_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                                     include.lowest = TRUE, 
                                     labels = c("Low", "Medium", "High"))
          W2V_Mem_ggpredict = as.data.frame(ggpredict(M5W2V_mem , c('tp_fac','insight_sum', 'SME_solve_fac')))
          W2V_Mem_ggpredict$SME_solve_fac = as.factor(W2V_Mem_ggpredict$facet)
          W2V_Mem_ggpredict$insight_bin=  as.factor(W2V_Mem_ggpredict$group)
          W2V_Mem_ggpredict$tp_fac =as.factor(W2V_Mem_ggpredict$x)
          W2V_Mem_ggpredict$insight_bin <- factor(W2V_Mem_ggpredict$insight_bin,
                                                   levels = c("6.6", "8.7", "10.9"),
                                                   labels = c("Low", "Medium", "High"))
          
          W2V_Mem_finalPlot = ggplot(RSAdata, aes(x = tp_fac, y = predicted, fill = insight_bin)) +
            geom_violin(aes(group = interaction(tp_fac, insight_bin)),
                        position = position_dodge(width = 0.8),
                        trim = FALSE, alpha = 0.8, adjust = .6) +
            geom_point(data = W2V_Mem_ggpredict, 
                       aes(x = tp_fac, y = predicted, color = insight_bin), inherit.aes = FALSE,
                       position = position_dodge(width = 0.8), 
                       size = 1.3) +
            geom_errorbar(data = W2V_Mem_ggpredict, 
                          aes(x = tp_fac, ymin = predicted - std.error, ymax = predicted+ std.error, color = insight_bin), inherit.aes = FALSE,
                          position = position_dodge(width = 0.8), 
                          width = 0.4) +
            facet_wrap(~ SME_solve_fac, nrow = 1) +
            theme_minimal() +
            #scale_y_continuous(limits = c(-.11, .18)) +
            labs(x = "Time", y = "Rep-Str (W2V)", fill = "Insight") +
            scale_x_discrete(labels = c("Pre", "Post")) +
            scale_fill_manual(values = c("Low" = "#f5d6e2", 
                                         "Medium" = "#cd7397", 
                                         "High" = "#733e54")) +
            scale_color_manual(values = c("Low" = "black", 
                                          "Medium" = "black", 
                                          "High" = "black"), guide = "none")+
            theme(axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),  
                  axis.text.x  = element_text(size = 11), axis.text.y  = element_text(size = 11),
                  strip.text   = element_text(size = 14, face = "bold")) 
          

##### plot FUNCTIONAL CONNECTIVITY data  #################
#those are Fisher transformed z values for each ROI combination per Subject for HI-I & LO-I from CONN
          
          conn<- read.csv("HIvsLO_longData_y.csv", sep = ",", dec = ".", header=T) 
          conn$ROI1 = as.factor(conn$ROI1)
          conn$ROI2 = as.factor(conn$ROI2)
          conn$ID = as.factor(conn$ID)
          conn_long <- conn %>% pivot_longer(cols = c(y_LO, y_HI),  names_to = "Insight", # Name of the new column
                                             values_to = "Beta")    # Values for the new column
          conn_long$Insight <- factor(conn_long$Insight, levels = c( "y_LO", "y_HI"), labels = c( "Low","High"))
          
          conn_long = conn_long[!is.na(conn_long$Beta),]
          
          # Compute summary statistics: mean and SEM for each condition
          summary_stats <- conn_long %>% group_by(Insight) %>%
            summarise(Mean = mean(Beta, na.rm = TRUE),SEM = sd(Beta, na.rm = TRUE) / sqrt(n()))
          
          # Compute summary statistics: mean and SEM for each condition
          conn_short <- conn_long %>%group_by(ID, Insight) %>%summarise(Mean = mean(Beta, na.rm = TRUE))
          
          Conn_plot = ggplot(conn_short, aes(x = Insight, y = Mean, fill = Insight)) +
            geom_violin(trim = F,  alpha = 0.9, adjust = .8) +  # Violin plot for distribution
            geom_point(data = summary_stats, aes(x = Insight, y = Mean), 
                       color = "black", size = 3) +
            geom_errorbar(data = summary_stats, 
                          aes(x = Insight, ymin = Mean - 1.96*SEM, ymax = Mean+1.96*SEM),inherit.aes = FALSE,
                          width = 0.2, color = "black") +
            labs( y = "Fisher z-values", x = "Insight", fill = "Insight")+ #Fisher transformed correlation values between each pair of ROIs and for each subject
            theme_minimal() +theme(legend.position = "none") + 
            scale_fill_manual(values = c("Low" = "#f5d6e2", "High" = "#733e54")) +  # Custom colors
            theme(axis.title.x = element_text(size = 18),  axis.title.y = element_text(size = 18),  
                  axis.text.x  = element_text(size = 14), axis.text.y  = element_text(size = 14),
                  strip.text   = element_text(size = 18, face = "bold"))  #portrait, 6/3 inch
          

######################################################################################  -
### 4) Plotting all results together #####################     
    library(ggpubr)
  
    ### Figure 2: Behavioral Insight Memory Effect.
    Fig2 <- ggarrange( AHA_amount_plot,IME_ggpredict_plot,IME_RT_finalPlot,AM_plot,
                                               legend = "none", ncol = 2, nrow = 2,labels = c("A", "B", "C","D")) 
    
    ##Figure S5 Amount of trials for each insight and insight memory condition
    FigS5 <- ggarrange( p1a,p2a,common.legend = T, legend = "bottom",
                         ncol = 2, nrow = 1,labels = c("A", "B")) 
    
    ### Figure 3-B
    RSA_AN8_W2V_ggarrangeplot <- ggarrange( RSAAN8_finalPlot_all, W2V_finalPlot_all,   
                                            common.legend = T, legend = "bottom",
                                            ncol = 2, nrow = 1) #                               
                        
    ### Figure 6: IME for MVPS, Rep.Strength, Amy activity and FC
    Fig6 <- ggarrange(ERS_Mem_finalPlot, AN08_Mem_finalPlot,IME_Amy_HC_ggplot_finalPlot, W2V_Mem_finalPlot,
                                    common.legend = TRUE, legend = "right",
                                    labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
    
    ### Figure S3
    FigS4 <- ggarrange( FigS3_RT,FigS3_RT_cor,FigS3_RT_st,FigS3_RT_cor_st, ncol = 2, nrow = 2, labels = c("A","", "B"),legend = "top",common.legend = TRUE)

    ### Figure S5
    FigS5 <- ggarrange(  p1a, p2a, common.legend = F, ncol = 2, nrow = 1, labels = c("A", "B"), legend= "none")
    
    ### Figure S6
    FigS6 <- ggarrange(  VOTC_ggplot_finalPlot, common.legend = T, legend = "bottom",ncol = 1, nrow = 1)
    