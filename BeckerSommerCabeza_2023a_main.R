
#### Code for Manuscript Becker, Sommer & Cabeza 2023 
#### working Title: Creativity and memory: Cortical representational change along with amygdala activation predict the insight memory effect
#### (c) almaxi@gmail.com
#### last update: 27/July/2023

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
#library(parameters)
library(tidyverse)
library(glmmTMB)
library(effectsize)
library(ggplot2)
library(dplyr)
library(ggpubr)
#library(hrbrthemes)
library(emmeans)
#install.packages("remotes")                    # Install remotes package
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)                           # Load ggpattern package

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
        labs(x=feature, y = "Density") +scale_fill_manual( values = c("#37c0c6", "#f98578"))
      plt + guides(fill=guide_legend(title=label_column))
    } 

#### load main data #####################################################
setwd('C:/Users/Maxi/Google Drive/studies/Maxi/PVI/GitHub_4_publication') 
load('BeckerSommerCabeza_2023_data.Rda') 

##### 1) Behavioral data analysis  #############################################

    behave_data = PPSS[PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1,]
    behave_data$age = 2021 - behave_data$age 
    behave_data$insight_ms = NA
    behave_data[behave_data$insight_mediansplit == "HI-I",]$insight_ms = 1
    behave_data[behave_data$insight_mediansplit == "LO-I",]$insight_ms = 0
    behave_data$RT_correct = NA
    behave_data[behave_data$cor2 == 1,]$RT_correct = behave_data[behave_data$cor2 == 1,]$RT
    behave_data$RT_incorrect = NA
    behave_data[behave_data$cor2 == 0,]$RT_incorrect = behave_data[behave_data$cor2 == 0,]$RT
    behave_data$insight_ms_correct = NA
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1,]$insight_ms_correct = 1
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1,]$insight_ms_correct = 0
    
    behave_data$insight_ms_incorrect = NA
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 0,]$insight_ms_incorrect = 1
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 0,]$insight_ms_incorrect = 0
    
    behave_data$HII_ms_RTcorrect = NA
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1,]$HII_ms_RTcorrect = behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 1,]$RT
    behave_data$LOI_ms_RTcorrect = NA
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1,]$LOI_ms_RTcorrect = behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 1,]$RT
    
    behave_data$HII_ms_RTincorrect = NA
    behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 0,]$HII_ms_RTincorrect = behave_data[behave_data$insight_mediansplit == "HI-I" & behave_data$cor2 == 0,]$RT
    behave_data$LOI_ms_RTincorrect = NA
    behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 0,]$LOI_ms_RTincorrect = behave_data[behave_data$insight_mediansplit == "LO-I" & behave_data$cor2 == 0,]$RT
    
    behave_data$SME_solve_all = NA
    behave_data[ behave_data$cor1 == 1,]$SME_solve_all = behave_data[behave_data$cor1 == 1,]$SME_solve
    
    behave_data$SME_solve_cor2 = NA
    behave_data[ behave_data$cor2 == 1,]$SME_solve_cor2 = behave_data[ behave_data$cor2 == 1,]$SME_solve
    

  # plot response time distribution for supplement
  alldata1 <- behave_data[, c('RT','RT_correct','ID','insight_mediansplit')]
  agg_data_all1 <- alldata1 %>% group_by(insight_mediansplit,ID) %>% dplyr::summarise(across(c("RT","RT_correct"), ~mean(., na.rm=T))) 
  agg_data_all1 <- na.omit(agg_data_all1)
  
  FigS3_RT<- plot_multi_histogram_neu(agg_data_all1[!is.na(agg_data_all1$RT),], 'RT', 'insight_mediansplit ')+labs(x = "Solution time in sec (all)",y = "Density", fill = "Insight")+
    geom_vline(aes(xintercept=median(agg_data_all1[agg_data_all1$insight_mediansplit == "HI-I",]$RT, na.rm =T)), color="black", linetype="longdash", size=.8) +
    geom_vline(aes(xintercept=median(agg_data_all1[agg_data_all1$insight_mediansplit == "LO-I",]$RT, na.rm =T)), color="black", linetype="solid", size=.8)
  
  FigS3_RT_cor<- plot_multi_histogram_neu(behave_data[!is.na(behave_data$RT_correct),], 'RT_correct', 'insight_mediansplit ')+labs(x = "Solution time in sec (correct)",y = "Density", fill = "Insight")+
    geom_vline(aes(xintercept=median(behave_data[behave_data$insight_mediansplit == "HI-I",]$RT_correct, na.rm =T)), color="black", linetype="longdash", size=.8) +
    geom_vline(aes(xintercept=median(behave_data[behave_data$insight_mediansplit == "LO-I",]$RT_correct, na.rm =T)), color="black", linetype="solid", size=.8)
 
  
#### check whether AHA is latent factor #####

  library(lavaan)
  insightfac <-   'Insight  =~ Certain + Aha  + Sudden 
                   Insight ~~ 1*Insight
                  '
  fit <- lavaan(insightfac, data=behave_data[behave_data$cor2 == 1,],
               auto.var=TRUE, auto.fix.first=F,
               auto.cov.lv.x=TRUE)
 summary(fit, fit.measures=TRUE)
 

  # create aggregated data per subject 
  behave_data0 = behave_data[,c('ID', 'cor2', 'cor1', 'RT', 'RT_correct', 'RT_incorrect', 'SME_solve', 'SME_solve_cor2', 'SME_solve_all', 'age', 'sex', 
                               'Aha', 'Sudden', 'Certain', 'insight_ms', 'insight_ms_correct','insight_ms_incorrect', 
                               'HII_ms_RTcorrect','LOI_ms_RTcorrect', 'HII_ms_RTincorrect','LOI_ms_RTincorrect' )]
  behave_data1 = behave_data0 %>% group_by(ID) %>% summarise_all(funs(mean), na.rm =T)   

  #demographics
  b <- behave_data1 %>%dplyr::count( ID,sex); sum(b$sex)

  max(behave_data1$age)
  min(behave_data1$age)

  mean(behave_data1[behave_data1$sex == 1,]$age)
  mean(behave_data1[behave_data1$sex == 0,]$age)
  
  # solution related descriptive measures
  mean(behave_data1$cor1)
  sd(behave_data1$cor1)
  
  mean(behave_data1$cor2)
  sd(behave_data1$cor2)
  
  mean(1-behave_data1$cor2)
  sd(1-behave_data1$cor2)

  median(behave_data1$RT)
  sd(behave_data1$RT)

  median(behave_data1$RT_correct)
  sd(behave_data1$RT_correct)

  mean(behave_data1$insight_ms)
  sd(behave_data1$insight_ms)
  
  mean(behave_data1$insight_ms_correct)
  mean(1-behave_data1$insight_ms_correct)
  sd(behave_data1$insight_ms_correct)
  
  mean(behave_data1$insight_ms_incorrect)
  sd(behave_data1$insight_ms_incorrect)
  
  median(behave_data1$HII_ms_RTcorrect)
  sd(behave_data1$HII_ms_RTcorrect)
  
  median(behave_data1$LOI_ms_RTcorrect)
  sd(behave_data1$LOI_ms_RTcorrect)
  
  median(behave_data1$HII_ms_RTincorrect, na.rm =T)
  sd(behave_data1$HII_ms_RTincorrect, na.rm =T)
  
  median(behave_data1$LOI_ms_RTincorrect, na.rm =T)
  sd(behave_data1$LOI_ms_RTincorrect, na.rm =T)
  
  mean(behave_data1$Certain, na.rm =T)
  sd(behave_data1$Certain, na.rm =T)
  
  mean(behave_data1$Sudden, na.rm =T)
  sd(behave_data1$Sudden, na.rm =T)
  
  mean(behave_data1$Aha, na.rm =T)
  sd(behave_data1$Aha, na.rm =T)

    #difference in accuracy between insight conditions
    data_new = PPSS[PPSS$insight_mediansplit != "not solved" & PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1,]
    M0_cor2 <- glmer(cor2 ~                     +sessionblock  + (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_cor2 <- glmer(cor2 ~ insight_mediansplit +sessionblock  + (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    emmeans(M1_cor2, list(pairwise ~  insight_mediansplit ), adjust = 'tukey')
    anova(M0_cor2, M1_cor2)
    tab_model(M1_cor2)
    ggpredict(M1_cor2 , c(  'insight_mediansplit')) #%>% plot() + ggplot2::theme_classic()
    
    #difference in solution time between insight conditions
    M0_RT <- lmer(log(RT) ~   sessionblock+                   (1|ID) + (1|Item),data= data_new, na.action  = na.omit)
    M1_RT <- lmer(log(RT) ~ insight_mediansplit +sessionblock  + (1|ID) + (1|Item),data= data_new, na.action  = na.omit)
    anova(M0_RT, M1_RT)
    hist(resid(M1_RT))
    tab_model(M1_RT, show.std  = T)
    ggpredict(M1_RT , c(  'insight_mediansplit')) #%>% plot() + ggplot2::theme_classic()
    
    # overall amount of solved trials divided by condition: HI-L LO-I and not solved
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1,]
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
      geom_jitter(position = position_jitter(width = 0.1, height = 0.1), color = "#545454")+ #+ shape = 21, 
      labs(title = "during fMRI",x = "",y = "overall amount in %")+theme_classic()+scale_y_continuous(limits = c(0, 85))+
      scale_fill_manual(breaks = c('not solved','LO-I', 'HI-I'),values=c("#babcba", "#37c0c6", "#f98578"))
    
  
### memory related  measures - cue identity recall
    load('BeckerSommerCabeza_2023_Subsequent_memory_data.Rda')
    
    #remembered items 
    data$SM.recognition_see = NA
    data[data$SM.certainty_present <=2 & !is.na(data$SM.certainty_present),]$SM.recognition_see  = 1
    data[data$SM.certainty_present >2 & !is.na(data$SM.certainty_present),]$SM.recognition_see  = 0
    
    data$SM.recognition_solve = NA
    data[data$SM.certainty_present <=2 & data$SM.certainty_solved <=2 & !is.na(data$SM.certainty_solved),]$SM.recognition_solve  = 1
    data[data$SM.certainty_present <=2 & data$SM.certainty_solved >2 & !is.na(data$SM.certainty_solved),]$SM.recognition_solve  = 0
    
    data$SM.recognition_solvecor2 = NA
    data[data$SM.certainty_present <=2 & data$SM.cor2 ==1 & !is.na(data$SM.cor2) & !is.na(data$SM.certainty_present),]$SM.recognition_solvecor2  = 1
    data[data$SM.certainty_present <=2 & data$SM.cor2 ==0 & !is.na(data$SM.cor2) & !is.na(data$SM.certainty_present),]$SM.recognition_solvecor2  = 0
    
    data$SM.recognition_see_RT_HII = NA
    data[data$insight == 1 & !is.na(data$insight),]$SM.recognition_see_RT_HII = data[data$insight == 1 & !is.na(data$insight),]$SM.RT_cert_present 

    data$SM.recognition_see_RT_LOI = NA
    data[data$insight == 0 & !is.na(data$insight),]$SM.recognition_see_RT_LOI = data[data$insight == 0 & !is.na(data$insight),]$SM.RT_cert_present 

    data$SM.recognition_see_RT_HII_cor2 = NA
    data[data$cor2 == 1 & data$insight == 1  & !is.na(data$insight),]$SM.recognition_see_RT_HII_cor2 = data[data$cor2 == 1 & data$insight == 1 & !is.na(data$insight) ,]$SM.RT_cert_present 
    
    data$SM.recognition_see_RT_LOI_cor2 = NA
    data[data$cor2 == 1 & data$insight == 0 & !is.na(data$insight),]$SM.recognition_see_RT_LOI_cor2 = data[data$cor2 == 1 & data$insight == 0 & !is.na(data$insight),]$SM.RT_cert_present 
    
    #forgotten items (of all forgotten items x% were not remembered and x% were not correctly remembered)
    data$SM.not_remembered = NA
    data[ data$SM.cor2 ==0 & !is.na(data$SM.cor2) ,]$SM.not_remembered  = 0
    data[(data$SM.certainty_present >2) & data$SM.cor2 ==0 & !is.na(data$SM.cor2) & !is.na(data$SM.certainty_present),]$SM.not_remembered= 1
    
    data$SM.incorrectly_remembered = NA
    data[ data$SM.cor2 ==0 & !is.na(data$SM.cor2) ,]$SM.incorrectly_remembered  = 0
    data[(data$SM.certainty_present <=2 | data$SM.certainty_solved<=2) & data$SM.cor2 ==0 & !is.na(data$SM.cor2) & !is.na(data$SM.certainty_present) & !is.na(data$SM.certainty_solved),]$SM.incorrectly_remembered= 1
    
    
    SMdata = data[,c('ID', 'SM.recognition_see', 'SM.recognition_solve', 'SM.recognition_solvecor2', 
                     'SM.recognition_see_RT_HII', 'SM.recognition_see_RT_LOI','SM.cor2', 'SM.TruHIT_solve', 'SM.RT_cert_present',
                     'SM.recognition_see_RT_HII_cor2', 'SM.recognition_see_RT_LOI_cor2','SM.not_remembered', 'SM.incorrectly_remembered'),]
    
    SMdata_agg = SMdata %>% group_by(ID) %>% summarise_all(funs(median), na.rm =T)   
    SMdata_agg_mean = SMdata %>% group_by(ID) %>% summarise_all(.funs = list(m = ~ mean(x=., na.rm =T))) 
    
    mean(SMdata_agg_mean$SM.recognition_see_m); sd(SMdata_agg_mean$SM.recognition_see_m)
    mean(SMdata_agg_mean$SM.recognition_solve_m); sd(SMdata_agg_mean$SM.recognition_solve_m)
    mean(SMdata_agg_mean$SM.recognition_solvecor2_m); sd(SMdata_agg_mean$SM.recognition_solvecor2_m)
    mean(SMdata_agg_mean$SM.not_remembered_m); sd(SMdata_agg_mean$SM.not_remembered_m)
    mean(SMdata_agg_mean$SM.incorrectly_remembered_m); sd(SMdata_agg_mean$SM.incorrectly_remembered_m)
    
    mean(SMdata_agg$SM.RT_cert_present)/1000;sd(SMdata_agg$SM.RT_cert_present)/1000
    mean(SMdata_agg$SM.recognition_see_RT_HII)/1000;sd(SMdata_agg$SM.recognition_see_RT_HII)/1000
    mean(SMdata_agg$SM.recognition_see_RT_LOI)/1000;sd(SMdata_agg$SM.recognition_see_RT_LOI)/1000
    mean(SMdata_agg$SM.recognition_see_RT_HII_cor2)/1000;sd(SMdata_agg$SM.recognition_see_RT_HII_cor2)/1000
    mean(SMdata_agg$SM.recognition_see_RT_LOI_cor2)/1000;sd(SMdata_agg$SM.recognition_see_RT_LOI_cor2)/1000
    
    ######-
    mean(behave_data1[behave_data1$ID != 45,]$SME_solve_all, na.rm =T)
    sd(behave_data1[behave_data1$ID != 45,]$SME_solve_all, na.rm =T)
    
    mean(behave_data1[behave_data1$ID != 45,]$SME_solve_cor2, na.rm =T)
    sd(behave_data1[behave_data1$ID != 45,]$SME_solve_cor2, na.rm =T)

    # False Alarm Rate for new pictures
    Cor.Rejection = c(48,	52,	53,	24,	42,	60,	46,	53,	34,	55,	50,	58,	55,	51,	53,	51,	43,	48,	33,	39,	57,	56,	47,	53,	51,	53,	59,	43,	46,	53,	35) #Anzahl der richtig rejecteten neuen Mooney Bilder (es gab 60 Neue) 
    Cor.Rejection_perc = (100*Cor.Rejection)/60
    FA = 100-Cor.Rejection_perc
    mean(FA); sd(FA)
    FA = as.data.frame(FA)
    FA$n =100-Cor.Rejection_perc 
                
    # behavioral generation and insight memory effect not controlled for RT
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1  ,]
    M0_IME <- glmer(SME_solve_num ~ sessionblock+                   (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_IME <- glmer(SME_solve_num ~ sessionblock+insight_mediansplit+(1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    anova(M0_IME, M1_IME)
    tab_model(M1_IME, show.std  = T)
    emmeans(M1_IME, list(pairwise ~ insight_mediansplit  ), adjust = "tukey")      

    #  for display purproses (without RT)
     IME_ggpredict =ggpredict(M1_IME , c(  'insight_mediansplit'))
     IME_ggpredict_plot = ggplot(IME_ggpredict, aes(x= x, y = predicted*100 , fill=  x)) +
      geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
      geom_errorbar(aes(ymin=conf.low*100 , ymax=conf.high*100 ), width=.2,size = .95,position=position_dodge(.9)) +
                      theme_classic() +labs(title = "5 days after fMRI",x = "",y = "Mooney solution recall in %")+
                      scale_y_continuous(limits = c(0, 85))+
                      scale_fill_manual(breaks = c('not solved','LO-I','HI-I'),values=c('#babcba', "#37c0c6", "#f98578")) #scale_fill_manual(breaks = c('not solved','LO-I','HI-I'),values=c('#babcba', "#37c0c6", "#f98578"))
     
    # behavioral insight memory effect additionally controlled for RT (note, has no "not solved" condition anymore)
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1  ,]
    M0_IME_RT <- glmer(SME_solve_fac ~ cor2 + RT+ sessionblock+                      (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_IME_RT <- glmer(SME_solve_fac ~ cor2 + RT+ sessionblock + insight_mediansplit +(1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)

    anova(M0_IME_RT, M1_IME_RT)
    tab_model(M1_IME_RT, show.std  = T)
    ggpredict(M1_IME_RT , c(  'insight_mediansplit')) %>% plot() + ggplot2::theme_classic()
    

####### 2) Univariate ROI analysis #################

### 2a) HI vs LO Insight #####################

      ###### HI vs LO Insight 
    # load Amygdala in standard space for HI vs LO Insight  
    r_Amy    <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_Amy_22-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_Amy    <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_Amy_22-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    r_aHC    <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_aHC_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_aHC    <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_aHC_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    r_pHC    <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_pHC_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_pHC    <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_pHC_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    
    # load VOTC in subject space for HI vs LO Insight  
    r_Amy    <- read.table("s2_ALL_HIvsLO_Insight_AHAmsplit_SubjSp_BP_r_Amy_23-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_Amy    <- read.table("s2_ALL_HIvsLO_Insight_AHAmsplit_SubjSp_BP_l_Amy_23-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    r_aHC    <- read.table("s2_ALL_HIvsLO_Insight_AHAmsplit_SubjSp_r_aHC_18-Feb-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_aHC    <- read.table("s2_ALL_HIvsLO_Insight_AHAmsplit_SubjSp_l_aHC_18-Feb-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    r_pHC    <- read.table("s2_ALL_HIvsLO_Insight_AHAmsplit_SubjSp_r_pHC_18-Feb-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_pHC    <- read.table("s2_ALL_HIvsLO_Insight_AHAmsplit_SubjSp_l_pHC_18-Feb-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    
   
   ###### load VOTC in standard space for HI vs LO Insight  
   l_aFusG  <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_antFusG_marsbar_97_115_97_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))   
   r_aFusG  <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_antFusG_marsbar_97_115_97_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))  
   r_pFusG  <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_postFusG_marsbar_97_115_97_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))   
   l_pFusG  <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_postFusG_marsbar_97_115_97_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" )) 
   l_iLOC   <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_iLOC_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))   
   r_iLOC   <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_iLOC_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))     
   r_toITG  <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_toITG_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))  
   l_toITG  <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_toITG_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))    
   l_aITG   <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_aITG_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))   
   r_aITG   <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_aITG_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))     
   r_pITG   <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_pITG_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))    
   l_pITG   <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_pITG_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))    
   
   Amy= data.frame(matrix(nrow = 31, ncol = 1))
   Amy$ID = l_Amy$ID
   Amy$vis_HIInsight_1 <- (l_Amy$vis_HIInsight_1   + r_Amy$vis_HIInsight_1)/2
   Amy$vis_HIInsight_2 <- (l_Amy$vis_HIInsight_2   + r_Amy$vis_HIInsight_2)/2
   Amy$vis_HIInsight_3 <- (l_Amy$vis_HIInsight_3   + r_Amy$vis_HIInsight_3)/2
   Amy$vis_HIInsight_4 <- (l_Amy$vis_HIInsight_4   + r_Amy$vis_HIInsight_4)/2
   
   Amy$vis_LOInsight_1 <- (l_Amy$vis_LOInsight_1   + r_Amy$vis_LOInsight_1)/2
   Amy$vis_LOInsight_2 <- (l_Amy$vis_LOInsight_2   + r_Amy$vis_LOInsight_2)/2
   Amy$vis_LOInsight_3 <- (l_Amy$vis_LOInsight_3   + r_Amy$vis_LOInsight_3)/2
   Amy$vis_LOInsight_4 <- (l_Amy$vis_LOInsight_4   + r_Amy$vis_LOInsight_4)/2
   
   aHC= data.frame(matrix(nrow = 31, ncol = 1))
   aHC$ID = l_aHC$ID
   aHC$vis_HIInsight_1 <- (l_aHC$vis_HIinsight_1   + r_aHC$vis_HIinsight_1)/2
   aHC$vis_HIInsight_2 <- (l_aHC$vis_HIinsight_2   + r_aHC$vis_HIinsight_2)/2
   aHC$vis_HIInsight_3 <- (l_aHC$vis_HIinsight_3   + r_aHC$vis_HIinsight_3)/2
   aHC$vis_HIInsight_4 <- (l_aHC$vis_HIinsight_4   + r_aHC$vis_HIinsight_4)/2
   
   aHC$vis_LOInsight_1 <- (l_aHC$vis_LOinsight_1   + r_aHC$vis_LOinsight_1)/2
   aHC$vis_LOInsight_2 <- (l_aHC$vis_LOinsight_2   + r_aHC$vis_LOinsight_2)/2
   aHC$vis_LOInsight_3 <- (l_aHC$vis_LOinsight_3   + r_aHC$vis_LOinsight_3)/2
   aHC$vis_LOInsight_4 <- (l_aHC$vis_LOinsight_4   + r_aHC$vis_LOinsight_4)/2
   
   pHC= data.frame(matrix(nrow = 31, ncol = 1))
   pHC$ID = l_pHC$ID
   pHC$vis_HIInsight_1 <- (l_pHC$vis_HIinsight_1   + r_pHC$vis_HIinsight_1)/2
   pHC$vis_HIInsight_2 <- (l_pHC$vis_HIinsight_2   + r_pHC$vis_HIinsight_2)/2
   pHC$vis_HIInsight_3 <- (l_pHC$vis_HIinsight_3   + r_pHC$vis_HIinsight_3)/2
   pHC$vis_HIInsight_4 <- (l_pHC$vis_HIinsight_4   + r_pHC$vis_HIinsight_4)/2
   
   pHC$vis_LOInsight_1 <- (l_pHC$vis_LOinsight_1   + r_pHC$vis_LOinsight_1)/2
   pHC$vis_LOInsight_2 <- (l_pHC$vis_LOinsight_2   + r_pHC$vis_LOinsight_2)/2
   pHC$vis_LOInsight_3 <- (l_pHC$vis_LOinsight_3   + r_pHC$vis_LOinsight_3)/2
   pHC$vis_LOInsight_4 <- (l_pHC$vis_LOinsight_4   + r_pHC$vis_LOinsight_4)/2
   
   
   iLOC= data.frame(matrix(nrow = 31, ncol = 1))
   iLOC$ID = l_iLOC$ID
   iLOC$vis_HIInsight_1 <- (l_iLOC$vis_HIinsight_1   + r_iLOC$vis_HIinsight_1)/2
   iLOC$vis_HIInsight_2 <- (l_iLOC$vis_HIinsight_2   + r_iLOC$vis_HIinsight_2)/2
   iLOC$vis_HIInsight_3 <- (l_iLOC$vis_HIinsight_3   + r_iLOC$vis_HIinsight_3)/2
   iLOC$vis_HIInsight_4 <- (l_iLOC$vis_HIinsight_4   + r_iLOC$vis_HIinsight_4)/2
   
   iLOC$vis_LOInsight_1 <- (l_iLOC$vis_LOinsight_1   + r_iLOC$vis_LOinsight_1)/2
   iLOC$vis_LOInsight_2 <- (l_iLOC$vis_LOinsight_2   + r_iLOC$vis_LOinsight_2)/2
   iLOC$vis_LOInsight_3 <- (l_iLOC$vis_LOinsight_3   + r_iLOC$vis_LOinsight_3)/2
   iLOC$vis_LOInsight_4 <- (l_iLOC$vis_LOinsight_4   + r_iLOC$vis_LOinsight_4)/2
   
   
   pFusG= data.frame(matrix(nrow = 31, ncol = 1))
   pFusG$ID = l_pFusG$ID
   pFusG$vis_HIInsight_1 <- (r_pFusG$vis_HIinsight_1   + l_pFusG$vis_HIinsight_1)/2
   pFusG$vis_HIInsight_2 <- (r_pFusG$vis_HIinsight_2   + l_pFusG$vis_HIinsight_2)/2
   pFusG$vis_HIInsight_3 <- (r_pFusG$vis_HIinsight_3   + l_pFusG$vis_HIinsight_3)/2
   pFusG$vis_HIInsight_4 <- (r_pFusG$vis_HIinsight_4   + l_pFusG$vis_HIinsight_4)/2
   
   pFusG$vis_LOInsight_1 <- (r_pFusG$vis_LOinsight_1   + l_pFusG$vis_LOinsight_1)/2
   pFusG$vis_LOInsight_2 <- (r_pFusG$vis_LOinsight_2   + l_pFusG$vis_LOinsight_2)/2
   pFusG$vis_LOInsight_3 <- (r_pFusG$vis_LOinsight_3   + l_pFusG$vis_LOinsight_3)/2
   pFusG$vis_LOInsight_4 <- (r_pFusG$vis_LOinsight_4   + l_pFusG$vis_LOinsight_4)/2
  
   aFusG= data.frame(matrix(nrow = 31, ncol = 1))
   aFusG$ID = l_aFusG$ID
   aFusG$vis_HIInsight_1 <- (r_aFusG$vis_HIinsight_1   + l_aFusG$vis_HIinsight_1)/2
   aFusG$vis_HIInsight_2 <- (r_aFusG$vis_HIinsight_2   + l_aFusG$vis_HIinsight_2)/2
   aFusG$vis_HIInsight_3 <- (r_aFusG$vis_HIinsight_3   + l_aFusG$vis_HIinsight_3)/2
   aFusG$vis_HIInsight_4 <- (r_aFusG$vis_HIinsight_4   + l_aFusG$vis_HIinsight_4)/2
   
   aFusG$vis_LOInsight_1 <- (r_aFusG$vis_LOinsight_1   + l_aFusG$vis_LOinsight_1)/2
   aFusG$vis_LOInsight_2 <- (r_aFusG$vis_LOinsight_2   + l_aFusG$vis_LOinsight_2)/2
   aFusG$vis_LOInsight_3 <- (r_aFusG$vis_LOinsight_3   + l_aFusG$vis_LOinsight_3)/2
   aFusG$vis_LOInsight_4 <- (r_aFusG$vis_LOinsight_4   + l_aFusG$vis_LOinsight_4)/2
   
   pITG= data.frame(matrix(nrow = 31, ncol = 1))
   pITG$ID = l_toITG$ID
   pITG$vis_HIInsight_1 <- (r_toITG$vis_HIinsight_1   + l_toITG$vis_HIinsight_1)/2
   pITG$vis_HIInsight_2 <- (r_toITG$vis_HIinsight_2   + l_toITG$vis_HIinsight_2)/2
   pITG$vis_HIInsight_3 <- (r_toITG$vis_HIinsight_3   + l_toITG$vis_HIinsight_3)/2
   pITG$vis_HIInsight_4 <- (r_toITG$vis_HIinsight_4   + l_toITG$vis_HIinsight_4)/2
   
   pITG$vis_LOInsight_1 <- (r_toITG$vis_LOinsight_1   + l_toITG$vis_LOinsight_1)/2
   pITG$vis_LOInsight_2 <- (r_toITG$vis_LOinsight_2   + l_toITG$vis_LOinsight_2)/2
   pITG$vis_LOInsight_3 <- (r_toITG$vis_LOinsight_3   + l_toITG$vis_LOinsight_3)/2
   pITG$vis_LOInsight_4 <- (r_toITG$vis_LOinsight_4   + l_toITG$vis_LOinsight_4)/2

   
   aITG= data.frame(matrix(nrow = 31, ncol = 1))
   aITG$ID = l_aITG$ID
   aITG$vis_HIInsight_1 <- (r_aITG$vis_HIinsight_1   + l_aITG$vis_HIinsight_1)/2
   aITG$vis_HIInsight_2 <- (r_aITG$vis_HIinsight_2   + l_aITG$vis_HIinsight_2)/2
   aITG$vis_HIInsight_3 <- (r_aITG$vis_HIinsight_3   + l_aITG$vis_HIinsight_3)/2
   aITG$vis_HIInsight_4 <- (r_aITG$vis_HIinsight_4   + l_aITG$vis_HIinsight_4)/2
   
   aITG$vis_LOInsight_1 <- (r_aITG$vis_LOinsight_1   + l_aITG$vis_LOinsight_1)/2
   aITG$vis_LOInsight_2 <- (r_aITG$vis_LOinsight_2   + l_aITG$vis_LOinsight_2)/2
   aITG$vis_LOInsight_3 <- (r_aITG$vis_LOinsight_3   + l_aITG$vis_LOinsight_3)/2
   aITG$vis_LOInsight_4 <- (r_aITG$vis_LOinsight_4   + l_aITG$vis_LOinsight_4)/2

   
   mITG= data.frame(matrix(nrow = 31, ncol = 1))
   mITG$ID = l_pITG$ID
   mITG$vis_HIInsight_1 <- (r_pITG$vis_HIinsight_1   + l_pITG$vis_HIinsight_1)/2
   mITG$vis_HIInsight_2 <- (r_pITG$vis_HIinsight_2   + l_pITG$vis_HIinsight_2)/2
   mITG$vis_HIInsight_3 <- (r_pITG$vis_HIinsight_3   + l_pITG$vis_HIinsight_3)/2
   mITG$vis_HIInsight_4 <- (r_pITG$vis_HIinsight_4   + l_pITG$vis_HIinsight_4)/2
   
   mITG$vis_LOInsight_1 <- (r_pITG$vis_LOinsight_1   + l_pITG$vis_LOinsight_1)/2
   mITG$vis_LOInsight_2 <- (r_pITG$vis_LOinsight_2   + l_pITG$vis_LOinsight_2)/2
   mITG$vis_LOInsight_3 <- (r_pITG$vis_LOinsight_3   + l_pITG$vis_LOinsight_3)/2
   mITG$vis_LOInsight_4 <- (r_pITG$vis_LOinsight_4   + l_pITG$vis_LOinsight_4)/2
   
  
   
   data_HILOI <- rbind(Amy[,-1],aHC[,-1],pHC[,-1])#  iLOC[-1],aFusG[,-1], pFusG[,-1], aITG[,-1],  mITG[,-1], pITG[,-1] )#
   data_HILOI$roi=  rep(c('Amy','aHC','pHC'# 'iLOC', 'aFusG', 'pFusG',  'aITG', 'mITG','pITG'   #, 
   ), each=31)# 
   library(tidyr)
   data_HILOI_long <- data_HILOI %>% pivot_longer(cols = 'vis_HIInsight_1':'vis_LOInsight_4', names_to='condition', values_to="betas")
   data_HILOI_long$session = rep(c(1,2,3,4), 2*31*3) #'3*conds*31subj*7ROIs'
   data_HILOI_long$condition <- gsub('vis_', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition <- gsub('_1', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition <- gsub('_2', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition <- gsub('_3', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition <- gsub('_4', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition = ordered(data_HILOI_long$condition, levels = c('LOInsight','HIInsight'), 
                                       labels=c('LO-I','HI-I' ))
  
   data_HILOI_long$roi_ord = ordered(data_HILOI_long$roi, levels=c("Amy" ,"aHC","pHC"  ),
                                             labels=c("Amy" ,"aHC","pHC"))
   
  ### test for amygdala and hippocampal activity during solution as a function of Insight
  HIvLOI_m0_Amy <- lmer(betas ~                   session +(1|ID) ,data= data_HILOI_long, na.action  = na.omit) #[data_HILOI_long$roi == "Amy",]
  HIvLOI_m1_Amy <- lmer(betas ~           roi_ord+session +(1|ID) ,data= data_HILOI_long, na.action  = na.omit) #[data_HILOI_long$roi == "Amy",]
  HIvLOI_m2_Amy <- lmer(betas ~ condition+roi_ord+session +(1|ID) ,data= data_HILOI_long, na.action  = na.omit) #[data_HILOI_long$roi == "Amy",]
  HIvLOI_m3_Amy <- lmer(betas ~ condition*roi_ord+session +(1|ID) ,data= data_HILOI_long, na.action  = na.omit) #[data_HILOI_long$roi == "Amy",]
  
  anova(HIvLOI_m0_Amy, HIvLOI_m1_Amy,HIvLOI_m2_Amy,HIvLOI_m3_Amy)
  tab_model(HIvLOI_m2_Amy, show.std = T)
  emmeans(HIvLOI_m3_Amy, list(pairwise ~ condition| roi_ord), adjust = "tukey") 
  
  # plot Insight activity results at solution for amygdala
  HIvLOI_Amy_ggplot = ggpredict(HIvLOI_m3_Amy , c( 'roi_ord','condition')) 
  HIvLOI_Amy_finalplot= ggplot(HIvLOI_Amy_ggplot, aes(x= x, y = predicted ,fill=group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T) + #, color = "black"
    geom_errorbar(aes(ymin=predicted-std.error , ymax=predicted+std.error ), width=.2,size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs( x="Insight",
                  y = "Beta estimate during solution", fill = "Insight")+
                   scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))
  
  
   ### test for VOTC activity during solution as a function of Insight
   HIvLOI_m0_VS <- lmer(betas ~ session+roi  +(1|ID) ,data= data_HILOI_long, na.action  = na.omit) #
   HIvLOI_m1_VS <- lmer(betas ~ condition+session+roi  +(1|ID) ,data= data_HILOI_long, na.action  = na.omit) #
   HIvLOI_m2_VS <- lmer(betas ~ condition*roi +session +(1|ID) ,data= data_HILOI_long, na.action  = na.omit) #
   
   anova(HIvLOI_m0_VS, HIvLOI_m1_VS,HIvLOI_m2_VS)
   tab_model(HIvLOI_m1_VS, show.std = T)
   emmeans(HIvLOI_m2_VS, list(pairwise ~ condition| roi), adjust = "none") 
   #adjust for multiple comparison
   p = c(0.9412,0.1992, .0001,0.4261,.0001, 0.0855 )
   p.adjust(p, method = 'fdr', n=length(p))
   
   # plot Insight activity results at solution for VOTC areas
   HIvLOI_VOTC_ggplot = ggpredict(HIvLOI_m2_VS , c('roi',  'condition')) #%>%plot()+ ggplot2::theme_classic() +
   # labs(title = "Predicted probabilities for Condition", x = "Conditions", y = "Beta estimate")
   HIvLOI_VOTC_finalplot= ggplot(HIvLOI_VOTC_ggplot, aes(x= x, y = predicted,fill= group )) + #
     geom_bar(stat="identity",  position=position_dodge(),show.legend = T) + #, color = "black"
     geom_errorbar(aes(ymin=predicted-std.error , ymax=predicted+std.error ), width=.2, size = 1.1,
                   position=position_dodge(.9)) + theme_classic() +labs(fill = "Insight",#title = "InsightVOTC regions",
              x = "ROI", y = "Beta estimate during solution", fill = "Solution style")+ #scale_fill_grey() 
     scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))
   
   
#######################################################################################-  


####2c) Amy & HC: Insight*Memory: with  single trials betas #####  
    newdata = PPSS[ 
      PPSS$ROI == "r_Amygdala"  |
        PPSS$ROI == "l_Amygdala"  |
        PPSS$ROI == "l_aHC"  |
        PPSS$ROI == "r_aHC"  |
        PPSS$ROI == "l_pHC" |
        PPSS$ROI == "r_pHC" 
      ,] 
    
    newdata$ROI_bilat = NA
    newdata[newdata$ROI == "r_Amygdala",]$ROI_bilat = 'Amy'
    newdata[newdata$ROI == "l_Amygdala",]$ROI_bilat = 'Amy'
    newdata[newdata$ROI == "l_aHC",]$ROI_bilat = 'aHC'
    newdata[newdata$ROI == "r_aHC",]$ROI_bilat = 'aHC'
    newdata[newdata$ROI == "r_pHC",]$ROI_bilat = 'pHC'
    newdata[newdata$ROI == "l_pHC",]$ROI_bilat = 'pHC'
    
    # Amy & HC Insight
    Activity_data = newdata[ !is.na(newdata$insight_mediansplit)  & newdata$tp == 1 ,]
    Activity_M1 <- lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat+                        (1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M2a <-lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat+insight_mediansplit_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M3 <- lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat*insight_mediansplit_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    
    hist(residuals(Activity_M3)) # residuals are normally distributed -> model is adecuate
    anova( Activity_M1,Activity_M2a,Activity_M3)
    check_collinearity(Activity_M2a)
    effectsize(Activity_M2a);    
    effectsize(Activity_M3);
    emmeans(Activity_M3, list(pairwise ~  insight_mediansplit_fac|ROI_bilat), adjust = 'tukey')
    
    #Amy & HC Insight Memory
    Activity_data = newdata[  !is.na(newdata$SME_solve) & !is.na(newdata$insight_mediansplit)  & newdata$tp == 1  ,] #& newdata$ROI_bilat == "xaHC"
    Activity_M2b <-lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat+insight_mediansplit_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M4 <- lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat+insight_mediansplit_fac+SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M5 <- lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat+insight_mediansplit_fac*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_M6 <- lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat*insight_mediansplit_fac*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)

    anova( Activity_M2b, Activity_M4,Activity_M5,Activity_M6)
    effectsize(Activity_M4);    
    effectsize(Activity_M5);    
    effectsize(Activity_M6);    
    
    plot(check_collinearity(Activity_M5))
    tab_model(Activity_M5, show.std =T)
    emmeans(Activity_M3, list(pairwise ~  insight_mediansplit_fac), adjust = 'tukey')
    emmeans(Activity_M4, list(pairwise ~  insight_mediansplit_fac|ROI_bilat), adjust = 'tukey')
    emmeans(Activity_M5, list(pairwise ~  SME_solve_fac |ROI_bilat  ), adjust = 'tukey')#,pbkrtest.limit = 10278)
    emmeans(Activity_M6, list(pairwise ~  insight_mediansplit_fac|SME_solve_fac |ROI_bilat  ), adjust = 'tukey')#,pbkrtest.limit = 10278)
    
    IME_Amy_HC_ggplot = ggpredict(Activity_M6 , c('SME_solve_fac','insight_mediansplit_fac','ROI_bilat'))#%>%plot()+ggplot2::theme_classic()
    IME_Amy_HC_ggplot_finalPlot=ggplot(IME_Amy_HC_ggplot, aes(x= x, y = predicted , fill= group)) +
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  #coord_cartesian(ylim=c(0,.13)) + #+color = "black"
      geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                    position=position_dodge(.9)) + theme_classic()  +labs( fill = "Insight", #title = "",
                      x = "Subsequent Memory",y = 'Beta estimate during solution' ) +  
      scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578")) +
        facet_wrap(~ facet, nrow=1)#"#fdfd00", 
    IME_Amy_HC_ggplot_finalPlot
    
###### Exploratory: Check for Insight*Memory Interaction for VOTC Areas ########

    newdata = PPSS[ 
      PPSS$ROI_bilat == "pFusG"  |
        PPSS$ROI_bilat == "aFusG"  |
        PPSS$ROI_bilat == "pITG"  |
        PPSS$ROI_bilat == "aITG"  |
        PPSS$ROI_bilat == "mITG"  |
        PPSS$ROI_bilat == "iLOC"   ,] 
    
    #Amy & HC Insight Memory
    Activity_data = newdata[  !is.na(newdata$SME_solve) & !is.na(newdata$insight_mediansplit)  & newdata$tp == 1  ,] #& newdata$ROI_bilat == "xaHC"
    Activity_data[Activity_data$SME_solve== "Remembered" ,]$SME_solve= "R"
    Activity_data[Activity_data$SME_solve== "Forgotten",]$SME_solve= "F"
    Activity_data$SME_solve_fac = as.factor(Activity_data$SME_solve)
    Activity_V4 <- lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat+insight_mediansplit_fac+              (1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_V5 <- lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat+insight_mediansplit_fac+SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_V6 <- lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat+insight_mediansplit_fac*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    Activity_V7 <- lmer(Beta_Buttonpress ~ cor2+sessionblock +RT+ROI_bilat*insight_mediansplit_fac*SME_solve_fac+(1|ID) + (1|Item),data= Activity_data, na.action  = na.omit)
    
    anova( Activity_V4, Activity_V5,Activity_V6,Activity_V7)
    effectsize(Activity_V5);    
    effectsize(Activity_V6);    
    effectsize(Activity_V7);    
    
    plot(check_collinearity(Activity_V5))
    tab_model(Activity_V7, show.std =T)
    emmeans(Activity_V7, list(pairwise ~  insight_mediansplit_fac|SME_solve_fac |ROI_bilat  ), adjust = 'tukey')#,pbkrtest.limit = 10278)
    
    IME_VOTC_ggplot = ggpredict(Activity_V7 , c('SME_solve_fac','insight_mediansplit_fac','ROI_bilat'))#%>%plot()+ggplot2::theme_classic()
    IME_VOTC_ggplot_finalPlot=ggplot(IME_VOTC_ggplot, aes(x= x, y = predicted , fill= group)) +
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  #coord_cartesian(ylim=c(0,.13)) + #+color = "black"
      geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                    position=position_dodge(.9)) + theme_classic()  +labs( fill = "Insight", #title = "",
                                                                           x = "Subsequent Memory",y = 'Beta estimate during solution' ) +  
      scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578")) +
      facet_wrap(~ facet, nrow=1)#"#fdfd00", 
    IME_VOTC_ggplot_finalPlot
    
####### 3) multivariate ROI analysis - Brain areas representing sudden conceptual update during insight #####

    newdata = PPSS[ 
      PPSS$ROI_bilat == "pFusG"  |
        PPSS$ROI_bilat == "aFusG"  |
        PPSS$ROI_bilat == "pITG"  |
        PPSS$ROI_bilat == "aITG"  |
        PPSS$ROI_bilat == "mITG"  |
        PPSS$ROI_bilat == "iLOC"   ,] 
    
######## 3a) RSA1-Insight - Multivoxel Pattern Similarity ######################################################
  ERSdata = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) )  &   !is.na(newdata$insight_mediansplit) & newdata$tp == 1,]
  ERSdata$ERS_PrePost_R_delta = 1-ERSdata$ERS_PrePost_R 
  ERS_M0 <- lmer(ERS_PrePost_R_delta ~ cor2_fac + RT+                    ROI_bilat +sessionblock+(1|ID) + (1|Item),data= ERSdata, na.action  = na.omit)
  ERS_M1 <- lmer(ERS_PrePost_R_delta ~ cor2_fac + RT+insight_mediansplit+ROI_bilat +sessionblock+(1|ID) + (1|Item),data= ERSdata, na.action  = na.omit)
  ERS_M2 <- lmer(ERS_PrePost_R_delta ~ cor2_fac + RT+insight_mediansplit*ROI_bilat +sessionblock+(1|ID) + (1|Item) , data= ERSdata, na.action  = na.omit)
  
  hist(residuals(ERS_M2)) # residuals are normally distributed -> model is adecuate
  
  anova( ERS_M0,ERS_M1, ERS_M2)#
  tab_model(ERS_M1, show.std  = T)
  emmeans(ERS_M2, list(pairwise ~ insight_mediansplit | ROI_bilat ), adjust = "tukey")

  # plot RSA1-Insight results for pFusG & iLOC
  ERS_ggpredict = ggpredict(ERS_M2 , c('ROI_bilat','insight_mediansplit'))
  ERS_finalPlot= ggplot(ERS_ggpredict, aes(x= x, y = predicted , fill= group)) + scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))+
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  coord_cartesian(ylim=c(0.67,.93)) + #+color = "black"
    geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs( fill = "Insight", #title = "",
                  x = "ROI",y = expression("Δ MVPS: 1-r"[post - pre])  ) 

#p.adjust(p_verb1, method = 'bonferroni', n = length(p_verb1))
#p.adjust(p_verb2, method = 'bonferroni', n = length(p_verb2))


#### 3a1) RSA1-Insight*Memory Multivoxel Pattern Similarity ############  

newdata = newdata[ 
    newdata$ROI_bilat == "pFusG"  |
    newdata$ROI_bilat == "iLOC"  #|
  ,] 

  ERSdata = newdata[!is.na(newdata$SME_solve_fac) & newdata$tp == 1,]
  ERSdata$ERS_PrePost_R_delta = 1-ERSdata$ERS_PrePost_R 

  ERS_M0_IME <- lmer(ERS_PrePost_R_delta ~ cor2_fac+RT+               insight_mediansplit_fac+ROI_bilat +sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit)
  ERS_M1_IME <- lmer(ERS_PrePost_R_delta ~ cor2_fac+RT+SME_solve_fac+ insight_mediansplit_fac+ROI_bilat +sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit)
  ERS_M2_IME <- lmer(ERS_PrePost_R_delta ~ cor2_fac+RT+SME_solve_fac* insight_mediansplit_fac+ROI_bilat +sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit)
  ERS_M3_IME <- lmer(ERS_PrePost_R_delta ~ cor2_fac+RT+SME_solve_fac* insight_mediansplit_fac*ROI_bilat +sessionblock+(1|ID)+ (1|Item), data= ERSdata, na.action  = na.omit)
  
  hist(residuals(ERS_M2_IME)) # residuals are normally distributed -> model is adecuate
  anova(ERS_M0_IME, ERS_M1_IME, ERS_M2_IME,ERS_M3_IME)#, RSA4)#,  M4)#
  ggpredict(ERS_M3_IME , c(  'ROI_bilat','insight_mediansplit_fac', 'SME_solve_fac')) %>% plot(show.title = F) + ggplot2::theme_classic()
  emmeans(ERS_M3_IME, list(pairwise ~  SME_solve_fac | insight_mediansplit_fac |  ROI_bilat   ), adjust = "tukey")
  tab_model(ERS_M3_IME, show.std = T)
 
  ERS_Mem_ggpredict = ggpredict(ERS_M3_IME , c( 'SME_solve_fac','insight_mediansplit_fac', 'ROI_bilat')) #%>%plot()+ggplot2::theme_classic()
  ERS_Mem_finalPlot= ggplot(ERS_Mem_ggpredict, aes(x= x, y = predicted , fill= group)) +
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  coord_cartesian(ylim=c(0.69,.95)) + #+color = "black"
    geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs(fill = "Insight", #title = "", 
                    x = "Subsequent Memory",y = expression("Δ MVPS: 1-r"[post - pre])  ) +  
    scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578")) + facet_wrap(~ facet, nrow=1)#"#fdfd00", 
  ERS_Mem_finalPlot


########  3b) RSA2-Insight  ####################################################
newdata = PPSS[ 
    PPSS$ROI_bilat == "pFusG"  |
    PPSS$ROI_bilat == "iLOC"  |
    PPSS$ROI_bilat == "pITG" 
  ,] 
#### 3b1) RSA2-Insight using AlexNet8 ######
# insight_cutoff 
RSAdata = newdata[  !is.na(newdata$insight_mediansplit) & newdata$RSA_AlexNet8_realpix_control_corr_n_vis > 10 ,]
M0AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ cor2_fac+RT+tp_fac+ ROI_bilat                   +sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
M1AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ cor2_fac+RT+tp_fac+insight_mediansplit_fac+ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
M2AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ cor2_fac+RT+tp_fac*insight_mediansplit_fac+ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
M3AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ cor2_fac+RT+tp_fac*insight_mediansplit_fac*ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)

  hist(residuals(M3AN)) # residuals are normally distributed -> model is adecuate
  anova(M0AN, M1AN, M2AN, M3AN)
  emmeans(M3AN, list(pairwise ~ insight_mediansplit | tp_fac | ROI_bilat), adjust = "tukey")
  emmeans(M2AN, list(pairwise ~ insight_mediansplit | tp_fac), adjust = "tukey")
  tab_model(M2AN, show.std = T)
  
  #plot RSA2-Insight results for pFusG
  RSAN8_ggpredict = ggpredict(M3AN , c('tp_fac','insight_mediansplit_fac', 'ROI_bilat'))
  RSAAN8_finalPlot_all= ggplot(RSAN8_ggpredict, aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2, size =.95,
                  position=position_dodge(.9)) + theme_classic() +labs( fill = "condition", #title = "pFusG",
                    x = "Time",y = "Rep-Str (AlexNet)")+scale_x_discrete(labels=c('Pre', 'Post'))+
                    scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))+ facet_wrap(~ facet, nrow=1)#
  

#### 3b1a) RSA2-IME using AlexNet8: Insight memory effect for representations #####

  newdata = newdata[ 
    newdata$ROI_bilat == "pFusG"  |
      newdata$ROI_bilat == "iLOC"  #|
  ,]     

  # insight_sum 
  RSAdata = newdata[ !is.na(newdata$SME_solve_fac)  & newdata$RSA_AlexNet8_realpix_control_corr_n_vis > 10 & newdata$tp != 2 ,]
  M0AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ cor2_fac+RT+tp_fac*insight_mediansplit_fac+             +ROI_bilat+sessionblock+(1|ID)+(1|Item),data= RSAdata, na.action  = na.omit)
  M1AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ cor2_fac+RT+tp_fac+SME_solve_fac*insight_mediansplit_fac+ROI_bilat+sessionblock+(1|ID)+(1|Item),data= RSAdata, na.action  = na.omit)
  M2AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ cor2_fac+RT+tp_fac*SME_solve_fac*insight_mediansplit_fac+ROI_bilat+sessionblock+(1|ID)+(1|Item),data= RSAdata, na.action  = na.omit)
  M3AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ cor2_fac+RT+tp_fac*SME_solve_fac*insight_mediansplit_fac*ROI_bilat+sessionblock+(1|ID)+(1|Item),data= RSAdata, na.action  = na.omit)
  
  hist(residuals(M1AN_mem)) # residuals are normally distributed -> model is adecuate
  anova(M0AN_mem, M1AN_mem, M2AN_mem,M3AN_mem)
  tab_model(M2AN_mem, show.std =T)
  ggpredict(M2AN_mem , c( 'tp_fac', 'insight_mediansplit_fac', 'SME_solve_fac')) %>% plot(show.title = F) + ggplot2::theme_classic()
  emmeans(M2AN_mem, list(pairwise ~  insight_mediansplit_fac  |  SME_solve_fac |tp_fac ), adjust = "tukey")
  summary(M2AN_mem)
  AN08_Mem_ggpredict = ggpredict(M2AN_mem , c('tp_fac','insight_mediansplit_fac','SME_solve_fac' ))#%>%plot()+ggplot2::theme_classic()
  AN08_Mem_finalPlot=ggplot(AN08_Mem_ggpredict, aes(x= x, y = predicted , fill= group)) +
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  coord_cartesian(ylim=c(0,.08)) + #+color = "black"
    geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs(fill = "Insight", #title = "Stimulus Onset", 
                   x = "Subsequent Memory",y = 'Rep.Str (AN08) ' ) +  
    scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578")) + facet_wrap(~ facet, nrow=1)#"#fdfd00", 
  AN08_Mem_finalPlot
  
#### 3b2) RSA2-Insight using W2V (word2vec) ################

  newdata = PPSS[ 
      PPSS$ROI_bilat == "pFusG"  |
      PPSS$ROI_bilat == "iLOC"  |
      PPSS$ROI_bilat == "pITG" 
    ,]
  
  RSAdata = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) ) & !is.na(newdata$insight_mediansplit) 
                     & newdata$RSA_W2V_control_corr_n_vis > 10 ,]
  M0W2V <- lmer(RSA_W2V_R_vis ~ cor2_fac+RT+tp_fac                    +ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M1W2V <- lmer(RSA_W2V_R_vis ~ cor2_fac+RT+tp_fac+insight_mediansplit+ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2W2V <- lmer(RSA_W2V_R_vis ~ cor2_fac+RT+tp_fac*insight_mediansplit+ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M3W2V <- lmer(RSA_W2V_R_vis ~ cor2_fac+RT+tp_fac*insight_mediansplit*ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  
  hist(residuals(M2W2V)) # residuals are normally distributed -> model is adecuate
  anova(M0W2V, M1W2V, M2W2V,M3W2V)
  tab_model(M2W2V, show.std = T)
  emmeans(M3W2V, list(pairwise ~ insight_mediansplit | tp_fac | ROI_bilat), adjust = "tukey")
  
  # plot W2V-Insight results for pFusG
  W2V_ggpredict = ggpredict(M3W2V , c('tp_fac','insight_mediansplit', 'ROI_bilat'))
  W2V_finalPlot_all= ggplot(W2V_ggpredict, aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2, size =.95,
                  position=position_dodge(.9)) + theme_classic() +labs( fill = "condition", #title = "pFusG",
                 x = "Time",y = "Rep-Str (W2V)")+scale_x_discrete(labels=c('Pre', 'Post'))+
    scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))+ facet_wrap(~ facet, nrow=1)#
  

#### 3b2a) RSA2-Insight*Memory using W2V: Insight memory effect for representations  #####
  
  newdata = PPSS[ PPSS$ROI_bilat == "pFusG"  |PPSS$ROI_bilat == "iLOC"   ,]
  
 RSAdata = newdata[ !is.na(newdata$SME_solve_fac) & newdata$RSA_W2V_control_corr_n_vis > 10 & newdata$tp != 2,]
    M0W2V_mem <- lmer(RSA_W2V_R_vis ~cor2_fac+RT+tp_fac*insight_mediansplit_fac+ROI_bilat+sessionblock+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M1W2V_mem <- lmer(RSA_W2V_R_vis ~cor2_fac+RT+tp_fac*insight_mediansplit_fac+SME_solve_fac+ROI_bilat+sessionblock+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M2W2V_mem <- lmer(RSA_W2V_R_vis ~cor2_fac+RT+tp_fac*insight_mediansplit_fac*SME_solve_fac+ROI_bilat+sessionblock+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
    M3W2V_mem <- lmer(RSA_W2V_R_vis ~cor2_fac+RT+tp_fac*insight_mediansplit_fac*SME_solve_fac*ROI_bilat+sessionblock+(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)

    hist(residuals(M2W2V_mem)) # residuals are normally distributed -> model is adecuate
    anova(M0W2V_mem, M1W2V_mem, M2W2V_mem,M3W2V_mem)#,M3W2V_mem)#, RSA4)#,  M4)#
    tab_model(M2W2V_mem, show.std =T)
    summary(M2W2V_mem)
    ggpredict(M3W2V_mem , c( 'tp_fac', 'insight_mediansplit_fac', 'SME_solve_fac')) %>% plot(show.title = F) + ggplot2::theme_classic()
    emmeans(M3W2V_mem, list(pairwise ~  SME_solve_fac | insight_mediansplit_fac   ), adjust = "tukey")
    
    
    W2V_Mem_ggpredict = ggpredict(M2W2V_mem , c('tp_fac' ,'insight_mediansplit_fac','SME_solve_fac' ))#%>%plot()+ggplot2::theme_classic()
    W2V_Mem_finalPlot=ggplot(W2V_Mem_ggpredict, aes(x= x, y = predicted , fill= group)) +
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  coord_cartesian(ylim=c(0,.05)) + #+color = "black"
      geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                    position=position_dodge(.9)) + theme_classic() +labs(fill = "Insight", #title = "Stimulus Onset", 
                                                                         x = "Subsequent Memory",y = 'Rep.Str (W2V) ' ) +  
      scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578")) + facet_wrap(~ facet, nrow=1)#"#fdfd00", 
    W2V_Mem_finalPlot
    

######################################################################################  -
#### 5) Plotting all results together #####################     
    library(ggpubr)
  
    ### Figure 2: plot behavioral data 
    Insight_behave_ggarangeplot <- ggarrange( AHA_amount_plot,IME_ggpredict_plot,
                                              common.legend = T, legend = "right",
                                              ncol = 2, nrow = 1,labels = c("A", "B"))
    
    ### Figure 3-A
    ERS_ggarangeplot <- ggarrange( ERS_finalPlot,
                                   common.legend = F, legend = "bottom",
                                   ncol = 1, nrow = 1)#labels = c("Change in MVPS"),
    
    ### Figure 3-B
    RSA_AN8_W2V_ggarrangeplot <- ggarrange( RSAAN8_finalPlot_all, W2V_finalPlot_all,
                                            common.legend = FALSE, legend = "top",
                                            ncol = 1, nrow = 2) #                               
    ### Figure 3-AB
    FigX <- ggarrange( ERS_ggarangeplot,RSA_AN8_W2V_ggarrangeplot,
                       common.legend = T, legend = "bottom",
                       ncol = 2, nrow = 1, labels = c("A", "B"))
                        
    #### Figure 5: IME for MVPS, Rep.Strength, Amy activity and FC
    IME_AN8_W2V_Amy_hC <- ggarrange(ERS_Mem_finalPlot, AN08_Mem_finalPlot,IME_Amy_HC_ggplot_finalPlot, W2V_Mem_finalPlot,#IME_conn_finalPlot,
                                    common.legend = TRUE, legend = "bottom",
                                    labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
    
    ### Figure S3
    FigS3 <- ggarrange( FigS3_RT,FigS3_RT_cor,ncol = 2, nrow = 1, legend = "top",common.legend = TRUE)
    
    ### Figure S4
    FigS4 <- ggarrange(  HIvLOI_VOTC_finalplot, IME_VOTC_ggplot_finalPlot, common.legend = T, legend = "bottom",
                         ncol = 2, nrow = 1, labels = c("A", "B"))
  
    