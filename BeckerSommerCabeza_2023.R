
#### Code for Manuscript Becker, Sommer & Cabeza 2023 
#### Title: Creativity and memory: Cortical representational change along with amygdala activation predict the insight memory effect
#### (c) almaxi@gmail.com
#### last update: 02/June/2023

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
#library(effectsize)
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
setwd('C:/Users/Maxi/Google Drive/studies/Maxi/PVI/results/data_4_publication') 
load('BeckerSommerCabeza_2023.Rda') 

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
  
  s12_RT<- plot_multi_histogram_neu(agg_data_all1[!is.na(agg_data_all1$RT),], 'RT', 'insight_mediansplit ')+labs(x = "Solution time in sec (all)",y = "Density", fill = "Insight")+
    geom_vline(aes(xintercept=median(agg_data_all1[agg_data_all1$insight_mediansplit == "HI-I",]$RT, na.rm =T)), color="black", linetype="longdash", size=.8) +
    geom_vline(aes(xintercept=median(agg_data_all1[agg_data_all1$insight_mediansplit == "LO-I",]$RT, na.rm =T)), color="black", linetype="solid", size=.8)
  
  s12_RT_cor<- plot_multi_histogram_neu(behave_data[!is.na(behave_data$RT_correct),], 'RT_correct', 'insight_mediansplit ')+labs(x = "Solution time in sec (correct)",y = "Density", fill = "Insight")+
    geom_vline(aes(xintercept=median(behave_data[behave_data$insight_mediansplit == "HI-I",]$RT_correct, na.rm =T)), color="black", linetype="longdash", size=.8) +
    geom_vline(aes(xintercept=median(behave_data[behave_data$insight_mediansplit == "LO-I",]$RT_correct, na.rm =T)), color="black", linetype="solid", size=.8)
 
  s12_distribution <- ggarrange( s12_RT,s12_RT_cor, 
                                 ncol = 2, nrow = 1, legend = "top",common.legend = TRUE)
  
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
    M0_IME <- glmer(SME_solve ~    sessionblock+                   (1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
    M1_IME <- glmer(SME_solve ~ insight_mediansplit+ sessionblock+(1|ID) + (1|Item),data= data_new,family = binomial(link ="logit"), na.action  = na.omit)
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
     
    # behavioral insight memory effect controlled for RT (note, has no "not solved" condition anymore)
    data_new = PPSS[ PPSS$ROI == 'l_Amygdala' & PPSS$tp == 1 & PPSS$insight_mediansplit != "not solved" ,]
    M0_IME_RT <- glmer(SME_solve ~ RT+  sessionblock+                      (1|ID) + (1|Item),data= data_new[data_new$cor2==1,],family = binomial(link ="logit"), na.action  = na.omit)
    M1_IME_RT <- glmer(SME_solve ~ RT+ insight_mediansplit +sessionblock + (1|ID) + (1|Item),data= data_new[data_new$cor2==1,],family = binomial(link ="logit"), na.action  = na.omit)

    anova(M0_IME_RT, M1_IME_RT)
    tab_model(M1_IME_RT, show.std  = T)
    ggpredict(M1_IME_RT , c(  'insight_mediansplit')) #%>% plot() + ggplot2::theme_classic()
    

####### 2) Univariate ROI analysis #################

### 2a) HI vs LO Insight #####################
    
  ###### HI vs LO Insight 
    # load Amygdala in standard space for HI vs LO Insight  
    r_Amy    <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_r_Amy_22-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_Amy    <- read.table("s2_ALL_HIvsLO_insight_AHAmsplit_StdSp_BP_l_Amy_22-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
   
    # load VOTC in subject space for HI vs LO Insight  
    r_Amy    <- read.table("s2_ALL_HIvsLO_Insight_AHAmsplit_SubjSp_BP_r_Amy_23-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_Amy    <- read.table("s2_ALL_HIvsLO_Insight_AHAmsplit_SubjSp_BP_l_Amy_23-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
  
   
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
   
  
   
   data_HILOI <- rbind(  iLOC[-1],aFusG[,-1], pFusG[,-1], aITG[,-1],  mITG[,-1], pITG[,-1] , Amy[,-1])
   data_HILOI$roi=  rep(c(  'iLOC', 'aFusG', 'pFusG',  'aITG', 'mITG','pITG' , 'Amy' #,
   ), each=31)# 
   library(tidyr)
   data_HILOI_long <- data_HILOI %>% pivot_longer(cols = 'vis_HIInsight_1':'vis_LOInsight_4', names_to='condition', values_to="betas")
   data_HILOI_long$session = rep(c(1,2,3,4), 2*31*7) #'3*conds*31subj*7ROIs'
   data_HILOI_long$condition <- gsub('vis_', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition <- gsub('_1', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition <- gsub('_2', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition <- gsub('_3', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition <- gsub('_4', '', as.character(data_HILOI_long$condition)) 
   data_HILOI_long$condition = ordered(data_HILOI_long$condition, levels = c('LOInsight','HIInsight'), 
                                       labels=c('LO-I','HI-I' ))
  
  ### test for amygdala activity during solution as a function of Insight
  HIvLOI_m0_Amy <- lmer(betas ~ session  +(1|ID) ,data= data_HILOI_long[data_HILOI_long$roi == "Amy",], na.action  = na.omit) #
  HIvLOI_m1_Amy <- lmer(betas ~ condition+session +(1|ID) ,data= data_HILOI_long[data_HILOI_long$roi == "Amy",], na.action  = na.omit) #
  
  anova(HIvLOI_m0_Amy, HIvLOI_m1_Amy)
  tab_model(HIvLOI_m1_Amy, show.std = T)
  emmeans(HIvLOI_m1_Amy, list(pairwise ~ condition), adjust = "tukey") 
  
  # plot Insight activity results at solution for amygdala
  HIvLOI_Amy_ggplot = ggpredict(HIvLOI_m1_Amy , c('condition')) 
  HIvLOI_Amy_finalplot= ggplot(HIvLOI_Amy_ggplot, aes(x= x, y = predicted ,fill=x)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T) + #, color = "black"
    geom_errorbar(aes(ymin=predicted-std.error , ymax=predicted+std.error ), width=.2,size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs(
                  y = "Beta estimate during solution", fill = "Insight")+
                   scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))
  
   ### test for VOTC activity during solution as a function of Insight
   HIvLOI_m0_VS <- lmer(betas ~ session+roi  +(1|ID) ,data= data_HILOI_long[data_HILOI_long$roi != "Amy",], na.action  = na.omit) #
   HIvLOI_m1_VS <- lmer(betas ~ condition+session+roi  +(1|ID) ,data= data_HILOI_long[data_HILOI_long$roi != "Amy",], na.action  = na.omit) #
   HIvLOI_m2_VS <- lmer(betas ~ condition*roi +session +(1|ID) ,data= data_HILOI_long[data_HILOI_long$roi != "Amy",], na.action  = na.omit) #
   
   anova(HIvLOI_m0_VS, HIvLOI_m1_VS,HIvLOI_m2_VS)
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
                   position=position_dodge(.9)) + theme_classic() +labs(title = "VOTC regions",fill = "Insight",
              x = "ROI", y = "Beta estimate during solution", fill = "Solution style")+ #scale_fill_grey() 
     scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))
   
   
 
#######################################################################################-  
   ###2b) IME #####################

   ####### Rem-HI-I > Rem-LO-I > Forgotten 
   
    # Amy activity in standard space
    r_Amy    <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_r_Amy_23-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_Amy    <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_l_Amy_23-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
   
    # Amy activity in Subject space 
    r_Amy    <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_subjSp_BP_r_Amy_24-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
    l_Amy    <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_subjSp_BP_l_Amy_24-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))
 
    #VOTC areas in standard space
    l_aFusG  <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_l_antFusG_marsbar_97_115_97_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))   
    r_aFusG  <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_r_antFusG_marsbar_97_115_97_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))  
    r_pFusG  <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_r_postFusG_marsbar_97_115_97_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))   
    l_pFusG  <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_l_postFusG_marsbar_97_115_97_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" )) 
    l_iLOC   <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_l_iLOC_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))   
    r_iLOC   <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_r_iLOC_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))     
    r_pITG   <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_r_toITG_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))  
    l_pITG   <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_l_toITG_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))    
    l_aITG   <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_l_aITG_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))   
    r_aITG   <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_r_aITG_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))     
    r_mITG   <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_r_pITG_26-May-2023.dat" , sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))    
    l_mITG   <- read.table("s2_ALL_SOLVE_noRec_RecnI_RecI_AHAmsplit_StdSp_BP_l_pITG_26-May-2023.dat", sep = ",", dec = ".", header=TRUE, na.strings=c("", " " , "NA", "NAN" ))    

    Amy= data.frame(matrix(nrow = 31, ncol = 1))
    Amy$ID = l_Amy$ID
    Amy$vis_RemI_1 <- (l_Amy$vis_RemI_1   + r_Amy$vis_RemI_1)/2
    Amy$vis_RemI_2 <- (l_Amy$vis_RemI_2   + r_Amy$vis_RemI_2)/2
    Amy$vis_RemI_3 <- (l_Amy$vis_RemI_3   + r_Amy$vis_RemI_3)/2
    Amy$vis_RemI_4 <- (l_Amy$vis_RemI_4   + r_Amy$vis_RemI_4)/2
    
    Amy$vis_RemnoI_1 <- (l_Amy$vis_RemnoI_1   + r_Amy$vis_RemnoI_1)/2
    Amy$vis_RemnoI_2 <- (l_Amy$vis_RemnoI_2   + r_Amy$vis_RemnoI_2)/2
    Amy$vis_RemnoI_3 <- (l_Amy$vis_RemnoI_3   + r_Amy$vis_RemnoI_3)/2
    Amy$vis_RemnoI_4 <- (l_Amy$vis_RemnoI_4   + r_Amy$vis_RemnoI_4)/2
    
    Amy$vis_noRem_1 <- (l_Amy$vis_noRem_1   + r_Amy$vis_RemnoI_1)/2
    Amy$vis_noRem_2 <- (l_Amy$vis_noRem_2   + r_Amy$vis_RemnoI_2)/2
    Amy$vis_noRem_3 <- (l_Amy$vis_noRem_3   + r_Amy$vis_RemnoI_3)/2
    Amy$vis_noRem_4 <- (l_Amy$vis_noRem_4   + r_Amy$vis_RemnoI_4)/2
    
    iLOC= data.frame(matrix(nrow = 31, ncol = 1))
    iLOC$ID = l_iLOC$ID
    iLOC$vis_RemI_1 <- (l_iLOC$vis_RemI_1   + r_iLOC$vis_RemI_1)/2
    iLOC$vis_RemI_2 <- (l_iLOC$vis_RemI_2   + r_iLOC$vis_RemI_2)/2
    iLOC$vis_RemI_3 <- (l_iLOC$vis_RemI_3   + r_iLOC$vis_RemI_3)/2
    iLOC$vis_RemI_4 <- (l_iLOC$vis_RemI_4   + r_iLOC$vis_RemI_4)/2
    
    iLOC$vis_RemnoI_1 <- (l_iLOC$vis_RemnoI_1   + r_iLOC$vis_RemnoI_1)/2
    iLOC$vis_RemnoI_2 <- (l_iLOC$vis_RemnoI_2   + r_iLOC$vis_RemnoI_2)/2
    iLOC$vis_RemnoI_3 <- (l_iLOC$vis_RemnoI_3   + r_iLOC$vis_RemnoI_3)/2
    iLOC$vis_RemnoI_4 <- (l_iLOC$vis_RemnoI_4   + r_iLOC$vis_RemnoI_4)/2
    
    iLOC$vis_noRem_1 <- (l_iLOC$vis_noRem_1   + r_iLOC$vis_noRem_1)/2
    iLOC$vis_noRem_2 <- (l_iLOC$vis_noRem_2   + r_iLOC$vis_noRem_2)/2
    iLOC$vis_noRem_3 <- (l_iLOC$vis_noRem_3   + r_iLOC$vis_noRem_3)/2
    iLOC$vis_noRem_4 <- (l_iLOC$vis_noRem_4   + r_iLOC$vis_noRem_4)/2

    pFusG= data.frame(matrix(nrow = 31, ncol = 1))
    pFusG$ID = l_pFusG$ID
    pFusG$vis_RemI_1 <- (r_pFusG$vis_RemI_1   + l_pFusG$vis_RemI_1)/2
    pFusG$vis_RemI_2 <- (r_pFusG$vis_RemI_2   + l_pFusG$vis_RemI_2)/2
    pFusG$vis_RemI_3 <- (r_pFusG$vis_RemI_3   + l_pFusG$vis_RemI_3)/2
    pFusG$vis_RemI_4 <- (r_pFusG$vis_RemI_4   + l_pFusG$vis_RemI_4)/2
    
    pFusG$vis_RemnoI_1 <- (r_pFusG$vis_RemnoI_1   + l_pFusG$vis_RemnoI_1)/2
    pFusG$vis_RemnoI_2 <- (r_pFusG$vis_RemnoI_2   + l_pFusG$vis_RemnoI_2)/2
    pFusG$vis_RemnoI_3 <- (r_pFusG$vis_RemnoI_3   + l_pFusG$vis_RemnoI_3)/2
    pFusG$vis_RemnoI_4 <- (r_pFusG$vis_RemnoI_4   + l_pFusG$vis_RemnoI_4)/2
    
    pFusG$vis_noRem_1 <- (r_pFusG$vis_noRem_1   + l_pFusG$vis_noRem_1)/2
    pFusG$vis_noRem_2 <- (r_pFusG$vis_noRem_2   + l_pFusG$vis_noRem_2)/2
    pFusG$vis_noRem_3 <- (r_pFusG$vis_noRem_3   + l_pFusG$vis_noRem_3)/2
    pFusG$vis_noRem_4 <- (r_pFusG$vis_noRem_4   + l_pFusG$vis_noRem_4)/2
    
    
    aFusG= data.frame(matrix(nrow = 31, ncol = 1))
    aFusG$ID = l_aFusG$ID
    aFusG$vis_RemI_1 <- (r_aFusG$vis_RemI_1   + l_aFusG$vis_RemI_1)/2
    aFusG$vis_RemI_2 <- (r_aFusG$vis_RemI_2   + l_aFusG$vis_RemI_2)/2
    aFusG$vis_RemI_3 <- (r_aFusG$vis_RemI_3   + l_aFusG$vis_RemI_3)/2
    aFusG$vis_RemI_4 <- (r_aFusG$vis_RemI_4   + l_aFusG$vis_RemI_4)/2
    
    aFusG$vis_RemnoI_1 <- (r_aFusG$vis_RemnoI_1   + l_aFusG$vis_RemnoI_1)/2
    aFusG$vis_RemnoI_2 <- (r_aFusG$vis_RemnoI_2   + l_aFusG$vis_RemnoI_2)/2
    aFusG$vis_RemnoI_3 <- (r_aFusG$vis_RemnoI_3   + l_aFusG$vis_RemnoI_3)/2
    aFusG$vis_RemnoI_4 <- (r_aFusG$vis_RemnoI_4   + l_aFusG$vis_RemnoI_4)/2
   
    aFusG$vis_noRem_1 <- (r_aFusG$vis_noRem_1   + l_aFusG$vis_noRem_1)/2
    aFusG$vis_noRem_2 <- (r_aFusG$vis_noRem_2   + l_aFusG$vis_noRem_2)/2
    aFusG$vis_noRem_3 <- (r_aFusG$vis_noRem_3   + l_aFusG$vis_noRem_3)/2
    aFusG$vis_noRem_4 <- (r_aFusG$vis_noRem_4   + l_aFusG$vis_noRem_4)/2
    
    pITG= data.frame(matrix(nrow = 31, ncol = 1))
    pITG$ID = l_pITG$ID
    pITG$vis_RemI_1 <- (r_pITG$vis_RemI_1   + l_pITG$vis_RemI_1)/2
    pITG$vis_RemI_2 <- (r_pITG$vis_RemI_2   + l_pITG$vis_RemI_2)/2
    pITG$vis_RemI_3 <- (r_pITG$vis_RemI_3   + l_pITG$vis_RemI_3)/2
    pITG$vis_RemI_4 <- (r_pITG$vis_RemI_4   + l_pITG$vis_RemI_4)/2
    
    pITG$vis_RemnoI_1 <- (r_pITG$vis_RemnoI_1   + l_pITG$vis_RemnoI_1)/2
    pITG$vis_RemnoI_2 <- (r_pITG$vis_RemnoI_2   + l_pITG$vis_RemnoI_2)/2
    pITG$vis_RemnoI_3 <- (r_pITG$vis_RemnoI_3   + l_pITG$vis_RemnoI_3)/2
    pITG$vis_RemnoI_4 <- (r_pITG$vis_RemnoI_4   + l_pITG$vis_RemnoI_4)/2
    
    pITG$vis_noRem_1 <- (r_pITG$vis_noRem_1   + l_pITG$vis_noRem_1)/2
    pITG$vis_noRem_2 <- (r_pITG$vis_noRem_2   + l_pITG$vis_noRem_2)/2
    pITG$vis_noRem_3 <- (r_pITG$vis_noRem_3   + l_pITG$vis_noRem_3)/2
    pITG$vis_noRem_4 <- (r_pITG$vis_noRem_4   + l_pITG$vis_noRem_4)/2
    
    aITG= data.frame(matrix(nrow = 31, ncol = 1))
    aITG$ID = l_aITG$ID
    aITG$vis_RemI_1 <- (r_aITG$vis_RemI_1   + l_aITG$vis_RemI_1)/2
    aITG$vis_RemI_2 <- (r_aITG$vis_RemI_2   + l_aITG$vis_RemI_2)/2
    aITG$vis_RemI_3 <- (r_aITG$vis_RemI_3   + l_aITG$vis_RemI_3)/2
    aITG$vis_RemI_4 <- (r_aITG$vis_RemI_4   + l_aITG$vis_RemI_4)/2
    
    aITG$vis_RemnoI_1 <- (r_aITG$vis_RemnoI_1   + l_aITG$vis_RemnoI_1)/2
    aITG$vis_RemnoI_2 <- (r_aITG$vis_RemnoI_2   + l_aITG$vis_RemnoI_2)/2
    aITG$vis_RemnoI_3 <- (r_aITG$vis_RemnoI_3   + l_aITG$vis_RemnoI_3)/2
    aITG$vis_RemnoI_4 <- (r_aITG$vis_RemnoI_4   + l_aITG$vis_RemnoI_4)/2
    
    aITG$vis_noRem_1 <- (r_aITG$vis_noRem_1   + l_aITG$vis_noRem_1)/2
    aITG$vis_noRem_2 <- (r_aITG$vis_noRem_2   + l_aITG$vis_noRem_2)/2
    aITG$vis_noRem_3 <- (r_aITG$vis_noRem_3   + l_aITG$vis_noRem_3)/2
    aITG$vis_noRem_4 <- (r_aITG$vis_noRem_4   + l_aITG$vis_noRem_4)/2
    
    mITG= data.frame(matrix(nrow = 31, ncol = 1))
    mITG$ID = l_pITG$ID
    mITG$vis_RemI_1 <- (r_mITG$vis_RemI_1   + l_mITG$vis_RemI_1)/2
    mITG$vis_RemI_2 <- (r_mITG$vis_RemI_2   + l_mITG$vis_RemI_2)/2
    mITG$vis_RemI_3 <- (r_mITG$vis_RemI_3   + l_mITG$vis_RemI_3)/2
    mITG$vis_RemI_4 <- (r_mITG$vis_RemI_4   + l_mITG$vis_RemI_4)/2
    
    mITG$vis_RemnoI_1 <- (r_mITG$vis_RemnoI_1   + l_mITG$vis_RemnoI_1)/2
    mITG$vis_RemnoI_2 <- (r_mITG$vis_RemnoI_2   + l_mITG$vis_RemnoI_2)/2
    mITG$vis_RemnoI_3 <- (r_mITG$vis_RemnoI_3   + l_mITG$vis_RemnoI_3)/2
    mITG$vis_RemnoI_4 <- (r_mITG$vis_RemnoI_4   + l_mITG$vis_RemnoI_4)/2
    
    mITG$vis_noRem_1 <- (r_mITG$vis_noRem_1   + l_mITG$vis_noRem_1)/2
    mITG$vis_noRem_2 <- (r_mITG$vis_noRem_2   + l_mITG$vis_noRem_2)/2
    mITG$vis_noRem_3 <- (r_mITG$vis_noRem_3   + l_mITG$vis_noRem_3)/2
    mITG$vis_noRem_4 <- (r_mITG$vis_noRem_4   + l_mITG$vis_noRem_4)/2
    
    data_SOLVE <- rbind( iLOC[-1],aFusG[,-1], pFusG[,-1], aITG[,-1], mITG[,-1],  pITG[,-1], Amy[,-1]) 
    
    data_SOLVE$roi=  rep(c( 'iLOC', 'aFusG', 'pFusG',  'aITG', 'mITG','pITG','Amy' ), each=31)# 
    library(tidyr)
    data_SOLVE_long <- data_SOLVE %>% pivot_longer(cols = 'vis_RemI_1':'vis_noRem_4', names_to='condition', values_to="betas")
    data_SOLVE_long$session = rep(c(1,2,3,4), 3*31*7) #'3*conds*31subj*7ROIs'
    #data_SOLVE_long$condition <- gsub('vis_', '', as.character(data_SOLVE_long$condition)) 
    data_SOLVE_long$condition <- gsub('_1', '', as.character(data_SOLVE_long$condition)) 
    data_SOLVE_long$condition <- gsub('_2', '', as.character(data_SOLVE_long$condition)) 
    data_SOLVE_long$condition <- gsub('_3', '', as.character(data_SOLVE_long$condition)) 
    data_SOLVE_long$condition <- gsub('_4', '', as.character(data_SOLVE_long$condition)) 
    data_SOLVE_long$condition = ordered(data_SOLVE_long$condition, levels = c( 'vis_noRem',  'vis_RemnoI','vis_RemI'), 
                             labels=c('Forgotten','Rem_LO-I', 'Rem_HI-I'   ))


### test for amygdala activity during solution as a function of IME
  SOLVE_m0_Amy <- lmer(betas ~ session  +(1|ID) ,data= data_SOLVE_long[data_SOLVE_long$roi == "Amy",], na.action  = na.omit) #
  SOLVE_m1_Amy <- lmer(betas ~ condition+session  +(1|ID) ,data= data_SOLVE_long[data_SOLVE_long$roi == "Amy",], na.action  = na.omit) #
  
  anova(SOLVE_m0_Amy, SOLVE_m1_Amy)
  tab_model(SOLVE_m1_Amy, show.std = T)
  emmeans(SOLVE_m1_Amy, list(pairwise ~ condition), adjust = "tukey") 

  # plot Insight activity results at solution for amygdala
  IME_Amy_ggplot = ggpredict(SOLVE_m1_Amy , c('condition')) #%>%plot()+ ggplot2::theme_classic() +
  IME_Amy_finalplot= ggplot(IME_Amy_ggplot, aes(x= x, y = predicted ,fill= x)) +  #, pattern= x
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T)+#, color = "black") + #scale_pattern_manual(name = "x", values = c("Forgotten" = "none", "Rem_LO-I" = "stripe","Rem_HI-I" = "stripe")) + geom_bar_pattern(stat = "identity", pattern_color = "white", pattern_fill = "white", aes(pattern_angle = x ) , pattern_density = 0.05, pattern_spacing = 0.2)+ #
    geom_errorbar(aes(ymin=predicted-std.error , ymax=predicted+std.error ), width=.2,size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs( #title = "Amygdala activity"
                  x = "",y = "Beta estimate during solution", fill = "Insight Memory")+
                  scale_fill_manual(breaks = c('Forgotten','Rem_LO-I', 'Rem_HI-I'),values=c("#fde56e" ,"#0090fe", "#ff7d45"))+  
                 scale_x_discrete(labels=c('Forg', 'R LO-I', 'R HI-I'))#+

  
    Uni_AN8_W2V_subjspace <- ggarrange(HIvLOI_Amy_finalplot,IME_Amy_finalplot,
                              common.legend = F, legend = "bottom", ncol = 2, nrow = 1)
                              #labels = c('Amygdala activity'),
                        

## mixed models for VOTC areas
  SOLVE_m0 <- lmer(betas ~ roi+session  +(1|ID) ,data= data_SOLVE_long[data_SOLVE_long$roi != "Amy",], na.action  = na.omit) #
  SOLVE_m1 <- lmer(betas ~ condition+roi+session  +(1|ID) ,data= data_SOLVE_long[data_SOLVE_long$roi != "Amy",], na.action  = na.omit) #
  SOLVE_m2 <- lmer(betas ~ condition*roi+session  +(1|ID) ,data= data_SOLVE_long[data_SOLVE_long$roi != "Amy",], na.action  = na.omit) #
  anova(SOLVE_m0, SOLVE_m1,SOLVE_m2)
  hist(resid(SOLVE_m2))
  
  emmeans(SOLVE_m2, list(pairwise ~ condition | roi), adjust = "tukey") # tukey
  p = c(0.5948,0.6951, 0.9862,0.0918, 0.8923, 0.2303,0.0004, 0.0007, 0.9901 ,0.1706 ,0.7030,0.5785,0.3568, 0.6815,0.8539,0.0302 ,0.0885,0.9030 )
  p.adjust(p, method = 'fdr', n=length(p))
  
  # plot Insight activity results at solution for VOTC ROIs
  IME_VOTC_ggplot = ggpredict(SOLVE_m2 , c('roi','condition'))
  IME_VOTC_finalplot= ggplot(IME_VOTC_ggplot, aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T) + #, color = "black"
    geom_errorbar(aes(ymin=conf.low , ymax=conf.high ), width=.2,size = 1.1,
                  position=position_dodge(.9)) + theme_classic() +labs(title = "",fill = "Insight Memory",
                  x = "ROI",y = "Beta estimate during solution")+
             scale_fill_manual(breaks = c('Forgotten','Rem_LO-I', 'Rem_HI-I'),values=c("#fde56e" ,"#0090fe", "#ff7d45"))
  
    RSA_AN8_W2V_VOTC <- ggarrange( HIvLOI_VOTC_finalplot, IME_VOTC_finalplot,
                              common.legend = F, legend = "bottom",ncol = 2, nrow = 1,
                              labels = c('A', 'B'))


####2c) Univariate Amygdala (ROI) analysis with  single trials betas #####  

    newdata = PPSS[ PPSS$ROI_bilat == "Amy"  ,] 
    
    # Amy Insight
    Activity_data = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) )  & !is.na(newdata$insight_mediansplit) & newdata$tp == 1,]
    Activity_M0 <- lmer(Beta_Buttonpress ~ sessionblock + RT+ROI+(1|ID) + (1|Item),data= Activity_data[Activity_data$cor2 == 1,], na.action  = na.omit)
    Activity_M1 <- lmer(Beta_Buttonpress ~ sessionblock + RT+insight_mediansplit+ROI +(1|ID) + (1|Item),data= Activity_data[Activity_data$cor2 == 1,], na.action  = na.omit)
    Activity_M2 <- lmer(Beta_Buttonpress ~ sessionblock + RT+insight_mediansplit*ROI  +(1|ID) + (1|Item),data= Activity_data[Activity_data$cor2 == 1,], na.action  = na.omit)
    
    anova( Activity_M0,Activity_M1,Activity_M2)#,
    ggpredict(Activity_M1 , c("insight_mediansplit")) %>% plot(show.title = F) + ggplot2::theme_classic()
    tab_model(Activity_M1, show.std =T)
    emmeans(Activity_M1, list(pairwise ~  insight_mediansplit  ), adjust = 'tukey')
    
    # Amy Insight Memory
    Activity_data = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) )  & !is.na(newdata$insight_noRec_SOLVE_ord) & newdata$tp == 1,]
    Activity_M6 <- lmer(Beta_Buttonpress ~ sessionblock + RT+ROI+(1|ID) + (1|Item),data= Activity_data[Activity_data$cor2 == 1,], na.action  = na.omit)
    Activity_M7 <- lmer(Beta_Buttonpress ~ sessionblock + RT+insight_noRec_SOLVE_ord +ROI +(1|ID) + (1|Item),data= Activity_data[Activity_data$cor2 == 1,], na.action  = na.omit)
    Activity_M8 <- lmer(Beta_Buttonpress ~ sessionblock + RT+insight_noRec_SOLVE_ord *ROI +(1|ID) + (1|Item),data= Activity_data[Activity_data$cor2 == 1,], na.action  = na.omit)
    
    hist(residuals(Activity_M7)) # residuals are normally distributed -> model is adecuate
    anova( Activity_M6,Activity_M7,Activity_M8)
    tab_model(Activity_M7, show.std =T)
    emmeans(Activity_M7, list(pairwise ~  insight_noRec_SOLVE_ord ), adjust = 'tukey')
    
    
####### 3) multivariate ROI analysis - Brain areas representing sudden conceptual update during insight #####

newdata = PPSS[PPSS$ROI_bilat != "Amy",]

######## 3a) RSA1-Insight - Multivoxel Pattern Similarity ######################################################
  ERSdata = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) )  &   !is.na(newdata$insight_mediansplit) & newdata$tp == 1,]
  ERSdata$ERS_PrePost_R_delta = 1-ERSdata$ERS_PrePost_R 
  ERS_M0 <- lmer(ERS_PrePost_R_delta ~ RT+                    ROI_bilat +sessionblock+(1|ID) + (1|Item),data= ERSdata, na.action  = na.omit)
  ERS_M1 <- lmer(ERS_PrePost_R_delta ~ RT+insight_mediansplit+ROI_bilat +sessionblock+(1|ID) + (1|Item),data= ERSdata, na.action  = na.omit)
  ERS_M2 <- lmer(ERS_PrePost_R_delta ~ RT+insight_mediansplit*ROI_bilat +sessionblock+(1|ID) + (1|Item) , data= ERSdata, na.action  = na.omit)
  
  hist(residuals(ERS_M2)) # residuals are normally distributed -> model is adecuate
  anova( ERS_M0,ERS_M1, ERS_M2)#
  tab_model(ERS_M2, show.std  = T)
  emmeans(ERS_M2, list(pairwise ~ insight_mediansplit | ROI_bilat ), adjust = "tukey")

  # plot RSA1-Insight results for pFusG & iLOC
  ERS_ggpredict = ggpredict(ERS_M2 , c('ROI_bilat','insight_mediansplit'))
  ERS_finalPlot= ggplot(ERS_ggpredict, aes(x= x, y = predicted , fill= group)) + scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))+
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T ) +  coord_cartesian(ylim=c(0.67,.93)) + #+color = "black"
    geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs(title = "", fill = "Insight",
                  x = "ROI",y = expression("Δ MVPS: 1-r"[post - pre])  ) 

#p.adjust(p_verb1, method = 'FDR', n = length(p_verb1))
#p.adjust(p_verb2, method = 'FDR', n = length(p_verb2))


#### 3a1) RSA1-IME Multivoxel Pattern Similarity ############  

newdata = PPSS[ 
    PPSS$ROI_bilat == "pFusG"  |
    PPSS$ROI_bilat == "iLOC"  #|
  ,] 

  ERSdata = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) )  &   !is.na(newdata$insight_noRec_SOLVE_ord) & newdata$tp == 1,]
  ERSdata$ERS_PrePost_R_delta = 1-ERSdata$ERS_PrePost_R 
  
  ERS_M0_IME <- lmer(ERS_PrePost_R_delta ~ RT+                        ROI_bilat +sessionblock+(1|ID) + (1|Item),data= ERSdata[ ERSdata$cor2 == 1,], na.action  = na.omit)
  ERS_M1_IME <- lmer(ERS_PrePost_R_delta ~ RT+insight_noRec_SOLVE_ord+ROI_bilat +sessionblock+(1|ID) + (1|Item),data= ERSdata[ ERSdata$cor2 == 1,], na.action  = na.omit)
  ERS_M2_IME <- lmer(ERS_PrePost_R_delta ~ RT+insight_noRec_SOLVE_ord*ROI_bilat +sessionblock+(1|ID)+ (1|Item) , data= ERSdata[ERSdata$cor2 == 1,], na.action  = na.omit)
  hist(residuals(ERS_M2_IME)) # residuals are normally distributed -> model is adecuate
  anova(ERS_M0_IME, ERS_M1_IME, ERS_M2_IME)#, RSA4)#,  M4)#
  ggpredict(ERS_M2_IME , c(  'ROI_bilat','insight_noRec_SOLVE_ord')) %>% plot(show.title = F) + ggplot2::theme_classic()
  emmeans(ERS_M2_IME, list(pairwise ~   insight_noRec_SOLVE_ord | ROI_bilat   ), adjust = "tukey")
  tab_model(ERS_M2_IME, show.std = T)
  
  # plot RSA1-IME results for pFusG & iLOC
  IME_ERS <- ggpredict(ERS_M2_IME , c( 'ROI_bilat', 'insight_noRec_SOLVE_ord' )) #%>% plot(show.title = F) + ggplot2::theme_classic()+labs(x="Insight Memory Factor", y="Representational strength (AlexNet)", color = "ROI")
  IME_ERS_finalPlot= ggplot(IME_ERS, aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T) +  coord_cartesian(ylim=c(0.67,.83)) + #+color = "black"
    geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size=.95,
                  position=position_dodge(.9)) + theme_classic() +labs(
                  x = "ROI",y =  expression("Δ MVPS: 1-r"[post - pre]), fill = "Insight Memory")+
    scale_fill_manual(breaks = c('Forgotten','Rem_LO-I', 'Rem_HI-I'),values=c("#fde56e" ,"#0090fe", "#ff7d45"))



########  3b) RSA2-Insight  ####################################################
newdata = PPSS[ 
    PPSS$ROI_bilat == "pFusG"  |
    PPSS$ROI_bilat == "iLOC"  |
    PPSS$ROI_bilat == "pITG" 
  ,] 
#### 3b1) RSA2-Insight using AlexNet8 ######
# insight_cutoff 
RSAdata = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) ) &  !is.na(newdata$insight_mediansplit) 
                  & newdata$RSA_AlexNet8_realpix_control_corr_n_vis > 10 ,]
M0AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ RT+tp_fac+                    ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
M1AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ RT+tp_fac+insight_mediansplit+ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
M2AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ RT+tp_fac*insight_mediansplit+ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)
M3AN <- lmer(RSA_AlexNet8_realpix_R_vis ~ RT+tp_fac*insight_mediansplit*ROI_bilat+sessionblock +(1|ID)+ (1|Item), na.action  = na.omit,data= RSAdata)

  hist(residuals(M3AN)) # residuals are normally distributed -> model is adecuate
  anova(M0AN, M1AN, M2AN, M3AN)
  emmeans(M3AN, list(pairwise ~ insight_mediansplit | tp_fac | ROI_bilat), adjust = "tukey")
  emmeans(M2AN, list(pairwise ~ insight_mediansplit | tp_fac), adjust = "tukey")
  tab_model(M2AN, show.std = T)
  
  #plot RSA2-Insight results for pFusG
  RSAN8_ggpredict = ggpredict(M3AN , c('tp_fac','insight_mediansplit', 'ROI_bilat'))
  RSAAN8_finalPlot_pFusC= ggplot(RSAN8_ggpredict[RSAN8_ggpredict$facet == "pFusG",], aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2, size =.95,
                  position=position_dodge(.9)) + theme_classic() +labs(title = "pFusG", fill = "condition",
                    x = "Time",y = "Rep-Str (AlexNet)")+scale_x_discrete(labels=c('Pre', 'Post'))+
                    scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))
  
  #plot RSA2-Insight results for iLOC
  RSAAN8_finalPlot_iLOC= ggplot(RSAN8_ggpredict[RSAN8_ggpredict$facet == "iLOC",], aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2, size =.95,
                  position=position_dodge(.9)) + theme_classic() +labs(title = "iLOC", fill = "condition",
                  x = "Time",y = "Rep-Str (AlexNet)")+scale_x_discrete(labels=c('Pre', 'Post'))+
                  scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))
  
  #plot RSA2-Insight results for pITG
  RSAAN8_finalPlot_pITG= ggplot(RSAN8_ggpredict[RSAN8_ggpredict$facet == "pITG",], aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2, size =.95,
                  position=position_dodge(.9)) + theme_classic() +labs(title = "pITG", fill = "condition",
                  x = "Time",y = "Rep-Str (AlexNet)")+scale_x_discrete(labels=c('Pre', 'Post'))+
                  scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))

#### 3b1a) RSA2-IME using AlexNet8: Insight memory effect for representations #####

  newdata = PPSS[ 
      PPSS$ROI_bilat == "pFusG"  |
      PPSS$ROI_bilat == "iLOC"  #|
  ,]     

  #insight_CUTOFF_noRec_SOLVE, insight_noRec_SOLVE_ord, insight_sum 
  RSAdata = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) ) &  !is.na(newdata$insight_CUTOFF_noRec_SOLVE_ord) 
                    & newdata$RSA_AlexNet8_realpix_control_corr_n_vis > 10 & newdata$tp == 1 ,]
  M0AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ RT                        +ROI_bilat+sessionblock+(1|ID)+(1|Item),data= RSAdata[RSAdata$cor2== 1,], na.action  = na.omit)
  M1AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ RT+insight_noRec_SOLVE_ord+ROI_bilat+sessionblock+(1|ID)+(1|Item),data= RSAdata[RSAdata$cor2== 1,], na.action  = na.omit)
  M2AN_mem <- lmer(RSA_AlexNet8_realpix_R_vis ~ RT+insight_noRec_SOLVE_ord*ROI_bilat+sessionblock+(1|ID)+(1|Item),data= RSAdata[RSAdata$cor2== 1,], na.action  = na.omit)

    hist(residuals(M1AN_mem)) # residuals are normally distributed -> model is adecuate
  anova(M0AN_mem, M1AN_mem, M2AN_mem)
  emmeans(M1AN_mem, list(pairwise ~   insight_noRec_SOLVE_ord  ), adjust = "tukey")
  tab_model(M1AN_mem, show.std =T)
  
  # plot IME results for AlexNet
  IME_AN8 <- ggpredict(M2AN_mem , c( 'ROI_bilat', 'insight_noRec_SOLVE_ord' )) 
  IME_AN8_finalPlot= ggplot(IME_AN8, aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = T) + #, color = "black"
    geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2, size=.95,
                  position=position_dodge(.9)) + theme_classic() +labs(
                  x = "ROI",y = "Rep-Strength (AlexNet)", fill = "Insight Memory")+
    scale_fill_manual(breaks = c('Forgotten','Rem_LO-I', 'Rem_HI-I'),values=c("#fde56e" ,"#0090fe", "#ff7d45"))

  
  
#### 3b2) RSA2-Insight using W2V (word2vec) ################

  newdata = PPSS[ 
      PPSS$ROI_bilat == "pFusG"  |
      PPSS$ROI_bilat == "iLOC"  |
      PPSS$ROI_bilat == "pITG" 
    ,]
  
  RSAdata = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) ) & !is.na(newdata$insight_mediansplit) 
                     & newdata$RSA_W2V_control_corr_n_vis > 10 ,]
  M0W2V <- lmer(RSA_W2V_R_vis ~ RT+tp_fac                    +ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M1W2V <- lmer(RSA_W2V_R_vis ~ RT+tp_fac+insight_mediansplit+ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M2W2V <- lmer(RSA_W2V_R_vis ~ RT+tp_fac*insight_mediansplit+ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  M3W2V <- lmer(RSA_W2V_R_vis ~ RT+tp_fac*insight_mediansplit*ROI_bilat+sessionblock  +(1|ID)+ (1|Item),data= RSAdata, na.action  = na.omit)
  
  hist(residuals(M2W2V)) # residuals are normally distributed -> model is adecuate
  anova(M0W2V, M1W2V, M2W2V,M3W2V)
  emmeans(M3W2V, list(pairwise ~ insight_mediansplit | tp_fac | ROI_bilat), adjust = "tukey")
  
  ggpredict(M2W2V , c( 'tp_fac', 'insight_mediansplit' )) #%>% plot(show.title = F) + ggplot2::theme_classic()
  RSAW2V_ggpredict <- ggpredict(M3W2V , c( 'tp_fac', 'insight_mediansplit', 'ROI_bilat' )) #%>% plot(show.title = F) + ggplot2::theme_classic()
  
  # plot W2V-Insight results for pFusG
  RSAW2V_finalPlot_pFusC= ggplot(RSAW2V_ggpredict[RSAW2V_ggpredict$facet == "pFusG",], aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) +#, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2,size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs( fill = "condition", #title = "",
                  x = "Time",y = "Rep-Str (W2V)")+scale_x_discrete(labels=c('Pre', 'Post'))+
    scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))
  
  # plot W2V-Insight results for iLOC
  RSAW2V_finalPlot_iLOC= ggplot(RSAW2V_ggpredict[RSAW2V_ggpredict$facet == "iLOC",], aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2,size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs( fill = "condition", #title = "",
                  x = "Time",y = "Rep-Str (W2V)")+scale_x_discrete(labels=c('Pre', 'Post'))+
    scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))
  
  # plot W2V-Insight results for pITG
   RSAW2V_finalPlot_pITG= ggplot(RSAW2V_ggpredict[RSAW2V_ggpredict$facet == "pITG",], aes(x= x, y = predicted , fill= group)) + 
    geom_bar(stat="identity",  position=position_dodge(),show.legend = F) + #, color = "black"
    geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted +std.error ), width=.2,size = .95,
                  position=position_dodge(.9)) + theme_classic() +labs(  fill = "condition", #title = "",
                  x = "Time",y = "Rep-Str (W2V)")+scale_x_discrete(labels=c('Pre', 'Post'))+
                  scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))


#### 3b2a) RSA2-IME using W2V: Insight memory effect for representations  #####


  #insight_CUTOFF_noRec_SOLVE_ord insight_noRec_SOLVE_ord
    RSAdata = newdata[( (newdata$RT>2 )  | is.na(newdata$RT) ) &  !is.na(newdata$insight_noRec_SOLVE_ord) & 
                        newdata$RSA_W2V_control_corr_n_vis > 10 & newdata$tp == 1,]
    M0W2V_mem <- lmer(RSA_W2V_R_vis ~ RT                       +ROI_bilat+sessionblock   + (1|ID) + (1|Item),data= RSAdata[RSAdata$cor2 == 1,], na.action  = na.omit)
    M1W2V_mem <- lmer(RSA_W2V_R_vis ~ RT+insight_noRec_SOLVE_ord+ROI_bilat+sessionblock+(1|ID) + (1|Item),data= RSAdata[RSAdata$cor2 == 1,], na.action  = na.omit)
    M2W2V_mem <- lmer(RSA_W2V_R_vis ~ RT+insight_noRec_SOLVE_ord*ROI_bilat+sessionblock+(1|ID)+ (1|Item) , data= RSAdata[RSAdata$cor2 == 1,], na.action  = na.omit)

    hist(residuals(M2W2V_mem)) # residuals are normally distributed -> model is adecuate
    anova(M0W2V_mem, M1W2V_mem, M2W2V_mem)#,M3W2V_mem)#, RSA4)#,  M4)#
    emmeans(M1W2V_mem, list(pairwise ~ insight_noRec_SOLVE_ord), adjust = "tukey")
    
     IME_W2V <- ggpredict(M1W2V_mem , c('ROI_bilat' ,'insight_noRec_SOLVE_ord' )) #%>% plot(show.title = F) + ggplot2::theme_classic()+labs(x="Insight Memory Factor", y="Representational strength (Word2Vec) ", color = "ROI")
     IME_W2V_finalPlot= ggplot(IME_W2V[IME_W2V$x != "pITG",], aes(x= x, y = predicted , fill= group)) + 
       geom_bar(stat="identity",  position=position_dodge(),show.legend = T) + #, color = "black"
       geom_errorbar(aes(ymin= predicted -std.error , ymax=predicted +std.error ), width=.2,size = .95,
                     position=position_dodge(.9)) + theme_classic() +labs(
                     x = "ROI",y = "Rep-Strength (W2V)", fill = "Insight Memory")+
     scale_fill_manual(breaks = c('Forgotten','Rem_LO-I', 'Rem_HI-I'),values=c("#fde56e" ,"#0090fe", "#ff7d45")) #ff9d0
     

    
###################################################################################################-    
##### 4) functional connectivity analysis #########################################################

    #### CONN analysis plotting 2nd Level results HI-I > LO-I & Rem_HI-I > Rem_LO-I > noRem  #####-
    
    #Insight
    #NOTE: HI-I & LO-I names were switched in CONN accidently -> RECODE!!! otherwise results are reversed
    CONN_Insight<- read.table("BeckerSommerCabeza_2023_Insight_single_connections_long2.csv", sep = ";", dec = ",", header=TRUE, na.strings=c("", " " , "NA", "NAN" )) # Pre_Solve_RSA_table_28-Oct-2022.txt

    CONN_Insight$condition = ordered(CONN_Insight$condition, levels=c("HI_I_vis" ,"LO_I_vis"  ),
                             labels=c("LO-I","HI-I"))
    Insightdata <- data_summary(CONN_Insight, varname="corr_value", groupnames=c("hemi_ROI", "condition"),31)
    
    Insight_conn_finalPlot= ggplot(Insightdata, aes(x= hemi_ROI, y = corr_value , fill= condition)) + 
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T) + #, color = "black"
      geom_errorbar(aes(ymin= corr_value -se , ymax=corr_value +se ), width=.3,size = .75,
                    position=position_dodge(.9)) + theme_classic() +labs(title= "Amygdala-VOTC functional connectivity", x = "ROI",y = "correlation coefficient",fill = "Insight")  + 
       scale_fill_manual(breaks = c('LO-I','HI-I'),values=c("#37c0c6", "#f98578"))+ theme(legend.position="bottom")
    
    
    #########-
    # IME
    CONN<- read.table("BeckerSommerCabeza_2023_IME_single_connections2_long.csv", sep = ";", dec = ",", header=TRUE, na.strings=c("", " " , "NA", "NAN" )) # Pre_Solve_RSA_table_28-Oct-2022.txt
    
    CONN$condition = ordered(CONN$condition, levels=c("nR" ,"R_LOI","R_HII"  ),
                                              labels=c("Forg" ,"R LO-I","R HI-I"))
    CONN$ROI = as.factor(CONN$ROI)
    CONN$hemi= as.factor(CONN$hemi)
    CONN$ID = as.factor(CONN$ID)
    IMEdata <- data_summary(CONN, varname="corr_values", groupnames=c( "ROI", "condition"),31)
    
    #library(glmmTMB)
    M0_conn <- lmer(corr_values ~ ROI+(1|ID) ,data= CONN, na.action = na.omit)#,family = binomial(link="logit"))
    M1_conn <- lmer(corr_values ~ ROI+hemi+(1|ID) ,data= CONN, na.action = na.omit)#,family = binomial(link="logit"))
    M2_conn <- lmer(corr_values ~ hemi+ROI+condition+(1|ID) ,data= CONN, na.action = na.omit)#,family = binomial(link="logit"))
    M3_conn <- lmer(corr_values ~ hemi+ROI*condition+(1|ID) ,data= CONN, na.action = na.omit)#,family = binomial(link="logit"))
    
    anova(M0_conn, M1_conn,M2_conn, M3_conn)
    summary(M2_conn)
    ggpredict(M2_conn , c('condition', 'ROI')) %>% plot(show.title = F) + ggplot2::theme_classic()
    emmeans(M2_conn, list(pairwise ~ condition | ROI), adjust = "tukey")
    
    #IME_conn <- ggpredict(M2_conn , c('ROI','condition' )) #%>% plot(show.title = F) + ggplot2::theme_classic()+labs(x="Insight Memory Factor", y="Representational strength (Word2Vec) ", color = "ROI")
    IME_conn_finalPlot= ggplot(IMEdata, aes(x= ROI, y = corr_values , fill= condition)) + 
      geom_bar(stat="identity",  position=position_dodge(),show.legend = T) + #, color = "black"
      geom_errorbar(aes(ymin= corr_values -se , ymax=corr_values +se ), width=.3,size = .75,
                    position=position_dodge(.9)) + theme_classic() +labs(x = "ROI",y = "correlation coefficient",   
                     fill = "Insight Memory")   +
      scale_fill_manual(breaks = c('Forg','R LO-I', 'R HI-I'),values=c("#fde56e" ,"#0090fe", "#ff7d45"))+ theme(legend.position="top")
    
######################################################################################  -
#### 5) Plotting all results together #####################     
    
  
    ### Figure 2: plot behavioral data 
    Insight_behave_ggarangeplot <- ggarrange( AHA_amount_plot,IME_ggpredict_plot,
                                              common.legend = T, legend = "right",
                                              ncol = 2, nrow = 1,labels = c("A", "B"))
    
    ### Figure 3-A
    ERS_ggarangeplot <- ggarrange( ERS_finalPlot,
                                   common.legend = F, legend = "bottom",
                                   ncol = 1, nrow = 1)#labels = c("Change in MVPS"),
    
    ### Figure 3-B
    RSA_AN8_W2V_ggarrangeplot <- ggarrange( RSAAN8_finalPlot_pFusC,RSAAN8_finalPlot_iLOC, RSAAN8_finalPlot_pITG,#,
                                            RSAW2V_finalPlot_pFusC,RSAW2V_finalPlot_iLOC, RSAW2V_finalPlot_pITG,
                                            common.legend = FALSE, legend = "top",
                                            ncol = 3, nrow = 2) #                               
    ### Figure 3-AB
    FigX <- ggarrange( ERS_ggarangeplot,RSA_AN8_W2V_ggarrangeplot,
                       common.legend = T, legend = "bottom",
                       ncol = 2, nrow = 1, labels = c("A", "B"))
    
    
    #### Figure 4-AB: Plotting the amy activity and FC for HI>LO Insight
    Insight_Amy_FC <- ggarrange(  HIvLOI_Amy_finalplot,Insight_conn_finalPlot,
                                  common.legend = TRUE, legend = "bottom",
                                  labels = c("A", "B"),
                                  ncol = 2, nrow = 1)
    
    #### Figure 5: IME for MVPS, Rep.Strength, Amy activity and FC
    IME_AN8_W2V_Amy_FC <- ggarrange(IME_ERS_finalPlot, IME_AN8_finalPlot,IME_W2V_finalPlot,IME_Amy_finalplot, IME_conn_finalPlot,
                                    common.legend = TRUE, legend = "bottom",
                                    labels = c("A", "B", "C", "D", "E"),
                                    ncol = 3, nrow = 2)