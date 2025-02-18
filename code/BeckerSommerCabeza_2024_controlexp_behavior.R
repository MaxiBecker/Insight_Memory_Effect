#### Code for Manuscript Becker, Sommer & Cabeza 2025
#### Neural Mechanisms of Creative Problem Solving: From Representational Change to Memory Formation
#### Control analysis Subsequent Memory  
#### (c) almaxi@gmail.com
#### last update: 18/Feb/2025

rm(list = ls())

## load libraries ##############################################################
  library(lme4)
  library(lmerTest)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)      
  library(sjstats) 
  library(ggeffects)
  library(tidyverse)
  library(ggplot2)


###########-
setwd("I:/Meine Ablage/uni/_studies/IN_REVISION/PVI/GitHub_4_publication/REVISION1/data/control_experiments")
expdata_all<- read.table("BeckerSommerCabeza_2024_controlexp.csv", sep = ";", dec = ".", header=T, na.strings=c("", " " , "NA", "NAN" )) 
memdata    <- read.table("BeckerSommerCabeza_2024_controlexp_SME.csv", sep = ";", dec = ".", header=T, na.strings=c("", " " , "NA", "NAN" )) 
###########-


### Demographics of this study #######

#recruited sample
    length(unique(memdata$subject)) #beinhaltet noch subject mit too few hits
    gender_all = expdata_all%>% group_by(subject) %>% dplyr::count(sex, age)
    alter_all=mean(gender_all$age, na.rm = T)
    geschlecht_all=mean(gender_all$sex, na.rm = T)
    alter_all_sd = sd(gender_all$age, na.rm = T)
    n_all= nrow(gender_all)
    memory_members = length(unique(memdata$subject))
    exp_members= length(unique(expdata_all$subject))

    sprintf("Recruited sample: %s subjects participated in the control experiment: women n=%s,overall age=%s (SD=%s) ", 
                  n_all,  round(geschlecht_all, digits = 4), round(alter_all,2), round(alter_all_sd,2) )
    
    expdata = expdata_all[
                          expdata_all$subject != 105 & #too few hits
                          expdata_all$subject != 103 & expdata_all$subject != 107 & expdata_all$subject != 111 & expdata_all$subject != 123 &  expdata_all$subject != 130 &
                          expdata_all$trialnum != 7 # practise trials
                      ,]
    
# final sample
    unique(expdata$subject)
    gender = expdata%>% group_by(subject) %>% dplyr::count(sex, age)
    alter=mean(gender$age)
    geschlecht=mean(gender$sex)
    alter_sd = sd(gender$age)
    n= nrow(gender)

    print(sprintf("Final sample: %s subjects participated in the control experiment: women n=%s,overall age=%s (SD=%s) ", 
                  n,  round(geschlecht, digits = 4), round(alter,2), round(alter_sd,2) ))

#######################################-

### prepare data
    memdata$Alternative =NA
    memdata[memdata$cor2== 2 & !is.na(memdata$cor2),]$Alternative = 1
    memdata[memdata$cor2 == 2 & !is.na(memdata$cor2),]$cor2 =1
    
    expdata$Alternative=NA
    expdata[expdata$cor2== 2 & !is.na(expdata$cor2),]$Alternative = 1
    expdata[expdata$cor2 == 2 & !is.na(expdata$cor2),]$cor2 =1
    expdata[expdata$cor2 == 3 & !is.na(expdata$cor2),]$cor2 =NA  # NOTE! hier nochmal aendern ggf.!!!!!
    expdata$ID = expdata$subject
    expdata$RT = expdata$RT/1000
    expdata[expdata$RT >= 10 & !is.na(expdata$RT),]$RT = NA
    
    expdata$AHA_response = expdata$AHA_response-1
    expdata$certain_response = expdata$certain_response-1
    expdata$sudden_response= expdata$sudden_response-1
    expdata$insight_sum = expdata$AHA_response + expdata$certain_response + expdata$sudden_response #note skala für aha, certainty, sudden fängt hier ab 2 statt ab 1 an

## calculate mediansplit for aha
    subjects = unique(expdata$subject)
    expdata$insight_mediansplit = NA

    expdata[expdata$cor1 == 0 & !is.na(expdata$cor1),]$insight_mediansplit = "not solved"
    
    for (subj in subjects){
      subj_median = median(expdata[expdata$subject == subj,]$insight_sum, na.rm = T)
      
      if(length(expdata[expdata$subject == subj & expdata$insight_sum> subj_median & !is.na(expdata$insight_sum) ,]$insight_mediansplit)!=0 )  {
        
        expdata[expdata$subject == subj & expdata$insight_sum<= subj_median & !is.na(expdata$insight_sum) ,]$insight_mediansplit = "LO-I"
        expdata[expdata$subject == subj & expdata$insight_sum>subj_median & !is.na(expdata$insight_sum) ,]$insight_mediansplit = "HI-I"
      }else{
        expdata[expdata$subject == subj & expdata$insight_sum< subj_median & !is.na(expdata$insight_sum) ,]$insight_mediansplit = "LO-I"
        expdata[expdata$subject == subj & expdata$insight_sum>=subj_median & !is.na(expdata$insight_sum) ,]$insight_mediansplit = "HI-I"
      }
      cat(sprintf("Current subject: %s has insight median %s \n", subj, subj_median))
    }

   expdata$insight_mediansplit_ord= ordered(expdata$insight_mediansplit, levels=c("not solved" ,"LO-I","HI-I"),labels=c("not solved" ,"LO-I","HI-I"))

### accuracy adjustment: calculate categorization error (to approximate correctly counted but wrong responses in fmri sample)  ####
   #0=no existing Kateogrie (random guess)	
   #1=same Kategorie	
   #2=different Kategorie	
   
   # proportion of correctly recognized images for HII
   expdata[expdata$cor2 == 1 & !is.na(expdata$cor2),]$Kategorie = 5 # correkt gelöste
   cat_data = expdata %>% group_by(ID) %>% dplyr::count(Kategorie) #[data_new$insight_mediansplit == "HI-I",]
   cat_data = cat_data[!is.na(cat_data$Kategorie),]
   cat_data$n_adjust = as.double (cat_data$n)
   cat_data[cat_data$Kategorie==0 & !is.na(cat_data$Kategorie==0),]$n_adjust =  cat_data[cat_data$Kategorie==0 & !is.na(cat_data$Kategorie==0),]$n_adjust*.25 # chance adjust the random categorie
   cat_data1 = cat_data[,-3]
   cat_data2 = spread(cat_data1, key = c(Kategorie), value = n_adjust)
   cat_data2$falsch_richtig = cat_data2$"1" +cat_data2$"0"
   falsch_richtig1 = mean(cat_data2$"1"/ (cat_data2$"1" +cat_data2$"0"))*100
   falsch_richtig0 = mean(cat_data2$"0"/ (cat_data2$"1" +cat_data2$"0"))*100
   
   # % falsche Antworten, die aber als richtig markiert wurden: Anteil all richtige  (Cat1+0.25*Cat2) / (cat_data$cor2 + Cat1+ 0.25*Cat2)
   cat_data2$cor2_falsch = cat_data2$falsch_richtig / (cat_data2$"1" +cat_data2$"0" + cat_data2$'5')
   mean(cat_data2$cor2_falsch)
   percent_1_same_category = mean(cat_data2$"1"  / (cat_data2$"1" +cat_data2$"0" + cat_data2$'5'))
   #percent_1_same_category_sd = sd(cat_data2$"1"  / (cat_data2$"1" +cat_data2$"0" + cat_data2$'5'))
   
   percent_0_random_category = mean(cat_data2$"0"  / (cat_data2$"1" +cat_data2$"0" + cat_data2$'5'))
   #percent_0_random_category_sd = sd(cat_data2$"0"  / (cat_data2$"1" +cat_data2$"0" + cat_data2$'5'))
   
### neue Vektoren festlegen
  expdata$Recognition_conf =NA
  expdata$Recognition_conf_RT =NA
  expdata$Correct_Recognition =NA
  
  expdata$Recall_conf =NA
  expdata$Recall_conf_RT =NA
  expdata$Correct_Recall =NA

#### merge memory data (~5 days later) with experimental data
for(i in 1:nrow(expdata)){
  for(j in 1:nrow(memdata)){
    if((expdata$moonie[i] == memdata$stimulusitem1[j]) & (expdata$subject[i] == memdata$subject[j])) {
      
      expdata$Recognition_conf[i] = memdata$response[j]
      expdata$Recognition_conf_RT[i] = memdata$latency[j]
      expdata$Correct_Recognition[i] = memdata$correct[j]
      
        if( (memdata$blocknum[j] == memdata$blocknum[j+1]) & (memdata$trialcode[j+1]=="solution_solved_VIS") & (expdata$subject[i] == memdata$subject[j])){
          
          expdata$Recall_conf[i] = memdata$response[j+1]
          expdata$Recall_conf_RT[i]= memdata$latency[j+1]
          
          if((expdata$cor2[i]==1 & memdata$response[j]>2 & !is.na(expdata$cor2[i])) | (expdata$cor2[i]==1 & memdata$response[j+1]>2) & !is.na(expdata$cor2[i])){ #note expdata$cor2 kann NA sein weil von 3 auf NA gesetzt oben
                expdata$Correct_Recall[i] = 0
          }else if (is.na(memdata$cor2[j+2])){
          # hier noch beachten dass cor2 noch ne 2 oder 3 annehmen kann! ->noch optimieren im Skript
          }else if( (memdata$blocknum[j] == memdata$blocknum[j+2]) & memdata$response[j+1]<=2 & (memdata$cor2[j+2]==1) ){
               expdata$Correct_Recall[i] = 1
          }else if( (memdata$blocknum[j] == memdata$blocknum[j+2]) &  (memdata$correct[j+2]==0) ){ 
               expdata$Correct_Recall[i] = 0
        }}
    }}}

  expdata$cor2_anteilig= NA
  expdata[expdata$cor2==1 & !is.na(expdata$cor2),]$cor2_anteilig=1
  expdata[expdata$cor2==0 & expdata$cor1==1 & !is.na(expdata$cor2),]$cor2_anteilig=0
  
  expdata$HII_ms_correct1 = NA
  expdata[expdata$insight_mediansplit == "HI-I" & expdata$cor2 == 1 & !is.na(expdata$cor2),]$HII_ms_correct1 = 1
  expdata[expdata$insight_mediansplit == "LO-I" & expdata$cor2 == 1 & !is.na(expdata$cor2),]$HII_ms_correct1 = 0
  
  expdata$LOI_ms_correct1 = NA
  expdata[expdata$insight_mediansplit == "LO-I" & expdata$cor2 == 1 & !is.na(expdata$cor2),]$LOI_ms_correct1 = 1
  expdata[expdata$insight_mediansplit == "HI-I" & expdata$cor2 == 1 & !is.na(expdata$cor2),]$LOI_ms_correct1 = 0
  
  expdata$RT_correct = NA
  expdata[expdata$cor2 == 1 & !is.na(expdata$cor2),]$RT_correct = expdata[expdata$cor2 == 1& !is.na(expdata$cor2),]$RT
  
  expdata$HII_ms_RTcorrect = NA
  expdata[expdata$insight_mediansplit == "HI-I" & expdata$cor2 == 1 & !is.na(expdata$cor2),]$HII_ms_RTcorrect = expdata[expdata$insight_mediansplit == "HI-I" & expdata$cor2 == 1 & !is.na(expdata$cor2),]$RT
  
  expdata$LOI_ms_RTcorrect = NA
  expdata[expdata$insight_mediansplit == "LO-I" & expdata$cor2 == 1 & !is.na(expdata$cor2),]$LOI_ms_RTcorrect = expdata[expdata$insight_mediansplit == "LO-I" & expdata$cor2 == 1 & !is.na(expdata$cor2),]$RT

###Memory measures
  ####  False Alarm Rate #####
  FA_data = memdata[memdata$trialcode == "visCATCH" ,c("subject", "correct")]
  FA_data_agg = FA_data %>% group_by(subject) %>% summarise(mean = mean(correct)) #  sum(funs(correct))   #& behave_data$cor2==1 
  FA_rate = mean(1-FA_data_agg$mean)
  FA_rate_sd = sd(1-FA_data_agg$mean)
  
  see_thresh = 2
  solve_thresh = 2
  
  # proportion of correctly recognized images 
  expdata$Correct_Recognition_cor2 = 0
  expdata[expdata$Recognition_conf <=see_thresh & expdata$cor2 == 1 & !is.na(expdata$cor2) & !is.na(expdata$Recognition_conf),]$Correct_Recognition_cor2 = 1

      # proportion of correctly recognized images for HII
      expdata$Correct_Recognition_cor2_HII = 0
      expdata[expdata$Recognition_conf <=see_thresh & expdata$cor2 == 1 & expdata$insight_mediansplit == "HI-I" & !is.na(expdata$cor2) & !is.na(expdata$Recognition_conf),]$Correct_Recognition_cor2_HII = 1

      # proportion of correctly recognized images for HII
      expdata$Correct_Recognition_cor2_HII1 = NA
      expdata[expdata$Recognition_conf <=see_thresh  & expdata$cor2 == 1 & expdata$insight_mediansplit == "HI-I" & !is.na(expdata$cor2) & !is.na(expdata$Recognition_conf),]$Correct_Recognition_cor2_HII1 = 1
      
      # proportion of correctly recognized images for HII
      expdata$Correct_Recognition_cor2_LOI = 0
      expdata[expdata$Recognition_conf <=see_thresh  & expdata$cor2 == 1 & expdata$insight_mediansplit == "LO-I" & !is.na(expdata$cor2) & !is.na(expdata$Correct_Recognition),]$Correct_Recognition_cor2_LOI = 1

      # proportion of correctly recognized images for HII
      expdata$Correct_Recognition_cor2_LOI1 = NA
      expdata[expdata$Recognition_conf <=see_thresh  & expdata$cor2 == 1 & expdata$insight_mediansplit == "LO-I" & !is.na(expdata$cor2) & !is.na(expdata$Recognition_conf),]$Correct_Recognition_cor2_LOI1 = 1

      
  # proportion of correctly recognized images remember to also have solved it
  expdata$Correct_Recognition_for_solved_cor2 = 0
  expdata[expdata$cor2 == 1 & !is.na(expdata$cor2)  & !is.na(expdata$Recall_conf) &  expdata$Recall_conf<=solve_thresh,]$Correct_Recognition_for_solved_cor2 = 1

      # proportion of correctly recognized images remember to also have solved it divided for HI insight
      expdata$Correct_Recognition_for_solved_cor2_HII = 0
      expdata[ expdata$cor2 == 1 & !is.na(expdata$cor2)  & !is.na(expdata$Recall_conf) &  expdata$Recall_conf<=solve_thresh & expdata$insight_mediansplit == "HI-I",]$Correct_Recognition_for_solved_cor2_HII = 1

      # proportion of correctly recognized images remember to also have solved it divided for HI insight  for display purposes
      expdata$Correct_Recognition_for_solved_cor2_HII1 = NA
      expdata[ expdata$cor2 == 1 & !is.na(expdata$cor2) & !is.na(expdata$Recall_conf) &  expdata$Recall_conf<=solve_thresh& expdata$insight_mediansplit == "HI-I",]$Correct_Recognition_for_solved_cor2_HII1 = 1

      # proportion of correctly recognized images remember to also have solved it divided by LO insight
      expdata$Correct_Recognition_for_solved_cor2_LOI = 0
      expdata[expdata$cor2 == 1 & !is.na(expdata$cor2) &  !is.na(expdata$Recall_conf) &  expdata$Recall_conf<=solve_thresh & expdata$insight_mediansplit == "LO-I",]$Correct_Recognition_for_solved_cor2_LOI = 1

      # proportion of correctly recognized images remember to also have solved it divided by LO insight  for display purposes
      expdata$Correct_Recognition_for_solved_cor2_LOI1 = NA
      expdata[expdata$cor2 == 1 & !is.na(expdata$cor2) & !is.na(expdata$Recall_conf) &  expdata$Recall_conf<=solve_thresh & expdata$insight_mediansplit == "LO-I",]$Correct_Recognition_for_solved_cor2_LOI1 = 1

  
  # proportion of correctly recognized images correct recall it
  expdata$Correct_Recall_anteilig = 0
  expdata[ expdata$cor2 == 1 & !is.na(expdata$cor2)  &  expdata$Correct_Recall == 1 & !is.na(expdata$Correct_Recall),]$Correct_Recall_anteilig = 1

      # proportion of correctly recognized images correct recall it divided by HI insight
      expdata$Correct_Recall_anteilig_HII = 0
      expdata[ expdata$cor2 == 1 & !is.na(expdata$cor2) &  expdata$Correct_Recall == 1 & !is.na(expdata$Correct_Recall) & expdata$insight_mediansplit == "HI-I",]$Correct_Recall_anteilig_HII = 1

      # proportion of correctly recognized images correct recall it divided by HI insight for display purposes
      expdata$Correct_Recall_anteilig_HII1 = NA
      expdata[ expdata$cor2 == 1 & !is.na(expdata$cor2)  &  expdata$Correct_Recall == 1 & !is.na(expdata$Correct_Recall) & expdata$insight_mediansplit == "HI-I",]$Correct_Recall_anteilig_HII1 = 1
      
      # proportion of correctly recognized images correct recall it divided by LO insight  for display purposes
      expdata$forget_Recall_anteilig_HII1 = NA
      expdata[ expdata$cor2 == 1 & !is.na(expdata$cor2)  & expdata$Correct_Recall == 0 & !is.na(expdata$Correct_Recall) & expdata$insight_mediansplit == "HI-I",]$forget_Recall_anteilig_HII1 = 1
      
      # proportion of correctly recognized images correct recall it divided by LO insight
      expdata$Correct_Recall_anteilig_LOI = 0
      expdata[ expdata$cor2 == 1 & !is.na(expdata$cor2) & expdata$Correct_Recall == 1 & !is.na(expdata$Correct_Recall) & expdata$insight_mediansplit == "LO-I",]$Correct_Recall_anteilig_LOI = 1

      # proportion of correctly recognized images correct recall it divided by LO insight  for display purposes
      expdata$Correct_Recall_anteilig_LOI1 = NA
      expdata[ expdata$cor2 == 1 & !is.na(expdata$cor2)  & expdata$Correct_Recall == 1 & !is.na(expdata$Correct_Recall) & expdata$insight_mediansplit == "LO-I",]$Correct_Recall_anteilig_LOI1 = 1

      # proportion of correctly recognized images correct recall it divided by LO insight  for display purposes
      expdata$forget_Recall_anteilig_LOI1 = NA
      expdata[ expdata$cor2 == 1 & !is.na(expdata$cor2)  & expdata$Correct_Recall == 0 & !is.na(expdata$Correct_Recall) & expdata$insight_mediansplit == "LO-I",]$forget_Recall_anteilig_LOI1 = 1
      
##### behavioral parameters ######
      
  behave_data1 = expdata[expdata$RT>=1.5 | is.na(expdata$RT),] %>% group_by(ID) %>% summarise_all(funs(mean), na.rm =T)   #& behave_data$cor2==1 
  
  # Performance measures
  mean(behave_data1$cor1)# solution button press
  sd(behave_data1$cor1)
  
  mean(behave_data1$cor2_anteilig) #=prozent korrekt von den bereits gelösten (button press)
  sd(behave_data1$cor2_anteilig)
  
  mean(behave_data1$HII_ms_correct1)#  proportion correctly solved high insight trials
  sd(behave_data1$HII_ms_correct1)
  
  mean(behave_data1$LOI_ms_correct1)# proportion correctly solved low insight trials
  sd(behave_data1$LOI_ms_correct1)
  
  median(behave_data1$RT_correct)# solution time for correctly solved trials
  sd(behave_data1$RT_correct)
  
  median(behave_data1$HII_ms_RTcorrect) #solution time for correctly solved high insight trials
  sd(behave_data1$HII_ms_RTcorrect)
  
  median(behave_data1$LOI_ms_RTcorrect, na.rm =T)# solution time for correctly solved low insight trials
  sd(behave_data1$LOI_ms_RTcorrect, na.rm =T)
  
  # Memory measures
  # Image Recognition
  mean(behave_data1$Correct_Recognition_cor2, na.rm = T) #=prozent korrekt von den bereits gelösten (button press)
  sd(behave_data1$Correct_Recognition_cor2, na.rm = T)
  
      mean(behave_data1$Correct_Recognition_cor2_HII, na.rm = T) #=prozent korrekt von den bereits gelösten (button press)
      sd(behave_data1$Correct_Recognition_cor2_HII, na.rm = T)
      
      mean(behave_data1$Correct_Recognition_cor2_LOI, na.rm = T) #=prozent korrekt von den bereits gelösten (button press)
      sd(behave_data1$Correct_Recognition_cor2_LOI, na.rm = T)
  
  # Having solved it - Recognition
  mean(behave_data1$Correct_Recognition_for_solved_cor2, na.rm = T) #=prozent korrekt von den bereits gelösten (button press)
  sd(behave_data1$Correct_Recognition_for_solved_cor2, na.rm = T)
  
      mean(behave_data1$Correct_Recognition_for_solved_cor2_HII, na.rm = T) #=prozent korrekt von den bereits gelösten (button press)
      sd(behave_data1$Correct_Recognition_for_solved_cor2_HII, na.rm = T)
  
      mean(behave_data1$Correct_Recognition_for_solved_cor2_LOI, na.rm = T) #=prozent korrekt von den bereits gelösten (button press)
      sd(behave_data1$Correct_Recognition_for_solved_cor2_LOI, na.rm = T)
  
  # Image Identity Recall
  mean(behave_data1$Correct_Recall_anteilig, na.rm = T) #=prozent korrekt von den bereits gelösten (button press)
  sd(behave_data1$Correct_Recall_anteilig, na.rm = T)
  
      mean(behave_data1$Correct_Recall_anteilig_HII, na.rm = T) #=prozent korrekt von den bereits gelösten (button press)
      sd(behave_data1$Correct_Recall_anteilig_HII, na.rm = T)
  
      mean(behave_data1$Correct_Recall_anteilig_LOI, na.rm = T) #=prozent korrekt von den bereits gelösten (button press)
      sd(behave_data1$Correct_Recall_anteilig_LOI, na.rm = T)
      
  ######-
      

      
      
      
      
      
      
      
  
#### Inferential statistics  #######
  
  # behavioral generation effect not controlled for RT
  
  ## calculate accuracy effect ####
  expdata2 = expdata[expdata$RT>=1.5 | is.na(expdata$RT) , ]
  M0_cor2 <- glmer(cor2 ~ blocknum+             (1|ID) + (1|stimnumber),data= expdata2[!is.na(expdata2$insight_sum),], family = binomial(link ="logit"),na.action  = na.omit)
  M1_cor2 <- glmer(cor2 ~ blocknum+insight_sum +(1|ID) + (1|stimnumber),data= expdata2[!is.na(expdata2$insight_sum),], family = binomial(link ="logit"),na.action  = na.omit)
  
  anova(M0_cor2, M1_cor2)
  tab_model(M1_cor2, show.std  = T)

  # solution time effect
  expdata2b = expdata[expdata$RT>=1.5 | is.na(expdata$RT) & expdata$cor2 , ]
  M0_RT <- lmer(log(RT) ~ blocknum+             (1|ID) + (1|stimnumber),data= expdata2b[!is.na(expdata2b$insight_sum) & expdata2$cor2 == 1,], na.action  = na.omit)
  M1_RT <- lmer(log(RT) ~ blocknum+insight_sum +(1|ID) + (1|stimnumber),data= expdata2b[!is.na(expdata2b$insight_sum) & expdata2$cor2 == 1,], na.action  = na.omit)
  
  anova(M0_RT, M1_RT)
  tab_model(M1_RT, show.std  = T)


  ###### Behavioral Insight Memory Effect ##########
  
  #  categorical factor
  expdata3 = expdata[(expdata$RT>=1.5 | is.na(expdata$RT)) & expdata$cor2==1 | is.na(expdata$RT), ]
  M0_IME_cat <- glmer(Correct_Recall ~ blocknum+                        (1|ID) + (1|stimnumber),data= expdata3,family = binomial(link ="logit"), na.action  = na.omit)
  M1_IME_cat <- glmer(Correct_Recall ~ blocknum+insight_mediansplit_ord+(1|ID) + (1|stimnumber),data= expdata3,family = binomial(link ="logit"), na.action  = na.omit)
  
  anova(M0_IME_cat, M1_IME_cat)
  tab_model(M1_IME_cat)
  
#### Figure S7-C ######
        data_new1 = expdata3[!is.na(expdata3$insight_mediansplit_ord) & !is.na(expdata3$Correct_Recall),]
        IME_ggpredict <- ggpredict(M1_IME_cat, terms = "insight_mediansplit_ord")
        data_new1 <- data_new1 %>% mutate(predicted = predict(M1_IME_cat, type = "response") * 100)
        
        # Create the plot:
        IME_ggpredict_plot=ggplot(data_new1, aes(x = insight_mediansplit_ord, y = predicted, fill = insight_mediansplit_ord)) +
          # Violin plot for the raw data distribution
          geom_violin(trim = FALSE, alpha = 0.6, width = 1.8,adjust = 0.5) +
          theme_classic() +
          labs(title = "5 days after experiment",
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
  
  ## continuous variable controlled for RT
  expdata3 = expdata[expdata$RT>=1.5 & expdata$cor2 ==1 & !is.na(expdata$insight_sum), ]
  M0_IME_RT <- glmer(Correct_Recall ~ blocknum+                      (1|ID) + (1|stimnumber),data= expdata3,family = binomial(link ="logit"), na.action  = na.omit)
  M1_IME_RT <- glmer(Correct_Recall ~ blocknum+scale(RT)+           +(1|ID) + (1|stimnumber),data= expdata3,family = binomial(link ="logit"), na.action  = na.omit)
  M2_IME_RT <- glmer(Correct_Recall ~ blocknum+scale(RT)+insight_sum+(1|ID) + (1|stimnumber),data= expdata3,family = binomial(link ="logit"), na.action  = na.omit)

  anova(M0_IME_RT, M1_IME_RT,M2_IME_RT)
  tab_model(M2_IME_RT)

#### Figure S7-D: (with RT) ######
    data_new1 = expdata3[!is.na(expdata3$insight_sum) & !is.na(expdata3$Correct_Recall),]
    IME_RT_ggplot = ggpredict(M2_IME_RT , c('insight_sum'))  
    data_new1 <- data_new1 %>%mutate(predicted = predict(M2_IME_RT, newdata = data_new1, type = "response") * 100)
    data_new1$insight_sum_fac = as.factor(data_new1$insight_sum)
    n_levels <- length(unique(data_new1$insight_sum_fac))
    gradient_palette <- colorRampPalette(c("#f5d6e2", "#733e54"))(n_levels)
    IME_RT_finalPlot=ggplot(data_new1, aes(x = insight_sum, y = predicted, fill = insight_sum_fac)) +
      geom_violin(trim = FALSE, alpha = 0.6, width = 1.8, adjust = 0.5) +
      theme_classic() +
      labs(title = "5 days after fMRI",
           x = "Insight (continuous)",
           y = "Mooney solution recall in %") +
      #scale_y_continuous(limits = c(0, 115)) +
      scale_x_continuous(breaks = 3:12) +   # Set x-axis ticks to 3, 4, 5, ... 12
      scale_fill_manual(values = gradient_palette) +
      theme(legend.position = "none") +      # Remove the legend for insight_sum_fac
      geom_point(data = IME_RT_ggplot, 
                 aes(x = x, y = predicted * 100), inherit.aes = FALSE,
                 color = "black", size = 3, shape = 16) +  # Black dots for EMMs
      geom_errorbar(data = IME_RT_ggplot,
                    aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100), inherit.aes = FALSE,
                    width = 0.2, size = 0.95, position = position_dodge(0.9)) 
    
  ###############################################################################################################-
  
#### Figure S7-A ####
  ## overall amount of solved trials divided by condition: HI-L LO-I and not solved  
  data_new = expdata[expdata$RT>=1.5 | is.na(expdata$RT) , ]
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
  descriptives_long$condition_ord = ordered(descriptives_long$condition, levels=c("not solved" ,"LO-I","HI-I"  ),labels=c("not solved" ,"LO-I","HI-I"))
  
  p <- ggplot(descriptives_long, aes(condition_ord,amount,fill = condition_ord))
  AHA_amount_plot <- p+ geom_bar( position = 'dodge', stat = 'summary', fun.y = 'mean') + #, color = "black"
    geom_jitter(position = position_jitter(width = 0.1, height = 0.1), color = "#949393")+ #+ shape = 21, 
    labs(title = "during control experiment",x = "Insight (categorical)",y = "overall amount in %")+theme_classic()+scale_y_continuous(limits = c(0, 70))+
    scale_fill_manual(breaks = c('not solved','LO-I', 'HI-I'),values=c("#d0cccc", "#f5d6e2", "#733e54"))+
    geom_errorbar(stat = 'summary', position = 'dodge', width = 0.2, size = .95) 
  

  ############################################################################################################### -
  #### Figure S7-B ######
  ## Daten aufbereiten für Plot mit ANzahl an Trial pro Zelle 
  behave_data = expdata[expdata$RT>=1.5 | is.na(expdata$RT) & expdata$cor2 == 1, ]
 
  # create aggregated data per subject 
  behave_data0 = behave_data[ ,c('ID', 'cor2', 'cor1', 'RT',
                                 'Correct_Recognition_cor2', 'Correct_Recognition_cor2_HII1', 'Correct_Recognition_cor2_LOI1',
                                 'Correct_Recognition_for_solved_cor2','Correct_Recognition_for_solved_cor2_HII1','Correct_Recognition_for_solved_cor2_LOI1',
                                 'Correct_Recall_anteilig','Correct_Recall_anteilig_HII1','Correct_Recall_anteilig_LOI1',
                                 'forget_Recall_anteilig_HII1','forget_Recall_anteilig_LOI1'
  )]
  behave_data_conditioncount1 <-  behave_data0 %>% group_by(ID) %>% summarise(Recog_HII = sum(!is.na(Correct_Recognition_cor2_HII1)),
                                                                              Recog_LOI = sum(!is.na(Correct_Recognition_cor2_LOI1)),
                                                                              Rec_s_HII = sum(!is.na(Correct_Recognition_for_solved_cor2_HII1)),
                                                                              Rec_s_LOI = sum(!is.na(Correct_Recognition_for_solved_cor2_LOI1)),
                                                                              Recall_HII = sum(!is.na(Correct_Recall_anteilig_HII1)),
                                                                              Fg_HII = sum(!is.na(forget_Recall_anteilig_HII1)),
                                                                              Recall_LOI = sum(!is.na(Correct_Recall_anteilig_LOI1)),
                                                                              Fg_LOI = sum(!is.na(forget_Recall_anteilig_LOI1))
                                                                              )
  
  library(data.table)
  behave_data_conditioncount_long1 <- as.data.frame(melt(setDT(behave_data_conditioncount1), id.vars = c("ID"), variable.name = "condition"))
  behave_data_conditioncount_long2 = na.omit(behave_data_conditioncount_long1)
  
  p1 <- ggplot(data = behave_data_conditioncount_long2,mapping = aes(x = condition, y = value, fill = condition))
  p1a <- p1+ geom_bar( position = 'dodge', stat = 'summary', fun.y = "mean") + 
    geom_jitter(position = position_jitter(width = 0.1, height = 0.1), color = "#949393")+
    scale_fill_manual(breaks = c('Recog_HII', 'Recog_LOI','Rec_s_HII','Rec_s_LOI','Recall_HII','Fg_HII','Recall_LOI','Fg_LOI'),
                      values=c("#733e54","#f5d6e2" , "#733e54","#f5d6e2","#733e54","#d0cccc","#f5d6e2","#d0cccc" ))+ 
    labs(title = "5 days after experiment", x = "conditions",y = "absolute amount") +theme_classic() +
    scale_y_continuous(breaks = round(seq(min(behave_data_conditioncount_long1$value), max(behave_data_conditioncount_long1$value), by = 5),1))+
    theme(legend.position = "none")+ geom_errorbar(stat = 'summary', position = 'dodge',  width = 0.2, size = .95) 

  
### Figure S7: plot all behavioral data together  #####
  library(ggpubr)
  Insight_behave_ggarangeplot <- ggarrange( AHA_amount_plot, p1a, IME_ggpredict_plot, IME_RT_finalPlot,
                                            common.legend = T, legend = "none",
                                            ncol = 2, nrow = 2,labels = c("A", "B", "C", "D")) #p2a 
  