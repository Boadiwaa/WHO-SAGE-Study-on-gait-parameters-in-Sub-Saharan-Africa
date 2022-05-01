library(knitr)
library(tidyverse)
library(haven)
library(labelled)
library(ggpubr)
library(gtsummary)
library(foreign)
library(modelsummary)


## ---- a
gh_data <- read.dta("ghana.dta",convert.factors = T) %>% as_tibble()
gh_data$q2507[gh_data$q2507 >990] <- 0
gh_data$q2507[gh_data$q2508 >990] <- 0

#Selection of relevant columns and finding averages of systolic and diastolic BP, pulse, from the 3 readings
gh_data <- select(gh_data, c(q0104,q1009,q1011,q1012,q1016,q1017,q1018,q1019,q1023,q2000:q2003,q2009,q2025,q2032,q2011,q2014,q2015,q2017,q2033,q2035:q2039,q2047,q2501_s:q2513, q4001,q4010,q4014,q4022,q4025,q4033,q4040,q4060,q4062,q4069,q3001,q3007,q7002))%>%
  rowwise()%>%
  mutate(avg_sbp = mean(c(q2501_s, q2502_s, q2503_s),na.rm = TRUE), avg_dbp = mean(c(q2501_d, q2502_d, q2503_d),na.rm = TRUE),avg_p=mean(c(q2501a_p, q2502a_p, q2503a_p),na.rm = TRUE), BMI=round(q2507/((q2506/100)^2),digits = 1),.keep = "unused")

gh_data <- gh_data %>% mutate(Age_Ranges = cut(q1011,c(18,30,40,50,60,70,80,90,100,120,130),include.lowest=TRUE),Edu_yrs = cut(q1017,c(0,6,9,12,15,20,25,28 ),include.lowest=TRUE),Age_Groups = cut(q1011,c(18,49,59,69,79,122),include.lowest=TRUE))

gh_data$BMI[gh_data$BMI< 18.5] <- "Underweight"
gh_data$BMI[gh_data$BMI %in% seq(30,122,0.1)] <- "Obese"
gh_data$BMI[gh_data$BMI %in%  seq(18.5,24.9,0.1)] <- "Normal Weight"
gh_data$BMI[gh_data$BMI %in%  seq(25.0,29.9,0.1)] <- "Overweight"


colnames(gh_data)[colnames(gh_data) %in% c("q0104","q1009","q1012","q7002","q4001","q4010","q4014", "q4022","q4025", "q4033","q4040", "q4060", "q4062", "q4069","q3001","q3007")] <- c("Residence", "Sex", "Marital_Status","Income","Arthritis","Stroke","Angina","Diabetes_Mellitus","Chronic_Lung_Disease","Asthma", "Depression", "Hypertension", "Cataracts","Injuries_RTA" ,"Tobacco_use", "Alcohol_use")

#SA Data
sa_data <- read.dta("sa.dta", convert.factors=T) %>% as_tibble()
sa_data$q2507[sa_data$q2507 >990] <- 0
sa_data$q2507[sa_data$q2508 >990] <- 0

#Selection of relevant columns and finding averages of systolic and diastolic BP, pulse, from the 3 readings
sa_data <- select(sa_data, c(q0104,q0105a,q0409,q1009,q1011,q1012,q1016,q1017,q1018,q1019,q1023,q2000:q2003,q2009,q2025,q2032,q2011,q2014,q2015,q2017,q2033,q2035:q2039,q2047,q2501_s:q2513, q4001,q4010,q4014,q4022,q4025,q4033,q4040,q4060,q4062,q4066,q4069,q3001,q3002,q3007,q3010,q7002))%>%
  rowwise()%>%
  mutate(avg_sbp = mean(c(q2501_s, q2502_s, q2503_s),na.rm = TRUE), avg_dbp = mean(c(q2501_d, q2502_d, q2503_d),na.rm = TRUE), avg_p=mean(c(q2501a_p, q2502a_p, q2503a_p),na.rm = TRUE),BMI=round(q2507/((q2506/100)^2),digits = 1),.keep = "unused")
sa_data <- sa_data %>% mutate(Age_Ranges = cut(q1011,c(18,30,40,50,60,70,80,90,100,120,130),include.lowest=TRUE),Edu_yrs = cut(q1017,c(0,6,9,12,15,20,25,28 ),include.lowest=TRUE),Age_Groups = cut(q1011,c(18,49,59,69,79,122),include.lowest=TRUE))

sa_data$BMI[sa_data$BMI< 18.5] <- "Underweight"
sa_data$BMI[sa_data$BMI %in% seq(30,225,0.1)] <- "Obese"
sa_data$BMI[sa_data$BMI %in%  seq(18.5,24.9,0.1)] <- "Normal Weight"
sa_data$BMI[sa_data$BMI %in%  seq(25.0,29.9,0.1)] <- "Overweight"

colnames(sa_data)[colnames(sa_data) %in% c("q0104","q1009","q1012","q7002","q4001","q4010","q4014", "q4022","q4025", "q4033","q4040", "q4060", "q4062", "q4069","q3001","q3007")] <- c("Residence", "Sex", "Marital_Status","Income","Arthritis","Stroke","Angina","Diabetes_Mellitus","Chronic_Lung_Disease","Asthma", "Depression", "Hypertension", "Cataracts","Injuries_RTA" ,"Tobacco_use", "Alcohol_use")

sa_data <-sa_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513, .keep = "unused")
gh_data <-gh_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513, .keep = "unused")

theme_gtsummary_mean_sd()
theme_gtsummary_journal(journal = "lancet")
theme_gtsummary_compact()


cd <-gh_data %>%
  select(Age_Groups,q1011,Edu_yrs,Residence,Marital_Status,Sex,Income, BMI,norm_gs,rap_gs,Angina,Chronic_Lung_Disease,Asthma,Arthritis,Stroke,Diabetes_Mellitus,Injuries_RTA,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>%  
      select(Age_Groups,q1011,Edu_yrs,Residence,Marital_Status,Sex,Income, BMI,norm_gs,Angina,rap_gs,Arthritis,Stroke,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Injuries_RTA,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
      mutate(ctry="South Africa")
  ) 
cd %>% tbl_summary(by=ctry, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{p}% ({n})"))  %>% add_p() %>% bold_labels() 

## ---- b
gh_data%>% select(Age_Groups,q1011,Edu_yrs,Residence,Marital_Status,Sex,Income, Injuries_RTA, BMI,Arthritis,Stroke,Angina,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use)%>% 
  as_factor() %>% gtsummary::tbl_summary(by=Sex,statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                 all_categorical() ~ "{p}% ({n})")) %>% add_p() %>% bold_labels() 

## ---- c
sa_data%>% select(Age_Groups,q1011,Edu_yrs,Residence,Marital_Status,Sex,Income, Injuries_RTA, BMI,Arthritis,Stroke,Angina,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use)%>% 
  as_factor() %>% gtsummary::tbl_summary(by=Sex,statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                 all_categorical() ~ "{p}% ({n})")) %>% add_p() %>% bold_labels() 

## ---- d
 cd %>% 
   select(ctry,Age_Groups,norm_gs,Edu_yrs,Residence,Sex,Income,Marital_Status) %>% tbl_strata(strata=ctry, .tbl_fun = ~.x %>% tbl_continuous(variable = norm_gs, statistic =everything() ~ "{mean} ({sd})") %>%
 modify_spanning_header(all_stat_cols() ~ "**Mean Gait Speed per Demographics **") %>% add_p()) %>% bold_labels()

#t <- cd %>% 
  #as_tibble() %>%select(Age_Groups,Edu_yrs,Residence,Marital_Status,Sex,Income,ctry,norm_gs)
   
  #datasummary( Age_Groups+Edu_yrs+Residence+Marital_Status+Sex+Income ~ ctry * `norm_gs` * (Mean + SD), data=t)
 
  ## ---- e
  cd %>% 
    select(ctry,norm_gs,Injuries_RTA, BMI,Arthritis,Stroke,Angina,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
    tbl_strata(strata=ctry, .tbl_fun = ~.x %>% tbl_continuous(variable = norm_gs, statistic =everything() ~ "{mean} ({sd})") %>%
                 modify_spanning_header(all_stat_cols() ~ "**Mean Gait Speed per Demographics **") %>% add_p()) %>% bold_labels()
  
   ## ---- m
   whodas <-gh_data %>%
     select(q2025,norm_gs,rap_gs,q2032,q2011,q2033, q2035, q2036, q2037, q2038,q2015,q2014,q1011) %>%
     mutate(
       ctry = "Ghana"
     ) %>% 
     union_all(
       sa_data  %>%  
         select(q2025,norm_gs,rap_gs,q2032,q2011,q2033, q2035, q2036, q2037, q2038,q2015,q2014,q1011) %>%
         mutate(ctry="South Africa"))
   
   whodas %>% select(-q1011) %>% tbl_summary(by=ctry, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                       all_categorical() ~ "{p}% ({n})")) %>% add_p() %>% bold_labels() 
   
   ## ---- n
   whodas %>% select(-q1011,-rap_gs) %>% 
     tbl_strata(strata=ctry, .tbl_fun = ~.x %>% 
                  tbl_continuous(variable = norm_gs, statistic =everything() ~ "{mean} ({sd})") %>%
                                              modify_spanning_header(all_stat_cols() ~ "**Mean Gait Speed per Demographics **") %>% 
                                                add_p()) %>% bold_labels()
   
   
  ## ---- f
  tbl_uvregression(cd,method = lm,y=norm_gs)
  
  ## ---- g
  gga<-gh_data %>% group_by(Age_Ranges)%>% summarise(mean_gs= mean(na.omit(norm_gs))) %>% ggplot(aes(x=Age_Ranges,y=mean_gs))+geom_point() +theme_cleveland() +ggtitle("Ghana")
  
  sga <-sa_data %>% group_by(Age_Ranges)%>% summarise(mean_gs= mean(na.omit(norm_gs))) %>% ggplot(aes(x=Age_Ranges,y=mean_gs))+geom_point() +theme_cleveland() +ggtitle("South Africa")
  ggarrange(gga,sga,ncol=2,nrow=1,widths=c(5,5),heights=c(4,4),hjust=0.5)
  
  ## ---- h
  splus <- gh_data %>% filter(q1011 >= 60)
  splus%>% group_by(q1011)%>% summarise(mean_gs=mean(norm_gs)) %>% ggplot(aes(x=q1011,y=mean_gs)) + geom_point() + geom_smooth(method = lm)
  
  ## ---- i
  
  g1 <-cd %>% group_by(Sex) %>% ggplot(aes(x= Sex,norm_gs, colour=Sex)) + geom_point() +
    theme(legend.position = "none")
  g2<-cd %>% group_by(Sex) %>% ggplot(aes(x= Sex, rap_gs,colour= Sex)) + geom_point() +
    theme(legend.position = "none")
  ggarrange(g1,g2,ncol = 2, nrow = 1)
  
  ## ---- j
  g3<-cd %>% group_by(Sex) %>% ggplot(aes(x= norm_gs,Sex, colour=Sex)) + geom_point() +
    theme(legend.position = "none")
  g4<-cd %>% group_by(Sex) %>% ggplot(aes(x= rap_gs, Sex,colour= Sex)) + geom_point() +
    theme(legend.position = "none")
  ggarrange(g3,g4,ncol = 2, nrow = 1)
  
  ## ---- k
  #Difficulty in daily life due to pain and Problems due to not feeling refreshed during the day (from lack of energy)
  cp <-gh_data %>%
    select(q2009,norm_gs,rap_gs,q2017,q2000,q1011, q2001) %>%
    mutate(
      ctry = "Ghana"
    ) %>% 
    union_all(
      sa_data  %>%  
        select(q2009,norm_gs,Angina,rap_gs,q2017,q1011, q2000, q2001) %>%
        mutate(ctry="South Africa"))

  cp %>% group_by(q2009) %>% ggplot(aes(x= q2009, norm_gs, fill=ctry)) +geom_boxplot() 
  cp %>% group_by(q2017) %>% ggplot(aes(x= q2017, norm_gs, fill=ctry)) +geom_boxplot()
    
    
  ## ---- l
  #Health Rating, and Difficulty with household activities
  cp %>% group_by(q2000) %>% ggplot(aes(x= q2000, norm_gs, fill=ctry)) +geom_boxplot()
  cp %>% group_by(q2001) %>% ggplot(aes(x= q2001, norm_gs, fill=ctry)) +geom_boxplot()
  
 