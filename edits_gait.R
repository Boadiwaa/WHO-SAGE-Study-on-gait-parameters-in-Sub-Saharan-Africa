library(knitr)
library(tidyverse)
library(haven)
library(labelled)
library(ggpubr)
library(gtsummary)
library(foreign)
library(ggsci)


## ---- a
gh_data <- read.dta("ghana.dta",convert.factors = T) %>% as_tibble()
gh_data$q2507[gh_data$q2507 >990] <- 0
gh_data$q2507[gh_data$q2508 >990] <- 0

#Selection of relevant columns and finding averages of systolic and diastolic BP, pulse, from the 3 readings
gh_data <- select(gh_data, c(q0104,q1009,q1011,q1012,q1016,q1017,q1018,q1019,q1023,q2000:q2003,q2009,q2025,q2032,q2011,q2014,q2015,q2017,q2033,q2035:q2039,q2047,q2501_s:q2513, q4001,q4010,q4014,q4022,q4025,q4033,q4040,q4060,q4062,q4069,q3001,q3007,q7002))%>%
  rowwise()%>%
  mutate(avg_sbp = mean(c(q2501_s, q2502_s, q2503_s),na.rm = TRUE),
         avg_dbp = mean(c(q2501_d, q2502_d, q2503_d),na.rm = TRUE),
         avg_p=mean(c(q2501a_p, q2502a_p, q2503a_p),na.rm = TRUE), 
         BMI=round(q2507/((q2506/100)^2),digits = 1),.keep = "unused")

gh_data <- gh_data %>% mutate(Age_Ranges = cut(q1011,c(18,30,40,50,60,70,80,90,100,120,130),include.lowest=TRUE),Edu_yrs = cut(q1017,c(0,6,9,12,15,20,25,28 ),include.lowest=TRUE),Age_Groups = cut(q1011,c(18,49,59,69,79,122),include.lowest=TRUE))

gh_data$BMI[gh_data$BMI< 18.5] <- "Underweight"
gh_data$BMI[gh_data$BMI %in% seq(30,122,0.1)] <- "Obese"
gh_data$BMI[gh_data$BMI %in%  seq(18.5,24.9,0.1)] <- "Normal Weight"
gh_data$BMI[gh_data$BMI %in%  seq(25.0,29.9,0.1)] <- "Overweight"


colnames(gh_data)[colnames(gh_data) %in% c("q0104","q1009","q1012","q3007","q4001","q4010","q4014", "q4022","q4025", "q4033","q4040", "q4060", "q4062", "q4069","q3001","q7002")] <- c("Residence", "Sex", "Marital_Status","Alcohol_use","Arthritis","Stroke","Angina","Diabetes_Mellitus","Chronic_Lung_Disease","Asthma", "Depression", "Hypertension", "Cataracts","Injuries_RTA" ,"Tobacco_use", "Income")

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

colnames(sa_data)[colnames(sa_data) %in% c("q0104","q1009","q1012","q7002","q4001","q4010","q4014", "q4022","q4025", "q4033","q4040", "q4060", "q4062", "q4069","q3001","q3007")] <- c("Residence", "Sex", "Marital_Status","Alcohol_use","Arthritis","Stroke","Angina","Diabetes_Mellitus","Chronic_Lung_Disease","Asthma", "Depression", "Hypertension", "Cataracts","Injuries_RTA" ,"Tobacco_use", "Income")


sa_data <-sa_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
gh_data <-gh_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
gh_data_c <-gh_data%>% filter(q2511 < 12.1, q2513 >1.1, q2513 <13.1)%>%rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
gh_data$norm_gs[gh_data$q2511 >12.0] <- 0.7861
gh_data$rap_gs[gh_data$q2513 >13.0] <- 1.1840
gh_data$rap_gs[gh_data$q2513 < 1.2] <- 1.1840

gh_data <- select(gh_data, -q2511, -q2513)
rm(gh_data_c)
sa_data_c <-sa_data%>% filter(q2511 < 12.1, q2513 <13.1)%>%rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
sa_data$norm_gs[sa_data$q2511 >12.0] <- 0.8486
gh_data$rap_gs[gh_data$q2513 >13.0] <- 1.3180
sa_data <- select(sa_data, -q2511, -q2513)
rm(sa_data_c)

theme_gtsummary_journal(journal = "lancet")
theme_gtsummary_compact()
theme_customs <- theme(
  text = element_text(family = 'firasans', size = 16),
  plot.title.position = 'plot',
  plot.title = element_text(
    face = 'bold', 
    colour = thematic::okabe_ito(8)[6],
    margin = margin(t = 2, r = 0, b = 7, l = 0, unit = "mm")
  ),
)

theme_set(theme_minimal() + theme_customs)


cd <-gh_data %>%
  select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,norm_gs,rap_gs, BMI,Angina,Chronic_Lung_Disease,Asthma,Arthritis,Stroke,Diabetes_Mellitus,Injuries_RTA,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>%  
      select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,norm_gs,rap_gs, BMI,Angina,Arthritis,Stroke,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Injuries_RTA,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
      mutate(ctry="South Africa")
  ) 
cd %>% tbl_summary(by=ctry, label = list(Age_Groups ~"Age Groups",
                                        q1011 ~"Age",
                                                                                             Edu_yrs ~"Years of Education",
                                                                                             Income ~"Income Satisfaction",
                                                                                            Marital_Status ~"Marital Status",
                                                                                             norm_gs ~" Normal Gait Speed",
                                                                                             rap_gs ~ "Rapid Gait Speed",
                                                                                             Angina ~"History of Angina",
                                                                                             Chronic_Lung_Disease ~"History of CLD*",
                                                                                             Asthma ~"History of Asthma",
                                                                                             Arthritis ~ "History of Arthritis",
                                                                                             Stroke ~"History of Stroke",
                                                                                             Diabetes_Mellitus ~"History of DM*",
                                                                                             Injuries_RTA ~"History of RTA*",
                                                                                             Depression ~"History of Depression",
                                                                                             Hypertension ~"History of HPT*",
                                                                                             Cataracts ~ "History of Cataracts",
                                                                                             Tobacco_use ~"History of Tobacco use",
                                                                                             Alcohol_use ~ "History of Alcohol use"))%>% bold_labels() 

## ---- b
gh_data%>% select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,norm_gs,rap_gs,BMI,Injuries_RTA,Arthritis,Stroke,Angina,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use)%>% 
   gtsummary::tbl_summary(by=Sex,
                                         label = list(Age_Groups ~"Age Groups",
                                                      q1011 ~"Age",
                                                      Edu_yrs ~"Years of Education",
                                                      Income ~"Income Satisfaction",
                                                      Marital_Status ~"Marital Status",
                                                      norm_gs ~" Normal Gait Speed",
                                                      rap_gs ~ "Rapid Gait Speed",
                                                      Angina ~"History of Angina",
                                                      Chronic_Lung_Disease ~"History of CLD*",
                                                      Asthma ~"History of Asthma",
                                                      Arthritis ~ "History of Arthritis",
                                                      Stroke ~"History of Stroke",
                                                      Diabetes_Mellitus ~"History of DM*",
                                                      Injuries_RTA ~"History of RTA*",
                                                      Depression ~"History of Depression",
                                                      Hypertension ~"History of HPT*",
                                                      Cataracts ~ "History of Cataracts",
                                                      Tobacco_use ~"History of Tobacco use",
                                                      Alcohol_use ~ "History of Alcohol use")) %>% add_p() %>% bold_labels() 

## ---- c
sa_data%>% select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,norm_gs,rap_gs,BMI,Injuries_RTA,Arthritis,Stroke,Angina,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use)%>% 
  as_factor() %>% gtsummary::tbl_summary(by=Sex,
                                                
                                         label = list(Age_Groups ~"Age Groups",
                                                      q1011 ~"Age",
                                                      Edu_yrs ~"Years of Education",
                                                      Marital_Status ~"Marital Status",
                                                      norm_gs ~" Normal Gait Speed",
                                                      rap_gs ~ "Rapid Gait Speed",
                                                      Income ~ "Income Satisfaction",
                                                    Angina ~"History of Angina",
                                                      Chronic_Lung_Disease ~"History of CLD*",
                                                      Asthma ~"History of Asthma",
                                                      Arthritis ~ "History of Arthritis",
                                                      Stroke ~"History of Stroke",
                                                      Diabetes_Mellitus ~"History of DM*",
                                                      Injuries_RTA ~"History of RTA*",
                                                      Depression ~"History of Depression",
                                                      Hypertension ~"History of HPT*",
                                                      Cataracts ~ "History of Cataracts",
                                                      Tobacco_use ~"History of Tobacco use",
                                                      Alcohol_use ~ "History of Alcohol use")) %>% add_p() %>% bold_labels() 

## ---- d
 cd %>% 
   select(ctry,Age_Groups,norm_gs,Edu_yrs,Residence,Sex,Income,Marital_Status) %>% tbl_strata(strata=ctry, .tbl_fun = ~.x %>% tbl_continuous(variable = norm_gs,
                                                                                                                                             label = list(Age_Groups ~"Age Groups",
                                                                                                                                                          Edu_yrs ~"Years of Education",
                                                                                                                                                          Marital_Status ~"Marital Status",
                                                                                                                                                          Income ~"Income Satisfaction",
                                                                                                                                                          norm_gs ~" Normal Gait Speed")) %>% 
                                                                                                modify_spanning_header(all_stat_cols() ~ "**Mean Gait Speed per Demographics **")) %>% bold_labels()
                                                                                                 


  #datasummary( Age_Groups+Edu_yrs+Residence+Marital_Status+Sex+Income ~ ctry * `norm_gs` * (Mean + SD), data=t)
 
## ---- e
  cd %>% 
    select(ctry,norm_gs,BMI,Injuries_RTA,Arthritis,Stroke,Angina,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
    tbl_strata(strata=ctry, .tbl_fun = ~.x %>% tbl_continuous(variable = norm_gs, 
                                                              label = list(
                                                                           norm_gs ~" Normal Gait Speed",
                                                                           Angina ~"History of Angina",
                                                                           Chronic_Lung_Disease ~"History of CLD*",
                                                                           Asthma ~"History of Asthma",
                                                                           Arthritis ~ "History of Arthritis",
                                                                           Stroke ~"History of Stroke",
                                                                           Diabetes_Mellitus ~"History of DM*",
                                                                           Injuries_RTA ~"History of RTA*",
                                                                           Depression ~"History of Depression",
                                                                           Hypertension ~"History of HPT*",
                                                                           Cataracts ~ "History of Cataracts",
                                                                           Tobacco_use ~"History of Tobacco use",
                                                                           Alcohol_use ~ "History of Alcohol use"))) %>% bold_labels()
  
## ---- m
     whodas <-gh_data %>%
     select(q2025,norm_gs,q2032,q2011,q2033, q2035,q2036, q2037, q2038,q2015,q2014) %>%
     mutate(
       ctry = "Ghana"
     ) %>% 
     union_all(
       sa_data  %>%  
         select(q2025,norm_gs,q2032,q2011,q2033, q2035, q2036, q2037, q2038,q2015,q2014) %>%
         mutate(ctry="South Africa"))
   
   whodas %>% select(-norm_gs) %>% tbl_summary(by=ctry, 
                                               label = list( q2025~ "in sitting for long periods?",
                                                               q2014 ~ "with making new friendships or
                                                               maintaining current friendships?",
                                                               q2015~"with dealing with strangers?",
                                                               q2011 ~"did you have in learning a new task?",
                                                               q2032 ~"in taking care of your household
                                                               responsibilities?",
                                                               q2033 ~"in joining in community activities? ",
                                                             q2035 ~"concentrating on doing something for
                                                             10 minutes?",
                                                             q2036 ~"in walking a long distance such as a
                                                             kilometer?",
                                                             q2037~"in bathing/washing your whole body?",
                                                             q2038~"in getting dressed?")) %>% bold_labels() 
   
## ---- n
   whodas %>% select(-ctry) %>% 
     tbl_uvregression(method = lm,y=norm_gs, 
                                 label = list( q2025~ "in sitting for long periods?",
                                               q2014 ~ "with making new friendships or
                                                               maintaining current friendships?",
                                               q2015~"with dealing with strangers?",
                                               q2011 ~"did you have in learning a new task?",
                                               q2032 ~"in taking care of your household
                                                               responsibilities?",
                                               q2033 ~"in joining in community activities? ",
                                               q2035 ~"concentrating on doing something for
                                                             10 minutes?",
                                               q2036 ~"in walking a long distance such as a
                                                             kilometer?",
                                               q2037~"in bathing/washing your whole body?",
                                               q2038~"in getting dressed?"))%>%
                                                                             bold_labels()
   
   
## ---- f
cd %>% select(-rap_gs,-q1011) %>% 
   
  tbl_uvregression(method = lm,y=norm_gs, label =list(Age_Groups ~"Age Groups",
                                                  
                                                         ctry ~"Country of Residence",
                                                                 Edu_yrs ~"Years of Education",
                                                                 Income ~"Income Satisfaction",
                                                                 Marital_Status ~"Marital Status",
                                                                 norm_gs ~" Normal Gait Speed",
                                                                 Angina ~"History of Angina",
                                                                 Chronic_Lung_Disease ~"History of CLD*",
                                                                 Asthma ~"History of Asthma",
                                                                 Arthritis ~ "History of Arthritis",
                                                                 Stroke ~"History of Stroke",
                                                                 Diabetes_Mellitus ~"History of DM*",
                                                                 Injuries_RTA ~"History of RTA*",
                                                                 Depression ~"History of Depression",
                                                                 Hypertension ~"History of HPT*",
                                                                 Cataracts ~ "History of Cataracts",
                                                                 Tobacco_use ~"History of Tobacco use",
                                                                 Alcohol_use ~ "History of Alcohol use")) %>% bold_labels()
  
## ---- g
 gga<- gh_data %>% group_by(Age_Ranges)%>% summarise(mean_gs= median(na.omit(norm_gs))) %>% 
   ggplot(aes(x=Age_Ranges,y=mean_gs))+ theme_cleveland()+
    geom_point()+labs(title = "Ghana",y="Median Gait Speed", x="Age Ranges")
  
  sga<-sa_data %>% group_by(Age_Ranges)%>% summarise(mean_gs= median(na.omit(norm_gs))) %>% 
    ggplot(aes(x=Age_Ranges,y=mean_gs))+ theme_cleveland()+
    geom_point() + labs(title = "South Africa",y="Median Gait Speed", x="Age Ranges")
  
  ggarrange(gga,sga,ncol=1,nrow=2,widths=c(4,4),heights=c(5,5),hjust=0.5) %>% annotate_figure(top = "Trend of Gait Speed with Increasing Age")
  
## ---- h
  splus <- cd %>% filter(q1011 >= 60)
  splus%>% group_by(q1011)%>% summarise(med_gs=median(norm_gs)) %>% ggplot(aes(x=q1011,y=med_gs)) + 
    geom_point() + 
  geom_smooth(method = lm)+ 
    labs(title = "Gait Speed in Age 60 and above",y="Median Gait Speed", x="Age")
  
## ---- i
  
 a<- cd %>% na.omit(ctry) %>%  group_by(ctry,Sex) %>% summarise(mean_gs= median(na.omit(norm_gs))) %>% 
    ggplot(aes(x = ctry,y=mean_gs, col = Sex)) +
    geom_linerange(aes(x = ctry, ymin = 0, ymax = mean_gs, colour = Sex), 
                   position = position_dodge(width = 1),size=2)+
    geom_point(position=position_dodge(width=1),size=6)+     
    labs( title= "Normal Gait Speed", y="Median Gait Speed", x= "Country")+
scale_colour_jama()
 
 b<- cd %>% na.omit(ctry) %>%  group_by(ctry,Sex) %>% summarise(mean_gs= median(na.omit(rap_gs))) %>% 
   ggplot(aes(x = ctry,y=mean_gs, col = Sex)) +
   geom_linerange(aes(x = ctry, ymin = 0, ymax = mean_gs, colour = Sex), 
                  position = position_dodge(width = 1),size=2)+
   geom_point(position=position_dodge(width=1),size=6)+     
   labs(title= "Rapid Gait Speed", y="Median Gait Speed", x= "Country")+
   scale_colour_uchicago()
 ggarrange(a,b,ncol=2,nrow=1) %>% annotate_figure(top = "Gender vs Gait Speed")
 
    
## ---- k
  #Difficulty in daily life due to pain and Problems due to not feeling refreshed during the day (from lack of energy)

   cp <-gh_data %>%
    select(q2000,q2009,norm_gs,rap_gs,q2017,q1011, q2001) %>%
    mutate(
      ctry = "Ghana"
    ) %>% 
    union_all(
      sa_data  %>%  
        select(q2000,q2009,norm_gs,rap_gs,q2017,q1011,q2001) %>%
        mutate(ctry="South Africa"))
   
   cp %>%select(q2000,q2001,q2009,q2017,ctry) %>%  
   
  tbl_summary(by=ctry, label = list(q2009 ~"Difficulty due to pain",
                                        
                                            q2017 ~"Difficulty due to lack of energy",
                                            
                                            q2000 ~"Health Rating",
                                            q2001 ~"Difficulty with activities")) %>% bold_labels()
## ---- km
                                            
   cp %>% select(-rap_gs,-q1011,-ctry) %>% tbl_uvregression(method = lm,y=norm_gs, label =list(q2009 ~"Difficulty due to pain",
                                                           
                                                           q2017 ~"Difficulty due to lack of energy",
                                                           norm_gs ~"Median Gait Speed",
                                                           q2000 ~"Health Rating",
                                                           q2001 ~"Difficulty with activities")) %>% bold_labels()
     
                                         
                                            
## ---- ps  
library(ggstatsplot)
  cp  %>% select(q2009,ctry,norm_gs) %>% drop_na() %>%
    ggbetweenstats(x=q2009,y=norm_gs)+ scale_color_rickandmorty()+
    labs(title = "Pain severity vs Gait Speed", y="Mean Gait Speed", x= "Pain Severity")
  
  cp %>% select(q2017,ctry,norm_gs) %>% drop_na() %>%
    ggbetweenstats(x=q2017,y=norm_gs)+ scale_fill_jco() + scale_color_jco() +
    labs(title = "Lack of Energy vs Gait Speed", y="Mean Gait Speed", x= "Lack of Energy")
  
    
## ---- l
  #Health Rating, and Difficulty with household activities
  # library(ggridges)
  # ggplot(aes(x=norm_gs,y=q2000,fill=q2000)) +
  #   geom_density_ridges(alpha=0.6, bandwidth=4) + xlim(-5,5) +
  #   theme(legend.position = "none",
  #         strip.text.x = element_text(size = 8))
  cp %>% select(q2000,ctry,norm_gs) %>% 
    ggbetweenstats(x=q2000,y=norm_gs)+
    scale_fill_jama() +
    scale_color_jama() +
  labs(title = "Health Rating vs Gait Speed", y="Mean Gait Speed", x= "Health Rating")
  
## ---- o
  cp %>% group_by(q2001) %>%
    ggbetweenstats(x=q2001,y=norm_gs) + scale_color_uchicago() +
    labs(title = "Difficulty with activities vs Gait Speed", y="Mean Gait Speed", x= "Difficulty level")
  
## ---- p
  colors <- thematic::okabe_ito(8)[-6] 
  
  # q2000 (health rating) vs mean gait speed vs age, especially for those with poor health ratings
  cp %>% filter(q2000 != "don't know") %>% group_by(q2000,q1011) %>% summarise(med=median(norm_gs)) %>% 
    ggplot(aes(x = q1011, y = med, col = q2000)) +
    geom_point(size = 1.5) + geom_smooth(method = "loess",se=FALSE)+
    scale_color_manual(values = colors) +
    xlim (18, 124) + ylim(0.3,1.3) + 
    labs(title = "Health rating vs Age vs Gait Speed", y="Median Gait Speed", x= "Age")#+ theme(element_text(legend.title="Level of Difficulty"))
  
  
## ---- q
   cp %>% filter(q2001 != "don't know") %>%  group_by(q2001,q1011) %>% summarise(med=median(norm_gs)) %>% 
     ggplot(aes(x = q1011, y = med, col = q2001)) +
     geom_point(size = 1.5) + geom_smooth(method = "loess",se=FALSE)+
     scale_color_jama() +
     xlim (18, 124) + ylim(0.3,1.3) + 
     labs(title = "Level of Difficulty with Activities vs Age vs Gait Speed", y="Median Gait Speed", x= "Age")#+ theme(element_text(legend.title="Level of Difficulty"))
   
## ---- r

  # start typing results for word doc
   
   
   