library(knitr)
library(tidyverse)
library(haven)
library(labelled)
library(ggpubr)
library(gtsummary)
library(foreign)
library(ggsci)


## ---- a
theme_gtsummary_journal(journal = "lancet")
theme_gtsummary_compact()
theme_set(theme_minimal())

gh_data <- read.dta("ghana.dta",convert.factors = T) %>% as_tibble()
gh_data$q2507[gh_data$q2507 >990] <- 0
gh_data$q2507[gh_data$q2508 >990] <- 0

#Selection of relevant columns and finding averages of systolic and diastolic BP, pulse, from the 3 readings
gh_data <- select(gh_data, c(q0104,q1009,q1011,q1012,q1016,q1017,q1018,q1019,q1023,q2000:q2003,q2009,q2025,q2028,
                             q2032,q2011,q2014,q2015,q2017,q2033,q2035:q2039,q2047,q2501_s:q2513, q4001,q4010,
                             q4014,q4022,q4025,q4033,q4040,q4060,q4062,q4069,q3001,q3007,q7002))%>%
  rowwise()%>%
  mutate(avg_sbp = mean(c(q2501_s, q2502_s, q2503_s),na.rm = TRUE),
         avg_dbp = mean(c(q2501_d, q2502_d, q2503_d),na.rm = TRUE),
         avg_p=mean(c(q2501a_p, q2502a_p, q2503a_p),na.rm = TRUE), 
         BMI=round(q2507/((q2506/100)^2),digits = 1),.keep = "unused")

gh_data <- gh_data %>% mutate(Edu_yrs = cut(q1017,c(0,6,9,12,15,20,30 ),include.lowest=TRUE),
                              Age_Groups = cut(q1011,c(18,49,59,69,79,122),include.lowest=TRUE))

gh_data$BMI[gh_data$BMI< 18.5] <- "Underweight"
gh_data$BMI[gh_data$BMI %in% seq(30,122,0.1)] <- "Obese"
gh_data$BMI[gh_data$BMI %in%  seq(18.5,24.9,0.1)] <- "Normal Weight"
gh_data$BMI[gh_data$BMI %in%  seq(25.0,29.9,0.1)] <- "Overweight"
levels(gh_data$Age_Groups) <- c("18 to 49", "50 to 59", "60 to 69", "70 to 79", "80 and above")
levels(gh_data$Edu_yrs) <- c("0 to 6", "7 to 9", "10 to 12", "13 to 15", "16 to 20", "22 to 30")

colnames(gh_data)[colnames(gh_data) %in% c("q0104","q1009","q1012","q7002","q4001","q4010","q4014", 
                                           "q4022","q4025", "q4033","q4040", "q4060", "q4062", "q4069","q3001","q3007")] <- c(
                                             "Residence", "Sex", "Marital_Status","Alcohol_use","Arthritis","Stroke",
                                             "Angina","Diabetes_Mellitus","Chronic_Lung_Disease","Asthma", "Depression", 
                                             "Hypertension", "Cataracts","Injuries_RTA" ,"Tobacco_use", "Income")


#SA Data
sa_data <- read.dta("sa.dta", convert.factors=T) %>% as_tibble()
sa_data$q2507[sa_data$q2507 >990] <- 0
sa_data$q2507[sa_data$q2508 >990] <- 0

#Selection of relevant columns and finding averages of systolic and diastolic BP, pulse, from the 3 readings
sa_data <- select(sa_data, c(q0104,q0105a,q0409,q1009,q1011,q1012,q1016,q1017,
                             q1018,q1019,q1023,q2000:q2003,q2009,q2025,q2028,q2032,q2011,q2014,
                             q2015,q2017,q2033,q2035:q2039,q2047,q2501_s:q2513, q4001,q4010,q4014,
                             q4022,q4025,q4033,q4040,q4060,q4062,q4066,q4069,q3001,q3002,q3007,q3010,q7002))%>%
  rowwise()%>%
  mutate(avg_sbp = mean(c(q2501_s, q2502_s, q2503_s),na.rm = TRUE),
         avg_dbp = mean(c(q2501_d, q2502_d, q2503_d),na.rm = TRUE), avg_p=mean(c(q2501a_p, q2502a_p, q2503a_p),na.rm = TRUE),
         BMI=round(q2507/((q2506/100)^2),digits = 1),.keep = "unused")
sa_data <- sa_data %>% mutate(Edu_yrs = cut(q1017,c(0,6,9,12,15,20,30 ),include.lowest=TRUE),
                              Age_Groups = cut(q1011,c(18,49,59,69,79,122),include.lowest=TRUE))

sa_data$BMI[sa_data$BMI< 18.5] <- "Underweight"
sa_data$BMI[sa_data$BMI %in% seq(30,225,0.1)] <- "Obese"
sa_data$BMI[sa_data$BMI %in%  seq(18.5,24.9,0.1)] <- "Normal Weight"
sa_data$BMI[sa_data$BMI %in%  seq(25.0,29.9,0.1)] <- "Overweight"
levels(sa_data$Age_Groups) <- c("18 to 49", "50 to 59", "60 to 69", "70 to 79", "80 and above")
levels(sa_data$Edu_yrs) <- c("0 to 6", "7 to 9", "10 to 12", "13 to 15", "16 to 20", "22 to 30")


colnames(sa_data)[colnames(sa_data) %in% 
                    c("q0104","q1009","q1012","q7002","q4001","q4010",
                                           "q4014", "q4022","q4025", "q4033","q4040", "q4060", "q4062", 
                                           "q4069","q3001","q3007")] <- c("Residence", "Sex", "Marital_Status",
                                                                          "Alcohol_use","Arthritis","Stroke","Angina",
                                                                          "Diabetes_Mellitus","Chronic_Lung_Disease","Asthma",
                                                                          "Depression", "Hypertension", "Cataracts",
                                                                          "Injuries_RTA" ,"Tobacco_use", "Income")

# Combined demographics

dem <-gh_data %>%
  select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,#norm_gs,rap_gs, 
         BMI,Angina,Chronic_Lung_Disease,Asthma,Arthritis,Stroke,Diabetes_Mellitus,Injuries_RTA,Depression,
         Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>%  
      select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,#norm_gs,rap_gs,
             BMI,Angina,Arthritis,Stroke,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Injuries_RTA,Depression,
             Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
      mutate(ctry="South Africa")
  ) 
n <- list(Age_Groups ~"Age (in years)",
          q1011 ~"Age",
          Edu_yrs ~"Years of Education",
          Income ~"Income Satisfaction",
          Marital_Status ~"Marital Status",
          #norm_gs ~" Normal Gait Speed",
          #rap_gs ~ "Rapid Gait Speed",
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
          Alcohol_use ~ "History of Alcohol use")

dem %>% tbl_summary(by=ctry, label=n) %>% 
  bold_labels() %>% 
  modify_caption("**Table 1. Overview of Socio-Demographic and Health Characteristics**")



# Dealing with gait speed outliers

sa_data <-sa_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
gh_data <-gh_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
gh_data_c <-gh_data%>% filter(q2511 < 12.1, q2513 >1.1, q2513 <13.1)%>%
  rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
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

cd <-gh_data %>%
  select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,norm_gs,rap_gs, 
         BMI,Angina,Chronic_Lung_Disease,Asthma,Arthritis,Stroke,Diabetes_Mellitus,Injuries_RTA,Depression,
         Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>%  
      select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,norm_gs,rap_gs,
             BMI,Angina,Arthritis,Stroke,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Injuries_RTA,Depression,
             Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
      mutate(ctry="South Africa")
  ) 

n <- list(Age_Groups ~"Age (in years)",
          q1011 ~"Age",
          Edu_yrs ~"Years of Education",
          Income ~"Income Satisfaction",
          Marital_Status ~"Marital Status",
          #norm_gs ~" Normal Gait Speed",
          #rap_gs ~ "Rapid Gait Speed",
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
          Alcohol_use ~ "History of Alcohol use")


## ---- b

gh_data%>% select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,norm_gs,
                  rap_gs,BMI,Injuries_RTA,Arthritis,Stroke,Angina,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Depression,
                  Hypertension,Cataracts,Tobacco_use,Alcohol_use)%>% 
   gtsummary::tbl_summary(by=Sex,
                                  label = n) %>% add_p() %>% bold_labels() 

## ---- c

sa_data%>% select(Age_Groups,q1011,Sex,Edu_yrs,Residence,Marital_Status,Income,norm_gs,
                  rap_gs,BMI,Injuries_RTA,Arthritis,Stroke,Angina,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,Depression,
                  Hypertension,Cataracts,Tobacco_use,Alcohol_use)%>% 
  as_factor() %>% gtsummary::tbl_summary(by=Sex,
              label = n) %>%
  add_p() %>% bold_labels() 

## ---- gs
cd %>% select(ctry,norm_gs,rap_gs) %>% 
  tbl_summary(by=ctry,
              label=list(norm_gs ~"Normal Gait Speed",
                         rap_gs ~"Rapid Gait Speed")) %>% 
  bold_labels()  %>%
  modify_caption("**Table 2. Country-wise Comparison of Gait Speed**")


cd %>% select(ctry,norm_gs) %>% ggplot(aes(x = norm_gs, 
                                           fill = ctry)) +
  xlim(0,1.75)+
  geom_density(alpha = 0.75)+
  scale_fill_lancet()+ labs(fill="Country",
                            title= "Distribution of the Normal Gait Speed in Ghana and South Africa",
                            x="Normal Gait Speed")

## ---- d

 cd %>% 
   select(ctry,Age_Groups,norm_gs,Edu_yrs,Residence,Sex,Income,Marital_Status) %>% 
   tbl_strata(strata=ctry, .tbl_fun = ~.x %>% tbl_continuous(variable = norm_gs,
                                                            label = list(Age_Groups ~"Age (in years)",
                                                            Edu_yrs ~"Years of Education",
                                                            Marital_Status ~"Marital Status",
                                                            Income ~"Income Satisfaction",
                                                            norm_gs ~" Normal Gait Speed")) %>% 
                                                          modify_spanning_header(all_stat_cols() ~ "**Mean Gait Speed per Demographics **")) %>% 
                                                   bold_labels()



  #datasummary( Age_Groups+Edu_yrs+Residence+Marital_Status+Sex+Income ~ ctry * `norm_gs` * (Mean + SD), data=t)
 
## ---- e
 
 ds<- list(
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
   Alcohol_use ~ "History of Alcohol use")
  
 cd %>% 
    select(ctry,norm_gs,BMI,Injuries_RTA,Arthritis,Stroke,Angina,Diabetes_Mellitus,Chronic_Lung_Disease,Asthma,
           Depression,Hypertension,Cataracts,Tobacco_use,Alcohol_use) %>%
    tbl_strata(strata=ctry, .tbl_fun = ~.x %>% tbl_continuous(variable = norm_gs, 
                                                              label = ds)) %>% bold_labels()
  
## ---- m
     whodas <-gh_data %>%
     select(q2011,q2014,q2015,q2028,norm_gs,q2032,q2033, q2035,q2036, q2037, q2038,q2039,q2047) %>%
     mutate(
       ctry = "Ghana"
     ) %>% 
     union_all(
       sa_data  %>%  
         select(q2011,q2014,q2015,q2028,norm_gs,q2032,q2033, q2035,q2036, q2037, q2038,q2039,q2047) %>%
         mutate(ctry="South Africa"))
     
     who<- list( q2039~ "in your day to day work?",
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
                 q2028 ~ "in standing for long periods (such as 30 minutes)?",
                 q2037~"in bathing/washing your whole body?",
                 q2038~"in getting dressed?",
                 q2047 ~"In the last 30 days, how much have you been emotionally
                                                             affected by your health condition(s)?")
   
   whodas %>% select(-norm_gs) %>% tbl_summary(by=ctry, 
                                               label = who) %>% bold_labels()
   
## ---- n
   whodas %>% select(-ctry) %>% 
     tbl_uvregression(method = lm,y=norm_gs, 
                                 label = who)%>% bold_labels()
                                                                             
   
   
## ---- f
cd %>% select(-rap_gs,-q1011) %>% 
   
  tbl_uvregression(method = lm,y=norm_gs, label = ds) %>% bold_labels()
  
## ---- g
 gh_data %>%filter(Age_Groups != "NA") %>% group_by(Age_Groups)%>% 
   summarise(mean_gs= median(na.omit(norm_gs))) %>% 
   ggplot(aes(x=Age_Groups,y=mean_gs))+ 
    geom_point(size=2)+labs(title = "Ghana", y ="Median Gait Speed", x="Age (in years)")
  
  sa_data %>% filter(Age_Groups != "NA") %>% group_by(Age_Groups)%>% 
    summarise(mean_gs= median(na.omit(norm_gs))) %>% 
    ggplot(aes(x=Age_Groups,y=mean_gs))+ 
    geom_point(size=2) + labs(title = "South Africa", y ="Median Gait Speed", x="Age (in years)")
  

  
## ---- h
  splus <- cd %>% filter(q1011 >= 60)
  splus%>% group_by(q1011)%>% summarise(med_gs=median(norm_gs)) %>% ggplot(aes(x=q1011,y=med_gs)) + 
    geom_point(color = "gray40", alpha = .5) + 
  geom_smooth(method = lm,color="black", fill= "firebrick")+ 
    labs(title = "Gait Speed in Age 60 and above",y="Median Gait Speed", x="Age (in years)")
  
## ---- i
  
 a<- cd %>% na.omit(ctry) %>%  group_by(ctry,Sex) %>% summarise(mean_gs= median(na.omit(norm_gs))) %>% 
    ggplot(aes(x = ctry,y=mean_gs, col = Sex)) +
    geom_linerange(aes(x = ctry, ymin = 0, ymax = mean_gs, colour = Sex), 
                   position = position_dodge(width = 1),size=2)+
    geom_point(position=position_dodge(width=1),size=6)+     
    labs( title= "Normal Gait Speed", y="Median Gait Speed", x= "Country")+
scale_colour_jama()+
   coord_flip()
 
 b<- cd %>% na.omit(ctry) %>%  group_by(ctry,Sex) %>% summarise(mean_gs= median(na.omit(rap_gs))) %>% 
   ggplot(aes(x = ctry,y=mean_gs, col = Sex)) +
   geom_linerange(aes(x = ctry, ymin = 0, ymax = mean_gs, colour = Sex), 
                  position = position_dodge(width = 1),size=2)+
   geom_point(position=position_dodge(width=1),size=6)+     
   labs(title= "Rapid Gait Speed", y="Median Gait Speed", x= "Country")+
   scale_colour_uchicago()+
   coord_flip()
 ggarrange(a,b,ncol=1,nrow=2) %>% annotate_figure(top = "Gender vs Gait Speed")
 
    
## ---- k
  #Difficulty in daily life due to pain and Problems due to not feeling refreshed during the day (from lack of energy)

   cp <-gh_data %>%
    select(q2000,q2009,Sex,norm_gs,rap_gs,q2017,q1011, q2001) %>%
    mutate(
      ctry = "Ghana"
    ) %>% 
    union_all(
      sa_data  %>%  
        select(q2000,q2009,Sex,norm_gs,rap_gs,q2017,q1011,q2001) %>%
        mutate(ctry="South Africa"))
   
   pv <-list(q2009 ~"Difficulty due to pain",
             
             q2017 ~"Difficulty due to lack of energy",
             
             q2000 ~"Health Rating",
             q2001 ~"Difficulty with activities")
   
   cp %>%select(q2000,q2001,q2009,q2017,ctry) %>%  
   
  tbl_summary(by=ctry, label = pv ) %>% bold_labels()
## ---- km
                                            
   cp %>% select(-rap_gs,-q1011,-ctry) %>% tbl_uvregression(method = lm,y=norm_gs, label = pv) %>% bold_labels()
     
                                         
                                            
## ---- ps  
library(ggstatsplot)
library(ggbeeswarm)
library(ggthemes)
   
   cp %>% select(q2000,ctry,norm_gs) %>% 
     filter(q2000 != "don't know") %>% 
     ggplot(aes(x=q2000,y=norm_gs,color=q2000))+ 
     stat_compare_means(label.x = 0.75,
                        label.y=0.15)+
     geom_quasirandom(alpha = 0.7,
                      size = 1.5) + ylim(0,2.5)+
     theme_economist()+ 
     geom_boxplot()+
     theme(legend.position = "none")+
     scale_color_economist(name=NULL)+
     labs(title = "Health Rating vs Gait Speed", y="Normal Gait Speed",
          x= "Health Rating")
   
  

  cp %>% select(q2017,ctry,norm_gs) %>% drop_na() %>%
    filter(q2017 != "don't know") %>%
    ggplot(aes(x=q2017,y=norm_gs,color=q2017))+
    stat_compare_means(label.x = 0.75,
                       label.y=0.15)+
    geom_quasirandom(alpha = 0.7,
                     size = 1.5) +
    geom_boxplot()+
    scale_color_uchicago() +
    ylim(0,2.5)+ theme(legend.position = "none")+
    labs(title = "Lack of Energy vs Gait Speed", y="Normal Gait Speed",
         x= "Lack of Energy")


    
## ---- l
  
cp  %>% select(q2009,ctry,norm_gs) %>% 
    filter(q2009 != "don't know", q2009 != "not applicable") %>% 
    ggplot(aes(x=q2009,y=norm_gs,color=q2009))+ 
    stat_compare_means(label.x = 0.75,
                       label.y=0.15)+
    geom_quasirandom(alpha = 0.7,
                     size = 1.5) + 
    geom_boxplot() +
    scale_color_jama() +
    ylim(0,2.5)+ theme(legend.position = "none")+
    labs(title = "Pain severity vs Gait Speed", y="Normal Gait Speed", 
         x= "Pain Severity")
  #Health Rating, and Difficulty with household activities
#    library(ggridges)
# cp %>% filter(Sex!="Unknown") %>% 
#   ggplot(aes(x=norm_gs,y=q2000,fill=Sex)) +
#     geom_density_ridges(alpha=0.6, bandwidth=4) + xlim(-10,20) +
#   scale_fill_cyclical(
#                       # labels = c(`1997 Summer` = "Summer",
#                       #            `1997 Winter` = "Winter"),
#                       values = c("tomato", "dodgerblue"),
#                       name = "Sex:", guide = "legend") +
#     theme_ridges(grid = FALSE) +
#   theme(legend.position = "none",
#           strip.text.x = element_text(size = 8))

 
  
## ---- o
  cp %>% group_by(q2001) %>%
    filter(q2001 != "don't know") %>% 
    ggbetweenstats(x=q2001,y=norm_gs) + 
    ylim(0,1.75)+
    scale_color_uchicago() +
    labs(title = "Difficulty with activities vs Gait Speed", 
         y="Mean Gait Speed", x= "Difficulty level")
  
## ---- p
  
  
  # q2000 (health rating) vs mean gait speed vs age, especially for those with poor health ratings
  cp %>% filter(q2000 != "don't know") %>% group_by(q2000,q1011) %>% summarise(med=median(norm_gs)) %>% 
    ggplot(aes(x = q1011, y = med, col = q2000)) +
    geom_point(size = 1.5) + geom_smooth(method = "loess",se=FALSE)+
    scale_color_jco() +
    xlim (18, 124) + ylim(0.3,1.3) + 
    labs(title = "Health rating vs Age vs Gait Speed", y="Median Gait Speed", x= "Age",
         color= "Health Rating")
  
## ---- q
   cp %>% filter(q2001 != "don't know") %>%  group_by(q2001,q1011) %>% summarise(med=median(norm_gs)) %>% 
     ggplot(aes(x = q1011, y = med, col = q2001)) +
     geom_point(size = 1.5) + geom_smooth(method = "loess",se=FALSE)+
     scale_color_jama() +
     xlim (18, 124) + ylim(0.3,1.3) + 
     labs(title = "Difficulty with Activities vs Age vs Gait Speed", 
          y="Median Gait Speed", x= "Age (in years)", color="Level of Difficulty")
   
## ---- r
   hpt <-gh_data %>%
     select(norm_gs,rap_gs,avg_sbp,avg_dbp,Hypertension) %>%
     mutate(
       ctry = "Ghana"
     ) %>% 
     union_all(
       sa_data  %>%  
         select(norm_gs,rap_gs,avg_sbp,avg_dbp,Hypertension) %>%
         mutate(ctry="South Africa")) %>%
     filter(avg_sbp >130 & avg_dbp > 90) %>% 
   
  mutate(c_bp=paste(as.character(round(avg_sbp, digits = 1)), 
    as.character(round(avg_dbp,digits = 1)), sep = "/")) 
   
 # hpt %>% filter(Hypertension !="NA") %>% 
 #   ggplot(aes(x=avg_sbp,y=norm_gs,color=Hypertension))+
 #   geom_point()+ ylim(0.35,2.5)+
 #   ggplot2::scale_x_continuous(
 #     limits = c(130, 235),
 #     breaks = seq(from = 130, to = 235, by = 10))+
 #   scale_color_jama()
 #   
  
   
   
   