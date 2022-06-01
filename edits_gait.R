library(knitr)
library(tidyverse)
library(haven)
library(labelled)
library(ggpubr)
library(gtsummary)
library(foreign)
library(ggsci)
library(grafify)
library(qpcR)
library(ggstatsplot)
library(ggbeeswarm)
library(ggthemes)
library(ggrepel)

## ---- a
theme_gtsummary_journal(journal = "lancet")
theme_gtsummary_compact()
theme_set(theme_minimal())

gh_data <- read.dta("ghana.dta",convert.factors = T)%>% as_tibble()
gh_data$q2507[gh_data$q2507 >990] <- 0
gh_data$q2507[gh_data$q2508 >990] <- 0
gh_data$q1017[is.na(gh_data$q1017)] = 0

gh_data<- gh_data %>%  dplyr::select(q0104,q1009,q1011,q1012,q1016,q1017,q1018,q1019,q1023,
                                     q2000:q2003,q2009,q2025,q2028,
                                     q2032,q2011,q2014,q2015,q2017,q2033,q2035:q2039,q2047,
                                     q2501_s:q2513,q4001,q4003,q4004,
                                     q4008,q4010,q4012,q4014:q4017,q4022,q4025,q4033,q4040,
                                     q4060:q4065,q3001,q3002,q3009a:q3009g, quintile_c) %>% 
  rowwise()%>%
  mutate(avg_sbp = mean(c(q2501_s, q2502_s, q2503_s),na.rm = TRUE),
         avg_dbp = mean(c(q2501_d, q2502_d, q2503_d),na.rm = TRUE),
         avg_p=mean(c(q2501a_p, q2502a_p, q2503a_p),na.rm = TRUE),
         BMI=round(q2507/((q2506/100)^2),digits = 1))%>% 
  mutate(Edu_yrs = cut(q1017,c(0,1,12,30),include.lowest=TRUE),
          quintile_c = factor(x = quintile_c, 
          levels=c("1","2", "3", "4", "5"), labels = c(
          "1st","2nd", "3rd", "4th", "5th")))

gh_data$BMI[gh_data$BMI< 18.5] <- "Underweight"
gh_data$BMI[gh_data$BMI %in% seq(30,122,0.1)] <- "Obese"
gh_data$BMI[gh_data$BMI %in%  seq(18.5,24.9,0.1)] <- "Normal Weight"
gh_data$BMI[gh_data$BMI %in%  seq(25.0,29.9,0.1)] <- "Overweight"
levels(gh_data$Edu_yrs) <- c("0", "1 to 12", "12 and above")

# s<-gh_data[(gh_data$q4001 == "yes" | gh_data$q4001 == "no") & 
#              (gh_data$q4025 == "yes" | gh_data$q4025 == "no"),]

test<-data.frame(rep(c("yes", "no"),times=c(601,4486)))
colnames(test)<-"acld"
gh_data<-qpcR:::cbind.na(gh_data,test) %>% mutate(acld=factor(acld))

t1<-data.frame(rep(c("yes", "no"),times=c(2646,2925)))
colnames(t1)<-"abp"
gh_data<-qpcR:::cbind.na(gh_data,t1) %>% mutate(abp=factor(abp))

t2<-data.frame(rep(c("yes", "no"),times=c(191,4892)))
colnames(t2)<-"stk"
gh_data<-qpcR:::cbind.na(gh_data,t2) %>% mutate(stk=factor(stk))

t3<-data.frame(rep(c("yes", "no"),times=c(1484,4087)))
colnames(t3)<-"ang"
gh_data<-qpcR:::cbind.na(gh_data,t3) %>% mutate(ang=factor(ang))

t4<-data.frame(rep(c("yes", "no"),times=c(5570,0)))
colnames(t4)<-"hp"
gh_data<-qpcR:::cbind.na(gh_data,t4) %>% mutate(hp=factor(hp))

t5<-data.frame(rep(c("yes", "no"),times=c(1484,4087)))
colnames(t5)<-"cat"
gh_data<-qpcR:::cbind.na(gh_data,t5) %>% mutate(cat=factor(cat))

gh_data<-gh_data %>% mutate_at(vars(q3009a:q3009g), ~replace(., is.na(.), 0)) %>% 
mutate_at(vars(q3009a:q3009g), ~replace(., . < 0, 0))
gh_data <- gh_data %>% rowwise()%>%
  mutate(alh = sum(c_across(q3009a:q3009g)), .keep = "unused")

t6<-data.frame(rep(c("yes", "no"),times=c(419,5154)))
colnames(t5)<-"alcohol"
gh_data<-qpcR:::cbind.na(gh_data,t5) %>% mutate(alcohol=factor(alcohol))

#those with symptoms and sBP of at least 180 = 294 for GH and 385 for SA.
#all those who had BP >=180 answered yes to at least one of the hpt questions

#q3001 being maintained for hx of tbc as no one selected no for 3002, even for those who selected 
#  no for 3001.


colnames(gh_data)[colnames(gh_data) %in% c("q0104","q1009","q1011","q1012", "quintile_c")] <- c(
                                          "Residence","Sex","Age", "Marital_Status","Income")


#SA Data
sa_data <- read.dta("sa.dta", convert.factors=T) %>% as_tibble()
sa_data$q2507[sa_data$q2507 >990] <- 0
sa_data$q2507[sa_data$q2508 >990] <- 0
sa_data$q1017[is.na(sa_data$q1017)] <- 0


#Selection of relevant columns and finding averages of systolic and diastolic BP, pulse, from the 3 readings


sa_data <- sa_data %>% dplyr::select(q0104,q1009,q1011,q1012,q1016,q1017,q1018,q1019,q1023,
                                    q2000:q2003,q2009,q2025,q2028,
                                    q2032,q2011,q2014,q2015,q2017,q2033,q2035:q2039,q2047,
                                    q2501_s:q2513,q4001,q4003,q4004,
                                    q4008,q4010,q4012,q4014:q4017,q4022,q4025,q4033,q4040,
                                    q4060:q4065,q3001,q3002,q3009a:q3009g, quintile_c)%>%
  rowwise()%>%
  mutate(avg_sbp = mean(c(q2501_s, q2502_s, q2503_s),na.rm = TRUE),
         avg_dbp = mean(c(q2501_d, q2502_d, q2503_d),na.rm = TRUE),
         avg_p=mean(c(q2501a_p, q2502a_p, q2503a_p),na.rm = TRUE),
         BMI=round(q2507/((q2506/100)^2),digits = 1))%>% 
  mutate(Edu_yrs = cut(q1017,c(0,1,12,30),include.lowest=TRUE),
         quintile_c = factor(x = quintile_c, 
                             levels=c("1","2", "3", "4", "5"), labels = c(
                               "1st","2nd", "3rd", "4th", "5th")))

sa_data$BMI[sa_data$BMI< 18.5] <- "Underweight"
sa_data$BMI[sa_data$BMI %in% seq(30,122,0.1)] <- "Obese"
sa_data$BMI[sa_data$BMI %in%  seq(18.5,24.9,0.1)] <- "Normal Weight"
sa_data$BMI[sa_data$BMI %in%  seq(25.0,29.9,0.1)] <- "Overweight"
levels(sa_data$Edu_yrs) <- c("0", "1 to 12", "12 and above")

test<-data.frame(rep(c("yes", "no"),times=c(236,3790)))
colnames(test)<-"acld"
sa_data<-qpcR:::cbind.na(sa_data,test) %>% mutate(acld=factor(acld))
 
t1<-data.frame(rep(c("yes", "no"),times=c(2146,2077)))
colnames(t1)<-"abp"
sa_data<-qpcR:::cbind.na(sa_data,t1) %>% mutate(abp=factor(abp))
 
t2<-data.frame(rep(c("yes", "no"),times=c(211,3808)))
colnames(t2)<-"stk"
sa_data<-qpcR:::cbind.na(sa_data,t2) %>% mutate(stk=factor(stk))

t3<-data.frame(rep(c("yes", "no"),times=c(811,3411)))
colnames(t3)<-"ang"
sa_data<-qpcR:::cbind.na(sa_data,t3) %>% mutate(ang=factor(ang))
 
t4<-data.frame(rep(c("yes", "no"),times=c(4268,0)))
colnames(t4)<-"hp"
sa_data<-qpcR:::cbind.na(sa_data,t4) %>% mutate(hp=factor(hp))
 
t5<-data.frame(rep(c("yes", "no"),times=c(1367,2696)))
colnames(t5)<-"cat"
sa_data<-qpcR:::cbind.na(sa_data,t5) %>% mutate(cat=factor(cat))

sa_data<-sa_data %>% mutate_at(vars(q3009a:q3009g), ~replace(., is.na(.), 0)) %>% 
  mutate_at(vars(q3009a:q3009g), ~replace(., . < 0, 0))
sa_data <- sa_data %>% rowwise()%>%
  mutate(alh = sum(c_across(q3009a:q3009g)), .keep = "unused")

t6<-data.frame(rep(c("yes", "no"),times=c(419,5154)))
colnames(t5)<-"alcohol"
sa_data<-qpcR:::cbind.na(sa_data,t5) %>% mutate(alcohol=factor(alcohol))
 
colnames(sa_data)[colnames(sa_data) %in% c("q0104","q1009","q1011","q1012", "quintile_c")] <- c(
  "Residence","Sex","Age", "Marital_Status","Income")

# Combined demographics

dem <-gh_data %>% filter(Age>49) %>% 
  mutate(Age_Groups = cut(Age,c(50,59,69,79,140),include.lowest=TRUE)) %>% 
  dplyr::select(Age_Groups,Age,Sex,Edu_yrs,Residence,Marital_Status,Income) %>%  
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>% filter(Age>49) %>% 
      mutate(Age_Groups = cut(Age,c(50,59,69,79,140),include.lowest=TRUE)) %>%
      dplyr::select(Age_Groups,Age,Sex,Edu_yrs,Residence,Marital_Status,Income) %>% 
      mutate(ctry="South Africa")
  ) 

dem<-dem %>% mutate(mst = as.character(Marital_Status))

dem$mst[dem$mst == "cohabiting"] <- "cohabiting/currently married"
dem$mst[dem$mst == "currently married"] <- "cohabiting/currently married"
levels(dem$Age_Groups) <- c("50 to 59", "60 to 69", "70 to 79", "80 and above")

dem %>% dplyr::select(-Marital_Status) %>% 
  mutate(Residence=droplevels(Residence))%>% 
  filter_at(vars(-Edu_yrs),all_vars(!is.na(.))) %>% 
  tbl_summary(by=ctry, label=list(Age_Groups ~"Age Groups (in years)",
                                  Age ~"Age (in years)",
                                  Edu_yrs ~"Years of Education",
                                  Income ~"Income (in quintiles)",
                                  mst ~"Marital Status")) %>% bold_labels() %>% 
modify_caption("**Table 1. Overview of Socio-Demographic Characteristics**")

com <- gh_data %>% filter(Age>49) %>% 
  dplyr::select(Age,Sex,alcohol,q3001,q4040,q4022,acld,abp,
                stk,ang,hp,cat,alcohol) %>% 
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>% filter(Age>49) %>%
      dplyr::select(Age,Sex,alcohol,q3001,q4040,q4022,acld,
                    abp,stk,ang,hp,cat,alcohol)%>% 
      mutate(ctry="South Africa")
  ) 


## ---- com
com<-com %>%mutate_at(vars(starts_with("q")),all_vars(as.character(.)))
com$q4022[com$q4022 == "don't know"] <- NA
com$q4040[com$q4040 == "don't know"] <- NA
com$q3001[com$q3001 == "don't know"] <- NA

com %>% filter_all(all_vars(!is.na(.))) %>% 
  dplyr::select(-Sex, -Age) %>% 
  tbl_summary(by=ctry,type=all_categorical()~"categorical",label=
              list(ang ~"History of Angina",
                    acld ~"History of Asthma and Chronic Lung Disease",
                    abp ~ "History of Arthritis and Musuloskeletal Pains",
                    stk ~"History of Stroke",
                    q4022 ~"History of Diabetes Mellitus",
                    q4040 ~"History of Depression",
                    hp ~"History of Hypertension",
                    cat ~ "History of Cataracts",
                    q3001 ~"History of Tobacco use",
                    alcohol ~ "History of Alcohol use")) %>% bold_labels() %>% 
  modify_caption("**Table 2. Overview of Relevant Comorbidities**")

# Dealing with gait speed outliers

sa_data <-sa_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>%
  mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
gh_data <-gh_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>%
  mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)

gh_data_c <-gh_data%>% filter(q2511 < 12.1, q2513 >1.1, q2513 <13.1)%>%
  rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
gh_data$norm_gs[gh_data$q2511 >12.0] <- 0.7861
gh_data$rap_gs[gh_data$q2513 >13.0] <- 1.1840
gh_data$rap_gs[gh_data$q2513 < 1.2] <- 1.1840
gh_data <- dplyr::select(gh_data, -q2511, -q2513)
rm(gh_data_c)

sa_data_c <-sa_data%>% filter(q2511 < 12.1, q2513 <13.1)%>%rowwise()%>% 
  mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
sa_data$norm_gs[sa_data$q2511 >12.0] <- 0.8486
sa_data$rap_gs[sa_data$q2513 >13.0] <- 1.3180
sa_data <- dplyr::select(sa_data, -q2511, -q2513)
rm(sa_data_c)


## ---- b
gh_data<-gh_data %>% mutate(mst = as.character(Marital_Status))
gh_data$mst[gh_data$mst == "cohabiting"] <- "cohabiting/currently married"
gh_data$mst[gh_data$mst == "currently married"] <- "cohabiting/currently married"

sa_data<-sa_data %>% mutate(mst = as.character(Marital_Status))
sa_data$mst[sa_data$mst == "cohabiting"] <- "cohabiting/currently married"
sa_data$mst[sa_data$mst == "currently married"] <- "cohabiting/currently married"

cd<-gh_data %>% 
  mutate(Age_Groups = cut(Age,c(18,50,59,69,79,140),include.lowest=TRUE)) %>% 
  dplyr::select(Age_Groups,Age,Sex,Edu_yrs,Residence,mst,Income,norm_gs,rap_gs) %>%  
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>% 
      mutate(Age_Groups = cut(Age,c(18,50,59,69,79,140),include.lowest=TRUE)) %>%
      dplyr::select(Age_Groups,Age,Sex,Edu_yrs,Residence,mst,Income,norm_gs,rap_gs) %>% 
      mutate(ctry="South Africa")
  ) 
levels(cd$Age_Groups) <-c("18 to 49","50 to 59", "60 to 69", "70 to 79", "80 and above")

fcd<-cd %>% filter(Age>49) %>% filter_all(all_vars(!is.na(.))) %>% 
  mutate(Residence=droplevels(Residence),mst=as.factor(mst),
         Age_Groups=cut(Age,c(50,59,69,79,140),include.lowest=TRUE)) 

levels(fcd$Age_Groups) <-c("50 to 59", "60 to 69", "70 to 79", "80 and above")
  
fcd %>% dplyr::select(-rap_gs) %>% 
  tbl_strata(
    strata = ctry,
    ~.x %>%
      tbl_summary(
        by = Sex,
        type = where(is.numeric) ~ "continuous",
        label=list(norm_gs ~"Normal Gait Speed",
                   Age_Groups ~"Age Groups (in years)",
                   Age ~"Age (in years)",
                   Edu_yrs ~"Years of Education",
                   Income ~"Income (in quintiles)",
                   mst ~"Marital Status")
      ) %>%
      modify_header(all_stat_cols() ~ "**{level}**") %>% 
      bold_labels() %>% add_p()
  )


## ---- c
com %>% dplyr::select(-Age) %>%  filter_all(all_vars(!is.na(.))) %>% 
  tbl_strata(
    strata = ctry,
    ~.x %>%
      tbl_summary(
        by = Sex,
        type = all_categorical()~"categorical",
        label=list(ang ~"History of Angina",
                   acld ~"History of Asthma and Chronic Lung Disease",
                   abp ~ "History of Arthritis and Musuloskeletal Pains",
                   stk ~"History of Stroke",
                   q4022 ~"History of Diabetes Mellitus",
                   q4040 ~"History of Depression",
                   hp ~"History of Hypertension",
                   cat ~ "History of Cataracts",
                   q3001 ~"History of Tobacco use",
                   alcohol ~ "History of Alcohol use")
      ) %>%
      modify_header(all_stat_cols() ~ "**{level}**") %>% 
      bold_labels() %>% add_p()
  )

## ---- d

 cd %>% 
   dplyr::select(ctry,Age_Groups,norm_gs,Edu_yrs,mst,Residence,Sex,Income) %>% 
   filter_all(all_vars(. != "Unknown")) %>% 
   filter_all(all_vars(. != "don't know")) %>% 
   filter_all(all_vars(. != "not applicable")) %>% 
   mutate_at(vars(!contains("norm_gs")), as.character) %>% 
   tbl_strata(strata=ctry, .tbl_fun = ~.x %>% 
    tbl_uvregression(method = lm,y=norm_gs,
                  label=list(
                  Age_Groups ~"Age Groups (in years)",
                  Edu_yrs ~"Years of Education",
                  Income ~"Income (in quintiles)",
                  mst ~"Marital Status")
                  ) %>%  
      modify_caption("**Table 5. Multiple Regression:Gait Speed vs Demographic Variables**")%>%
      bold_labels())

  #datasummary( Age_Groups+Edu_yrs+Residence+Marital_Status+Sex+Income ~ ctry * `norm_gs` * (Mean + SD), data=t)
 
 
## ---- e
cr <- gh_data %>% 
  dplyr::select(norm_gs,alcohol,q3001,q4040,q4022,acld,abp,stk,ang,hp,cat,alcohol) %>%  
  mutate(
    ctry = "Ghana"
   ) %>% 
  union_all(
    sa_data  %>% 
      dplyr::select(norm_gs,alcohol,q3001,q4040,q4022,acld,abp,stk,ang,hp,cat,alcohol)%>% 
      mutate(ctry="South Africa")
   ) 

cr %>% dplyr::select(-hp) %>% 
  filter_all(all_vars(. != "Unknown")) %>% 
  filter_all(all_vars(. != "don't know")) %>% 
  filter_all(all_vars(. != "not applicable")) %>% 
  mutate_at(vars(!contains("norm_gs")), as.character) %>% 
  tbl_strata(strata=ctry, .tbl_fun = ~.x %>% 
    tbl_uvregression(method = lm,y=norm_gs,
    label=list(ang ~"History of Angina",
              acld ~"History of Asthma and Chronic Lung Disease",
              abp ~ "History of Arthritis and Musuloskeletal Pains",
              stk ~"History of Stroke",
              q4022 ~"History of Diabetes Mellitus",
              q4040 ~"History of Depression",
              cat ~ "History of Cataracts",
              q3001 ~"History of Tobacco use",
              alcohol ~ "History of Alcohol use")
                    ) %>% 
modify_caption("**Table 6. Multiple Regression: Gait Speed vs Health-Related Variables**")%>%
  bold_labels())
 
# ## ---- m
#      whodas <-gh_data %>%
#      select(Age,q2011,q2014,q2015,q2028,q2032,q2033, q2035,q2036, q2037, q2038,q2039,q2047,norm_gs) %>%
#      mutate(
#        ctry = "Ghana"
#      ) %>% 
#      union_all(
#        sa_data  %>%  
#          select(Age,q2011,q2014,q2015,q2028,q2032,q2033, q2035,q2036, q2037, q2038,q2039,q2047,norm_gs) %>%
#          mutate(ctry="South Africa"))
#      
#      who<- list( q2039~ "in your day to day work?",
#                  q2014 ~ "with making new friendships or
#                       maintaining current friendships?",
#                  q2015~"with dealing with strangers?",
#                  q2011 ~"did you have in learning a new task?",
#                  q2032 ~"in taking care of your household
#                         responsibilities?",
#                  q2033 ~"in joining in community activities? ",
#                  q2035 ~"concentrating on doing something for
#                         10 minutes?",
#                  q2036 ~"in walking a long distance such as a
#                         kilometer?",
#                  q2028 ~ "in standing for long periods (such as 30 minutes)?",
#                  q2037~"in bathing/washing your whole body?",
#                  q2038~"in getting dressed?",
#                  q2047 ~"In the last 30 days, how much have you been emotionally
#                         affected by your health condition(s)?")
#    
#      wd<- whodas %>% filter_at(vars(contains("q")), all_vars(. != "don't know")) %>% 
#        filter_at(vars(contains("q")),
#                  all_vars(. != "not applicable"))
#        
#      
#      wd%>%select(-norm_gs,-Age)%>%tbl_summary(by=ctry, 
#                                                label = who) %>% bold_labels()
#    
# ## ---- n
# wd <- wd %>% mutate_at(vars(contains("q")), as.character)  
# wd[wd=="none"] <- as.character(0)
# wd[wd=="mild"] <- as.character(1)
# wd[wd=="moderate"] <- as.character(2)
# wd[wd=="severe"] <- as.character(3)
# wd[wd=="extreme"] <- as.character(4)
# 
# wd <- wd %>% mutate_at(vars(contains("q")), as.numeric) %>% rowwise()%>%
#   mutate(Score = sum(q2011:q2047,na.rm = TRUE)) 
# 
# wd%>% select(Score,ctry) %>% tbl_summary(by=ctry,type = list(Score ~ "categorical")) %>% 
#   bold_labels()
# 
# wd %>% select(norm_gs,Score,ctry) %>%tbl_strata(strata=ctry, .tbl_fun = ~.x %>% 
#                                                   tbl_continuous(variable = norm_gs)) %>% 
#   bold_labels()
# 
# wd<- wd %>% mutate(Score= factor(Score, 
#                                     levels=c("0","1","2","3","4","5", "6", "7", "9", "10"),
#                                     ordered=TRUE))
# 
# ## ----wg  
# wd %>% group_by(Age,Score) %>% summarise(med=median(norm_gs)) %>% 
#   ggplot(aes(x = Age, y = med, col = Score)) +
#   geom_point(size = 1.5) + geom_smooth(method = "lm",se=FALSE)+
#   scale_color_grafify(palette="muted") +
#   xlim (50, 124) + ylim(0.3,1.3) +
#   labs(title = "WHODAS Score vs Age vs Gait Speed",
#        y="Median Gait Speed", x= "Age (in years)", color="WHODAS Score")

## ---- g

cd <- cd %>%filter(Age>49) %>% 
  mutate(Ages = cut(Age, seq(50,140,5),include.lowest=TRUE))

levels(cd$Ages) <- c("50 to 55", "56 to 60 ","61 to 65","66 to 70","71 to 75",
                     "76 to 80","81 to 85","86 to 90",
                     "91 to 95","96 to 100","101 to 105","106 to 110","111 to 115",
                     "101 to 105","106 to 110","111 to 115","116 to 120",
                     "121 to 125", "126 to 130","131 to 135","136 to 140")
cd%>%
  group_by(ctry,Sex,Ages)%>% 
  summarise(mean_gs= median(na.omit(norm_gs))) %>% 
  ggplot(aes(x=Ages,y=mean_gs,color=Sex,
             linetype=ctry, 
             group=interaction(Sex,ctry)))+ 
  geom_point(size=2)+ geom_smooth(method="lm",se=F)+
  labs(title = "Trend of Gait Speed with Age", 
       y ="Median Gait Speed", x="Age (in years)")+scale_colour_jama()

 
## ---- h
cd %>% filter(Age >= 75) %>% 
ggplot(aes(x=Age,y=norm_gs)) + 
    geom_point(color = "gray40", alpha = .5) + 
  geom_smooth(method = lm,color="black", fill= "firebrick")+
  ylim(0,1.5)+
    labs(title = "Gait Speed in the Older Olds (Age 75 and above)",y="Median Gait Speed", x="Age (in years)")
  
## ---- i
  
 a<- cd %>% filter(Age>49) %>% na.omit(ctry) %>%
   group_by(ctry,Sex) %>% 
   summarise(mean_gs= round(median(na.omit(norm_gs)),digits = 2)) %>% 
    ggplot(aes(x = ctry,y=mean_gs, col = Sex,label=mean_gs)) +
    geom_linerange(aes(x = ctry, ymin = 0.1, ymax = mean_gs, colour = Sex), 
                   position = position_dodge(width = 1),size=2)+
    geom_point(position=position_dodge(width=1),size=6)+ 
   geom_text_repel(
     size= 2.5,
     force = 0.5,
     nudge_x = 0,
     direction= "y",
     hjust = -0.75,
     segment.size = 0.2)+
    labs( title= "Normal Gait Speed", y="Median Gait Speed", x= "Country")+
   ylim(0.1,1.5)+
scale_colour_jama()+
   coord_flip()
 
 b<- cd %>% filter(Age>49) %>% na.omit(ctry) %>% 
   group_by(ctry,Sex) %>% 
   summarise(mean_gs= round(median(na.omit(rap_gs)),digits = 2)) %>% 
   ggplot(aes(x = ctry,y=mean_gs, col = Sex,label=mean_gs)) +
   geom_linerange(aes(x = ctry, ymin = 0.1, ymax = mean_gs, colour = Sex), 
                  position = position_dodge(width = 1),size=2)+
   geom_point(position=position_dodge(width=1),size=6)+ 
   geom_text_repel(
     size= 2.5,
     force = 0.5,
     nudge_x = 0,
     direction= "y",
     hjust = -0.75,
     segment.size = 0.2)+
   labs(title= "Rapid Gait Speed", y="Median Gait Speed", x= "Country")+
   scale_colour_uchicago()+
   ylim(0.1,1.5)+
   coord_flip()
 ggarrange(a,b,ncol=1,nrow=2) %>% annotate_figure(top = "Gender vs Gait Speed")


   # pv <-list(q2009 ~"Difficulty due to pain",
   #           
   #           q2017 ~"Difficulty due to lack of energy",
   #           
   #           q2000 ~"Health Rating",
   #           q2001 ~"Difficulty with activities")
   # 
  
## ---- ps  
 cp <-gh_data %>% filter(Age>49) %>% 
   dplyr::select(q2000,q2001,q2009,q2017,norm_gs)%>%  
   mutate(
     ctry = "Ghana"
   ) %>% 
   union_all(
     sa_data  %>% filter(Age>49) %>% 
       dplyr::select(q2000,q2001,q2009,q2017,norm_gs) %>% 
       mutate(ctry="South Africa"))
   
   cp %>% dplyr::select(q2000,ctry,norm_gs) %>% 
     filter(q2000 != "don't know") %>% 
     ggplot(aes(x=q2000,y=norm_gs,color=q2000))+ 
     stat_compare_means(label.x = 0.75,
                        label.y=0.15)+
     geom_quasirandom(alpha = 0.7,
                      size = 1.5) + ylim(0,1.5)+
     theme_economist()+ 
     geom_boxplot()+
     theme(legend.position = "none")+
     scale_color_economist(name=NULL)+
     labs(title = "Health Rating vs Gait Speed", y="Normal Gait Speed",
          x= "Health Rating")
