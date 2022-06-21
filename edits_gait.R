library(knitr)
library(haven)
library(labelled)
library(gtsummary)
library(foreign)
library(ggsci)
library(grafify)
library(qpcR)
library(ggpubr)
library(ggbeeswarm)
library(ggthemes)
library(ggrepel)
library(gt)
library(tidyverse)

## ---- a
theme_gtsummary_journal(journal = "lancet")
theme_gtsummary_compact()
theme_set(theme_minimal())

gh_data <- read.dta("ghana.dta",convert.factors = T)%>% as_tibble()
gh_data$q2507[gh_data$q2507 >990] <- 0
gh_data$q2507[gh_data$q2508 >990] <- 0
gh_data$q1017[is.na(gh_data$q1017)] = 0

gh_data<- gh_data %>%  dplyr::select(q0104,q1009,q1011,q1012,q1016:q1019,q1023,
                                     q2000:q2003,q2009,q2025,q2028,
                                     q2032,q2011,q2014,q2015,q2017,q2033,q2035:q2039,
                                     q2042,q2044,q2047,
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

#"Yes"means diagnosed of hpt, or average bp was found to be at least 180
t4<-data.frame(rep(c("yes", "no"),times=c(811,4759)))
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
sa_data <- sa_data %>% dplyr::select(q0104,q1009,q1011,q1012,q1016:q1019,q1023,
                                    q2000:q2003,q2009,q2025,q2028,
                                    q2032,q2011,q2014,q2015,q2017,q2033,q2035:q2039,
                                    q2042,q2044,q2047,
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
 
t4<-data.frame(rep(c("yes", "no"),times=c(1393,2875)))
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

#Finding those who could not walk at all.
no_gs <- gh_data %>%  filter(q2511==0) %>% 
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>% 
      filter(q2511==0) %>% 
      mutate(ctry="South Africa") #only 2 entries and all from SA had 0 gait speed
  )
# %>% rowwise() %>% 
#   mutate(gs = 4/q2511, Ages = cut(Age, seq(50,140,5),include.lowest=TRUE))

# Dealing with gait speed outliers
sa_data <-sa_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>%
  mutate(norm_gs = 4/q2511, rap_gs = 4/q2513) #entries reduced to 3666 now (2 with 0 gait speed, the rest are NAs)
gh_data <-gh_data%>% filter(q2511 > 0, q2513 >0)%>% rowwise()%>%
  mutate(norm_gs = 4/q2511, rap_gs = 4/q2513) #entries reduced to 4835 now (all NAs)

## ---- aig
#Gait Speed of 0.1m/s(q2511>=40) is used as the cut-off for impaired gait speed in most studies
abn_gs <-gh_data %>% dplyr::select(Age,q2511) %>% filter(Age>49,q2511>=40) %>% 
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>% 
      dplyr::select(Age,q2511) %>% filter(Age>49,q2511>=40) %>% 
      mutate(ctry="South Africa")
  ) %>% rowwise() %>% 
  mutate(gs = 4/q2511, Ages = cut(Age, seq(50,140,5),include.lowest=TRUE))
levels(abn_gs$Ages) <- c("50 to 55", "56 to 60 ","61 to 65","66 to 70","71 to 75",
                         "76 to 80","81 to 85","86 to 90",
                         "91 to 95","96 to 100","101 to 105","106 to 110","111 to 115",
                         "101 to 105","106 to 110","111 to 115","116 to 120",
                         "121 to 125", "126 to 130","131 to 135","136 to 140")

abn_gs %>% 
  ggplot(aes(x=Ages,y=gs,col=ctry))+
  geom_point(position=position_dodge(width=.5))+ 
  geom_line(position=position_dodge(width=.5))+
 scale_color_jama() + #coord_flip()+
  labs(title="Age Ranges vs Median Gait Speed amongst those with Impaired Gait Speed",
       x= "Age Groups", y="Median Gait Speed",col="Country")

# gh_data_c <-gh_data%>% filter(q2511 < 40, q2513 <40)%>%
#   rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513) #4775 have both gait speeds above 0.1m/s

# Finding the 25th and 75th percentile values and dealing with outliers

gh_data$norm_gs[gh_data$norm_gs < 0.56338029] <- 0.56338029
gh_data$rap_gs[gh_data$rap_gs < 0.88888889] <- 0.88888889
gh_data$norm_gs[gh_data$norm_gs > 0.93023252] <- 0.93023252
gh_data$rap_gs[gh_data$rap_gs > 1.37931030] <- 1.37931030

sa_data$norm_gs[sa_data$norm_gs < 0.54794519] <- 0.54794519
sa_data$rap_gs[sa_data$rap_gs < 0.80000000] <- 0.80000000
sa_data$norm_gs[sa_data$norm_gs > 1.00000000] <- 1.00000000
sa_data$rap_gs[sa_data$rap_gs > 1.33333333] <- 1.33333333

# gh_data <- dplyr::select(gh_data, -q2511, -q2513)
# rm(gh_data_c)

# sa_data_c <-sa_data%>% filter(q2511 < 40, q2513 <40)%>%
#   rowwise()%>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513) #3621 have both gait speeds above 0.1m/s
# sa_data$norm_gs[sa_data$q2511 >= 40.0] <- 0.8353
# sa_data$rap_gs[sa_data$q2513 >= 40.0] <- 1.2986
# sa_data <- dplyr::select(sa_data, -q2511, -q2513)
# rm(sa_data_c)


## ---- asa
egs<-sa_data %>% dplyr::select(Age,q1018,norm_gs,rap_gs,Income) %>% 
  filter(Age>49,q1018!= "Don't know") %>% 
mutate(
    Ages = cut(Age, seq(50,140,5),include.lowest=TRUE)
  )
levels(egs$Ages) <- c("50 to 55", "56 to 60 ","61 to 65","66 to 70","71 to 75",
                      "76 to 80","81 to 85","86 to 90",
                      "91 to 95","96 to 100","101 to 105","106 to 110","111 to 115",
                      "101 to 105","106 to 110","111 to 115","116 to 120",
                      "121 to 125", "126 to 130","131 to 135","136 to 140")

egs%>% 
  filter(!is.na(q1018))%>% group_by(q1018) %>% 
  summarize(mgs = round(median(norm_gs),digits=2),rgs = round(median(rap_gs),digits=2)) %>% 
  gt() %>% 
  cols_label(
    q1018 = md("**Ethnicity**"),
    mgs = md("**Median Normal Gait Speed**"),
    rgs = md("**Median Rapid Gait Speed**")
  ) %>% tab_header(title=md("**Ethnicity in South Africa vs.Gait Speed**"))

egs %>% filter(q1018 != "Other") %>% filter(!is.na(q1018))%>% group_by(q1018,Ages)%>%
  summarize(mgs = round(median(norm_gs),digits=2),rgs = round(median(rap_gs),digits=2))%>% 
ggplot(aes(x=mgs, y=Ages,fill=q1018))+geom_col(position="dodge")+
  scale_fill_jama()+
  labs(title="Age Ranges vs Ethnicity vs Median Gait Speed in South Africa",
       y= "Age Groups", x="Median Gait Speed",fill="Ethnicity")
  
## ---- ieg
#Results show that the gait speed is not related to income.
egs %>% filter(q1018 != "Other") %>% filter(!is.na(Income)) %>% group_by(q1018,Income)%>%
  summarize(mgs = round(median(norm_gs),digits=2),rgs = round(median(rap_gs),digits=2))%>% 
  ggplot(aes(x=mgs, y=q1018,fill=Income))+geom_col(position="dodge")+
  scale_fill_jama()+
  labs(title="Income Quintiles vs Ethnicity vs Median Gait Speed in South Africa",
       x= "Median Gait Speed", y="Ethnicity",fill="Income Quintile")

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
  
fcd %>%  
  tbl_strata(
    strata = ctry,
    ~.x %>%
      tbl_summary(
        by = Sex,
        type = where(is.numeric) ~ "continuous",
        label=list(norm_gs ~"Normal Gait Speed",
                   rap_gs ~"Rapid Gait Speed",
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
 
## ---- n
     whodas <-gh_data %>%
     dplyr::select(Age,Sex,q2506, q2507,BMI,q2011,q2014,q2015,q2028,q2032,q2033, q2035,q2036, 
                   q2037, q2038,q2039,q2042,q2044,q2047,norm_gs) %>%
     mutate(
       ctry = "Ghana"
     ) %>%
     union_all(
       sa_data  %>%
         dplyr::select(Age,Sex,q2506, q2507,BMI,q2011,q2014,q2015,q2028,q2032,q2033, q2035,q2036, 
                       q2037, q2038,q2039,q2042,q2044,q2047,norm_gs) %>%
         mutate(ctry="South Africa"))%>% 
       filter_at(vars(contains("q")), all_vars(. != "don't know")) %>%
       filter_at(vars(contains("q")),
                 all_vars(. != "not applicable"))

     # who<- list(q2506~ "Height",
     #            q2507~ "Weight",
     #            q2039~ "in your day to day work?",
     #             q2014 ~ "with making new friendships or
     #                  maintaining current friendships?",
     #             q2015~"with dealing with strangers?",
     #             q2011 ~"did you have in learning a new task?",
     #             q2032 ~"in taking care of your household
     #                    responsibilities?",
     #             q2033 ~"in joining in community activities? ",
     #             q2035 ~"concentrating on doing something for
     #                    10 minutes?",
     #             q2036 ~"in walking a long distance such as a
     #                    kilometer?",
     #             q2028 ~ "in standing for long periods (such as 30 minutes)?",
     #             q2037~"in bathing/washing your whole body?",
     #             q2038~"in getting dressed?",
     #             q2047 ~"In the last 30 days, how much have you been emotionally
     #                    affected by your health condition(s)?")


#Height,BMI,ADLs and WHODAS in one table.
whodas <- whodas %>% filter(Age>49) %>%  mutate_at(vars(contains("q")), as.character)
whodas[whodas=="none"] <- as.character(0)
whodas[whodas=="mild"] <- as.character(1)
whodas[whodas=="moderate"] <- as.character(2)
whodas[whodas=="severe"] <- as.character(3)
whodas[whodas=="extreme"] <- as.character(4)

whodas <- whodas %>% mutate_at(vars(contains("q")), as.numeric)
#Rearranging the columns to place the 3 q-variables dealing with ADLS at the end
whodas<-whodas[,c(1:14,16,19,15,17,18,20:21)]
whodas<-mutate(whodas, heightm =round(q2506/100,digits=2))
whodas$Score<-rowSums(whodas[6:17],na.rm = TRUE)
whodas$aScore<-rowSums(whodas[17:19],na.rm = TRUE)

#WHODAS being converted into percentages and finding the quartiles to find cut-off points
whodas<-whodas %>% mutate(wp= round((Score/48)*100,digits=2))
#quantile(whodas$wp)
whodas$wp[whodas$wp<2.08] <- "none/mild"
whodas$wp[whodas$wp %in% seq(2.08,27.08,0.01)] <- "moderate"
whodas$wp[whodas$wp %in% seq(27.09,100.00,0.01)] <- "severe"

#quantile(whodas$heightm): finding quartiles to find cut-off points for the heights
whodas$heightm[whodas$heightm<1.54] <- "below average"
whodas$heightm[whodas$heightm %in% seq(1.54,1.67,0.01)] <- "average"
whodas$heightm[whodas$heightm %in% seq(1.68,9.99,0.01)] <- "above average"

#ADLs break it into the three questions and dichotomize each
whodas$q2038[whodas$q2038==0] <- "none"
whodas$q2042[whodas$q2042==0] <- "none"
whodas$q2044[whodas$q2044==0] <- "none"

whodas$q2038[whodas$q2038 %in% seq(1,5,1) ] <- "moderate"
whodas$q2042[whodas$q2042 %in% seq(1,5,1)] <- "moderate"
whodas$q2044[whodas$q2044 %in% seq(1,5,1)] <- "moderate"

whodas$q2038[whodas$q2038 %in% seq(6,12,1) ] <- "severe"
whodas$q2042[whodas$q2042 %in% seq(6,12,1)] <- "severe"
whodas$q2044[whodas$q2044 %in% seq(6,12,1)] <- "severe"

#i can stratify the table below according to gender

whodas %>% dplyr::select(heightm,BMI,wp,q2038,q2042,q2044,norm_gs,Sex) %>% 
  filter(BMI %in% c("Normal Weight","Obese", "Overweight","Underweight")) %>% #to account for some outliers
  mutate(BMI = factor(BMI, levels = c("Normal Weight","Underweight", "Overweight","Obese")),
         wp=factor(wp, levels = c("none/mild","moderate","severe")),
         q2038= factor(q2038, levels = c("none", "moderate")),
         q2042= factor(q2042, levels = c("none", "moderate")),
         q2044= factor(q2044, levels = c("none", "moderate"))
  ) %>% 
  tbl_strata(strata = Sex,
             ~.x %>%
               tbl_continuous(
                 variable = norm_gs,
                 statistic=list(norm_gs ~ "{median}"),
                 label=list(norm_gs ~ "Gait Speed",
                            heightm ~ "height (in metres)",
                            wp ~"WHODAS score",
                            q2038 ~"Difficulty in getting dressed",
                            q2042 ~"Difficulty with eating",
                            q2044 ~ "Difficulty with getting to 
                            and using the toilet"))%>%  
               modify_caption("**Table 7. Median Gait Speed According to BMI,WHODAS and ADLs**")%>%
               bold_labels())

## ---- g
cd <- cd %>%filter(Age>49) %>% 
  mutate(Ages = cut(Age, seq(50,140,5),include.lowest=TRUE))

levels(cd$Ages) <- c("50", "56","61","66","71",
                     "76","81","86","91","96","101","106","111",
                     "116","121","126","131","136")


cd%>% filter(Age < 111) %>% 
  group_by(ctry,Sex,Ages)%>% 
  summarise(mean_gs= median(na.omit(norm_gs))) %>% 
  ggplot(aes(x=Ages,y=mean_gs,color=Sex,
             linetype=ctry, 
             group=interaction(Sex,ctry)))+ 
  geom_point(size=2)+ geom_smooth(method="lm",se=F)+
  labs(title = "Trend of Gait Speed with Age", 
       y ="Median Gait Speed", x="Age (in years)")+scale_colour_jama()+
  scale_linetype_discrete(name="Country")

## ---- h
cd %>% filter(Age >= 75) %>% 
ggplot(aes(x=Age,y=norm_gs)) + 
    geom_point(color = "gray40", alpha = .5) + 
  geom_smooth(method = lm,color="black", fill= "firebrick")+
  ylim(0,1.5)+
    labs(title = "Gait Speed in the Older Olds (Age 75 and above)",y="Median Gait Speed", x="Age (in years)")
  
## ---- i
#  a<- cd %>% filter(Age>49) %>% na.omit(ctry) %>%
#    group_by(ctry,Sex) %>% 
#    summarise(mean_gs= round(median(na.omit(norm_gs)),digits = 2)) %>% 
#     ggplot(aes(x = ctry,y=mean_gs, col = Sex,label=mean_gs)) +
#     geom_linerange(aes(x = ctry, ymin = 0.1, ymax = mean_gs, colour = Sex), 
#                    position = position_dodge(width = 1),size=2)+
#     geom_point(position=position_dodge(width=1),size=6)+ 
#    geom_text_repel(
#      size= 2.5,
#      force = 0.5,
#      nudge_x = 0,
#      direction= "y",
#      hjust = -0.75,
#      segment.size = 0.2)+
#     labs( title= "Normal Gait Speed", y="Median Gait Speed", x= "Country")+
#    ylim(0.1,1.5)+
# scale_colour_jama()+
#    coord_flip()
 
 # b<- cd %>% filter(Age>49) %>% na.omit(ctry) %>% 
 #   group_by(ctry,Sex) %>% 
 #   summarise(mean_gs= round(median(na.omit(rap_gs)),digits = 2)) %>% 
 #   ggplot(aes(x = ctry,y=mean_gs, col = Sex,label=mean_gs)) +
 #   geom_linerange(aes(x = ctry, ymin = 0.1, ymax = mean_gs, colour = Sex), 
 #                  position = position_dodge(width = 1),size=2)+
 #   geom_point(position=position_dodge(width=1),size=6)+ 
 #   geom_text_repel(
 #     size= 2.5,
 #     force = 0.5,
 #     nudge_x = 0,
 #     direction= "y",
 #     hjust = -0.75,
 #     segment.size = 0.2)+
 #   labs(title= "Rapid Gait Speed", y="Median Gait Speed", x= "Country")+
 #   scale_colour_uchicago()+
 #   ylim(0.1,1.5)+
 #   coord_flip()
 # ggarrange(a,b,ncol=1,nrow=2) %>% annotate_figure(top = "Gender vs Gait Speed")
  
## ---- ps  
 cp <-gh_data %>% filter(Age>49) %>% 
   dplyr::select(Age,q2000,q2001,q2009,q2017,norm_gs)%>%  
   mutate(
     ctry = "Ghana"
   ) %>% 
   union_all(
     sa_data  %>% filter(Age>49) %>% 
       dplyr::select(Age,q2000,q2001,q2009,q2017,norm_gs) %>% 
       mutate(ctry="South Africa"))
   
   cp %>% dplyr::select(q2000,ctry,norm_gs) %>% 
     filter(q2000 != "don't know") %>% 
     ggplot(aes(x=q2000,y=norm_gs,color=q2000))+ 
     stat_compare_means(label.x = 0.75, #stat_compare_means is from ggpubr
                        label.y=0.15)+
     geom_quasirandom(alpha = 0.7,
                      size = 1.5) + ylim(0,1.5)+
     theme_economist()+ 
     geom_boxplot()+
     theme(legend.position = "none")+
     scale_color_economist(name=NULL)+
     labs(title = "Health Rating vs Gait Speed", y="Normal Gait Speed",
          x= "Health Rating")
   
## ---- pe
#filtering out Age 100 and below as beyond this range not more than 3 observations
#are seen and also tend to be in one country alone, thus not significant. 
   
cp<-cp %>% filter(Age <=100,
                            q2017!= "don't know") %>% 
mutate(Ages = cut(Age, seq(50,100,5),include.lowest=TRUE))
 
levels(cp$Ages) <- c("50 to 55", "56 to 60 ","61 to 65","66 to 70","71 to 75",
                        "76 to 80","81 to 85","86 to 90",
                        "91 to 95","96 to 100")
cp%>%
dplyr::select(q2017,Ages,norm_gs) %>%
tbl_strata(
 strata = q2017,
 ~.x %>%
 tbl_continuous(
 variable = norm_gs,
 statistic=list(norm_gs ~ "{median}"),
 label=list(norm_gs ~ "Gait Speed"))%>%  
 modify_caption("**Table 9. Median Gait Speed According to Age and Exhaustion Severity**")%>%
   bold_labels())

cp%>%filter(q2009 != "don't know",q2009 != "not applicable") %>%
  drop_na() %>% 
dplyr::select(q2009,Ages,norm_gs) %>%
     tbl_strata(
       strata = q2009,
       ~.x %>%
         tbl_continuous(
           variable = norm_gs,
           statistic=list(norm_gs ~ "{median}"),
           label=list(norm_gs ~ "Gait Speed"))%>%  
         modify_caption("**Table 10. Median Gait Speed According to Age and Pain Severity**")%>%
         bold_labels())
  