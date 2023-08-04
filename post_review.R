## ---- a
library(knitr)
library(haven)
library(labelled)
library(gtsummary)
library(foreign)
library(ggsci)
library(qpcR)
library(ggpubr)
library(ggbeeswarm)
library(ggthemes)
library(ggrepel)
library(gt)
library(tidyverse)
library(quantreg)
library(WRTDStidal)
library(gridExtra)
library(car)

theme_gtsummary_journal(journal = "lancet")
theme_gtsummary_compact()
theme_set(theme_minimal())
gh_data <- read.dta("ghana.dta",convert.factors = T)%>% as_tibble()
gh_data$q2507[gh_data$q2507 >990] <- 0
gh_data$q2507[gh_data$q2508 >990] <- 0
gh_data$q1017[is.na(gh_data$q1017)] = 0

#Selection of relevant columns and finding the average mean blood pressures
#and pulse from a set of three readings; re-coding of a few columns to facilitate analysis and visualization

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
         height = q2506/100,
         BMI=round(q2507/((q2506/100)^2),digits = 1))%>% 
  mutate(Edu_yrs = cut(q1017,c(0,1,12,30),include.lowest=TRUE),
         quintile_c = factor(x = quintile_c, 
                             levels=c("1","2", "3", "4", "5"), labels = c(
                               "1st","2nd", "3rd", "4th", "5th")))

gh_data$BMI[gh_data$BMI< 18.5] <- "Underweight"
gh_data$BMI[gh_data$BMI %in% seq(30,250,0.1)] <- "Obese"
gh_data$BMI[gh_data$BMI %in%  seq(18.5,24.9,0.1)] <- "Normal Weight"
gh_data$BMI[gh_data$BMI %in%  seq(25.0,29.9,0.1)] <- "Overweight"
levels(gh_data$Edu_yrs) <- c("0", "1 to 12", "12 and above")

#The goal of the following lines of code is to create new columns 
#that combine multiple answers under different questions into one answer.
#e.g. A respndent who answers "yes"to q1a, q1b but no to q1c might still
#qualify as "yes" for overall q1 and thus has the disease. Filters were applied 
#to the data in the viewer pane to come up with the number of "yes", "no" and "NAs"
#for the following new columns.

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
#no for 3001.

colnames(gh_data)[colnames(gh_data) %in% c("q0104","q1009","q1011","q1012", "quintile_c")] <- c(
  "Residence","Sex","Age", "Marital_Status","Income")
#SA Data

#The same processes applied to the Ghana Data were applied to the SA Data

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
         height = q2506/100,
         BMI=round(q2507/((q2506/100)^2),digits = 1))%>% 
  mutate(Edu_yrs = cut(q1017,c(0,1,12,30),include.lowest=TRUE),
         quintile_c = factor(x = quintile_c, 
                             levels=c("1","2", "3", "4", "5"), labels = c(
                               "1st","2nd", "3rd", "4th", "5th")))

sa_data$BMI[sa_data$BMI< 18.5] <- "Underweight"
sa_data$BMI[sa_data$BMI %in% seq(30,250,0.1)] <- "Obese"
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

t4<-data.frame(rep(c("yes", "no"),times=c(1393,2825)))
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

# Finding the descriptive analysis of the demographics for Ghana and SA combined.

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

#Recoding the column on Marriage because benefits of married life applies to those cohabiting

dem$mst[dem$mst == "cohabiting"] <- "cohabiting/currently married"
dem$mst[dem$mst == "currently married"] <- "cohabiting/currently married"
dem$mst[dem$mst == "never married"] <- "single/separated"
dem$mst[dem$mst == "separated/divorced"] <- "single/separated"
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

# Finding the descriptive analysis of the data on comorbidities for Ghana and SA combined.

## ---- com

# com<-com %>%mutate_at(vars(starts_with("q")),all_vars(as.character(.)))
# com$q4022[com$q4022 == "don't know"] <- NA
# com$q4040[com$q4040 == "don't know"] <- NA
# com$q3001[com$q3001 == "don't know"] <- NA
# 
# com %>% filter_all(all_vars(!is.na(.))) %>%
#   dplyr::select(-Sex, -Age) %>%
#   tbl_summary(by=ctry,type=all_categorical()~"categorical",label=
#                 list(ang ~"History of Angina",
#                      acld ~"History of Asthma and Chronic Lung Disease",
#                      abp ~ "History of Arthritis and Musculoskeletal Pains",
#                      stk ~"History of Stroke",
#                      q4022 ~"History of Diabetes Mellitus",
#                      q4040 ~"History of Depression",
#                      hp ~"History of Hypertension",
#                      cat ~ "History of Cataracts",
#                      q3001 ~"History of Tobacco use",
#                      alcohol ~ "History of Alcohol use")) %>% bold_labels() %>%
#   modify_caption("**Table 2. Overview of Relevant Comorbidities**")

# 683 NAs in Ghana normal gait speed, 345 in SA
ghna <- gh_data %>% filter(is.na(gh_data$q2511))
sana <- sa_data %>% filter(is.na(sa_data$q2511))

gh_data <- gh_data %>% filter(!is.na(gh_data$q2511))
sa_data <- sa_data %>% filter(!is.na(sa_data$q2511))

nasquad <- ghna %>% mutate(mst = as.character(Marital_Status),
  ctry = "Ghana", Age_Groups = cut(Age,c(50,59,69,79,140),include.lowest=TRUE)) %>%
union_all(sana %>% mutate(mst = as.character(Marital_Status),ctry="South Africa", Age_Groups = cut(Age,c(50,59,69,79,140),include.lowest=TRUE)
                          ))
nasquad$mst[nasquad$mst == "cohabiting"] <- "cohabiting/currently married"
nasquad$mst[nasquad$mst == "currently married"] <- "cohabiting/currently married"
nasquad$mst[nasquad$mst == "never married"] <- "single/separated"
nasquad$mst[nasquad$mst == "separated/divorced"] <- "single/separated"
levels(nasquad$Age_Groups) <- c("50 to 59", "60 to 69", "70 to 79", "80 and above")

nasquad %>% dplyr::select(Age,Age_Groups,Sex,Edu_yrs,Residence,mst,Income,ctry) %>% 
  filter(Age > 49) %>% 
 tbl_summary(by=ctry)

rm(ghna)
rm(sana)

#The following lines of code filter out -ve values and then gait speed is found
gh_data <- gh_data %>% filter(q2511 >= 0) %>% mutate(norm_gs = 4/q2511, rap_gs = 4/q2513)
sa_data <- sa_data %>% filter(q2511 >= 0) %>% mutate(norm_gs = 4/q2511, rap_gs= 4/q2513)

#Gait speed greater than infinity
gh_data <- gh_data %>% filter(norm_gs != Inf)
sa_data <- sa_data %>% filter(norm_gs != Inf)

# gh_data$norm_gs[gh_data$norm_gs == Inf] <- 0
# sa_data$norm_gs[sa_data$norm_gs == Inf] <- 0

## ---- aig

#Checking for normality

isnorm <- gh_data %>% filter(Age>49) %>% select(norm_gs) %>% 
  union_all(sa_data  %>% filter(Age>49) %>% select(norm_gs)
  )
shapiro.test(sample(isnorm$norm_gs,5000))

ggqqplot(isnorm$norm_gs)
# Finding the 95th percentile values and dealing with outliers
quantile(gh_data$norm_gs, probs= c(0.05,0.95))
quantile(sa_data$norm_gs, probs= c(0.05,0.95))

#No. of imputed gait speeds = 415.equivalent to 4.83965% of total population.
gh_data %>% filter(norm_gs > 1.2500000) %>% nrow() #229
sa_data %>% filter(norm_gs > 1.9571429) %>% nrow() #186

ftgh <- table(gh_data$norm_gs)
ftgh <- as.data.frame(ftgh)
gt(ftgh)

ftsa <- table(sa_data$norm_gs)
#ftsa <- as.data.frame(ftsa)
#gt(ftsa)


gh_data$norm_gs[gh_data$norm_gs > 1.2500000 ] <- 1.2500000 
sa_data$norm_gs[sa_data$norm_gs > 1.9571429] <- 1.9571429

## ---- asa

#Comparing gait speed ethnicity-wise in SA as SA is made of different "races".

egs<-sa_data %>% dplyr::select(Age,q1018,norm_gs,rap_gs,Income,height,BMI,alcohol,q3001,q4040,q4022,q4060,
                               avg_sbp,acld,abp,stk,ang,hp,cat) %>%
  mutate(
    Ages = cut(Age, c(50,59,69,79,150),include.lowest=TRUE), q1018=
      as.character(q1018)
  ) %>%
  filter(Age>49,q1018 %in% c("African/Black", "White", "Coloured", "Indian/Asian")) %>%
  mutate(q1018 = as.factor(q1018))
levels(egs$Ages) <- c("50 to 59", "60 to 69", "70 to 79", "80 and above")

egs %>% select(q1018) %>% tbl_summary(label = list(q1018 ~ "Ethnicity in South Africa")) %>% bold_labels()
#Grouping SA gait speed into percentiles for ordinal log regression

egs <- egs %>% mutate(norm_gsc = round(norm_gs,digits = 4))

egs %>% filter(!is.na(ang)) %>% group_by(ang) %>% summarise(med = median(norm_gsc))
quantile(egs$norm_gsc, probs=c(.25,.75))

egs$gsc[egs$norm_gsc < 0.5128] <- "below 25th percentile"
egs$gsc[egs$norm_gsc > 1.0000] <- "above 75th percentile"
egs$gsc[egs$norm_gsc %in% seq(0.5128,1.0000,	0.0001)] <- "25th to 75th percentile"
egs$gsc[is.na(egs$gsc)] <- "25th to 75th percentile"
egs$gsc <- factor(egs$gsc,
                  levels=c("below 25th percentile",
                           "25th to 75th percentile",
                           "above 75th percentile"),
                  ordered = TRUE)
# 
# #Ordinal log:
library(MASS)
egs <- egs %>% mutate(ang = relevel(ang, ref = "yes"))
modelu <- polr(gsc ~ BMI,data=egs, Hess=TRUE)
ctable <- coef(summary(modelu))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2

gtsummary::tbl_regression(modelu, exponentiate = T)
(ctable)
(ctable <- cbind(ctable, "p value" = p))

egs<-egs %>% filter(q4022 != "don't know", q4060!= "don't know")
model <- polr(gsc ~ q1018+Age+BMI+q4022+q4060+ang,Hess = TRUE,
              #+alcohol+q3001+q4040+q4022+q4060+acld+abp
              #stk+ang+hp+cat
              data=egs )
summary(model)

ggs<-gh_data %>% dplyr::select(Age,q1018,norm_gs,rap_gs,Income,height,BMI,alcohol,q3001,q4040,q4022,q4060,
                               avg_sbp,acld,abp,stk,ang,hp,cat) %>%
  mutate(
    Ages = cut(Age, c(50,59,69,79,150),include.lowest=TRUE), q1018=
      as.character(q1018)
  ) %>%
  filter(Age>49) %>%
  mutate(q1018 = as.factor(q1018))
levels(egs$Ages) <- c("50 to 59", "60 to 69", "70 to 79", "80 and above")

ggs <- ggs %>% mutate(norm_gsc = round(norm_gs,digits = 4))
ggs %>% filter(!is.na(ang)) %>% group_by(ang) %>% summarise(med = median(norm_gsc))

quantile(ggs$norm_gsc, probs=c(.25,.75))
ggs$gsc[ggs$norm_gsc < 0.5479] <- "below 25th percentile"
ggs$gsc[ggs$norm_gsc > 0.9091] <- "above 75th percentile"
ggs$gsc[ggs$norm_gsc %in% seq(0.5479,0.9091,0.0001)] <- "25th to 75th percentile"
ggs$gsc[is.na(ggs$gsc)] <- "25th to 75th percentile"
ggs$gsc <- factor(ggs$gsc,
                  levels=c("below 25th percentile",
                           "25th to 75th percentile",
                           "above 75th percentile"),
                  ordered = TRUE)

ggs <- ggs %>% mutate(ang = relevel(ang, ref = "yes"))
gmodelu <- polr(gsc ~ ang,data=ggs, Hess=TRUE)

ctable <- coef(summary(gmodelu))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2

gtsummary::tbl_regression(gmodelu, exponentiate = T)
(ctable)
(ctable <- cbind(ctable, "p value" = p))

ggs<-egs %>% filter(q4022 != "don't know", q4060!= "don't know")
gmodel <- polr(gsc ~ Age+BMI+q4022+q4060+ang,
              Hess = TRUE,data=ggs )
summary(gmodel)
gtsummary::tbl_regression(gmodel, exponentiate = T)

vif(model)

# 
# #p-values:
ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2

gtsummary::tbl_regression(model, exponentiate = T)
(ctable <- cbind(ctable, "p value" = p))

# #coefficient value: For one unit increase in age we expect about 0.036 decrease in the expected
# #value of gait speed in the log odds scale, given that all other variables in the model are held constant.

# #1-exp(coef(model))
# 
# #For every one unit increase in Age the odds of being in the 2th percentile and above is multiplied 0.96 times 
# #i.e., decreaseas by 0.035x), holding constant all other variables.
# 
# #misclassification error
# pred <- predict(model, type= 'class') 
# 
# cm <- table(pred,egs$gsc)
# 
# 1-sum(diag(cm))/sum(cm)
#error rate: 0.3805995, better than the previous with NAs and negatives: 0.4835566


#library(ordinal) #ordinal regression package
# library(rcompanion) pseudo R square
# library(MASS)also an ordinal regression package, different method
# plyr method for getting data that allows
# the test of proport
# library(brant) test of proportional odds

#2nd ordinal regression method;
# modelnull <-  clm(gsc ~1, data = egs, link = "logit")
# model1 <- clm(gsc ~ Age + q1018, data= egs, link = "logit")
# anova(modelnull, model1)
# #if AIC and p-value for model1 is 
# #less, then it's a better fit
# 

## ---- sadiv
sa_data %>% select(q1018) %>% tbl_summary()
#This takes out the Black and Coloured populations in SA for the analysis.

sa_data<- sa_data %>% filter(q1018 ==  "African/Black") #%in% c("African/Black", "Coloured"))

## ---- b
#Recoding the column on Marriage because benefits of married life applies to those cohabiting

gh_data<-gh_data %>% mutate(mst = as.character(Marital_Status))
gh_data$mst[gh_data$mst == "cohabiting"] <- "cohabiting/currently married"
gh_data$mst[gh_data$mst == "currently married"] <- "cohabiting/currently married"
gh_data$mst[gh_data$mst == "never married"] <- "single/separated"
gh_data$mst[gh_data$mst == "separated/divorced"] <- "single/separated"

sa_data<-sa_data %>% mutate(mst = as.character(Marital_Status))
sa_data$mst[sa_data$mst == "cohabiting"] <- "cohabiting/currently married"
sa_data$mst[sa_data$mst == "currently married"] <- "cohabiting/currently married"
sa_data$mst[sa_data$mst == "never married"] <- "single/separated"
sa_data$mst[sa_data$mst == "separated/divorced"] <- "single/separated"

#Finding in-country demographics and comparing them gender-wise

cd<-gh_data %>% 
  # mutate(Age_Groups = cut(Age,c(18,50,59,69,79,140),include.lowest=TRUE)) %>% 
  dplyr::select(Age,Sex,Edu_yrs,Residence,q1018,mst,Income,height,BMI,norm_gs) %>%  
  mutate(
    ctry = "Ghana"
  ) %>% 
  union_all(
    sa_data  %>% 
      # mutate(Age_Groups = cut(Age,c(18,50,59,69,79,140),include.lowest=TRUE)) %>%
      dplyr::select(Age,Sex,Edu_yrs,Residence,q1018,mst,Income,height,BMI,norm_gs) %>% 
      mutate(ctry="South Africa")
  ) 

gh_data %>% filter(Age >49) %>% nrow() #4084
sa_data %>% filter(Age >49) %>% nrow() # 1847

fcda<-cd %>% filter(Age>49)%>% #filter_all(all_vars(!is.na(.))) %>%
  mutate(Residence=droplevels(Residence),mst=as.factor(mst),
         Age_Groups=cut(Age,c(50,59,69,79,140),include.lowest=TRUE))

levels(fcda$Age_Groups) <-c("50 to 59", "60 to 69", "70 to 79", "80 and above")

#Demographics table of main study pop

fcda %>% dplyr::select(-c(norm_gs,Age,q1018)) %>%  
  #filter_at(vars(-Edu_yrs),all_vars(!is.na(.))) %>%  
  tbl_summary(by =ctry, label=list(Age_Groups ~"Age Groups (in years)",
                                   #Age ~"Age (in years)",
                                   Edu_yrs ~"Years of Education",
                                   Income ~"Income (in quintiles)",
                                   mst ~"Marital Status")) %>% bold_labels() %>% 
  modify_caption("**Overview of Socio-Demographic Characteristics of Study Population**")

com <- gh_data %>%filter(Age>49) %>% 
  mutate(Age_Groups = cut(Age,c(50,59,69,79,140),include.lowest=TRUE)) %>%
  dplyr::select(Age,Sex,Residence,alcohol,q3001,q4040,q4022,q4060,
                avg_sbp,acld,abp,stk,ang,hp,cat,alcohol) %>% mutate(
                  ctry = "Ghana") %>% 
  union_all(
    sa_data %>% filter(Age>49) %>% 
      mutate(Age_Groups = cut(Age,c(50,59,69,79,140),include.lowest=TRUE)) %>%
      dplyr::select(Age,Sex,Residence,alcohol,q3001,q4040,q4022,q4060,
                    avg_sbp,acld,abp,stk,ang,hp,cat,alcohol) %>% 
      mutate(
        ctry = "South Africa"))

com<-com %>%mutate_at(vars(starts_with("q")),all_vars(as.character(.)))
com$q4022[com$q4022 == "don't know"] <- NA
com$q4040[com$q4040 == "don't know"] <- NA
com$q3001[com$q3001 == "don't know"] <- NA

com %>% filter_all(all_vars(!is.na(.))) %>%
  dplyr::select(-Sex, -Age) %>%
  tbl_summary(by=ctry,type=all_categorical()~"categorical",label=
                list(ang ~"History of Angina",
                     acld ~"History of Asthma and Chronic Lung Disease",
                     abp ~ "History of Arthritis and Musculoskeletal Pains",
                     stk ~"History of Stroke",
                     q4022 ~"History of Diabetes Mellitus",
                     q4040 ~"History of Depression",
                     hp ~"History of Hypertension",
                     cat ~ "History of Cataracts",
                     q3001 ~"History of Tobacco use",
                     alcohol ~ "History of Alcohol use")) %>% bold_labels() %>%
  modify_caption("**Table 2. Overview of Relevant Comorbidities**")

nacom<- nasquad %>% filter(Age >49) %>% dplyr::select(Age,Sex,Residence,alcohol,q3001,q4040,q4022,q4060,
                                              avg_sbp,acld,abp,stk,ang,hp,cat,alcohol)

nacom<-nacom %>%mutate_at(vars(starts_with("q")),all_vars(as.character(.)))
nacom$q4022[nacom$q4022 == "don't know"] <- NA
nacom$q4040[nacom$q4040 == "don't know"] <- NA
nacom$q3001[nacom$q3001 == "don't know"] <- NA

nacom %>% filter_all(all_vars(!is.na(.))) %>%
  dplyr::select(-Sex, -Age) %>%
  tbl_summary(type=all_categorical()~"categorical",label=
                list(ang ~"History of Angina",
                     acld ~"History of Asthma and Chronic Lung Disease",
                     abp ~ "History of Arthritis and Musculoskeletal Pains",
                     stk ~"History of Stroke",
                     q4022 ~"History of Diabetes Mellitus",
                     q4040 ~"History of Depression",
                     hp ~"History of Hypertension",
                     cat ~ "History of Cataracts",
                     q3001 ~"History of Tobacco use",
                     alcohol ~ "History of Alcohol use")) %>% bold_labels()
# fcd<-cd %>% filter(Age>49)%>% filter_all(all_vars(!is.na(.))) %>%
#   mutate(Residence=droplevels(Residence),mst=as.factor(mst),
#          Age_Groups=cut(Age,c(50,59,69,79,140),include.lowest=TRUE))
# 
# levels(fcd$Age_Groups) <-c("50 to 59", "60 to 69", "70 to 79", "80 and above")
# 
# fcd %>% dplyr::select(-c(norm_gs,rap_gs,Age)) %>%  
#   filter_at(vars(-Edu_yrs),all_vars(!is.na(.))) %>%  
#   tbl_summary(by =ctry, label=list(Age_Groups ~"Age Groups (in years)",
#                                   #Age ~"Age (in years)",
#                                   Edu_yrs ~"Years of Education",
#                                   Income ~"Income (in quintiles)",
#                                   mst ~"Marital Status")) %>% bold_labels() %>% 
#   modify_caption("**Overview of Socio-Demographic Characteristics of Study Population**")

## ---- d

#Ordinal Regression with Gait Speed as dependent variable and the demographics as independent variables
fcda <- fcda %>% mutate(norm_gsc = round(norm_gs,digits = 4))
quantile(fcda$norm_gsc, probs=c(.25,.75))

fcda$gsc[fcda$norm_gsc < 0.5333] <- "below 25th percentile"
fcda$gsc[fcda$norm_gsc > 0.9091 ] <- "above 75th percentile"
fcda$gsc[fcda$norm_gsc %in% seq(0.5333, 0.9091 ,	0.0001)] <- "25th to 75th percentile"
fcda$gsc[is.na(fcda$gsc)] <- "25th to 75th percentile"
fcda$gsc <- factor(fcda$gsc,
                  levels=c("below 25th percentile",
                           "25th to 75th percentile",
                           "above 75th percentile"),
                  ordered = TRUE)

fcda %>% group_by(Sex) %>% summarize(mgs = median(norm_gs),
                                    q = quantile(norm_gs, probs=c(.25,.75)))

#Plot of Independent Variables and Gait Speed
mgs <- fcda %>% group_by(Sex,Age) %>% 
  summarize(mean_gs = mean(norm_gs,na.rm = TRUE))

mgs%>%filter(Age <= 110) %>% 
  ggplot(aes(Age,mean_gs, color=Sex))+
  scale_x_continuous(limits = c(40,120))+
  geom_point()+
  #geom_beeswarm(cex=3)+#,priority = "density")+
  #stat_ellipse(aes(color = Age), type = "t")+
  geom_smooth(method = "lm",se = TRUE)+
  labs(color="Sex",
       y="Mean Gait Speed")+scale_color_aaas()+ 
  theme(text=element_text(size=20))
ggsave('men_vs_women.jpg', width = 10, height = 6, unit = "in", dpi = 300)

fcd %>% ggplot(aes(Residence,norm_gs))+
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red")+
  labs(y="Gait Speed")+theme(text=element_text(size=20))


ggsave('residence.jpg', width = 10, height = 6, unit = "in", dpi = 300)

#Unadjusted model
modelfu <- polr(gsc ~ height,  
               data=fcda,Hess = TRUE)
cutable <- coef(summary(modelfu))
p <- pnorm(abs(cutable[, "t value"]), lower.tail = FALSE)*2

(cutable <- cbind(cutable, "p value" = p))

gtsummary::tbl_regression(modelfu, exponentiate = T) %>% bold_labels()

#Adjusted model
modelf <- polr(gsc ~ .-norm_gs -ctry -norm_gsc -height -q1018 -Age_Groups -Edu_yrs -Income -mst,  
               data=fcda,Hess = TRUE)

#p-values:
ctable <- coef(summary(modelf))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2

gtsummary::tbl_regression(modelf, exponentiate = T) %>% bold_labels()
(ctable <- cbind(ctable, "p value" = p))
vif(modelf)
#cm <- table(predict(modelf),fcda$gsc)
cm <- table(predict(modelf),drop_na(fcda)[,  "gsc"])

1-sum(diag(cm))/sum(cm) #misclassification error rate of 0.4867596

#the ordinal logistic regression model assumes the ORs comparing outcome groups based 
#on different cutoffs are the same at all cutoffs (the proportional odds assumption).

#Test for Proportional Odds Assumption

# Create a series of binary variables
cdt <- fcda %>%
  mutate(Age2 = Age/15,
    # Y1 = GS > level1
    l1 = fct_collapse(gsc,
                      ">level1"  = c("25th to 75th percentile", "above 75th percentile"),
                      "<=level1" = "below 25th percentile"),
    #l1 = relevel(l1, ref = "<=level1"),
    # Y2 = GS > level2
    l2 = fct_collapse(gsc,
                      ">level2"  = "above 75th percentile",
                      "<=level2" = c("below 25th percentile", 
                                     "25th to 75th percentile")))
    #l2 = relevel(l2, ref = "<=level2"))

fit.ordinal <- MASS::polr(gsc~ Age + Sex+ Residence,  
                          data=cdt)

fit.binary1 <- glm(l1 ~ Age + Sex+ Residence,  
                   data=cdt,
                   family = binomial)

fit.binary2 <- glm(l2 ~ Age +Sex+ Residence,  
                   data=cdt,
                   family = binomial)

dt <- exp(
  cbind(
    "ordinal"  = fit.ordinal$coefficients,
    "binary 1" = fit.binary1$coefficients[-1],
    "binary 2" = fit.binary2$coefficients[-1]
  )
)

dt <- as.data.frame(dt)
gt(dt)

#Since our ORs for the binary regressions are similar for the one for ordinal and within
# the CI ranges for the ordinal, the PO assumption has been met by all our three main predictors
## ---- e

# Ordinal log regression just as in code chunk above, but with comorbidities as independent variables
cr <- gh_data %>%
  dplyr::select(Age,Sex,Residence,norm_gs,alcohol,q3001,q4040,q4022,q4060,
                avg_sbp,acld,abp,stk,ang,hp,cat) %>%
filter(Age>49)%>%
mutate(
    ctry = "Ghana"
  ) %>%
  union_all(
    sa_data  %>%
      dplyr::select(Age,Sex,Residence,norm_gs,alcohol,q3001,q4040,q4022,q4060,
                    avg_sbp,acld,abp,stk,ang,hp,cat)%>%
filter(Age>49)%>%
      mutate(ctry="South Africa")
  )
# 
cr<- cr %>% mutate(avg_sbp = round(avg_sbp, digits = 0))
cr$avg_sbp[cr$avg_sbp >= 180] <- "high bp 1"
cr$avg_sbp[cr$avg_sbp < 140 ] <- "normal bp"
cr$avg_sbp[cr$avg_sbp %in% seq(140,179,1)] <- "high bp 2"
cr$avg_sbp[cr$avg_sbp %in% seq(0,89,1)] <- "low"
cr$avg_sbp[cr$avg_sbp %in% seq(90,99,1)] <- "normal bp"
cr <- cr %>% 
  filter_at(vars(starts_with("q")), all_vars(. != "don't know")) %>%
mutate(norm_gsc = round(norm_gs,digits = 4))

cr$avg_sbp <- factor(cr$avg_sbp, levels=c("normal bp",
  "high bp 1", "high bp 2", "low"))
                  
quantile(cr$norm_gsc, probs=c(.25,.75))
# 
cr$gsc[cr$norm_gsc < 0.5333 ] <- "below 25th percentile"
cr$gsc[cr$norm_gsc > 0.9091 ] <- "above 75th percentile"
cr$gsc[cr$norm_gsc %in% seq(0.5333,0.9091,0.0001)] <- "25th to 75th percentile"
cr$gsc[is.na(cr$gsc)] <- "25th to 75th percentile"
cr$gsc <- factor(cr$gsc,
                  levels=c("below 25th percentile",
                           "25th to 75th percentile",
                           "above 75th percentile"),
                  ordered = TRUE)
cr <- cr%>% mutate(ang = relevel(ang, ref = "yes"))
# 
modelfd <- polr(gsc ~ . -norm_gs -ctry -norm_gsc -hp -alcohol -cat, 
                data=cr, Hess = TRUE) #-alcohol -cat, -acld -hp -q4040

set.seed(123)
training.samples <- cr$gsc %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- cr[training.samples, ]
test.data <- cr[-training.samples, ]

modelfd <- polr(gsc ~ . -norm_gs -ctry -norm_gsc, 
                data=cr, Hess = TRUE) #-alcohol -cat, -acld -hp -q4040

coef(modelfd)

ctable <- coef(summary(modelfd))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2

gtsummary::tbl_regression(modelfd, exponentiate = T) %>% bold_labels()
(ctable <- cbind(ctable, "p value" = p))

cm <- table(predict(modelfd),drop_na(cr)[,  "gsc"])

1-sum(diag(cm))/sum(cm) # error rate: 0.4649682

gtsummary::tbl_regression(modelfd2, exponentiate = T,
                          label=list(ang ~"History of Angina",
                                     q4022 ~"History of Diabetes Mellitus"
                                     #alcohol ~ "History of alcohol use",
                                     #q3001 ~"History of tobacco use"
                                     )) %>%
  modify_caption("**Table 7.Ordinal Logistic Regression: Gait Speed vs Health-Related Variables**") %>%
  bold_labels()

modelfd2 <- polr(gsc ~ .-Age -Sex -Residence - avg_sbp -abp -norm_gs -q3001 -ctry -norm_gsc -stk -cat -alcohol -q4040 -acld -hp, 
                data=cr, Hess = TRUE)
coef(modelfd2)
ctable <- coef(summary(modelfd2))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2

gtsummary::tbl_regression(modelfd2, exponentiate = T) %>% bold_labels()
(ctable)
(ctable <- cbind(ctable, "p value" = p))

cm <- table(predict(modelfd2),drop_na(cr)[,  "gsc"])
1-sum(diag(cm))/sum(cm)  #error rate: 0.4811578

summary(modelfd2)
vif(modelfd2)
tb <- polr(gsc ~ Residence*q4022,
     data=cr, Hess = TRUE)
gtsummary::tbl_regression(tb, exponentiate = T) %>% bold_labels()

ctable <- coef(summary(tb))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
(ctable <- cbind(ctable, "p value" = p))

modelall <- polr(gsc ~ . -norm_gs -ctry -norm_gsc -stk -cat -alcohol -q4040 -acld -hp, 
                 data=cr, Hess = TRUE)
gtsummary::tbl_regression(modelall, exponentiate = T) %>% bold_labels()

## ---- n

#WHODAS scoring system for measurement of disability

whodas <-gh_data %>%
  dplyr::select(Age,Sex,q2506, q2507,BMI,q2011,q2014,q2015,q2028,q2032,q2033, q2035,q2036, 
                q2037, q2038,q2039,q2042,q2044,q2047,norm_gs,q4022,abp,ang,q3001) %>%
  mutate(
    ctry = "Ghana"
  ) %>%
  union_all(
    sa_data  %>%
      dplyr::select(Age,Sex,q2506, q2507,BMI,q2011,q2014,q2015,q2028,q2032,q2033, q2035,q2036, 
                    q2037, q2038,q2039,q2042,q2044,q2047,norm_gs,q4022,abp,ang,q3001) %>%
      mutate(ctry="South Africa"))%>% 
  filter_at(vars(contains("q")), all_vars(. != "don't know")) %>%
  filter_at(vars(contains("q")),
            all_vars(. != "not applicable"))
colnames(whodas)[colnames(whodas) %in% c("q4022","q3001")] <- c(
  "dm","tb")

#Height,BMI,ADLs and WHODAS in one table.
whodas <- whodas %>% filter(Age>49) %>%  mutate_at(vars(contains("q")), as.character)
whodas[whodas=="none"] <- as.character(0)
whodas[whodas=="mild"] <- as.character(1)
whodas[whodas=="moderate"] <- as.character(2)
whodas[whodas=="severe"] <- as.character(3)
whodas[whodas=="extreme"] <- as.character(4)

whodas <- whodas %>% mutate_at(vars(contains("q")), as.numeric)
#Rearranging the columns to place the 3 q-variables dealing with ADLS at the end
whodas<-whodas[,c(1:14,16,19,15,17,18,20:25)]
whodas<-mutate(whodas, heightm =round(q2506/100,digits=2))
whodas$Score<-rowSums(whodas[6:17],na.rm = TRUE)
#whodas$aScore<-rowSums(whodas[17:19],na.rm = TRUE) #ADLs

#WHODAS being converted into percentages and finding the quartiles to find cut-off points

whodas<-whodas %>% mutate(wp= round((Score/48)*100,digits=2))
quantile(whodas$wp)
whodas$wp[whodas$wp<25] <- "none or mild"
whodas$wp[whodas$wp %in% seq(25,49,0.01)] <- "moderate"
whodas$wp[whodas$wp %in% seq(50,100,0.01)] <- "severe or extreme"

quantile(whodas$heightm)
whodas$heightm[whodas$heightm<1.55] <- "below average"
whodas$heightm[whodas$heightm %in% seq(1.55,1.68,0.01)] <- "average"
whodas$heightm[whodas$heightm %in% seq(1.69,9.98,0.01)] <- "above average"

#ADL score recoded
# whodas$aScore[whodas$aScore==0] <- "none"
# whodas$aScore[whodas$aScore %in% seq(1,4,1)] <- "mild"
# whodas$aScore[whodas$aScore %in% seq(5,8,1)] <- "moderate"
# whodas$aScore[whodas$aScore %in% seq(9,12,1)] <- "severe"

whodas <- whodas %>% dplyr::select(Age,heightm,BMI,wp,ctry,q2038,q2042,q2044,norm_gs,Sex,dm,abp,ang,tb) %>% 
  filter(BMI %in% c("Normal Weight","Obese", "Overweight","Underweight"),
         Age < 106)
# 
# whodas$BMI[whodas$BMI == "Underweight"] <- "Underweight (BMI less than 18.5)"
# whodas$BMI[whodas$BMI == "Normal Weight"] <- "Normal Weight (BMI: 18.5 - 24.9)"
# whodas$BMI[whodas$BMI == "Overweight"] <- "Overweight (BMI: 25.0 - 29.9)"
# whodas$BMI[whodas$BMI == "Obese"] <- "Obese (BMI: 30 and above)"

whodas <- whodas %>% #to account for some outliers
  mutate(BMI = factor(BMI, levels = c(
    "Normal Weight",
    "Underweight",
    "Overweight",
    "Obese")
  ),
  wp=factor(wp, levels = c("none or mild","moderate","severe or extreme")),
  #aScore=factor(aScore, levels = c("none","mild", "moderate","severe")),
  Ages = cut(Age, seq(50,110,10),include.lowest=TRUE))

levels(whodas$Ages) <- c("50", "61","71","81","91","101")

fig1 <-whodas %>% ggplot(aes(x=wp,y=norm_gs))+
    geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red")+
    labs(y="Gait Speed",
         x="WHODAS score category")+ theme(text=element_text(size=20))
  
ggsave('whodas.jpg', width = 10, height = 6, unit = "in", dpi = 300)

whodas <- whodas %>% mutate(BMI = relevel(BMI, ref = "Underweight"))
fig2 <- whodas %>% ggplot(aes(x=BMI,y=norm_gs))+
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red")+
  labs(x="BMI category",
       y="Gait Speed")+
  #scale_y_continuous(limits = c(0,1.5))+
    theme(text=element_text(size=20))
fig2

ggsave('bmi.jpg', width = 10, height = 6, unit = "in", dpi = 300) 

cb <- grid.arrange(fig1,fig2, nrow=1)

ggsave('combi.jpg', width = 20, height = 6, unit = "in", dpi = 300) 

## ---- g

cd <- cd %>%filter(Age>49) %>% 
  mutate(Ages = cut(Age, c(50,55,60,65,70,75,80,114),include.lowest=TRUE))

levels(cd$Ages) <- c("50", "56","61","66","71",
                     "76","81+")

whodas <- whodas %>% mutate(norm_gsc = round(norm_gs,digits = 4))
quantile(whodas$norm_gsc, probs=c(.25,.75))

whodas$gsc[whodas$norm_gsc < 0.5333 ] <- "below 25th percentile"
whodas$gsc[whodas$norm_gsc > 0.9302 ] <- "above 75th percentile"
whodas$gsc[whodas$norm_gsc %in% seq(0.5333, 0.9302,	0.0001)] <- "25th to 75th percentile"
whodas$gsc[is.na(whodas$gsc)] <- "25th to 75th percentile"
whodas$gsc <- factor(whodas$gsc,
                     levels=c("below 25th percentile",
                              "25th to 75th percentile",
                              "above 75th percentile"),
                     ordered = TRUE)

#Unadjusted model
modelwu <- polr(gsc ~ BMI,  
                data=whodas,Hess = TRUE)
cutable <- coef(summary(modelwu))

p <- pnorm(abs(cutable[, "t value"]), lower.tail = FALSE)*2

(cutable <- cbind(cutable, "p value" = p))

gtsummary::tbl_regression(modelwu, exponentiate = T) %>% bold_labels()

#Adjusted model
modelw <- polr(gsc ~ wp + BMI,  
               data=whodas,Hess = TRUE)

#p-values:
ctable2 <- coef(summary(modelw))
p <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE)*2

gtsummary::tbl_regression(modelw, exponentiate = T) %>% bold_labels()
(ctable2 <- cbind(ctable2, "p value" = p))

cmw <- table(predict(modelw),drop_na(whodas)[,  "gsc"])
cmw <- table(predict(modelw),whodas$gsc)

1-sum(diag(cmw))/sum(cmw) #misclassification error rate of 0.460126

#Testing whodas for PO assumption
# Create a series of binary variables
wdt <- whodas %>%
  mutate( # Y1 = GS > level1
         l1 = fct_collapse(gsc,
                           ">level1"  = c("25th to 75th percentile", "above 75th percentile"),
                           "<=level1" = "below 25th percentile"),
         #l1 = relevel(l1, ref = "<=level1"),
         # Y2 = GS > level2
         l2 = fct_collapse(gsc,
                           ">level2"  = "above 75th percentile",
                           "<=level2" = c("below 25th percentile", 
                                          "25th to 75th percentile")))
#l2 = relevel(l2, ref = "<=level2"))

fit.ordinal <- MASS::polr(gsc~ wp+BMI,  
                          data=whodas)

fit.binary1 <- glm(l1 ~ wp + BMI,  
                   data=wdt,
                   family = binomial)

fit.binary2 <- glm(l2 ~ wp + BMI,  
                   data=wdt,
                   family = binomial)

wt <- exp(
  cbind(
    "ordinal"  = fit.ordinal$coefficients,
    "binary 1" = fit.binary1$coefficients[-1],
    "binary 2" = fit.binary2$coefficients[-1]
  )
)

wt <- as.data.frame(wt)
gt(wt)

#Analysis of excluded population

cr2 <- nasquad %>%
      dplyr::select(Age,Sex,Residence,alcohol,q3001,q4040,q4022,q4060,
                    avg_sbp,acld,abp,stk,ang,hp,cat,alcohol)%>% mutate(avg_sbp = round(avg_sbp, digits = 0))

cr2$avg_sbp[cr2$avg_sbp >= 180] <- "high bp 1"
cr2$avg_sbp[cr2$avg_sbp < 140 ] <- "normal bp"
cr2$avg_sbp[cr2$avg_sbp %in% seq(140,179,1)] <- "high bp 2"
cr2$avg_sbp[cr2$avg_sbp %in% seq(0,89,1)] <- "low"
cr2$avg_sbp[cr2$avg_sbp %in% seq(90,99,1)] <- "normal bp"
cr2$avg_sbp <- factor(cr2$avg_sbp, levels=c("normal bp",
                                          "high bp 1", "high bp 2", "low"))
cr2 %>% tbl_summary()

whodas2 <-nasquad %>%
  dplyr::select(Age,Sex,BMI,q2011,q2014,q2015,q2028,q2032,q2033, q2035,q2036, 
                q2037, q2038,q2039,q2042,q2044,q2047,q4022,abp,ang,q3001)
  # filter_at(vars(contains("q")), all_vars(. != "don't know")) %>%
  # filter_at(vars(contains("q")),
  #           all_vars(. != "not applicable"))
colnames(whodas2)[colnames(whodas2) %in% c("q4022","q3001")] <- c(
  "dm","tb")

#Height,BMI,ADLs and WHODAS in one table.
whodas2 <- whodas2 %>%  mutate_at(vars(contains("q")), as.character)
whodas2[whodas2=="none"] <- as.character(0)
whodas2[whodas2=="mild"] <- as.character(1)
whodas2[whodas2=="moderate"] <- as.character(2)
whodas2[whodas2=="severe"] <- as.character(3)
whodas2[whodas2=="extreme"] <- as.character(4)

#whodas variables: q2011,q2014, q2015, q2028, q2032, q2035, q2036, q2037, q2039, q2047, q2038
whodas2 <- whodas2 %>% select(-q2042, -q2044)

whodas2 <- whodas2 %>% mutate_at(vars(contains("q")), as.numeric) %>% 
  filter_at(vars(contains("q")),all_vars(!is.na(.)))

# whodas<-mutate(whodas, heightm =round(q2506/100,digits=2))

whodas2$Score<-rowSums(whodas2[4:15],na.rm = TRUE)

#WHODAS being converted into percentages and finding the quartiles to find cut-off points

whodas2<-whodas2 %>% mutate(wp= round((Score/48)*100,digits=2))
#quantile(whodas2$wp)
whodas2$wp[whodas2$wp<25] <- "none or mild"
whodas2$wp[whodas2$wp %in% seq(25,49,0.01)] <- "moderate"
whodas2$wp[whodas2$wp %in% seq(50,100,0.01)] <- "severe or extreme"

whodas2 %>% select(Age, Sex, BMI, wp) %>% filter(Age >49) %>% tbl_summary()

#The strength of association is lost when it is grouped into age ranges.
# who50 <- whodas %>% filter(Age >= 50, Age <70)
# who70 <- whodas %>% filter(Age >70)
# 
# modelw5 <- polr(gsc ~ BMI + wp + aScore,  
#                data=who50,Hess = TRUE)
# summary(modelw5)
# #p-values:
# ctable2 <- coef(summary(modelw5))
# p <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE)*2
# 
# gtsummary::tbl_regression(modelw5, exponentiate = T) %>% bold_labels()
# (ctable2 <- cbind(ctable2, "p value" = p))
# 
# cmw <- table(predict(modelw5),who50$gsc)
# 
# 1-sum(diag(cmw))/sum(cmw)

whodas %>% select(Age,BMI, wp,ctry) %>% filter(Age >49) %>% tbl_summary(by=ctry)

cr %>% filter(!is.na(q4060)) %>% group_by(q4060) %>% summarize(mgs = median(norm_gsc),
                                      q = quantile(norm_gs, probs=c(.25,.75)))
