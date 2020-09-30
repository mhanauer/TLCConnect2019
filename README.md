---
title: "TLC Connect 2019"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##################
Data cleaning
###############

```{r}
library(prettyR)
setwd("P:/Evaluation/TN Lives Count_Connect/Databases")
tlc_data = read.csv("TLCConnect_10_1_2019.csv", header = TRUE, na.strings = c(-6,-7,-8,-9))
head(tlc_data)
## Change youth id 
colnames(tlc_data)[1] = "YouthID"
head(tlc_data)


describe.factor(tlc_data$CSSRS1)
describe.factor(tlc_data$CSSRS4)
describe.factor(tlc_data$AttemptSuicide)
head(tlc_data)
tlc_data_analysis = tlc_data[,c(1,2,5:9, 11, 13:56, 69:112,118,124)]
tlc_data_analysis = data.frame(tlc_data_analysis, HoursPsychotherapy = tlc_data$HoursPsychotherapy, CurrentlyEngaged  =  tlc_data$CurrentlyEngaged, ReferralsEngaged = tlc_data$ReferralsEngaged, Attend75Referrals = tlc_data$Attend75Referrals, ReferralsProvided = tlc_data$ReferralsProvided, CrisisPlan80Time = tlc_data$CrisisPlan80Time)
head(tlc_data_analysis)
#Check all variables are within the ranges
tlc_data_analysis$INQ6_B[tlc_data_analysis$INQ6_B == 8] = NA
apply(tlc_data_analysis[-c(1)], 2, function(x){describe.factor(x,decr.order = FALSE)})
head(tlc_data_analysis)

### Generate average scores
head(tlc_data_analysis)

### Get rid of missing treatments
tlc_data_analysis$treat_missing = is.na(tlc_data_analysis$TXPackageAssigned)
tlc_data_analysis = subset(tlc_data_analysis, treat_missing == FALSE)
dim(tlc_data_analysis)
tlc_data_analysis$treat_missing = NULL

#f = 6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RAS_b_average = tlc_data_analysis[,9:28]

RAS_b_1_average = RAS_b_average[,c(6:13,20)]
RAS_b_1_average = apply(RAS_b_1_average, 1, mean, na.rm = TRUE)

#q=17,r=18,s=19
RAS_b_2_average = RAS_b_average[,17:19]
RAS_b_2_average = apply(RAS_b_2_average, 1, mean, na.rm = TRUE)

#a,b,c,d,e
RAS_b_3_average = RAS_b_average[,1:5]
RAS_b_3_average = apply(RAS_b_3_average, 1, mean, na.rm = TRUE)
#n=14,o=15,p=16
RAS_b_5_average = RAS_b_average[,14:16]
RAS_b_5_average = apply(RAS_b_5_average, 1, mean, na.rm = TRUE)

INQ_b_1_average = tlc_data_analysis[,29:34]
INQ_b_1_average = apply(INQ_b_1_average, 1, mean, na.rm = TRUE)

INQ_b_2_average = tlc_data_analysis[,35:40]

#https://www.marsja.se/reverse-scoring-using-r/
INQ_b_2_average = 8-INQ_b_2_average
INQ_b_2_average = apply(INQ_b_2_average,1, mean, na.rm = TRUE)

SSMI_b_average = tlc_data_analysis[,41:45]
SSMI_b_average =  apply(SSMI_b_average, 1, mean, na.rm = TRUE)



SIS_b_average = tlc_data_analysis[,46:52]

#a,b,c,d
SIS_b_1_average = SIS_b_average[,1:4]
SIS_b_1_average = apply(SIS_b_1_average, 1, mean, na.rm =TRUE)

#e,f,g
SIS_b_2_average = SIS_b_average[,5:7]
SIS_b_2_average = apply(SIS_b_2_average, 1, mean, na.rm =TRUE)



#### Discharge
#f = 6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RAS_d_average = tlc_data_analysis[53:72]

RAS_d_1_average = RAS_d_average[,c(6:13,20)]
RAS_d_1_average = apply(RAS_d_1_average, 1, mean, na.rm = TRUE)

#q=17,r=18,s=19
RAS_d_2_average = RAS_d_average[,17:19]
RAS_d_2_average = apply(RAS_d_2_average, 1, mean, na.rm = TRUE)

#a,b,c,d,e
RAS_d_3_average = RAS_d_average[,1:5]
RAS_d_3_average = apply(RAS_d_3_average, 1, mean, na.rm = TRUE)
#n=14,o=15,p=16
RAS_d_5_average = RAS_d_average[,14:16]
RAS_d_5_average = apply(RAS_d_5_average, 1, mean, na.rm = TRUE)

INQ_d_1_average = tlc_data_analysis[,73:78]
INQ_d_1_average = apply(INQ_d_1_average, 1, mean, na.rm = TRUE)

INQ_d_2_average = tlc_data_analysis[,79:84]
INQ_d_2_average = 8-INQ_d_2_average
INQ_d_2_average = apply(INQ_d_2_average, 1, mean, na.rm = TRUE)

SSMI_d_average = tlc_data_analysis[,85:89]
SSMI_d_average =  apply(SSMI_d_average, 1, mean, na.rm = TRUE)


SIS_d_average = tlc_data_analysis[,90:96]

#a,b,c,d
SIS_d_1_average = SIS_d_average[,1:4]
SIS_d_1_average = apply(SIS_d_1_average, 1, mean, na.rm =TRUE)

#e,f,g
SIS_d_2_average = SIS_d_average[,5:7]
SIS_d_2_average = apply(SIS_d_2_average, 1, mean, na.rm =TRUE)

### Create difference scores
RAS_1_diff = RAS_d_1_average - RAS_b_1_average
RAS_2_diff = RAS_d_2_average - RAS_b_2_average
RAS_3_diff = RAS_d_3_average - RAS_b_3_average
RAS_5_diff = RAS_d_5_average - RAS_b_5_average



INQ_1_diff = INQ_d_1_average - INQ_b_1_average
INQ_2_diff = INQ_d_2_average - INQ_b_2_average
SSMI_diff = SSMI_d_average-SSMI_b_average

SIS_1_diff = SIS_d_1_average-SIS_b_1_average
SIS_2_diff = SIS_d_2_average-SIS_b_2_average

PHQ9_b = tlc_data_analysis$PHQ9_1
PHQ9_d = tlc_data_analysis$PHQ9_4
PHQ9_diff = tlc_data_analysis$PHQ9_4 - tlc_data_analysis$PHQ9_1

#### Create new data with average scores
#apply(tlc_data_analysis, 2, function(x){describe.factor(x)})
tlc_data_analysis_average = data.frame(tlc_data_analysis[,c(1,2,4:8, 99:104)], RAS_b_1_average, RAS_b_2_average, RAS_b_3_average, RAS_b_5_average, INQ_b_1_average, INQ_b_2_average, SSMI_b_average, SIS_b_1_average, SIS_b_2_average, PHQ9_b = tlc_data_analysis$PHQ9_1,RAS_d_1_average, RAS_d_2_average, RAS_d_3_average, RAS_d_5_average, INQ_d_1_average, INQ_d_2_average, SSMI_d_average, SIS_d_1_average, SIS_d_2_average,PHQ9_d = tlc_data_analysis$PHQ9_4)




### Get rid of EHR vars and PHQ
tlc_data_analysis_average[,c(8:13, 23,33)] = NULL
tlc_psycho=  tlc_data_analysis
dim(tlc_psycho)
```
Evaluate missing data
Get percentage of missing data for each variable
Test missing assumption
Get rid of missing data
```{r}
library(MissMech)
library(naniar)
##### Get rid of EHR vars 

### Assessing global missing data 

dim(tlc_data_analysis_average)
var_missing =  miss_var_summary(tlc_data_analysis_average)
var_missing = data.frame(var_missing)
var_missing
describe.factor(tlc_data_analysis_average$TXPackageAssigned)
### Change to Quasi ITT data set
```
Descriptive statistics

Who provided (program staff) what services (modality, type, intensity, duration), to whom (individual characteristics), in what context (system, community)?

 (1) a minimum of 5,660  youth will receive all components of the enhanced post-crisis follow-up intervention by the discharge survey 
 
 (3) a minimum of  5,660  youth enrolled in the enhanced post-crisis follow-up intervention will complete a safety plan and successfully implement all aspects of the plan at least 80% of the time;
```{r}

head(tlc_data_analysis_average)
### N 
dim(tlc_data_analysis_average)
### 1 put into an excel form
dim(tlc_data_analysis_average)
library(psych)
con_vars  = tlc_data_analysis_average[,c(3,8:24)]
con_vars_mean = apply(con_vars,2, mean, na.rm = TRUE)
con_vars_sd = apply(con_vars, 2, sd, na.rm = TRUE)
con_vars_range = apply(con_vars, 2, range, na.rm = TRUE)
con_vars_range = t(con_vars_range)
con_vars_range = data.frame(con_vars_range)
con_vars_range = round(con_vars_range, 3)
con_vars_range$range = paste0(con_vars_range$X1, sep = ",", con_vars_range$X2)
con_vars_range = con_vars_range$range
con_vars_range
con_vars_results = data.frame(con_vars_mean, con_vars_sd, con_vars_range)
con_vars_results[,1:2] = round(con_vars_results[,1:2],3)
con_vars_results
colnames(con_vars_results) = c("mean_count", "sd_percent", "range")
con_vars_results
write.csv(con_vars_results, "con_vars_results.csv")
##### Get cat vars
head(tlc_data_analysis_average)
cat_vars = tlc_data_analysis_average[,c(2,4:7)]
cat_vars = apply(cat_vars, 2, function(x){describe.factor(x)})
cat_vars = data.frame(cat_vars)
cat_vars = t(cat_vars)
cat_vars = data.frame(cat_vars)
cat_vars$Percent = round(cat_vars$Percent, 3)
write.csv(cat_vars, "cat_vars.csv")
```



###########################
Imputted results
###########################
Imputation
```{r}
library(Amelia)
impute_dat = tlc_data_analysis_average
dim(impute_dat)
describe.factor(impute_dat$TXPackageAssigned)
### Try coding as binary for everything besies TXPackageAssigned package
##
impute_dat$female = ifelse(impute_dat$Gender == 2, 1, 0)
impute_dat$Gender = NULL
impute_dat$non_white = ifelse(impute_dat$RaceEthnicity == 3,0,1)
impute_dat$RaceEthnicity = NULL
impute_dat$sexual_minority = ifelse(impute_dat$SexualOrientation == 5,0,1)
impute_dat$SexualOrientation = NULL
impute_dat[5:22]

bounds = matrix(c(5,1,5, 6,1,5, 7,1,5, 8,1,5, 9,1,7, 10,1,7, 11,1,9, 12,1,5, 13,1,5, 14,1,5, 15,1,5, 16,1,5, 17,1,5, 18,1,7, 19,1,7, 20,1,9, 21,1,5, 22,1,5),nrow = 18, ncol = 3, byrow = TRUE)
bounds
#a.out = amelia(x = impute_dat, m = 5, noms = c("TXPackageAssigned" ,"female", "HispanicLatino", "non_white", "sexual_minority"), idvars = c("YouthID"), bounds = bounds)
setwd("P:/Evaluation/TN Lives Count_Connect/Databases")
a.out =  readRDS("a.out_tlc.rds")
impute_dat_loop = a.out$imputations
apply(impute_dat_loop$imp1, 2, range)
     
compare.density(a.out, var = "RAS_d_1_average")
compare.density(a.out, var = "RAS_d_2_average")
compare.density(a.out, var = "RAS_d_3_average")
compare.density(a.out, var = "RAS_d_5_average")
compare.density(a.out, var = "INQ_d_1_average")
compare.density(a.out, var = "INQ_d_2_average")
compare.density(a.out, var = "SIS_d_1_average")
compare.density(a.out, var = "SSMI_d_average")

```





######################
Within tlc results
######################
Phone only
Get means and sds then meld together so you don't have to deal with it, then you can calcaulate cohen's d by hand for all of them.
```{r}
#### Treatment 1
tlc_within_d1_base_t1 = subset(impute_dat_loop[[1]][,c(2,5:13)], TXPackageAssigned == 1)
tlc_within_d1_dis_t1 = subset(impute_dat_loop[[1]][,c(2,14:23)], TXPackageAssigned == 1)
tlc_within_d1_base_t1$TXPackageAssigned = NULL
tlc_within_d1_dis_t1$TXPackageAssigned = NULL

library(effsize)
tlc_within_results_d1_t1 = list()
for(i in 1:length(tlc_within_d1_base_t1)){
  tlc_within_results_d1_t1[[i]] = effsize::cohen.d(tlc_within_d1_dis_t1[[i]], tlc_within_d1_base_t1[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d1_t1[[i]] = tlc_within_results_d1_t1[[i]][c(3,5)]
}

tlc_within_results_d1_t1
tlc_within_results_d1_t1 = unlist(tlc_within_results_d1_t1)
tlc_within_results_d1_t1 = matrix(tlc_within_results_d1_t1, ncol = 3, byrow = TRUE)
tlc_within_results_d1_t1 = data.frame(tlc_within_results_d1_t1)
tlc_within_results_d1_t1 = round(tlc_within_results_d1_t1, 3)
tlc_within_results_d1_t1
colnames(tlc_within_results_d1_t1) = c("cohen_d", "lower", "upper")
tlc_within_results_d1_t1

tlc_within_d2_base_t1 = subset(impute_dat_loop[[2]][,c(2,5:13)], TXPackageAssigned == 1)
tlc_within_d2_dis_t1 = subset(impute_dat_loop[[2]][,c(2,14:22)], TXPackageAssigned == 1)
tlc_within_d2_base_t1$TXPackageAssigned = NULL
tlc_within_d2_dis_t1$TXPackageAssigned = NULL

tlc_within_results_d2_t1 = list()
for(i in 1:length(tlc_within_d2_base_t1)){
  tlc_within_results_d2_t1[[i]] = effsize::cohen.d(tlc_within_d2_dis_t1[[i]], tlc_within_d2_base_t1[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d2_t1[[i]] = tlc_within_results_d2_t1[[i]][c(3,5)]
}

tlc_within_results_d2_t1
tlc_within_results_d2_t1 = unlist(tlc_within_results_d2_t1)
tlc_within_results_d2_t1 = matrix(tlc_within_results_d2_t1, ncol = 3, byrow = TRUE)
tlc_within_results_d2_t1 = data.frame(tlc_within_results_d2_t1)
tlc_within_results_d2_t1 = round(tlc_within_results_d2_t1, 3)
tlc_within_results_d2_t1
colnames(tlc_within_results_d2_t1) = c("cohen_d", "lower", "upper")
tlc_within_results_d2_t1

tlc_within_d3_base_t1 = subset(impute_dat_loop[[3]][,c(2,5:13)], TXPackageAssigned == 1)
tlc_within_d3_dis_t1 = subset(impute_dat_loop[[3]][,c(2,14:22)], TXPackageAssigned == 1)
tlc_within_d3_base_t1$TXPackageAssigned = NULL
tlc_within_d3_dis_t1$TXPackageAssigned = NULL

tlc_within_results_d3_t1 = list()
for(i in 1:length(tlc_within_d3_base_t1)){
  tlc_within_results_d3_t1[[i]] = effsize::cohen.d(tlc_within_d3_dis_t1[[i]], tlc_within_d3_base_t1[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d3_t1[[i]] = tlc_within_results_d3_t1[[i]][c(3,5)]
}

tlc_within_results_d3_t1
tlc_within_results_d3_t1 = unlist(tlc_within_results_d3_t1)
tlc_within_results_d3_t1 = matrix(tlc_within_results_d3_t1, ncol = 3, byrow = TRUE)
tlc_within_results_d3_t1 = data.frame(tlc_within_results_d3_t1)
tlc_within_results_d3_t1 = round(tlc_within_results_d3_t1, 3)
tlc_within_results_d3_t1
colnames(tlc_within_results_d3_t1) = c("cohen_d", "lower", "upper")
tlc_within_results_d3_t1

tlc_within_d4_base_t1 = subset(impute_dat_loop[[4]][,c(2,5:13)], TXPackageAssigned == 1)
tlc_within_d4_dis_t1 = subset(impute_dat_loop[[4]][,c(2,14:22)], TXPackageAssigned == 1)
tlc_within_d4_base_t1$TXPackageAssigned = NULL
tlc_within_d4_dis_t1$TXPackageAssigned = NULL

tlc_within_results_d4_t1 = list()
for(i in 1:length(tlc_within_d4_base_t1)){
  tlc_within_results_d4_t1[[i]] = effsize::cohen.d(tlc_within_d4_dis_t1[[i]], tlc_within_d4_base_t1[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d4_t1[[i]] = tlc_within_results_d4_t1[[i]][c(3,5)]
}

tlc_within_results_d4_t1
tlc_within_results_d4_t1 = unlist(tlc_within_results_d4_t1)
tlc_within_results_d4_t1 = matrix(tlc_within_results_d4_t1, ncol = 3, byrow = TRUE)
tlc_within_results_d4_t1 = data.frame(tlc_within_results_d4_t1)
tlc_within_results_d4_t1 = round(tlc_within_results_d4_t1, 3)
tlc_within_results_d4_t1
colnames(tlc_within_results_d4_t1) = c("cohen_d", "lower", "upper")
tlc_within_results_d4_t1


tlc_within_d5_base_t1 = subset(impute_dat_loop[[5]][,c(2,5:13)], TXPackageAssigned == 1)
tlc_within_d5_dis_t1 = subset(impute_dat_loop[[5]][,c(2,14:22)], TXPackageAssigned == 1)
tlc_within_d5_base_t1$TXPackageAssigned = NULL
tlc_within_d5_dis_t1$TXPackageAssigned = NULL

tlc_within_results_d5_t1 = list()
for(i in 1:length(tlc_within_d5_base_t1)){
  tlc_within_results_d5_t1[[i]] = effsize::cohen.d(tlc_within_d5_dis_t1[[i]], tlc_within_d5_base_t1[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d5_t1[[i]] = tlc_within_results_d5_t1[[i]][c(3,5)]
}

tlc_within_results_d5_t1
tlc_within_results_d5_t1 = unlist(tlc_within_results_d5_t1)
tlc_within_results_d5_t1 = matrix(tlc_within_results_d5_t1, ncol = 3, byrow = TRUE)
tlc_within_results_d5_t1 = data.frame(tlc_within_results_d5_t1)
tlc_within_results_d5_t1 = round(tlc_within_results_d5_t1, 3)
tlc_within_results_d5_t1
colnames(tlc_within_results_d5_t1) = c("cohen_d", "lower", "upper")
tlc_within_results_d5_t1

######### Now combine just average
tlc_within_t1_cohen_d = data.frame(cohen_d1 = tlc_within_results_d1_t1$cohen_d, cohen_d2 = tlc_within_results_d2_t1$cohen_d, cohen_d3 = tlc_within_results_d3_t1$cohen_d, cohen_d4 = tlc_within_results_d4_t1$cohen_d, cohen_d5 = tlc_within_results_d5_t1$cohen_d)
tlc_within_t1_cohen_d = rowMeans(tlc_within_t1_cohen_d)
tlc_within_t1_cohen_d

tlc_within_t1_upper = data.frame(upper1 = tlc_within_results_d1_t1$upper, upper2 = tlc_within_results_d2_t1$upper, upper3 = tlc_within_results_d3_t1$upper, upper4 = tlc_within_results_d4_t1$upper, upper5 = tlc_within_results_d5_t1$upper)
tlc_within_t1_upper = rowMeans(tlc_within_t1_upper)
tlc_within_t1_upper

tlc_within_t1_lower = data.frame(lower1 = tlc_within_results_d1_t1$lower, lower2 = tlc_within_results_d2_t1$lower, lower3 = tlc_within_results_d3_t1$lower, lower4 = tlc_within_results_d4_t1$lower, lower5 = tlc_within_results_d5_t1$lower)
tlc_within_t1_lower = rowMeans(tlc_within_t1_lower)
tlc_within_t1_lower

tlc_within_t1_results = data.frame(cohen_d = tlc_within_t1_cohen_d, upper = tlc_within_t1_upper, lower = tlc_within_t1_lower)
tlc_within_t1_results = round(tlc_within_t1_results, 3)
tlc_within_t1_results

tlc_within_t1_results$cohen_d = ifelse(tlc_within_t1_results$upper > 0 & tlc_within_t1_results$lower < 0, tlc_within_t1_results$cohen_d, paste0(tlc_within_t1_results$cohen_d, "*"))
tlc_within_t1_results$ci_95 = paste0(tlc_within_t1_results$lower, sep = ",", tlc_within_t1_results$upper)
tlc_within_t1_results[,2:3] = NULL
tlc_within_t1_results

#### Treatment 2
tlc_within_d1_base_t2 = subset(impute_dat_loop[[1]][,c(2,5:13)], TXPackageAssigned == 2)
tlc_within_d1_dis_t2 = subset(impute_dat_loop[[1]][,c(2,14:22)], TXPackageAssigned == 2)
tlc_within_d1_base_t2$TXPackageAssigned = NULL
tlc_within_d1_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d1_t2 = list()
for(i in 1:length(tlc_within_d1_base_t2)){
  tlc_within_results_d1_t2[[i]] = effsize::cohen.d(tlc_within_d1_dis_t2[[i]], tlc_within_d1_base_t2[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d1_t2[[i]] = tlc_within_results_d1_t2[[i]][c(3,5)]
}

tlc_within_results_d1_t2
tlc_within_results_d1_t2 = unlist(tlc_within_results_d1_t2)
tlc_within_results_d1_t2 = matrix(tlc_within_results_d1_t2, ncol = 3, byrow = TRUE)
tlc_within_results_d1_t2 = data.frame(tlc_within_results_d1_t2)
tlc_within_results_d1_t2 = round(tlc_within_results_d1_t2, 3)
tlc_within_results_d1_t2
colnames(tlc_within_results_d1_t2) = c("cohen_d", "lower", "upper")
tlc_within_results_d1_t2

tlc_within_d2_base_t2 = subset(impute_dat_loop[[2]][,c(2,5:13)], TXPackageAssigned == 2)
tlc_within_d2_dis_t2 = subset(impute_dat_loop[[2]][,c(2,14:22)], TXPackageAssigned == 2)
tlc_within_d2_base_t2$TXPackageAssigned = NULL
tlc_within_d2_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d2_t2 = list()
for(i in 1:length(tlc_within_d2_base_t2)){
  tlc_within_results_d2_t2[[i]] = effsize::cohen.d(tlc_within_d2_dis_t2[[i]], tlc_within_d2_base_t2[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d2_t2[[i]] = tlc_within_results_d2_t2[[i]][c(3,5)]
}

tlc_within_results_d2_t2
tlc_within_results_d2_t2 = unlist(tlc_within_results_d2_t2)
tlc_within_results_d2_t2 = matrix(tlc_within_results_d2_t2, ncol = 3, byrow = TRUE)
tlc_within_results_d2_t2 = data.frame(tlc_within_results_d2_t2)
tlc_within_results_d2_t2 = round(tlc_within_results_d2_t2, 3)
tlc_within_results_d2_t2
colnames(tlc_within_results_d2_t2) = c("cohen_d", "lower", "upper")
tlc_within_results_d2_t2

tlc_within_d3_base_t2 = subset(impute_dat_loop[[3]][,c(2,5:13)], TXPackageAssigned == 2)
tlc_within_d3_dis_t2 = subset(impute_dat_loop[[3]][,c(2,14:22)], TXPackageAssigned == 2)
tlc_within_d3_base_t2$TXPackageAssigned = NULL
tlc_within_d3_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d3_t2 = list()
for(i in 1:length(tlc_within_d3_base_t2)){
  tlc_within_results_d3_t2[[i]] = effsize::cohen.d(tlc_within_d3_dis_t2[[i]], tlc_within_d3_base_t2[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d3_t2[[i]] = tlc_within_results_d3_t2[[i]][c(3,5)]
}

tlc_within_results_d3_t2
tlc_within_results_d3_t2 = unlist(tlc_within_results_d3_t2)
tlc_within_results_d3_t2 = matrix(tlc_within_results_d3_t2, ncol = 3, byrow = TRUE)
tlc_within_results_d3_t2 = data.frame(tlc_within_results_d3_t2)
tlc_within_results_d3_t2 = round(tlc_within_results_d3_t2, 3)
tlc_within_results_d3_t2
colnames(tlc_within_results_d3_t2) = c("cohen_d", "lower", "upper")
tlc_within_results_d3_t2

tlc_within_d4_base_t2 = subset(impute_dat_loop[[4]][,c(2,5:13)], TXPackageAssigned == 2)
tlc_within_d4_dis_t2 = subset(impute_dat_loop[[4]][,c(2,14:22)], TXPackageAssigned == 2)
tlc_within_d4_base_t2$TXPackageAssigned = NULL
tlc_within_d4_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d4_t2 = list()
for(i in 1:length(tlc_within_d4_base_t2)){
  tlc_within_results_d4_t2[[i]] = effsize::cohen.d(tlc_within_d4_dis_t2[[i]], tlc_within_d4_base_t2[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d4_t2[[i]] = tlc_within_results_d4_t2[[i]][c(3,5)]
}

tlc_within_results_d4_t2
tlc_within_results_d4_t2 = unlist(tlc_within_results_d4_t2)
tlc_within_results_d4_t2 = matrix(tlc_within_results_d4_t2, ncol = 3, byrow = TRUE)
tlc_within_results_d4_t2 = data.frame(tlc_within_results_d4_t2)
tlc_within_results_d4_t2 = round(tlc_within_results_d4_t2, 3)
tlc_within_results_d4_t2
colnames(tlc_within_results_d4_t2) = c("cohen_d", "lower", "upper")
tlc_within_results_d4_t2


tlc_within_d5_base_t2 = subset(impute_dat_loop[[5]][,c(2,5:13)], TXPackageAssigned == 2)
tlc_within_d5_dis_t2 = subset(impute_dat_loop[[5]][,c(2,14:22)], TXPackageAssigned == 2)
tlc_within_d5_base_t2$TXPackageAssigned = NULL
tlc_within_d5_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d5_t2 = list()
for(i in 1:length(tlc_within_d5_base_t2)){
  tlc_within_results_d5_t2[[i]] = effsize::cohen.d(tlc_within_d5_dis_t2[[i]], tlc_within_d5_base_t2[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d5_t2[[i]] = tlc_within_results_d5_t2[[i]][c(3,5)]
}

tlc_within_results_d5_t2
tlc_within_results_d5_t2 = unlist(tlc_within_results_d5_t2)
tlc_within_results_d5_t2 = matrix(tlc_within_results_d5_t2, ncol = 3, byrow = TRUE)
tlc_within_results_d5_t2 = data.frame(tlc_within_results_d5_t2)
tlc_within_results_d5_t2 = round(tlc_within_results_d5_t2, 3)
tlc_within_results_d5_t2
colnames(tlc_within_results_d5_t2) = c("cohen_d", "lower", "upper")
tlc_within_results_d5_t2

######### Now combine just average
tlc_within_t2_cohen_d = data.frame(cohen_d1 = tlc_within_results_d1_t2$cohen_d, cohen_d2 = tlc_within_results_d2_t2$cohen_d, cohen_d3 = tlc_within_results_d3_t2$cohen_d, cohen_d4 = tlc_within_results_d4_t2$cohen_d, cohen_d5 = tlc_within_results_d5_t2$cohen_d)
tlc_within_t2_cohen_d = rowMeans(tlc_within_t2_cohen_d)
tlc_within_t2_cohen_d

tlc_within_t2_upper = data.frame(upper1 = tlc_within_results_d1_t2$upper, upper2 = tlc_within_results_d2_t2$upper, upper3 = tlc_within_results_d3_t2$upper, upper4 = tlc_within_results_d4_t2$upper, upper5 = tlc_within_results_d5_t2$upper)
tlc_within_t2_upper = rowMeans(tlc_within_t2_upper)
tlc_within_t2_upper

tlc_within_t2_lower = data.frame(lower1 = tlc_within_results_d1_t2$lower, lower2 = tlc_within_results_d2_t2$lower, lower3 = tlc_within_results_d3_t2$lower, lower4 = tlc_within_results_d4_t2$lower, lower5 = tlc_within_results_d5_t2$lower)
tlc_within_t2_lower = rowMeans(tlc_within_t2_lower)
tlc_within_t2_lower

tlc_within_t2_results = data.frame(cohen_d = tlc_within_t2_cohen_d, upper = tlc_within_t2_upper, lower = tlc_within_t2_lower)
tlc_within_t2_results = round(tlc_within_t2_results, 3)
tlc_within_t2_results$cohen_d = ifelse(tlc_within_t2_results$upper > 0 & tlc_within_t2_results$lower < 0, tlc_within_t2_results$cohen_d, paste0(tlc_within_t2_results$cohen_d, "*"))
tlc_within_t2_results$ci_95 = paste0(tlc_within_t2_results$lower, sep = ",", tlc_within_t2_results$upper)
tlc_within_t2_results[,2:3] = NULL
tlc_within_t2_results

#### Treatment 3
tlc_within_d1_base_t3 = subset(impute_dat_loop[[1]][,c(2,5:13)], TXPackageAssigned == 3)
tlc_within_d1_dis_t3 = subset(impute_dat_loop[[1]][,c(2,14:22)], TXPackageAssigned == 3)
tlc_within_d1_base_t3$TXPackageAssigned = NULL
tlc_within_d1_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d1_t3 = list()
for(i in 1:length(tlc_within_d1_base_t3)){
  tlc_within_results_d1_t3[[i]] = effsize::cohen.d(tlc_within_d1_dis_t3[[i]], tlc_within_d1_base_t3[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d1_t3[[i]] = tlc_within_results_d1_t3[[i]][c(3,5)]
}

tlc_within_results_d1_t3
tlc_within_results_d1_t3 = unlist(tlc_within_results_d1_t3)
tlc_within_results_d1_t3 = matrix(tlc_within_results_d1_t3, ncol = 3, byrow = TRUE)
tlc_within_results_d1_t3 = data.frame(tlc_within_results_d1_t3)
tlc_within_results_d1_t3 = round(tlc_within_results_d1_t3, 3)
tlc_within_results_d1_t3
colnames(tlc_within_results_d1_t3) = c("cohen_d", "lower", "upper")
tlc_within_results_d1_t3

tlc_within_d2_base_t3 = subset(impute_dat_loop[[2]][,c(2,5:13)], TXPackageAssigned == 3)
tlc_within_d2_dis_t3 = subset(impute_dat_loop[[2]][,c(2,14:22)], TXPackageAssigned == 3)
tlc_within_d2_base_t3$TXPackageAssigned = NULL
tlc_within_d2_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d2_t3 = list()
for(i in 1:length(tlc_within_d2_base_t3)){
  tlc_within_results_d2_t3[[i]] = effsize::cohen.d(tlc_within_d2_dis_t3[[i]], tlc_within_d2_base_t3[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d2_t3[[i]] = tlc_within_results_d2_t3[[i]][c(3,5)]
}

tlc_within_results_d2_t3
tlc_within_results_d2_t3 = unlist(tlc_within_results_d2_t3)
tlc_within_results_d2_t3 = matrix(tlc_within_results_d2_t3, ncol = 3, byrow = TRUE)
tlc_within_results_d2_t3 = data.frame(tlc_within_results_d2_t3)
tlc_within_results_d2_t3 = round(tlc_within_results_d2_t3, 3)
tlc_within_results_d2_t3
colnames(tlc_within_results_d2_t3) = c("cohen_d", "lower", "upper")
tlc_within_results_d2_t3

tlc_within_d3_base_t3 = subset(impute_dat_loop[[3]][,c(2,5:13)], TXPackageAssigned == 3)
tlc_within_d3_dis_t3 = subset(impute_dat_loop[[3]][,c(2,14:22)], TXPackageAssigned == 3)
tlc_within_d3_base_t3$TXPackageAssigned = NULL
tlc_within_d3_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d3_t3 = list()
for(i in 1:length(tlc_within_d3_base_t3)){
  tlc_within_results_d3_t3[[i]] = effsize::cohen.d(tlc_within_d3_dis_t3[[i]], tlc_within_d3_base_t3[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d3_t3[[i]] = tlc_within_results_d3_t3[[i]][c(3,5)]
}

tlc_within_results_d3_t3
tlc_within_results_d3_t3 = unlist(tlc_within_results_d3_t3)
tlc_within_results_d3_t3 = matrix(tlc_within_results_d3_t3, ncol = 3, byrow = TRUE)
tlc_within_results_d3_t3 = data.frame(tlc_within_results_d3_t3)
tlc_within_results_d3_t3 = round(tlc_within_results_d3_t3, 3)
tlc_within_results_d3_t3
colnames(tlc_within_results_d3_t3) = c("cohen_d", "lower", "upper")
tlc_within_results_d3_t3

tlc_within_d4_base_t3 = subset(impute_dat_loop[[4]][,c(2,5:13)], TXPackageAssigned == 3)
tlc_within_d4_dis_t3 = subset(impute_dat_loop[[4]][,c(2,14:22)], TXPackageAssigned == 3)
tlc_within_d4_base_t3$TXPackageAssigned = NULL
tlc_within_d4_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d4_t3 = list()
for(i in 1:length(tlc_within_d4_base_t3)){
  tlc_within_results_d4_t3[[i]] = effsize::cohen.d(tlc_within_d4_dis_t3[[i]], tlc_within_d4_base_t3[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d4_t3[[i]] = tlc_within_results_d4_t3[[i]][c(3,5)]
}

tlc_within_results_d4_t3
tlc_within_results_d4_t3 = unlist(tlc_within_results_d4_t3)
tlc_within_results_d4_t3 = matrix(tlc_within_results_d4_t3, ncol = 3, byrow = TRUE)
tlc_within_results_d4_t3 = data.frame(tlc_within_results_d4_t3)
tlc_within_results_d4_t3 = round(tlc_within_results_d4_t3, 3)
tlc_within_results_d4_t3
colnames(tlc_within_results_d4_t3) = c("cohen_d", "lower", "upper")
tlc_within_results_d4_t3


tlc_within_d5_base_t3 = subset(impute_dat_loop[[5]][,c(2,5:13)], TXPackageAssigned == 3)
tlc_within_d5_dis_t3 = subset(impute_dat_loop[[5]][,c(2,14:22)], TXPackageAssigned == 3)
tlc_within_d5_base_t3$TXPackageAssigned = NULL
tlc_within_d5_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d5_t3 = list()
for(i in 1:length(tlc_within_d5_base_t3)){
  tlc_within_results_d5_t3[[i]] = effsize::cohen.d(tlc_within_d5_dis_t3[[i]], tlc_within_d5_base_t3[[i]], paired = TRUE, conf.level = .95)
  tlc_within_results_d5_t3[[i]] = tlc_within_results_d5_t3[[i]][c(3,5)]
}

tlc_within_results_d5_t3
tlc_within_results_d5_t3 = unlist(tlc_within_results_d5_t3)
tlc_within_results_d5_t3 = matrix(tlc_within_results_d5_t3, ncol = 3, byrow = TRUE)
tlc_within_results_d5_t3 = data.frame(tlc_within_results_d5_t3)
tlc_within_results_d5_t3 = round(tlc_within_results_d5_t3, 3)
tlc_within_results_d5_t3
colnames(tlc_within_results_d5_t3) = c("cohen_d", "lower", "upper")
tlc_within_results_d5_t3

######### Now combine just average
tlc_within_t3_cohen_d = data.frame(cohen_d1 = tlc_within_results_d1_t3$cohen_d, cohen_d2 = tlc_within_results_d2_t3$cohen_d, cohen_d3 = tlc_within_results_d3_t3$cohen_d, cohen_d4 = tlc_within_results_d4_t3$cohen_d, cohen_d5 = tlc_within_results_d5_t3$cohen_d)
tlc_within_t3_cohen_d = rowMeans(tlc_within_t3_cohen_d)
tlc_within_t3_cohen_d

tlc_within_t3_upper = data.frame(upper1 = tlc_within_results_d1_t3$upper, upper2 = tlc_within_results_d2_t3$upper, upper3 = tlc_within_results_d3_t3$upper, upper4 = tlc_within_results_d4_t3$upper, upper5 = tlc_within_results_d5_t3$upper)
tlc_within_t3_upper = rowMeans(tlc_within_t3_upper)
tlc_within_t3_upper

tlc_within_t3_lower = data.frame(lower1 = tlc_within_results_d1_t3$lower, lower2 = tlc_within_results_d2_t3$lower, lower3 = tlc_within_results_d3_t3$lower, lower4 = tlc_within_results_d4_t3$lower, lower5 = tlc_within_results_d5_t3$lower)
tlc_within_t3_lower = rowMeans(tlc_within_t3_lower)
tlc_within_t3_lower

tlc_within_t3_results = data.frame(cohen_d = tlc_within_t3_cohen_d, upper = tlc_within_t3_upper, lower = tlc_within_t3_lower)
tlc_within_t3_results = round(tlc_within_t3_results, 3)

compare_complete_missing_within_tlc =  tlc_within_t3_results$cohen_d 



tlc_within_t3_results$cohen_d = ifelse(tlc_within_t3_results$upper > 0 & tlc_within_t3_results$lower < 0, tlc_within_t3_results$cohen_d, paste0(tlc_within_t3_results$cohen_d, "*"))
tlc_within_t3_results$ci_95 = paste0(tlc_within_t3_results$lower, sep = ",", tlc_within_t3_results$upper)
tlc_within_t3_results[,2:3] = NULL

tlc_within_results = rbind(tlc_within_t1_results, tlc_within_t2_results, tlc_within_t3_results)

write.csv(tlc_within_results, "tlc_within_results.csv", row.names = FALSE)

```
##################
TLC Complete within
##################
```{r}

impute_dat_complete = na.omit(impute_dat)
impute_dat_complete$SIS_b_2_average = NULL
impute_dat_complete$SIS_d_2_average = NULL
dim(impute_dat_complete)
#### Treatment 1
tlc_within_base_t1 = subset(impute_dat_complete[,c(2,5:12)], TXPackageAssigned == 1)
head(tlc_within_base_t1)
tlc_within_dis_t1= subset(impute_dat_complete[,c(2,13:20)], TXPackageAssigned == 1)
head(tlc_within_dis_t1)
tlc_within_base_t1$TXPackageAssigned = NULL
tlc_within_dis_t1$TXPackageAssigned = NULL
tlc_within_base_t1 = as.list(tlc_within_base_t1)
tlc_within_dis_t1= as.list(tlc_within_dis_t1)
tlc_within_base_t1_results = list()
tlc_within_t1_cohen_d = list()
for(i in 1:length(tlc_within_base_t1)){
  tlc_within_base_t1_results[[i]] = effsize::cohen.d(tlc_within_dis_t1[[i]], tlc_within_base_t1[[i]] , paired = TRUE,  conf.level = .95, na.rm = TRUE)
  tlc_within_t1_cohen_d[[i]] = tlc_within_base_t1_results[[i]][c(3)]
  tlc_within_base_t1_results[[i]] = tlc_within_base_t1_results[[i]][c(3,5)]
}
tlc_within_base_t1_results
tlc_within_t1_cohen_d = unlist(tlc_within_t1_cohen_d)
mean(abs(tlc_within_t1_cohen_d))

impute_dat_complete = na.omit(impute_dat)
impute_dat_complete$SIS_b_2_average = NULL
impute_dat_complete$SIS_d_2_average = NULL
dim(impute_dat_complete)
#### Treatment 1
tlc_within_base_t2 = subset(impute_dat_complete[,c(2,5:12)], TXPackageAssigned == 2)
head(tlc_within_base_t2)
tlc_within_dis_t2= subset(impute_dat_complete[,c(2,13:20)], TXPackageAssigned == 2)
head(tlc_within_dis_t2)
tlc_within_base_t2$TXPackageAssigned = NULL
tlc_within_dis_t2$TXPackageAssigned = NULL
tlc_within_base_t2 = as.list(tlc_within_base_t2)
tlc_within_dis_t2= as.list(tlc_within_dis_t2)
tlc_within_base_t2_results = list()
tlc_within_t2_cohen_d = list()
for(i in 1:length(tlc_within_base_t2)){
  tlc_within_base_t2_results[[i]] = effsize::cohen.d(tlc_within_dis_t2[[i]], tlc_within_base_t2[[i]] , paired = TRUE,  conf.level = .95, na.rm = TRUE)
  tlc_within_t2_cohen_d[[i]] = tlc_within_base_t2_results[[i]][c(3)]
  tlc_within_base_t2_results[[i]] = tlc_within_base_t2_results[[i]][c(3,5)]
}
tlc_within_base_t2_results
tlc_within_t2_cohen_d = unlist(tlc_within_t2_cohen_d)

impute_dat_complete = na.omit(impute_dat)
impute_dat_complete$SIS_b_2_average = NULL
impute_dat_complete$SIS_d_2_average = NULL
dim(impute_dat_complete)
#### Treatment 1
tlc_within_base_t3 = subset(impute_dat_complete[,c(2,5:12)], TXPackageAssigned == 2)
head(tlc_within_base_t3)
tlc_within_dis_t3= subset(impute_dat_complete[,c(2,13:20)], TXPackageAssigned == 2)
head(tlc_within_dis_t3)
tlc_within_base_t3$TXPackageAssigned = NULL
tlc_within_dis_t3$TXPackageAssigned = NULL
tlc_within_base_t3 = as.list(tlc_within_base_t3)
tlc_within_dis_t3= as.list(tlc_within_dis_t3)
tlc_within_base_t3_results = list()
tlc_within_t3_cohen_d = list()
for(i in 1:length(tlc_within_base_t3)){
  tlc_within_base_t3_results[[i]] = effsize::cohen.d(tlc_within_dis_t3[[i]], tlc_within_base_t3[[i]] , paired = TRUE,  conf.level = .95, na.rm = TRUE)
  tlc_within_t3_cohen_d[[i]] = tlc_within_base_t3_results[[i]][c(3)]
  tlc_within_base_t3_results[[i]] = tlc_within_base_t3_results[[i]][c(3,5)]
}
tlc_within_base_t3_results
tlc_within_t3_cohen_d = unlist(tlc_within_t3_cohen_d)

tlc_within_cohen_d_all = data.frame(tlc_within_t1_cohen_d, tlc_within_t2_cohen_d, tlc_within_t3_cohen_d)
tlc_within_cohen_d_all = unlist(tlc_within_cohen_d_all) 
diff_complete_missing_within_tlc =  mean(abs(tlc_within_cohen_d_all)) - mean(abs(compare_complete_missing_within_tlc))
diff_complete_missing_within_tlc
```


######################
Between tlc
######################



```{r}

### Create difference scores
out_diff_dat = list()
impute_dat_loop[[1]][5:14]
for(i in 1:length(impute_dat_loop)){
  out_diff_dat[[i]] = impute_dat_loop[[i]][14:22]-impute_dat_loop[[i]][5:13]
  colnames(out_diff_dat[[i]]) = c("RAS_1_diff", "RAS_2_diff", "RAS_3_diff", "RAS_5_diff", "INQ_1_diff", "INQ_2_diff", "SSMI_diff", "SIS_1_diff", "SIS_2_diff")
  out_diff_dat[[i]] = scale(out_diff_dat[[i]])
  out_diff_dat[[i]] =cbind(impute_dat_loop[[i]], out_diff_dat[[i]])
}
out_diff_dat
impute_tlc_between_results = list()
impute_tlc_between_results_sum = list()
se_con = list()
for(i in 1:length(out_diff_dat)){
  impute_tlc_between_results[[i]]=lm(cbind(RAS_1_diff, RAS_2_diff,RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SSMI_diff, SIS_1_diff, SIS_2_diff) ~ factor(TXPackageAssigned), data = out_diff_dat[[i]])
}

impute_tlc_between_results_1 = summary(impute_tlc_between_results[[1]])
impute_tlc_between_results_2 = summary(impute_tlc_between_results[[2]])
impute_tlc_between_results_3 = summary(impute_tlc_between_results[[3]])
impute_tlc_between_results_4 = summary(impute_tlc_between_results[[4]])
impute_tlc_between_results_5 = summary(impute_tlc_between_results[[5]])


coefs_1 = list()
ses_1 = list()
for(i in 1:length(impute_tlc_between_results_1)){
  coefs_1[[i]] = impute_tlc_between_results_1[[i]]$coefficients[2:3,1]
  ses_1[[i]] = impute_tlc_between_results_1[[i]]$coefficients[2:3,2]
}
coefs_1
coefs_1 = unlist(coefs_1)
coefs_1 = matrix(coefs_1, ncol = 18)
coefs_1

ses_1
ses_1 = unlist(ses_1)
ses_1 = matrix(ses_1, ncol = 18)
ses_1


coefs_2 = list()
ses_2 = list()
for(i in 1:length(impute_tlc_between_results_2)){
  coefs_2[[i]] = impute_tlc_between_results_2[[i]]$coefficients[2:3,1]
  ses_2[[i]] = impute_tlc_between_results_2[[i]]$coefficients[2:3,2]
}
coefs_2
coefs_2 = unlist(coefs_2)
coefs_2 = matrix(coefs_2, ncol = 18)
coefs_2

ses_2
ses_2 = unlist(ses_2)
ses_2 = matrix(ses_2, ncol = 18)
ses_2

coefs_3 = list()
ses_3 = list()
for(i in 1:length(impute_tlc_between_results_3)){
  coefs_3[[i]] = impute_tlc_between_results_3[[i]]$coefficients[2:3,1]
  ses_3[[i]] = impute_tlc_between_results_3[[i]]$coefficients[2:3,2]
}
coefs_3
coefs_3 = unlist(coefs_3)
coefs_3 = matrix(coefs_3, ncol = 18)
coefs_3

ses_3
ses_3 = unlist(ses_3)
ses_3 = matrix(ses_3, ncol = 18)
ses_3

coefs_4 = list()
ses_4 = list()
for(i in 1:length(impute_tlc_between_results_4)){
  coefs_4[[i]] = impute_tlc_between_results_4[[i]]$coefficients[2:3,1]
  ses_4[[i]] = impute_tlc_between_results_4[[i]]$coefficients[2:3,2]
}
coefs_4
coefs_4 = unlist(coefs_4)
coefs_4 = matrix(coefs_4, ncol = 18)
coefs_4

ses_4
ses_4 = unlist(ses_4)
ses_4 = matrix(ses_4, ncol = 18)
ses_4

coefs_5 = list()
ses_5 = list()
for(i in 1:length(impute_tlc_between_results_5)){
  coefs_5[[i]] = impute_tlc_between_results_5[[i]]$coefficients[2:3,1]
  ses_5[[i]] = impute_tlc_between_results_5[[i]]$coefficients[2:3,2]
}
coefs_5
coefs_5 = unlist(coefs_5)
coefs_5 = matrix(coefs_5, ncol = 18)
coefs_5

ses_5
ses_5 = unlist(ses_5)
ses_5 = matrix(ses_5, ncol = 18)
ses_5

coefs_all = rbind(coefs_1, coefs_2, coefs_3, coefs_4, coefs_5)
ses_all = rbind(ses_1, ses_2, ses_3, ses_4, ses_5)
coefs_ses =  mi.meld(coefs_all,ses_all)
t_stats = coefs_ses$q.mi / coefs_ses$se.mi
# n = 206 minus 5 for parameters
p_values = round(2*pt(-abs(t_stats), df = 201),3)
#Critica t
critical_ts= abs(qt(0.05/2, 201))
critical_ts
upper = round(coefs_ses$q.mi+(critical_ts*coefs_ses$se.mi),3)
lower = round(coefs_ses$q.mi-(critical_ts*coefs_ses$se.mi),3)
ci_95 = paste0(lower, sep=",", upper)

tlc_between_impute_results = data.frame(t(coefs_ses$q.mi), t(coefs_ses$se.mi), t(p_values), ci_95)
colnames(tlc_between_impute_results) = c("parameter_estimate", "se", "p_value", "ci_95")
tlc_between_impute_results[,1:2] = round(tlc_between_impute_results[,1:2], 3)
compare_tlc_between_impute = tlc_between_impute_results$parameter_estimate

tlc_between_impute_results$parameter_estimate = ifelse(tlc_between_impute_results$p_value < .05, paste0(tlc_between_impute_results$parameter_estimate, "*"), tlc_between_impute_results$parameter_estimate)
tlc_between_impute_results
write.csv(tlc_between_impute_results, "tlc_between_impute_results.csv", row.names = FALSE)

```
##############
Check normality
```{r}
outcomes_tests = out_diff_dat[[1]][,26:33]
hist_results = list() 
qq_results = list()
shap_results = list()
for(i in 1:length(outcomes_tests)){
  hist_results[[i]] = hist(outcomes_tests[[i]],  main = paste("Histogram of" , names(outcomes_tests)[[i]]))
  qq_results[[i]] = qqnorm(outcomes_tests[[i]], main = names(outcomes_tests)[[i]])
  shap_results[[i]] = shapiro.test(outcomes_tests[[i]])
}
shap_results
```
####################
Between TLC complete
####################
```{r}
out_diff_dat = impute_dat_complete[8:15]-impute_dat_complete[16:23]
colnames(out_diff_dat) = c("RAS_1_diff", "RAS_2_diff", "RAS_3_diff", "RAS_5_diff", "INQ_1_diff", "INQ_2_diff", "SSMI_diff", "SIS_1_diff")
out_diff_dat
out_diff_dat = scale(out_diff_dat)
out_diff_dat =cbind(impute_dat_complete, out_diff_dat)
tlc_complete_between_results=lm(cbind(RAS_1_diff, RAS_2_diff,RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SSMI_diff, SIS_1_diff) ~ factor(TXPackageAssigned), data = out_diff_dat)

tlc_complete_between_results_sum = summary(tlc_complete_between_results)

treat_2_3_out = list()
for(i in 1:8){
treat_2_3_out[[i]]= tlc_complete_between_results_sum[i][[1]][[4]][c(2,3),1]
}
treat_2_3_out = unlist(treat_2_3_out)
treat_2_3_out = matrix(treat_2_3_out, ncol = 2, byrow = TRUE)
treat_2_3_out_mean = mean(abs(unlist(treat_2_3_out)))
treat_2_3_out_mean


```


############################
Between TLC Contrasts
###########################
```{r}
#### Get contrasts
se_con_between_d1 = list()
mean_con_bewteen_d1 = list()
for(i in 1:length(impute_tlc_between_results_1)){
  se_con_between_d1[[i]] = vcov(impute_tlc_between_results_1[[i]])
  se_con_between_d1[[i]] = sqrt((se_con_between_d1[[i]][,2:3][2]+se_con_between_d1[[i]][,2:3][6])-2*se_con_between_d1[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d1[[i]] = impute_tlc_between_results_1[[i]]$coefficients[2:3,1]
 }
mean_con_bewteen_d1 = unlist(mean_con_bewteen_d1)
mean_con_bewteen_d1 = matrix(mean_con_bewteen_d1, ncol= 2, byrow = TRUE)
mean_con_bewteen_d1 = mean_con_bewteen_d1[,1] - mean_con_bewteen_d1[,2]
se_con_between_d1 = unlist(se_con_between_d1)

#### D2
se_con_between_d2 = list()
mean_con_bewteen_d2 = list()
for(i in 1:length(impute_tlc_between_results_2)){
  se_con_between_d2[[i]] = vcov(impute_tlc_between_results_2[[i]])
  se_con_between_d2[[i]] = sqrt((se_con_between_d2[[i]][,2:3][2]+se_con_between_d2[[i]][,2:3][6])-2*se_con_between_d2[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d2[[i]] = impute_tlc_between_results_2[[i]]$coefficients[2:3,1]
}
mean_con_bewteen_d2 = unlist(mean_con_bewteen_d2)
mean_con_bewteen_d2 = matrix(mean_con_bewteen_d2, ncol= 2, byrow = TRUE)
mean_con_bewteen_d2 = mean_con_bewteen_d2[,1] - mean_con_bewteen_d2[,2]
se_con_between_d2 = unlist(se_con_between_d2)

#### D3
se_con_between_d3 = list()
mean_con_bewteen_d3 = list()
for(i in 1:length(impute_tlc_between_results_3)){
  se_con_between_d3[[i]] = vcov(impute_tlc_between_results_3[[i]])
  se_con_between_d3[[i]] = sqrt((se_con_between_d3[[i]][,2:3][2]+se_con_between_d3[[i]][,2:3][6])-2*se_con_between_d3[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d3[[i]] = impute_tlc_between_results_3[[i]]$coefficients[2:3,1]
}
mean_con_bewteen_d3 = unlist(mean_con_bewteen_d3)
mean_con_bewteen_d3 = matrix(mean_con_bewteen_d3, ncol= 2, byrow = TRUE)
mean_con_bewteen_d3 = mean_con_bewteen_d3[,1] - mean_con_bewteen_d3[,2]
se_con_between_d3 = unlist(se_con_between_d3)

#### D4
se_con_between_d4 = list()
mean_con_bewteen_d4 = list()
for(i in 1:length(impute_tlc_between_results_4)){
  se_con_between_d4[[i]] = vcov(impute_tlc_between_results_4[[i]])
  se_con_between_d4[[i]] = sqrt((se_con_between_d4[[i]][,2:3][2]+se_con_between_d4[[i]][,2:3][6])-2*se_con_between_d4[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d4[[i]] = impute_tlc_between_results_4[[i]]$coefficients[2:3,1]
}
mean_con_bewteen_d4 = unlist(mean_con_bewteen_d4)
mean_con_bewteen_d4 = matrix(mean_con_bewteen_d4, ncol= 2, byrow = TRUE)
mean_con_bewteen_d4 = mean_con_bewteen_d4[,1] - mean_con_bewteen_d4[,2]
se_con_between_d4 = unlist(se_con_between_d4)

#### D5
se_con_between_d5 = list()
mean_con_bewteen_d5 = list()
for(i in 1:length(impute_tlc_between_results_5)){
  se_con_between_d5[[i]] = vcov(impute_tlc_between_results_5[[i]])
  se_con_between_d5[[i]] = sqrt((se_con_between_d5[[i]][,2:3][2]+se_con_between_d5[[i]][,2:3][6])-2*se_con_between_d5[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d5[[i]] = impute_tlc_between_results_5[[i]]$coefficients[2:3,1]
}
mean_con_bewteen_d5 = unlist(mean_con_bewteen_d5)
mean_con_bewteen_d5 = matrix(mean_con_bewteen_d5, ncol= 2, byrow = TRUE)
mean_con_bewteen_d5 = mean_con_bewteen_d5[,1] - mean_con_bewteen_d5[,2]
se_con_between_d5 = unlist(se_con_between_d5)

### Now combine means and se for mind melding
mean_con_bewteen = rbind(mean_con_bewteen_d1, mean_con_bewteen_d2, mean_con_bewteen_d3, mean_con_bewteen_d4, mean_con_bewteen_d5)
se_con_between = rbind(se_con_between_d1, se_con_between_d2, se_con_between_d3, se_con_between_d4, se_con_between_d5)

con_bewteen = mi.meld(mean_con_bewteen, se_con_between)
con_bewteen

con_between = mi.meld(mean_con_bewteen, se_con_between)
con_between
critical_t = abs(qt(0.05/2, dim(impute_dat_loop[[1]])[[1]]-5))
est_con = data.frame(est_con  = con_between$q.mi)
se_con = data.frame(se_con = con_between$se.mi)
est_se_con = data.frame(est_con = t(est_con), se_con = t(se_con))
t_stats = est_se_con$est_con / est_se_con$se_con
est_se_con$p_values = round(2*pt(-abs(t_stats), df = dim(impute_dat_loop[[1]])[[1]]-5),3)
est_se_con
est_se_con = round(est_se_con,3)
est_se_con
#### 95% ci's
upper = round(est_se_con$est_con +(critical_t*est_se_con$se_con),3)
upper
lower = round(est_se_con$est_con -(critical_t*est_se_con$se_con),3)
lower
ci_95 = paste0(upper, sep =",", lower)
ci_95
est_se_con$ci_95 = ci_95
impute_t3_t2_par =  est_se_con$est_con
est_se_con$est_con = ifelse(est_se_con$p_values < .05, paste0(est_se_con$est_con, "*"), est_se_con$est_con)
est_se_con$est_con
est_se_con
write.csv(est_se_con, "est_se_con.csv")

library(multcomp)
### Check that first is correct
test_tlc_con_dat = out_diff_dat[[1]]
test_tlc_con_model = lm(RAS_1_diff ~ factor(TXPackageAssigned), data = test_tlc_con_dat)
test_tlc_con_model
K = matrix(c(0, 1,-1), ncol = 3, nrow = 1, byrow = TRUE)
t= glht(test_tlc_con_model, linfct = K)
summary(t)



```
##############
Complete contrasts tlc between
################
```{r}

mean_con_bewteen_complete_out_tlc = list()
treat_2_3_out
mean_con_bewteen_complete_tlc = treat_2_3_out
mean_con_bewteen_complete_tlc = data.frame(mean_con_bewteen_complete_tlc)
mean_con_bewteen_complete_tlc = mean_con_bewteen_complete_tlc$X1-mean_con_bewteen_complete_tlc$X2
mean(abs(mean_con_bewteen_complete_tlc))

```


#############################################
Missing data cleaning and results for between
#############################################
```{r}

#### Impute
impute_t3_t2_par
compare_tlc_between_impute = cbind(compare_tlc_between_impute, impute_t3_t2_par)
mean_between_par_impute_tlc = mean(unlist(abs(compare_tlc_between_impute)))

### Complete
# Parameter one and two
treat_2_3_out
mean_con_bewteen_complete_tlc
mean_between_par_complete_tlc = mean(abs(unlist(mean_con_bewteen_complete_tlc, treat_2_3_out)))

diff_between_par_tlc = mean_between_par_complete_tlc-mean_between_par_impute_tlc

```


Just omegas for each construct
RAS 1,2,3,5; INQ 1,2; SSMI, and SIS 1 and SIS 2.
```{r}
library(psych)

RAS_b_pyscho = tlc_data_analysis[,9:28]
RAS_b_1_pyscho = RAS_b_pyscho[,c(6:13,20)]
summary(omega(RAS_b_1_pyscho))

RAS_b_2_pyscho = RAS_b_pyscho[,17:19]
summary(omega(RAS_b_2_pyscho))

RAS_b_3_pyscho = RAS_b_pyscho[,1:5]
summary(omega(RAS_b_3_pyscho))

RAS_b_5_pyscho = RAS_b_pyscho[,14:16]
summary(omega(RAS_b_5_pyscho))

INQ_b_1_pyscho = tlc_data_analysis[,29:34]
summary(omega(INQ_b_1_pyscho, poly = TRUE))

INQ_b_2_pyscho = tlc_data_analysis[,35:40]
INQ_b_2_pyscho = 8-INQ_b_2_pyscho
summary(omega(INQ_b_2_pyscho, poly = TRUE))

SSMI_b_pyscho = tlc_data_analysis[,41:45]
summary(omega(SSMI_b_pyscho))

SIS_b_pyscho = tlc_data_analysis[,46:52]

SIS_b_1_pyscho = SIS_b_pyscho[,1:4]
summary(omega(SIS_b_1_pyscho))


SIS_b_2_pyscho = SIS_b_pyscho[,5:7]
summary(omega(SIS_b_2_pyscho))


```



Pyschometrics
Test confirmatory factor because we have support that should be one factor
Then do invar and see if related to any factors that you included

See Hirschfeld(2014) for details
```{r}
head(tlc_psycho)
INQ_b_average = tlc_psycho[,29:40]
INQ_b_average$ID = 1:dim(INQ_b_average)[1]
## Create a variable without any missing data
library(caret)
set.seed(123)
inTrain = createDataPartition(y = INQ_b_average$ID, p = .50, list = FALSE)
efa_b_inq = INQ_b_average[inTrain,]
cfa_b_inq = INQ_b_average[-inTrain,]
efa_b_inq$ID = NULL
cfa_b_inq$ID = NULL
INQ_b_average$ID = NULL

library(psych)
# Poly cor produces errors
efa_b_1 = fa(r = efa_b_inq, nfactors = 1, fm = "gls")
efa_b_2 = fa(r = efa_b_inq, nfactors = 2, fm = "gls")
efa_b_3 = fa(r = efa_b_inq, nfactors = 3, fm = "gls")

anova(efa_b_1, efa_b_2)
anova(efa_b_2, efa_b_3)
fa.diagram(efa_b_2)
### Need three items for measurement invariance
fa.diagram(efa_b_3)

####
vss(efa_b_inq, rotate = "oblimin")
###
fa.parallel(efa_b_inq, fm = "gls", fa = "fa")

### Try CFA

library(lavaan)
model_2  ='INQ12_1 =~ INQ1_B + INQ2_B + INQ3_B + INQ4_B + INQ5_B + INQ6_B
          INQ12_2 =~ INQ7_B + INQ8_B + INQ9_B+ INQ10_B + INQ10_B + INQ11_B + INQ12_B'

fit_2 = cfa(model_2, estimator = "WLSMVS", data = cfa_b_inq, ordered = TRUE)
summary(fit_2, fit.measures = TRUE, standardized = TRUE)


```
Measurement invariance
```{r}
### Measurement invariance at base for everything besides time 
library(semTools)

model_2  ='INQ12_1 =~ INQ1_B + INQ2_B + INQ3_B + INQ4_B + INQ5_B + INQ6_B
          INQ12_2 =~ INQ7_B + INQ8_B + INQ9_B+ INQ10_B + INQ10_B + INQ11_B + INQ12_B'
head(tlc_data_analysis)
measure_invar = tlc_data_analysis
head(measure_invar)
measure_invar$HispanicLatino = ifelse(measure_invar$HispanicLatino == 2, NA, measure_invar$HispanicLatino)
describe.factor(measure_invar$HispanicLatino)
describe.factor(measure_invar$RaceEthnicity)
### Non-white versus white
measure_invar$RaceEthnicity = ifelse(measure_invar$RaceEthnicity != 3, 0, 1)
describe.factor(measure_invar$Version)
###sexual minority versus white
describe.factor(measure_invar$SexualOrientation)
measure_invar$SexualOrientation = ifelse(measure_invar$SexualOrientation != 5, 0,1)
measure_invar$Age = as.numeric(measure_invar$Age)
## Greater than the average age so "older" youth
measure_invar$Age = ifelse(measure_invar$Age > mean(measure_invar$Age, na.rm = TRUE), 1, 0)
#female
measure_invar$Gender = ifelse(measure_invar$Gender == 1, 0, 1)

measure_invar_config = list()
measure_invar_weak = list()
measure_invar_strong = list()
measure_invar_strict = list()
anova_results = list()
measure_invar_names = names(measure_invar)[4:8]
for(i in 1:length(measure_invar_names)){
 measure_invar_config[[i]]= cfa(model_2, data = measure_invar, group = measure_invar_names[[i]], estimator = "MLR", missing = "ML")
 measure_invar_weak[[i]]= cfa(model_2, data = measure_invar, group = measure_invar_names[[i]], estimator = "MLR", missing = "ML", group.equal="loadings")
 measure_invar_strong[[i]]= cfa(model_2, data = measure_invar, group = measure_invar_names[[i]], estimator = "MLR", missing = "ML", group.equal=c("loadings", "intercepts"))
 measure_invar_strict[[i]]= cfa(model_2, data = measure_invar, group = measure_invar_names[[i]], estimator = "MLR", missing = "ML", group.equal=c("loadings", "intercepts", "residuals"))
 anova_results[[i]] = anova(measure_invar_config[[i]], measure_invar_weak[[i]], measure_invar_strong[[i]], measure_invar_strict[[i]])
}
anova_results
```




Concurrent and predictive with suicideal ideation
Get all three measures into total scores and then one data set
```{r}
measure_invar_names
con_pred = data.frame(INQ_b_1_average, INQ_b_2_average, SIS_b_1_average, SIS_b_2_average, SIS_d_1_average, SIS_d_2_average)
head(con_pred)
library(Hmisc)
rcorr(as.matrix(con_pred), type = "spearman")
```

Get Measurement invar over time
Get later too much brain power
```{r}
measure_invar = tlc_data_analysis
INQ_b_1 = measure_invar[,29:34]
INQ_b_1$id = rep(0, dim(INQ_b_1)[1])

INQ_b_2 = measure_invar[,35:40]
INQ_b_2 = 8-INQ_b_2
INQ_b_2$id = rep(0, dim(INQ_b_2)[1])

INQ_d_1 = measure_invar[,73:78]
INQ_d_1$id = rep(1, dim(INQ_d_1)[1])

INQ_d_2 = measure_invar[,79:84]
INQ_d_2$id = rep(1, dim(INQ_d_2)[1])

## Change names to be the same then rbind
colnames(INQ_d_1) = colnames(INQ_b_1)
colnames(INQ_d_2) = colnames(INQ_b_2)

INQ_b_d_1 = rbind(INQ_b_1, INQ_d_1)
INQ_b_d_2 = rbind(INQ_b_2, INQ_d_2)
INQ_b_d = cbind(INQ_b_d_1, INQ_b_d_2)
INQ_b_d
```
Now invar with time
```{r}
measure_invar_config = list()
measure_invar_weak = list()
measure_invar_strong = list()
measure_invar_strict = list()
anova_results = list()
library(lavaan)

model_2  ='INQ12_1 =~ INQ1_B + INQ2_B + INQ3_B + INQ4_B + INQ5_B + INQ6_B
          INQ12_2 =~ INQ7_B + INQ8_B + INQ9_B+ INQ10_B + INQ10_B + INQ11_B + INQ12_B'


measure_invar_names = names(INQ_b_d)[14]
for(i in 1:length(measure_invar_names)){
 measure_invar_config[[i]]= cfa(model_2, data = INQ_b_d[,1:13], group = measure_invar_names[[i]], estimator = "MLR", missing = "ML")
 measure_invar_weak[[i]]= cfa(model_2, data = INQ_b_d[,1:13], group = measure_invar_names[[i]], estimator = "MLR", missing = "ML", group.equal="loadings")
 measure_invar_strong[[i]]= cfa(model_2, data = INQ_b_d[,1:13], group = measure_invar_names[[i]], estimator = "MLR", missing = "ML", group.equal=c("loadings", "intercepts"))
 measure_invar_strict[[i]]= cfa(model_2, data = INQ_b_d[,1:13], group = measure_invar_names[[i]], estimator = "MLR", missing = "ML", group.equal=c("loadings", "intercepts", "residuals"))
 anova_results[[i]] = anova(measure_invar_config[[i]], measure_invar_weak[[i]], measure_invar_strong[[i]], measure_invar_strict[[i]])
}
anova_results

```
Get reliability for two factors, test-retest 
```{r}
inq12_b_fac1 = measure_invar[,29:34]
inq12_b_fac1_mean = apply(inq12_b_fac1, 1, mean, na.rm = TRUE)
inq12_b_fac2 = measure_invar[,35:40]
inq12_b_fac2_mean = apply(inq12_b_fac2, 1, mean, na.rm = TRUE)


inq12_d_fac1 = measure_invar[,73:78]
inq12_d_fac1_mean = apply(inq12_d_fac1, 1, mean, na.rm = TRUE)
inq12_d_fac2 = measure_invar[,79:84]
inq12_d_fac2_mean = apply(inq12_d_fac2, 1, mean, na.rm = TRUE)

summary(omega(inq12_b_fac1))
summary(omega(inq12_b_fac2))


hist(inq12_b_fac1_mean)
qqnorm(inq12_b_fac1_mean)

length(inq12_d_fac1_mean)
n_1 = data.frame(inq12_b_fac1_mean, inq12_d_fac1_mean)
dim(na.omit(n_1))
n_2 = data.frame(inq12_b_fac2_mean,inq12_d_fac2_mean)
dim(na.omit(n_2))
cor.test(inq12_b_fac1_mean, inq12_d_fac1_mean, method = "spearman")
cor.test(inq12_b_fac2_mean, inq12_d_fac2_mean, method = "spearman")
```
###################################
IRT analyses
###################################
Make sure reverse scoring is applied correctly

Create the data sets first
INQ_b_1_pyscho = tlc_data_analysis[,29:34]
INQ_b_2_pyscho = tlc_data_analysis[,35:40]
```{r}
irt_inq_b = tlc_psycho[,c(29:40)]
irt_inq_d = tlc_psycho[,c(73:84)]
names(irt_inq_d) = names(irt_inq_b)
irt_inq = rbind(irt_inq_b, irt_inq_d)
irt_inq
irt_inq[,c(7:12)] = 8 - irt_inq[,c(7:12)] 
irt_inq
### Need to stack half of the variables
library(naniar)
library(prettyR)
describe.factor(irt_inq$INQ6_B)
dim(irt_inq_b)[1]
dim(irt_inq_d)[1]
#apply(irt_inq, 2, function(x){prettyR::describe.factor(x, decr.order = FALSE)})
```
Maybe try dicot and see if results are better
```{r}
library(mirt)
#######################################################################
######################################################################
#fitting IRT model - graded
INQ_b_graded = mirt(irt_inq, model = 2, itemtype = "graded", technical=list(removeEmptyRows=TRUE))
INQ_b_graded


#INQ_b_graded_pcm = mirt(irt_inq, model=model_INQ_b, itemtype = "Rasch", technical=list(removeEmptyRows=TRUE))

#INQ_b_graded_gpcm<-mirt(irt_inq, model=model_INQ_b, itemtype = "gpcm", technical=list(removeEmptyRows=TRUE))

#anova(INQ_b_graded,INQ_b_graded_pcm)
#anova(INQ_b_graded_pcm,INQ_b_graded_gpcm)

```
Get overall model fit and plot
```{r}
INQ_b_graded_fit  = M2(INQ_b_graded, type='C2', na.rm=TRUE, theta_lim = c(-3, 3), CI = .95)
INQ_b_graded_fit
summary(INQ_b_graded, suppress = 0.25)
plot(INQ_b_graded, type = "score", rotate = "promax", theta_lim  = c(-3, 3))

```
Item level grm fit
```{r}
INQ_b_graded_grm_fit<-itemfit(INQ_b_graded, na.rm=TRUE)
INQ_b_graded_gpcm_fit

INQ_b_graded_grm_fit[,2:4] =round(as.matrix(INQ_b_graded_gpcm_fit[,2:4]), digits=3)
INQ_b_graded_grm_fit_results = INQ_b_graded_grm_fit
p_values <- INQ_b_graded_grm_fit_results[,5] #p values are stored in 5th column 
p_values
p_values_adj <-p.adjust(p_values, method="BH")
p_values_adj =  round(p_values_adj, digits=3)
#combined item fit
results<-cbind(INQ_b_graded_grm_fit_results, p_values_adj)
results
```
Now getting coefficients and plotting
A coefs are the spots on the dimensions where the item is most discriminating.

```{r}
#getting out coefficients
INQ_b_graded_coef <- coef(INQ_b_graded, rotate = 'promax', simplify = TRUE)
INQ_b_graded_coef

list_plots = list()
count_plots = 1:12
for(i in 1:length(count_plots)){
  list_plots[[i]] = itemplot(INQ_b_graded,item=count_plots[[i]], rotate = "promax", type="trace", theta_lim = c(-3,3))
}
list_plots


```
Test this
```{r}
## Not run:
dat <- expand.table(LSAT7)
x <- mirt(dat, 1)
coef(x)
coef(x, IRTpars = TRUE)
coef(x, simplify = TRUE)
#with computed information matrix
x <- mirt(dat, 1, SE = TRUE)
coef(x)
coef(x, printSE = TRUE)
coef(x, as.data.frame = TRUE)
#two factors
x2 <- mirt(Science, 2)
coef(x2)
coef(x2, rotate = 'varimax', simplify = TRUE)

```

