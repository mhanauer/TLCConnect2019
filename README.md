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
library(rstanarm)
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
apply(tlc_data_analysis, 2, function(x){describe.factor(x)})
head(tlc_data_analysis)

### Generate average scores
head(tlc_data_analysis)

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

head(tlc_data_analysis_average)

dim(tlc_data_analysis_average)
```
Evaluate missing data
Get percentage of missing data for each variable
Test missing assumption
Get rid of missing data
```{r}
library(MissMech)
library(naniar)
##### Get rid of EHR vars 
tlc_data_analysis_average[,8:13] = NULL
### Assessing global missing data 

dim(tlc_data_analysis_average)
var_missing =  miss_var_summary(tlc_data_analysis_average)
var_missing = data.frame(var_missing)
write.csv(var_missing, "var_missing.csv", row.names = FALSE)
#write.csv(var_missing, "var_missing.csv", row.names = FALSE)
full_n = dim(tlc_data_analysis_average)[1]
############## Getting rid of anyone who doesn't have at 50% complete data
quasi_itt =  apply(tlc_data_analysis_average, 1, function(x)(sum(is.na(x))))
quasi_itt_dat = data.frame(tlc_data_analysis_average,quasi_itt)
### Threshold is less than 50% 
quasi_itt_dat = subset(quasi_itt_dat, quasi_itt < dim(quasi_itt_dat)[2]/2)
quasi_itt_dat$quasi_itt = NULL
dim(quasi_itt_dat)
quasi_itt_n = dim(quasi_itt_dat)[1]
### Percentage of drop for quasi itt
quasi_itt_drop_out_rate = 1-(dim(quasi_itt_dat)[1]/dim(tlc_data_analysis_average)[1])
quasi_itt_drop_out_rate
quasi_itt_missing_percent = prop_miss_case(quasi_itt_dat)
####### 
quasi_tot_dat =  quasi_itt_dat 
quasi_tot_dat = na.omit(quasi_tot_dat)
quasi_tot_n = dim(quasi_tot_dat)[1]
quasi_tot_drop_out_rate = 1-(dim(quasi_tot_dat)[1]/dim(tlc_data_analysis_average)[1])



### Put together all results
missing_results = data.frame(full_n, quasi_itt_n, quasi_tot_n, quasi_itt_drop_out_rate, quasi_tot_drop_out_rate)
missing_results = round(missing_results, 3)
missing_results = t(missing_results)
colnames(missing_results)= "n_percent"
#### Add a column with explainations for each of them
explain = c("Total number of participants. Anyone assigned a treatment id.  Could have .1 or larger if that is the only piece of data. Excluded if not assigned a TXPackageAssigned.", "Total number of participants who have at least 50% of data. This data set still contains missing values.", "Total number of complete cases.", "Percentage of clients who did not complete at least 70% of discharge.", "Percentage of missing data.")
missing_results = data.frame(missing_results, explain)

write.csv(missing_results, "missing_results.csv")

describe.factor(quasi_itt_dat$TXPackageAssigned)
write.csv(tlc_data_analysis_average,"tlc_data_analysis_average.csv", row.names = FALSE)
```
Descriptive statistics

Who provided (program staff) what services (modality, type, intensity, duration), to whom (individual characteristics), in what context (system, community)?

 (1) a minimum of 5,660  youth will receive all components of the enhanced post-crisis follow-up intervention by the discharge survey 
 
 (3) a minimum of  5,660  youth enrolled in the enhanced post-crisis follow-up intervention will complete a safety plan and successfully implement all aspects of the plan at least 80% of the time;
```{r}
head(tlc_data_analysis_average)
tlc_data_analysis_average = quasi_itt_dat
### N 
dim(tlc_data_analysis_average)
### 1 put into an excel form
dim(tlc_data_analysis_average)
library(psych)
con_vars  = tlc_data_analysis_average[,c(3,8,14:33)]
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
cat_vars = tlc_data_analysis_average[,c(2,4:7,9:13)] 
cat_vars = apply(cat_vars, 2, function(x){describe.factor(x)})
cat_vars = data.frame(cat_vars)
cat_vars = t(cat_vars)
cat_vars = data.frame(cat_vars)
cat_vars$Percent = round(cat_vars$Percent, 3)
cat_vars
write.csv(cat_vars, "cat_vars.csv")
### Now percentage of missing for the count vars
var_missing =  miss_var_summary(tlc_data_analysis_average)
var_missing$pct_miss = round(var_missing$pct_miss, 3)
write.csv(var_missing, "var_missing.csv")
```
(4) self-stigma of mental illness, thwarted belongingness, and perceived burdensomeness will each decrease 30% among youth enrolled in enhanced post-crisis follow-up by the discharge assessment; and 
(5) scores on the suicidal ideation scale will decrease by 40% among youth enrolled in the enhanced post-crisis follow-up intervention
(6) Scores on the recovery assessment scale will increase by 35% among youth enrolled in the enhanced post-crisis follow-up intervention
```{r}
head(tlc_data_analysis_average)
### Pre 
pre_scores_tlc=  tlc_data_analysis_average[,14:23]
post_scores_tlc = tlc_data_analysis_average[,24:33]
post_scores_tlc = apply(post_scores_tlc, 2, mean, na.rm = TRUE)
pre_scores_tlc = apply(pre_scores_tlc, 2, mean, na.rm =TRUE)

p_change = (post_scores_tlc-pre_scores_tlc)/pre_scores_tlc

p_change_results = data.frame(pre_scores_tlc, post_scores_tlc, p_change)
p_change_results = round(p_change_results, 3)
p_change_results
```
(1)	a minimum of 5,660  youth will receive all components of the enhanced post-crisis follow-up intervention by the discharge survey 

referrals will be retained at least 50% of the time among youth enrolled in the post-crisis follow-up intervention and follow-through with appointments will occur at least 75% of the time
```{r}
tlc_data_analysis_average$ReferralsEngaged_binary = ifelse(tlc_data_analysis_average$ReferralsEngaged > 0,1,0)
describe.factor(tlc_data_analysis_average$ReferralsEngaged_binary)
describe.factor(tlc_data_analysis_average$Attend75Referrals)
```


Check assumptions of normality
```{r}
outcomes_tests = tlc_data_analysis_average[,14:22]
hist_results = list() 
qq_results = list()
shap_results = list()
for(i in 1:length(outcomes_tests)){
  hist_results[[i]] = hist(outcomes_tests[[i]])
  qq_results[[i]] = qqnorm(outcomes_tests[[i]])
  shap_results[[i]] = shapiro.test(outcomes_tests[[i]])
}
shap_results
```

###########################
SIS outcomes associated with it
##########################
```{r}
outcomes_freq
corr.test(outcomes_freq)


####### Suicide idea 
suicide_idea_model_0 = lm(SIS_1_diff ~ RAS_1_diff + RAS_2_diff + RAS_3_diff + INQ_1_diff + INQ_2_diff + SSMI_diff + PHQ9_diff + SIS_2_diff, data = outcomes_freq_stand)
vif(suicide_idea_model_0)
suicide_idea_model_sum_0 = summary(suicide_idea_model_0)
suicide_idea_model_sum_0
round(suicide_idea_model_sum_0$coefficients,3)

suicide_idea_model_1 = lm(SIS_1_diff ~ RAS_1_diff  + RAS_3_diff + INQ_1_diff + INQ_2_diff  + PHQ9_diff + SIS_2_diff, data = outcomes_freq_stand)
suicide_idea_model_1_sum = summary(suicide_idea_model_1)
suicide_idea_model_1_sum = round(suicide_idea_model_1_sum$coefficients,3)
write.csv(suicide_idea_model_1_sum, "suicide_idea_model_1_sum.csv")

checK_assump = gvlma(suicide_idea_model)
checK_assump
par(mar=c(1,1,1,1))
plot.gvlma(checK_assump)
library(car)
checkresiduals(suicide_idea_model)

### Second factor
resolved_model_0 = lm(SIS_2_diff ~ RAS_1_diff + RAS_2_diff + RAS_3_diff + INQ_1_diff + INQ_2_diff + SSMI_diff + PHQ9_diff + SIS_1_diff , data = outcomes_freq_stand)
vif(resolved_model_0)
resolved_model_sum_0 = summary(resolved_model_0)
resolved_model_sum_0
resolved_model_sum_0 = round(resolved_model_sum_0$coefficients,3)
write.csv(resolved_model_sum_0, "resolved_model_sum_0.csv")


resolved_model_1 = lm(SIS_2_diff ~ RAS_1_diff + RAS_2_diff + RAS_3_diff + INQ_1_diff + INQ_2_diff + SSMI_diff + PHQ9_diff , data = outcomes_freq_stand)
summary(resolved_model_1)

checK_assump = gvlma(resolved_model_0)
checK_assump
par(mar=c(1,1,1,1))
plot.gvlma(checK_assump)
library(car)
checkresiduals(resolved_model)


```
###########################
Imputted results
###########################
Imputation
```{r}
library(Amelia)
impute_dat = quasi_itt_dat
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

## EHR variables causing problems.
impute_dat[,5:10] = NULL


a.out = amelia(x = impute_dat, m = 5, noms = c("TXPackageAssigned" ,"female", "HispanicLatino", "non_white", "sexual_minority"))
compare.density(a.out, var = "SIS_d_2_average")
summary(a.out)
disperse(a.out)
impute_dat_loop = a.out$imputations
```


######################
Within tlc results
######################
Phone only
Get means and sds then meld together so you don't have to deal with it, then you can calcaulate cohen's d by hand for all of them.
```{r}
#### Treatment 1
tlc_within_d1_base_t1 = subset(impute_dat_loop[[1]][,c(2,5:14)], TXPackageAssigned == 1)
tlc_within_d1_dis_t1 = subset(impute_dat_loop[[1]][,c(2,15:24)], TXPackageAssigned == 1)
tlc_within_d1_base_t1$TXPackageAssigned = NULL
tlc_within_d1_dis_t1$TXPackageAssigned = NULL

library(effsize)
tlc_within_results_d1_t1 = list()
for(i in 1:length(tlc_within_d1_base_t1)){
  tlc_within_results_d1_t1[[i]] = cohen.d(tlc_within_d1_dis_t1[[i]], tlc_within_d1_base_t1[[i]], paired = TRUE, conf.level = .95)
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

tlc_within_d2_base_t1 = subset(impute_dat_loop[[2]][,c(2,5:14)], TXPackageAssigned == 1)
tlc_within_d2_dis_t1 = subset(impute_dat_loop[[2]][,c(2,15:24)], TXPackageAssigned == 1)
tlc_within_d2_base_t1$TXPackageAssigned = NULL
tlc_within_d2_dis_t1$TXPackageAssigned = NULL

tlc_within_results_d2_t1 = list()
for(i in 1:length(tlc_within_d2_base_t1)){
  tlc_within_results_d2_t1[[i]] = cohen.d(tlc_within_d2_dis_t1[[i]], tlc_within_d2_base_t1[[i]], paired = TRUE, conf.level = .95)
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

tlc_within_d3_base_t1 = subset(impute_dat_loop[[3]][,c(2,5:14)], TXPackageAssigned == 1)
tlc_within_d3_dis_t1 = subset(impute_dat_loop[[3]][,c(2,15:24)], TXPackageAssigned == 1)
tlc_within_d3_base_t1$TXPackageAssigned = NULL
tlc_within_d3_dis_t1$TXPackageAssigned = NULL

tlc_within_results_d3_t1 = list()
for(i in 1:length(tlc_within_d3_base_t1)){
  tlc_within_results_d3_t1[[i]] = cohen.d(tlc_within_d3_dis_t1[[i]], tlc_within_d3_base_t1[[i]], paired = TRUE, conf.level = .95)
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

tlc_within_d4_base_t1 = subset(impute_dat_loop[[4]][,c(2,5:14)], TXPackageAssigned == 1)
tlc_within_d4_dis_t1 = subset(impute_dat_loop[[4]][,c(2,15:24)], TXPackageAssigned == 1)
tlc_within_d4_base_t1$TXPackageAssigned = NULL
tlc_within_d4_dis_t1$TXPackageAssigned = NULL

tlc_within_results_d4_t1 = list()
for(i in 1:length(tlc_within_d4_base_t1)){
  tlc_within_results_d4_t1[[i]] = cohen.d(tlc_within_d4_dis_t1[[i]], tlc_within_d4_base_t1[[i]], paired = TRUE, conf.level = .95)
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


tlc_within_d5_base_t1 = subset(impute_dat_loop[[5]][,c(2,5:14)], TXPackageAssigned == 1)
tlc_within_d5_dis_t1 = subset(impute_dat_loop[[5]][,c(2,15:24)], TXPackageAssigned == 1)
tlc_within_d5_base_t1$TXPackageAssigned = NULL
tlc_within_d5_dis_t1$TXPackageAssigned = NULL

tlc_within_results_d5_t1 = list()
for(i in 1:length(tlc_within_d5_base_t1)){
  tlc_within_results_d5_t1[[i]] = cohen.d(tlc_within_d5_dis_t1[[i]], tlc_within_d5_base_t1[[i]], paired = TRUE, conf.level = .95)
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
tlc_within_d1_base_t2 = subset(impute_dat_loop[[1]][,c(2,5:14)], TXPackageAssigned == 2)
tlc_within_d1_dis_t2 = subset(impute_dat_loop[[1]][,c(2,15:24)], TXPackageAssigned == 2)
tlc_within_d1_base_t2$TXPackageAssigned = NULL
tlc_within_d1_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d1_t2 = list()
for(i in 1:length(tlc_within_d1_base_t2)){
  tlc_within_results_d1_t2[[i]] = cohen.d(tlc_within_d1_dis_t2[[i]], tlc_within_d1_base_t2[[i]], paired = TRUE, conf.level = .95)
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

tlc_within_d2_base_t2 = subset(impute_dat_loop[[2]][,c(2,5:14)], TXPackageAssigned == 2)
tlc_within_d2_dis_t2 = subset(impute_dat_loop[[2]][,c(2,15:24)], TXPackageAssigned == 2)
tlc_within_d2_base_t2$TXPackageAssigned = NULL
tlc_within_d2_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d2_t2 = list()
for(i in 1:length(tlc_within_d2_base_t2)){
  tlc_within_results_d2_t2[[i]] = cohen.d(tlc_within_d2_dis_t2[[i]], tlc_within_d2_base_t2[[i]], paired = TRUE, conf.level = .95)
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

tlc_within_d3_base_t2 = subset(impute_dat_loop[[3]][,c(2,5:14)], TXPackageAssigned == 2)
tlc_within_d3_dis_t2 = subset(impute_dat_loop[[3]][,c(2,15:24)], TXPackageAssigned == 2)
tlc_within_d3_base_t2$TXPackageAssigned = NULL
tlc_within_d3_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d3_t2 = list()
for(i in 1:length(tlc_within_d3_base_t2)){
  tlc_within_results_d3_t2[[i]] = cohen.d(tlc_within_d3_dis_t2[[i]], tlc_within_d3_base_t2[[i]], paired = TRUE, conf.level = .95)
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

tlc_within_d4_base_t2 = subset(impute_dat_loop[[4]][,c(2,5:14)], TXPackageAssigned == 2)
tlc_within_d4_dis_t2 = subset(impute_dat_loop[[4]][,c(2,15:24)], TXPackageAssigned == 2)
tlc_within_d4_base_t2$TXPackageAssigned = NULL
tlc_within_d4_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d4_t2 = list()
for(i in 1:length(tlc_within_d4_base_t2)){
  tlc_within_results_d4_t2[[i]] = cohen.d(tlc_within_d4_dis_t2[[i]], tlc_within_d4_base_t2[[i]], paired = TRUE, conf.level = .95)
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


tlc_within_d5_base_t2 = subset(impute_dat_loop[[5]][,c(2,5:14)], TXPackageAssigned == 2)
tlc_within_d5_dis_t2 = subset(impute_dat_loop[[5]][,c(2,15:24)], TXPackageAssigned == 2)
tlc_within_d5_base_t2$TXPackageAssigned = NULL
tlc_within_d5_dis_t2$TXPackageAssigned = NULL

tlc_within_results_d5_t2 = list()
for(i in 1:length(tlc_within_d5_base_t2)){
  tlc_within_results_d5_t2[[i]] = cohen.d(tlc_within_d5_dis_t2[[i]], tlc_within_d5_base_t2[[i]], paired = TRUE, conf.level = .95)
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
tlc_within_d1_base_t3 = subset(impute_dat_loop[[1]][,c(2,5:14)], TXPackageAssigned == 3)
tlc_within_d1_dis_t3 = subset(impute_dat_loop[[1]][,c(2,15:24)], TXPackageAssigned == 3)
tlc_within_d1_base_t3$TXPackageAssigned = NULL
tlc_within_d1_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d1_t3 = list()
for(i in 1:length(tlc_within_d1_base_t3)){
  tlc_within_results_d1_t3[[i]] = cohen.d(tlc_within_d1_dis_t3[[i]], tlc_within_d1_base_t3[[i]], paired = TRUE, conf.level = .95)
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

tlc_within_d2_base_t3 = subset(impute_dat_loop[[2]][,c(2,5:14)], TXPackageAssigned == 3)
tlc_within_d2_dis_t3 = subset(impute_dat_loop[[2]][,c(2,15:24)], TXPackageAssigned == 3)
tlc_within_d2_base_t3$TXPackageAssigned = NULL
tlc_within_d2_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d2_t3 = list()
for(i in 1:length(tlc_within_d2_base_t3)){
  tlc_within_results_d2_t3[[i]] = cohen.d(tlc_within_d2_dis_t3[[i]], tlc_within_d2_base_t3[[i]], paired = TRUE, conf.level = .95)
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

tlc_within_d3_base_t3 = subset(impute_dat_loop[[3]][,c(2,5:14)], TXPackageAssigned == 3)
tlc_within_d3_dis_t3 = subset(impute_dat_loop[[3]][,c(2,15:24)], TXPackageAssigned == 3)
tlc_within_d3_base_t3$TXPackageAssigned = NULL
tlc_within_d3_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d3_t3 = list()
for(i in 1:length(tlc_within_d3_base_t3)){
  tlc_within_results_d3_t3[[i]] = cohen.d(tlc_within_d3_dis_t3[[i]], tlc_within_d3_base_t3[[i]], paired = TRUE, conf.level = .95)
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

tlc_within_d4_base_t3 = subset(impute_dat_loop[[4]][,c(2,5:14)], TXPackageAssigned == 3)
tlc_within_d4_dis_t3 = subset(impute_dat_loop[[4]][,c(2,15:24)], TXPackageAssigned == 3)
tlc_within_d4_base_t3$TXPackageAssigned = NULL
tlc_within_d4_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d4_t3 = list()
for(i in 1:length(tlc_within_d4_base_t3)){
  tlc_within_results_d4_t3[[i]] = cohen.d(tlc_within_d4_dis_t3[[i]], tlc_within_d4_base_t3[[i]], paired = TRUE, conf.level = .95)
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


tlc_within_d5_base_t3 = subset(impute_dat_loop[[5]][,c(2,5:14)], TXPackageAssigned == 3)
tlc_within_d5_dis_t3 = subset(impute_dat_loop[[5]][,c(2,15:24)], TXPackageAssigned == 3)
tlc_within_d5_base_t3$TXPackageAssigned = NULL
tlc_within_d5_dis_t3$TXPackageAssigned = NULL

tlc_within_results_d5_t3 = list()
for(i in 1:length(tlc_within_d5_base_t3)){
  tlc_within_results_d5_t3[[i]] = cohen.d(tlc_within_d5_dis_t3[[i]], tlc_within_d5_base_t3[[i]], paired = TRUE, conf.level = .95)
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
tlc_within_t3_results$cohen_d = ifelse(tlc_within_t3_results$upper > 0 & tlc_within_t3_results$lower < 0, tlc_within_t3_results$cohen_d, paste0(tlc_within_t3_results$cohen_d, "*"))
tlc_within_t3_results$ci_95 = paste0(tlc_within_t3_results$lower, sep = ",", tlc_within_t3_results$upper)
tlc_within_t3_results[,2:3] = NULL

tlc_within_results = rbind(tlc_within_t1_results, tlc_within_t2_results, tlc_within_t3_results)

write.csv(tlc_within_results, "tlc_within_results.csv", row.names = FALSE)
```
######################
Between tlc
######################
```{r}

### Create difference scores
out_diff_dat = list()
impute_dat_loop[[1]][5:14]
for(i in 1:length(impute_dat_loop)){
  out_diff_dat[[i]] = impute_dat_loop[[i]][15:24]-impute_dat_loop[[i]][5:14]
  colnames(out_diff_dat[[i]]) = c("RAS_1_diff", "RAS_2_diff", "RAS_3_diff", "RAS_5_diff", "INQ_1_diff", "INQ_2_diff", "SSMI_diff", "SIS_1_diff", "SIS_2_diff", "PHQ9_diff")
  out_diff_dat[[i]] = scale(out_diff_dat[[i]])
  out_diff_dat[[i]] =cbind(impute_dat_loop[[i]], out_diff_dat[[i]])
}
out_diff_dat
impute_tlc_between_results = list()
impute_tlc_between_results_sum = list()
se_con = list()
for(i in 1:length(out_diff_dat)){
  impute_tlc_between_results[[i]]=lm(cbind(RAS_1_diff, RAS_2_diff,RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SSMI_diff, SIS_1_diff, SIS_2_diff, PHQ9_diff) ~ factor(TXPackageAssigned), data = out_diff_dat[[i]])
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
coefs_1 = matrix(coefs_1, ncol = 20)
coefs_1

ses_1
ses_1 = unlist(ses_1)
ses_1 = matrix(ses_1, ncol = 20)
ses_1


coefs_2 = list()
ses_2 = list()
for(i in 1:length(impute_tlc_between_results_2)){
  coefs_2[[i]] = impute_tlc_between_results_2[[i]]$coefficients[2:3,1]
  ses_2[[i]] = impute_tlc_between_results_2[[i]]$coefficients[2:3,2]
}
coefs_2
coefs_2 = unlist(coefs_2)
coefs_2 = matrix(coefs_2, ncol = 20)
coefs_2

ses_2
ses_2 = unlist(ses_2)
ses_2 = matrix(ses_2, ncol = 20)
ses_2

coefs_3 = list()
ses_3 = list()
for(i in 1:length(impute_tlc_between_results_3)){
  coefs_3[[i]] = impute_tlc_between_results_3[[i]]$coefficients[2:3,1]
  ses_3[[i]] = impute_tlc_between_results_3[[i]]$coefficients[2:3,2]
}
coefs_3
coefs_3 = unlist(coefs_3)
coefs_3 = matrix(coefs_3, ncol = 20)
coefs_3

ses_3
ses_3 = unlist(ses_3)
ses_3 = matrix(ses_3, ncol = 20)
ses_3

coefs_4 = list()
ses_4 = list()
for(i in 1:length(impute_tlc_between_results_4)){
  coefs_4[[i]] = impute_tlc_between_results_4[[i]]$coefficients[2:3,1]
  ses_4[[i]] = impute_tlc_between_results_4[[i]]$coefficients[2:3,2]
}
coefs_4
coefs_4 = unlist(coefs_4)
coefs_4 = matrix(coefs_4, ncol = 20)
coefs_4

ses_4
ses_4 = unlist(ses_4)
ses_4 = matrix(ses_4, ncol = 20)
ses_4

coefs_5 = list()
ses_5 = list()
for(i in 1:length(impute_tlc_between_results_5)){
  coefs_5[[i]] = impute_tlc_between_results_5[[i]]$coefficients[2:3,1]
  ses_5[[i]] = impute_tlc_between_results_5[[i]]$coefficients[2:3,2]
}
coefs_5
coefs_5 = unlist(coefs_5)
coefs_5 = matrix(coefs_5, ncol = 20)
coefs_5

ses_5
ses_5 = unlist(ses_5)
ses_5 = matrix(ses_5, ncol = 20)
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
tlc_between_impute_results$parameter_estimate = ifelse(tlc_between_impute_results$p_value < .05, paste0(tlc_between_impute_results$parameter_estimate, "*"), tlc_between_impute_results$parameter_estimate)
tlc_between_impute_results
write.csv(tlc_between_impute_results, "tlc_between_impute_results.csv", row.names = FALSE)

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
est_se_con
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




Try testing whether the inclusion of HoursPsychotherapy, CurrentlyEngaged makes a difference

•	What program/contextual factors are associated with which outcomes?
```{r}
describe(outcomes_freq_stand)
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned === 1)
cor(tlc_complete_t1[,7:12])
outcomes_t1 = tlc_complete_t1[,13:22]

results_t1 = list()
for(i in 1:length(outcomes_t1)){
  results_t1[[i]] = summary(stan_glm(outcomes_t1[[i]] ~ HoursPsychotherapy  + Attend75Referrals+ CrisisPlan80Time, data = tlc_complete_t1))
}

results_t1

tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned === 2)
outcomes_t2 = tlc_complete_t2[,13:22]
describe(outcomes_t2)

results_t2 = list()
for(i in 1:length(outcomes_t2)){
  results_t2[[i]] = summary(stan_glm(outcomes_t2[[i]] ~ HoursPsychotherapy  + Attend75Referrals+  CrisisPlan80Time, data = tlc_complete_t2))
}
results_t2

tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned === 3)
outcomes_t3 = tlc_complete_t3[,13:22]

results_t3 = list()
for(i in 1:length(outcomes_t3)){
  results_t3[[i]] = summary(stan_glm(outcomes_t3[[i]] ~ HoursPsychotherapy  + Attend75Referrals+    CrisisPlan80Time, data = tlc_complete_t3))
}
results_t3

```
Try testing whether the inclusion of demos makes a difference

•	What individual factors were associated with outcomes, including race/ethnicity/sexual identity (sexual orientation/gender identity)?
```{r}
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned === 1)
cor(tlc_complete_t1[,7:12])
outcomes_t1 = tlc_complete_t1[,13:22]
#Age, Gender (female), RaceEthnicity (non-white), SexualOrientation (sexual minor)
tlc_complete_t1$Gender = ifelse(tlc_complete_t1$Gender == 1, 0,1)
tlc_complete_t1$RaceEthnicity = ifelse(tlc_complete_t1$RaceEthnicity == 3, 0,1)
tlc_complete_t1$SexualOrientation = ifelse(tlc_complete_t1$SexualOrientation == 5,0,1)

head(tlc_complete_t1)

results_t1 = list()
model_t1 = list()
for(i in 1:length(outcomes_t1)){
  model_t1[[i]] = summary(stan_glm(outcomes_t1[[i]] ~ Age+ Gender +RaceEthnicity + SexualOrientation, data = tlc_complete_t1))
}

results_t1

tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned === 2)
outcomes_t2 = tlc_complete_t2[,13:22]
describe(outcomes_t2)
tlc_complete_t2$Gender = ifelse(tlc_complete_t2$Gender == 1, 0,1)
tlc_complete_t2$RaceEthnicity = ifelse(tlc_complete_t2$RaceEthnicity == 3, 0,1)
tlc_complete_t2$SexualOrientation = ifelse(tlc_complete_t2$SexualOrientation == 5,0,1)

results_t2 = list()
for(i in 1:length(outcomes_t2)){
  results_t2[[i]] = summary(stan_glm(outcomes_t2[[i]] ~ Age+ Gender +RaceEthnicity + SexualOrientation, data = tlc_complete_t2))
}
results_t2

tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned === 3)
outcomes_t3 = tlc_complete_t3[,13:22]
tlc_complete_t3$Gender = ifelse(tlc_complete_t3$Gender == 1, 0,1)
tlc_complete_t3$RaceEthnicity = ifelse(tlc_complete_t3$RaceEthnicity == 3, 0,1)
tlc_complete_t3$SexualOrientation = ifelse(tlc_complete_t3$SexualOrientation == 5,0,1)

results_t3 = list()
for(i in 1:length(outcomes_t3)){
  results_t3[[i]] = summary(stan_glm(outcomes_t3[[i]] ~Age+ Gender +RaceEthnicity + SexualOrientation, data = tlc_complete_t3))
}
results_t3
```

•	Does effectiveness of the program intervention vary according to clinical risk presentation (e.g., suicide risk score, history of past attempts)? 

```{r}
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned === 1)
outcomes_t1 = tlc_complete_t1[,13:22]

head(tlc_complete_t1)

results_t1 = list()
for(i in 1:length(outcomes_t1)){
  results_t1[[i]] = summary(stan_glm(outcomes_t1[[i]] ~ PHQ9_b, data = tlc_complete_t1))
}

results_t1

tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned === 2)
outcomes_t2 = tlc_complete_t2[,13:22]

results_t2 = list()
for(i in 1:length(outcomes_t2)){
  results_t2[[i]] = summary(stan_glm(outcomes_t2[[i]] ~PHQ9_b, data = tlc_complete_t2))
}
results_t2

tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned === 3)
outcomes_t3 = tlc_complete_t3[,13:22]

results_t3 = list()
for(i in 1:length(outcomes_t3)){
  results_t3[[i]] = summary(stan_glm(outcomes_t3[[i]] ~PHQ9_b, data = tlc_complete_t3))
}
results_t3

```


Pyschometrics
Test confirmatory factor because we have support that should be one factor
Then do invar and see if related to any factors that you included

See Hirschfeld(2014) for details
```{r}
head(tlc_data_analysis)
INQ_b_average = tlc_data_analysis[,29:40]
INQ_b_average$ID = 1:dim(INQ_b_average)[1]
## Create a variable without any missing data
library(caret)
inTrain = createDataPartition(y = INQ_b_average$ID, p = .50, list = FALSE)
efa_b_inq = INQ_b_average[inTrain,]
cfa_b_inq = INQ_b_average[-inTrain,]
efa_b_inq$ID = NULL
cfa_b_inq$ID = NULL
INQ_b_average$ID = NULL

library(psych)
efa_b_1 = fa(r = efa_b_inq, nfactors = 1, fm = "gls")
efa_b_2 = fa(r = efa_b_inq, nfactors = 2, fm = "gls")
efa_b_3 = fa(r = efa_b_inq, nfactors = 3, fm = "gls")

anova(efa_b_1, efa_b_2)
anova(efa_b_2, efa_b_3)
fa.diagram(efa_b_2)
fa.diagram(efa_b_3)

####
vss(efa_b_inq)
###
library(paran)
efa_b_inq_complete = na.omit(efa_b_inq)
paran(efa_b_inq_complete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)

### Try CFA

model_1  ='INQ12 =~ INQ1_B + INQ2_B + INQ3_B + INQ4_B + INQ5_B + INQ6_B + INQ7_B + INQ8_B + INQ9_B+ INQ10_B + INQ10_B + INQ11_B + INQ12_B'

library(lavaan)
fit_1 = cfa(model_1, estimator = "MLR", missing = "ML", data = cfa_b_inq)
summary(fit_1, fit.measures = TRUE, standardized = TRUE)

model_2  ='INQ12_1 =~ INQ1_B + INQ2_B + INQ3_B + INQ4_B + INQ5_B + INQ6_B
          INQ12_2 =~ INQ7_B + INQ8_B + INQ9_B+ INQ10_B + INQ10_B + INQ11_B + INQ12_B'

fit_2 = cfa(model_2, estimator = "MLR", missing = "ML", data = cfa_b_inq)
summary(fit_2, fit.measures = TRUE, standardized = TRUE)

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
###sexual minority versus sexual majority
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
measure_invar_names = names(measure_invar)[3:8]
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
con_pred = data.frame(INQ_b_1_average, INQ_b_2_average, SIS_b_1_average, SIS_b_2_average, SIS_d_1_average, SIS_d_2_average)
head(con_pred)
library(Hmisc)
con_pred
rcorr(as.matrix(con_pred), type = "spearman")
cor(con_pred)
```

Get Measurement invar over time
Get later too much brain power
```{r}
INQ_b_1 = tlc_data_analysis[,29:34]
INQ_b_1$id = rep(0, dim(INQ_b_1)[1])

INQ_b_2 = tlc_data_analysis[,35:40]
INQ_b_2 = 8-INQ_b_2
INQ_b_2$id = rep(0, dim(INQ_b_2)[1])

INQ_d_1 = tlc_data_analysis[,73:78]
INQ_d_1$id = rep(1, dim(INQ_d_1)[1])

INQ_d_2 = tlc_data_analysis[,79:84]
INQ_d_2 = 8-INQ_d_2
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





RAS psycho
```{r}
head(tlc_data_analysis)
RAS_b_average = tlc_data_analysis[,9:28]
RAS_b_average$ID = 1:dim(RAS_b_average)[1]
## Create a variable without any missing data
library(caret)
inTrain = createDataPartition(y = RAS_b_average$ID, p = .50, list = FALSE)
efa_b_RAS = RAS_b_average[inTrain,]
cfa_b_RAS = RAS_b_average[-inTrain,]
efa_b_RAS$ID = NULL
cfa_b_RAS$ID = NULL
RAS_b_average$ID = NULL

library(psych)
efa_b_1 = fa(r = efa_b_RAS, nfactors = 1, fm = "gls")
efa_b_2 = fa(r = efa_b_RAS, nfactors = 2, fm = "gls")
efa_b_3 = fa(r = efa_b_RAS, nfactors = 3, fm = "gls")
efa_b_4 = fa(r = efa_b_RAS, nfactors = 4, fm = "gls")
efa_b_5 = fa(r = efa_b_RAS, nfactors = 5, fm = "gls")


anova(efa_b_1, efa_b_2)
anova(efa_b_2, efa_b_3)
anova(efa_b_3, efa_b_4)
anova(efa_b_4, efa_b_5)

fa.diagram(efa_b_5)

####
vss(efa_b_RAS)
###
library(paran)
efa_b_RAS_complete = na.omit(efa_b_RAS)
paran(efa_b_RAS_complete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)


### Try CFA
#f = 6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
#q=17,r=18,s=19
#a,b,c,d,e
#n=14,o=15,p=16


model_1  ='RAS_1_B =~ RAS6_B + RAS7_B + RAS8_B + RAS9_B + RAS10_B + RAS11_B + RAS12_B + RAS13_B + RAS20_B
RAS_2_B =~  RAS17_B + RAS18_B + RAS19_B
RAS_3_B =~ RAS1_B + RAS2_B + RAS3_B + RAS4_B + RAS5_B
RAS_4_B =~ RAS14_B + RAS15_B + RAS16_B '

library(lavaan)
fit_1 = cfa(model_1, estimator = "MLR", missing = "ML", data = cfa_b_RAS)
summary(fit_1, fit.measures = TRUE, standardized = TRUE)
```
SSMI Pyscho Not bad
```{r}
head(tlc_data_analysis)
SSMI_b_average = tlc_data_analysis[,41:45]
SSMI_b_average$ID = 1:dim(SSMI_b_average)[1]
## Create a variable without any missing data
library(caret)
inTrain = createDataPartition(y = SSMI_b_average$ID, p = .50, list = FALSE)
efa_b_SSMI = SSMI_b_average[inTrain,]
cfa_b_SSMI = SSMI_b_average[-inTrain,]
efa_b_SSMI$ID = NULL
cfa_b_SSMI$ID = NULL
SSMI_b_average$ID = NULL

library(psych)
efa_b_1 = fa(r = efa_b_SSMI, nfactors = 1, fm = "gls")
efa_b_2 = fa(r = efa_b_SSMI, nfactors = 2, fm = "gls")
efa_b_3 = fa(r = efa_b_SSMI, nfactors = 3, fm = "gls")

anova(efa_b_1, efa_b_2)
anova(efa_b_2, efa_b_3)
fa.diagram(efa_b_2)

####
vss(efa_b_SSMI)
###
library(paran)
efa_b_SSMI_complete = na.omit(efa_b_SSMI)
paran(efa_b_SSMI_complete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)


### Try CFA

model_1  ='SSMI_B =~ SSMI1_B + SSMI2_B + SSMI3_B + SSMI4_B + SSMI5_B'

library(lavaan)
fit_1 = cfa(model_1, estimator = "MLR", missing = "ML", data = cfa_b_SSMI)
summary(fit_1, fit.measures = TRUE, standardized = TRUE)
```
SIS Excellent
```{r}
head(tlc_data_analysis)
SIS_b_average = tlc_data_analysis[,46:52]
SIS_b_average$ID = 1:dim(SIS_b_average)[1]
## Create a variable without any missing data
library(caret)
inTrain = createDataPartition(y = SIS_b_average$ID, p = .50, list = FALSE)
efa_b_SIS = SIS_b_average[inTrain,]
cfa_b_SIS = SIS_b_average[-inTrain,]
efa_b_SIS$ID = NULL
cfa_b_SIS$ID = NULL
SIS_b_average$ID = NULL

library(psych)
efa_b_1 = fa(r = efa_b_SIS, nfactors = 1, fm = "gls")
efa_b_2 = fa(r = efa_b_SIS, nfactors = 2, fm = "gls")
efa_b_3 = fa(r = efa_b_SIS, nfactors = 3, fm = "gls")

anova(efa_b_1, efa_b_2)
anova(efa_b_2, efa_b_3)
fa.diagram(efa_b_2)

####
vss(efa_b_SIS)
###
library(paran)
efa_b_SIS_complete = na.omit(efa_b_SIS)
paran(efa_b_SIS_complete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)


### Try CFA

model_1  ='SIS_1_B =~ SIS1_B + SIS2_B + SIS3_B + SIS4_B
          SIS_2_B =~ + SIS5_B + SIS6_B + SIS7_B'

library(lavaan)
fit_1 = cfa(model_1, estimator = "MLR", missing = "ML", data = cfa_b_SIS)
summary(fit_1, fit.measures = TRUE, standardized = TRUE)
```


Get reliability for two factors, test-retest 
```{r}
inq12_b_fac1 = tlc_data_analysis[,29:34]
inq12_b_fac1_mean = apply(inq12_b_fac1, 1, mean, na.rm = TRUE)
inq12_b_fac2 = tlc_data_analysis[,35:40]
inq12_b_fac2_mean = apply(inq12_b_fac2, 1, mean, na.rm = TRUE)


inq12_d_fac1 = tlc_data_analysis[,73:78]
inq12_d_fac1_mean = apply(inq12_d_fac1, 1, mean, na.rm = TRUE)
inq12_d_fac2 = tlc_data_analysis[,79:84]
inq12_d_fac2_mean = apply(inq12_d_fac2, 1, mean, na.rm = TRUE)

summary(omega(inq12_b_fac1))
summary(omega(inq12_b_fac2))


hist(inq12_b_fac1_mean)
qqnorm(inq12_b_fac1_mean)

cor.test(inq12_b_fac1_mean, inq12_d_fac1_mean, method = "kendall")
cor.test(inq12_b_fac2_mean, inq12_d_fac2_mean, method = "kendall")
```
