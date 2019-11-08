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

PHQ9_diff = tlc_data_analysis$PHQ9_4 - tlc_data_analysis$PHQ9_1

#### Create new data with average scores
#apply(tlc_data_analysis, 2, function(x){describe.factor(x)})
tlc_data_analysis_average = data.frame(tlc_data_analysis[,c(1,2,4:8, 99:104)], RAS_b_1_average, RAS_b_2_average, RAS_b_3_average, RAS_b_5_average, INQ_b_1_average, INQ_b_2_average, SSMI_b_average, SIS_b_1_average, SIS_b_2_average, PHQ9_b = tlc_data_analysis$PHQ9_1,RAS_d_1_average, RAS_d_2_average, RAS_d_3_average, RAS_d_5_average, INQ_d_1_average, INQ_d_2_average, SSMI_d_average, SIS_d_1_average, SIS_d_2_average,PHQ9_d = tlc_data_analysis$PHQ9_4, RAS_1_diff ,RAS_2_diff, RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SSMI_diff, SIS_1_diff, SIS_2_diff, PHQ9_diff)

head(tlc_data_analysis_average)

tlc_data_analysis_average

########## Invalid parts
## Excluding .1's those without a treatment package assigned
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
whole_number =  is.wholenumber(tlc_data_analysis_average$YouthID)
tlc_data_analysis_average$whole_number = whole_number
tlc_data_analysis_average = subset(tlc_data_analysis_average, whole_number == TRUE)
dim(tlc_data_analysis_average)
tlc_data_analysis_average$YouthID = NULL
tlc_data_analysis_average$whole_number = NULL
### Get rid of those with no treatment packages
describe.factor(tlc_data_analysis_average$TXPackageAssigned)
tlc_data_analysis_average$no_treat_package = is.na(tlc_data_analysis_average$TXPackageAssigned)
tlc_data_analysis_average = subset(tlc_data_analysis_average, no_treat_package == FALSE)
dim(tlc_data_analysis_average)
tlc_data_analysis_average$no_treat_package = NULL

```
Evaluate missing data
Get percentage of missing data for each variable
Test missing assumption
Get rid of missing data
```{r}
library(MissMech)
library(naniar)
### Assessing global missing data 
dim(tlc_data_analysis_average)
var_missing =  miss_var_summary(tlc_data_analysis_average)
var_missing = data.frame(var_missing)
write.csv(var_missing, "var_missing.csv", row.names = FALSE)
#write.csv(var_missing, "var_missing.csv", row.names = FALSE)
full_n = dim(tlc_data_analysis_average)[1]
############## Getting rid of anybody who doesn't have a follow-up
quasi_itt =  apply(tlc_data_analysis_average[,33:42], 1, function(x)(sum(is.na(x))))
quasi_itt_dat = data.frame(tlc_data_analysis_average,quasi_itt)
### Ten variables and threshold is less than 70% 
quasi_itt_dat = subset(quasi_itt_dat, quasi_itt < 8)
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


### No psychotherapy or phq-9 still reject
quasi_tot_dat =  quasi_itt_dat 
no_phq9_psycho_dat = quasi_tot_dat
no_phq9_psycho_dat$PHQ9_b = NULL
no_phq9_psycho_dat$PHQ9_d = NULL
no_phq9_psycho_dat$PHQ9_diff = NULL
no_phq9_psycho_dat$HoursPsychotherapy = NULL
no_phq9_psycho_dat_complete = na.omit(no_phq9_psycho_dat)
quasi_tot_no_phq9_psycho_n = dim(no_phq9_psycho_dat_complete)[1]
quasi_tot_no_phq9_psycho_n

### Put together all results
missing_results = data.frame(full_n, quasi_itt_n, quasi_tot_n, quasi_itt_drop_out_rate, quasi_tot_drop_out_rate, quasi_tot_no_phq9_psycho_n)
missing_results = round(missing_results, 3)
missing_results = t(missing_results)
colnames(missing_results)= "n_percent"
#### Add a column with explainations for each of them
explain = c("Total number of participants. Anyone who assigned an ID is included that was not .1. Excluded if not assigned a treatment", "Total number of participants who completed at least 70% of a discharge. This data set still contains missing values.", "Total number of complete cases.", "Percentage of clients who did not complete at least 70% of discharge.", "Percentage of missing data.", "N for complete data without PHQ-9 or Psychotherapy.")
missing_results = data.frame(missing_results, explain)

write.csv(missing_results, "missing_results.csv")

```
Descriptive statistics

Who provided (program staff) what services (modality, type, intensity, duration), to whom (individual characteristics), in what context (system, community)?

 (1) a minimum of 5,660  youth will receive all components of the enhanced post-crisis follow-up intervention by the discharge survey 
 
 (3) a minimum of  5,660  youth enrolled in the enhanced post-crisis follow-up intervention will complete a safety plan and successfully implement all aspects of the plan at least 80% of the time;
```{r}
head(tlc_complete)
### 1
dim(tlc_complete)
describe.factor(tlc_complete$TXPackageAssigned)
describe.factor(tlc_complete$Gender)
describe.factor(tlc_complete$HispanicLatino)
describe.factor(tlc_complete$RaceEthnicity)
describe.factor(tlc_complete$SexualOrientation)
describe.factor(tlc_complete$HoursPsychotherapy)
mean(tlc_complete$HoursPsychotherapy)
sd(tlc_complete$HoursPsychotherapy)
describe.factor(tlc_complete$CurrentlyEngaged)
mean(tlc_complete$ReferralsProvided)
sd(tlc_complete$ReferralsProvided)
### 3
describe.factor(tlc_complete$CrisisPlan80Time)
head(tlc_complete)
```
(4) self-stigma of mental illness, thwarted belongingness, and perceived burdensomeness will each decrease 30% among youth enrolled in enhanced post-crisis follow-up by the discharge assessment; and 
(5) scores on the suicidal ideation scale will decrease by 40% among youth enrolled in the enhanced post-crisis follow-up intervention
(6) Scores on the recovery assessment scale will increase by 35% among youth enrolled in the enhanced post-crisis follow-up intervention
```{r}
head(tlc_complete)
library(psych)
describe_results_base =  describe(tlc_complete[,13:22])
describe_results_base = describe_results_base[,c(3,4,8,9)]
describe_results_base = round(describe_results_base, 3)
range_base = paste0(describe_results_base$min, sep = ",", describe_results_base$max)
range_base = data.frame(range_base = range_base)
describe_results_base = data.frame(describe_results_base,range_base)
describe_results_base
describe_results_base = describe_results_base[,-c(3:4)] 
describe_results_base
write.csv(describe_results_base, "describe_results_base.csv", row.names = TRUE)

#####################
describe_results_discharge =  describe(tlc_complete[,23:32])
describe_results_discharge = describe_results_discharge[,c(3,4,8,9)]
describe_results_discharge = round(describe_results_discharge, 3)
range_discharge = paste0(describe_results_discharge$min, sep = ",", describe_results_discharge$max)
range_discharge = data.frame(range_discharge = range_discharge)
describe_results_discharge = data.frame(describe_results_discharge,range_discharge)
describe_results_discharge
describe_results_discharge = describe_results_discharge[,-c(3:4)] 
describe_results_discharge
write.csv(describe_results_discharge, "describe_results_discharge.csv", row.names = TRUE)

```
(1)	a minimum of 5,660  youth will receive all components of the enhanced post-crisis follow-up intervention by the discharge survey 

referrals will be retained at least 50% of the time among youth enrolled in the post-crisis follow-up intervention and follow-through with appointments will occur at least 75% of the time
```{r}
dim(tlc_complete)

tlc_complete$ReferralsEngaged_binary = ifelse(tlc_complete$ReferralsEngaged > 0,1,0)
describe.factor(tlc_complete$ReferralsEngaged_binary)
describe.factor(tlc_complete$Attend75Referrals)
```


Check assumptions of normality
```{r}
outcomes_tests = tlc_complete[,13:22]
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

 •	What are the effects of the interventions on participants?

Indiviudal treatment models
Put together model for each of the outcomes.  Then run loop on the outcomes

(4) self-stigma of mental illness, thwarted belongingness, and perceived burdensomeness will each decrease 30% among youth enrolled in enhanced post-crisis follow-up by the discharge assessment; and 
(5) scores on the suicidal ideation scale will decrease by 40% among youth enrolled in the enhanced post-crisis follow-up intervention, 
(6) Scores on the recovery assessment scale will increase by 35% among youth enrolled in the enhanced post-crisis follow-up intervention. 

```{r}
########## Phone only
library(gvlma)
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned == 1)
outcomes_b_t1 = tlc_complete_t1[13:22]
outcomes_d_t1 = tlc_complete_t1[23:32]
library(effsize)


results_t1 = list()
for(i in 1:length(outcomes_b_t1)){
  results_t1[[i]] = cohen.d(outcomes_d_t1[[i]],outcomes_b_t1[[i]], paired = TRUE)
  results_t1[[i]] = results_t1[[i]][c(3,5)]
}
results_t1 = unlist(results_t1)
results_t1 = matrix(results_t1, ncol= 3, byrow = TRUE)
colnames(results_t1) = c("d", "lower", "upper")
results_t1 = data.frame(round(results_t1,3))
sig = ifelse(results_t1$lower < 0 & results_t1$upper > 0, "", "*")
results_t1$d = paste0(results_t1$d, sig)

ci_95 = paste0(results_t1$lower, sep = ",", results_t1$upper)
results_t1 = data.frame(results_t1, ci_95)
results_t1 = data.frame(d = results_t1$d, ci_95 = results_t1$ci_95)
results_t1


########## Phone + Text
tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned == 2)
outcomes_b_t2 = tlc_complete_t2[13:22]
outcomes_d_t2 = tlc_complete_t2[23:32]
library(effsize)

results_t2 = list()
for(i in 1:length(outcomes_b_t2)){
  results_t2[[i]] = cohen.d(outcomes_d_t2[[i]],outcomes_b_t2[[i]], paired = TRUE)
  results_t2[[i]] = results_t2[[i]][c(3,5)]
}
results_t2 = unlist(results_t2)
results_t2 = matrix(results_t2, ncol= 3, byrow = TRUE)
colnames(results_t2) = c("d", "lower", "upper")
results_t2 = data.frame(round(results_t2,3))
sig = ifelse(results_t2$lower < 0 & results_t2$upper > 0, "", "*")
results_t2$d = paste0(results_t2$d, sig)

ci_95 = paste0(results_t2$lower, sep = ",", results_t2$upper)
results_t2 = data.frame(results_t2, ci_95)
results_t2 = data.frame(d = results_t2$d, ci_95 = results_t2$ci_95)
results_t2

####### Phone + text + face to face
tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned == 3)
outcomes_b_t3 = tlc_complete_t3[13:22]
outcomes_d_t3 = tlc_complete_t3[23:32]
library(effsize)

results_t3 = list()
for(i in 1:length(outcomes_b_t3)){
  results_t3[[i]] = cohen.d(outcomes_d_t3[[i]],outcomes_b_t3[[i]], paired = TRUE)
  results_t3[[i]] = results_t3[[i]][c(3,5)]
}
results_t3 = unlist(results_t3)
results_t3 = matrix(results_t3, ncol= 3, byrow = TRUE)
colnames(results_t3) = c("d", "lower", "upper")
results_t3 = data.frame(round(results_t3,3))
sig = ifelse(results_t3$lower < 0 & results_t3$upper > 0, "", "*")
results_t3$d = paste0(results_t3$d, sig)

ci_95 = paste0(results_t3$lower, sep = ",", results_t3$upper)
results_t3 = data.frame(results_t3, ci_95)
results_t3 = data.frame(d = results_t3$d, ci_95 = results_t3$ci_95)
results_t3


########### Combine all results
within_results_target = rbind(results_t1, results_t2, results_t3)
var_names = rep(names(outcomes_d_t3), 3)
within_results_target = data.frame(var_names, within_results_target) 
within_results_target

write.csv(within_results_target, "within_results_target.csv", row.names = FALSE)
```

Now comparison models for each 
Need to figure out how to grab the effects and compare them
Contrasts are asking whether t3-t2

•	Does the effect vary by mode of intervention (i.e., phone, phone and caring texts, phone and face-to-face contacts)

(8) Youth’s outcomes will not vary by mode of treatment (i.e., phone, phone & face-to-face, phone & caring texts). 
```{r}
### standardized
### Try linear regression version and see if contrasts are similar
library(multcomp)
### Try linear regression version and see if contrasts are similar
outcomes_freq = tlc_complete[,33:42]
outcomes_freq_stand = data.frame(apply(outcomes_freq, 2, function(x){scale(x)}))
outcomes_freq_results = list()
outcomes_freq_sum = list()
outcomes_freq_con = list()
outcomes_freq_results_conf = list()
outcomes_freq_results_check = list()
t = list()
t_sum = list()
t_conf = list()

for(i in 1:length(outcomes_freq)){
  outcomes_freq_results[[i]] = lm(outcomes_freq_stand[[i]] ~   factor(TXPackageAssigned), data = tlc_complete)
  outcomes_freq_sum[[i]] = summary(outcomes_freq_results[[i]])
  outcomes_freq_sum[[i]] = outcomes_freq_sum[[i]][[4]][2:3,1:2]
  outcomes_freq_results_conf[[i]] = confint(outcomes_freq_results[[i]])
  outcomes_freq_results_conf[[i]] = outcomes_freq_results_conf[[i]][2:3,]
  outcomes_freq_results_check[[i]] = gvlma(outcomes_freq_results[[i]])
  K = matrix(c(0, 1,-1), ncol = 3, nrow = 1, byrow = TRUE)
  t[[i]] = glht(outcomes_freq_results[[i]], linfct = K)
  t_sum[[i]] = summary(t[[i]])
  t_conf[[i]] = confint(t[[i]])
}
### For outcomes
outcomes_freq_sum
outcomes_freq_sum = unlist(outcomes_freq_sum)
write.csv(outcomes_freq_sum, "outcomes_freq_sum.csv", row.names = FALSE)
outcomes_freq_sum = read.csv("outcomes_freq_sum.csv", header = TRUE)
outcomes_freq_sum
outcomes_freq_sum = matrix(outcomes_freq_sum$x, ncol = 4, byrow = TRUE)
outcomes_freq_sum = data.frame(estimate_1 = outcomes_freq_sum[,1], se_1 = outcomes_freq_sum[,3], estimate_2 = outcomes_freq_sum[,2], se_2 = outcomes_freq_sum[,4])

est1 = data.frame(estimate = outcomes_freq_sum$estimate_1)
est2 = data.frame(estimate = outcomes_freq_sum$estimate_2)
ests = rbind(est1, est2)
ests

se1 = data.frame(se = outcomes_freq_sum$se_1)
se2 = data.frame(se = outcomes_freq_sum$se_2)
ses = rbind(se1, se2)
ses

ests_ses = round(data.frame(ests, ses),3)
ests_ses
### Now for ci of outcomes
outcomes_freq_results_conf
outcomes_freq_results_conf = unlist(outcomes_freq_results_conf)
write.csv(outcomes_freq_results_conf, "outcomes_freq_results_conf_stand.csv", row.names = FALSE)
outcomes_freq_results_conf = read.csv("outcomes_freq_results_conf_stand.csv", header = TRUE)
outcomes_freq_results_conf
outcomes_freq_results_conf = matrix(outcomes_freq_results_conf$x, ncol = 4, byrow = TRUE)
outcomes_freq_results_conf = data.frame(outcomes_freq_results_conf)
outcomes_freq_results_conf = data.frame(lower_1 = outcomes_freq_results_conf$X1, upper_1 = outcomes_freq_results_conf$X3, lower_2 = outcomes_freq_results_conf$X2, upper_2 = outcomes_freq_results_conf$X4)
outcomes_freq_results_conf

ci_1_upper = data.frame(upper = outcomes_freq_results_conf$upper_1)
ci_2_upper = data.frame(upper = outcomes_freq_results_conf$upper_2)
ci_upper = rbind(ci_1_upper, ci_2_upper)

ci_1_lower = data.frame(lower = outcomes_freq_results_conf$lower_1)
ci_2_lower = data.frame(lower = outcomes_freq_results_conf$lower_2)
ci_lower = rbind(ci_1_lower, ci_2_lower)

ci_upper_lower = round(data.frame(ci_lower, ci_upper),3)

sig_target_between = ifelse(ci_upper_lower$lower < 0 & ci_upper_lower$upper > 0, "", "*")
ests_ses$estimate = paste0(ests_ses$estimate,sig_target_between)


ci_target_between = paste0(ci_upper_lower$lower, sep = ",", ci_upper_lower$upper)


var_names_between = rep(names(outcomes_freq), 2)
between_target_results = data.frame(var_names_between, ests_ses, ci_target_between)
between_target_results

write.csv(between_target_results, "between_target_results.csv", row.names = FALSE)

t_sum
t_conf
outcomes_freq_stand
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
### Try coding as binary for everything besies treatment package
##
impute_dat$female = ifelse(impute_dat$Gender == 2, 1, 0)
impute_dat$Gender = NULL
impute_dat$non_white = ifelse(impute_dat$RaceEthnicity == 3,0,1)
impute_dat$RaceEthnicity = NULL
impute_dat$sexual_minority = ifelse(impute_dat$SexualOrientation == 5,0,1)
impute_dat$SexualOrientation = NULL

impute_dat[,30:39] = NULL

a.out = amelia(x = impute_dat, m = 5, noms = c("TXPackageAssigned" ,"female", "HispanicLatino", "non_white", "sexual_minority", "CurrentlyEngaged", "Attend75Referrals", "CrisisPlan80Time"), logs = c("ReferralsEngaged", "ReferralsProvided", "HoursPsychotherapy"))
compare.density(a.out, var = "non_white")
summary(a.out)
disperse(a.out)

impute_dat_loop = a.out$imputations
```


######################
Within target results
######################
Phone only
Get means and sds then meld together so you don't have to deal with it, then you can calcaulate cohen's d by hand for all of them.
```{r}
### Subset the data for just the variables that you want
tlc_within_dat_impute = list()
tlc_within_dat_impute_mean = list()
tlc_within_dat_impute_sd = list()
tlc_results_within_impute_t1 = list()
for(i in 1:length(impute_dat_loop)){
  tlc_within_dat_impute[[i]] = impute_dat_loop[[i]][10:29]
  tlc_within_dat_impute_mean[[i]] = apply(tlc_within_dat_impute[[i]], 2, mean)
  tlc_within_dat_impute_sd[[i]] = apply(tlc_within_dat_impute[[i]], 2, sd)
}
tlc_within_dat_impute_mean
tlc_within_dat_impute_mean = unlist(tlc_within_dat_impute_mean)
length(tlc_within_dat_impute_mean)
tlc_within_dat_impute_mean = matrix(tlc_within_dat_impute_mean, ncol = 20, byrow = TRUE)
tlc_within_dat_impute_mean

tlc_within_dat_impute_sd = unlist(tlc_within_dat_impute_sd)
tlc_within_dat_impute_sd = matrix(tlc_within_dat_impute_sd, ncol = 20, byrow = TRUE)

tlc_within_dat_impute_mean_sd = mi.meld(tlc_within_dat_impute_mean, tlc_within_dat_impute_sd)
tlc_within_dat_impute_mean_sd

### 

```


######################
Between target
######################
```{r}

### Create difference scores
out_diff_dat = list()
for(i in 1:length(impute_dat_loop)){
  out_diff_dat[[i]] = impute_dat_loop[[i]][20:29]-impute_dat_loop[[i]][10:19]
  colnames(out_diff_dat[[i]]) = c("RAS_1_diff", "RAS_2_diff", "RAS_3_diff", "RAS_5_diff", "INQ_1_diff", "INQ_2_diff", "SSMI_diff", "SIS_1_diff", "SIS_2_diff", "PHQ9_diff")
  out_diff_dat[[i]] = scale(out_diff_dat[[i]])
  out_diff_dat[[i]] =cbind(impute_dat_loop[[i]], out_diff_dat[[i]])
}
out_diff_dat
impute_target_between_results = list()
impute_target_between_results_sum = list()
se_con = list()
t = list()
for(i in 1:length(out_diff_dat)){
  impute_target_between_results[[i]]=lm(cbind(RAS_1_diff, RAS_2_diff,RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SSMI_diff, SIS_1_diff, SIS_2_diff, PHQ9_diff) ~ factor(TXPackageAssigned), data = out_diff_dat[[i]])
}

impute_target_between_results_1 = summary(impute_target_between_results[[1]])
impute_target_between_results_2 = summary(impute_target_between_results[[2]])
impute_target_between_results_3 = summary(impute_target_between_results[[3]])
impute_target_between_results_4 = summary(impute_target_between_results[[4]])
impute_target_between_results_5 = summary(impute_target_between_results[[5]])


coefs_1 = list()
ses_1 = list()
for(i in 1:length(impute_target_between_results_1)){
  coefs_1[[i]] = impute_target_between_results_1[[i]]$coefficients[2:3,1]
  ses_1[[i]] = impute_target_between_results_1[[i]]$coefficients[2:3,2]
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
for(i in 1:length(impute_target_between_results_2)){
  coefs_2[[i]] = impute_target_between_results_2[[i]]$coefficients[2:3,1]
  ses_2[[i]] = impute_target_between_results_2[[i]]$coefficients[2:3,2]
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
for(i in 1:length(impute_target_between_results_3)){
  coefs_3[[i]] = impute_target_between_results_3[[i]]$coefficients[2:3,1]
  ses_3[[i]] = impute_target_between_results_3[[i]]$coefficients[2:3,2]
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
for(i in 1:length(impute_target_between_results_4)){
  coefs_4[[i]] = impute_target_between_results_4[[i]]$coefficients[2:3,1]
  ses_4[[i]] = impute_target_between_results_4[[i]]$coefficients[2:3,2]
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
for(i in 1:length(impute_target_between_results_5)){
  coefs_5[[i]] = impute_target_between_results_5[[i]]$coefficients[2:3,1]
  ses_5[[i]] = impute_target_between_results_5[[i]]$coefficients[2:3,2]
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
p_values = round(2*pt(-abs(t_stats), df = 199),3)
#Critica t
critical_ts= abs(qt(0.05/2, 199))
critical_ts
upper = round(coefs_ses$q.mi+(critical_ts*coefs_ses$se.mi),3)
lower = round(coefs_ses$q.mi-(critical_ts*coefs_ses$se.mi),3)
ci_95 = paste0(lower, sep=",", upper)

tlc_between_impute_results = data.frame(t(coefs_ses$q.mi), t(coefs_ses$se.mi), t(p_values), ci_95)
colnames(tlc_between_impute_results) = c("parameter_estimate", "se", "p_value", "ci_95")
tlc_between_impute_results[,1:2] = round(tlc_between_impute_results[,1:2], 3)
tlc_between_impute_results$parameter_estimate = ifelse(tlc_between_impute_results$p_value < .05, paste0(tlc_between_impute_results$parameter_estimate, "*"), tlc_between_impute_results$parameter_estimate)
tlc_between_impute_results

#### Get contrasts
phone_text_par = tlc_between_impute_results$parameter_estimate[seq(from = 1, to = 20, by = 2)]
phone_text_se = tlc_between_impute_results$se[seq(from = 1, to = 20, by = 2)]

phone_text_face_par = tlc_between_impute_results$parameter_estimate[seq(from = 2, to = 20, by = 2)] 
phone_text_face_se = tlc_between_impute_results$se[seq(from = 2, to = 20, by = 2)] 

### Clogg 1995: z = (b1-b2)/sqrt((se1^2+se2^2))
par_diff = (phone_text_par-phone_text_face_par)
se = sqrt(phone_text_se^2+phone_text_face_se^2)
z_con = par_diff/se
p_value_con = round(2*pnorm(-abs(z_con)),3)
upper_con = par_diff+1.96*se
lower_con = par_diff-1.96*se

tlc_bewteen_impute_results_con = round(data.frame(parameter_estimate = par_diff, se = se, p_value = p_value_con, upper_con, lower_con),3)
tlc_bewteen_impute_results_con$ci_95 = paste0(tlc_bewteen_impute_results_con$lower_con, sep = ",", tlc_bewteen_impute_results_con$upper_con)
tlc_bewteen_impute_results_con[,4:5] = NULL
tlc_bewteen_impute_results_con

```
Look into the differences between the two data sets
```{r}
### Check out differences in means between the two
describe(impute_dat_test)
library(prettyR)
impute_dat_test = data.frame(impute_dat_test)
describe.factor(impute_dat_test$TXPackageAssigned, impute_dat_test$RAS_1_diff)
impute_dat_test

write.csv(impute_dat_test, "impute_dat_test.csv", row.names = FALSE)

write.csv(outcomes_freq_stand, "outcomes_freq_stand.csv", row.names = FALSE)
```




Try testing whether the inclusion of HoursPsychotherapy, CurrentlyEngaged makes a difference

•	What program/contextual factors are associated with which outcomes?
```{r}
describe(outcomes_freq_stand)
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned == 1)
cor(tlc_complete_t1[,7:12])
outcomes_t1 = tlc_complete_t1[,13:22]

results_t1 = list()
for(i in 1:length(outcomes_t1)){
  results_t1[[i]] = summary(stan_glm(outcomes_t1[[i]] ~ HoursPsychotherapy  + Attend75Referrals+ CrisisPlan80Time, data = tlc_complete_t1))
}

results_t1

tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned == 2)
outcomes_t2 = tlc_complete_t2[,13:22]
describe(outcomes_t2)

results_t2 = list()
for(i in 1:length(outcomes_t2)){
  results_t2[[i]] = summary(stan_glm(outcomes_t2[[i]] ~ HoursPsychotherapy  + Attend75Referrals+  CrisisPlan80Time, data = tlc_complete_t2))
}
results_t2

tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned == 3)
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
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned == 1)
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

tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned == 2)
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

tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned == 3)
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
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned == 1)
outcomes_t1 = tlc_complete_t1[,13:22]

head(tlc_complete_t1)

results_t1 = list()
for(i in 1:length(outcomes_t1)){
  results_t1[[i]] = summary(stan_glm(outcomes_t1[[i]] ~ PHQ9_b, data = tlc_complete_t1))
}

results_t1

tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned == 2)
outcomes_t2 = tlc_complete_t2[,13:22]

results_t2 = list()
for(i in 1:length(outcomes_t2)){
  results_t2[[i]] = summary(stan_glm(outcomes_t2[[i]] ~PHQ9_b, data = tlc_complete_t2))
}
results_t2

tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned == 3)
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
