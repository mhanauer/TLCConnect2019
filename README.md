---
title: "TLC Connect 2019"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load data
Double check data fall into each category
Do baseline descriptives with all baseline data and put the percentage of missing data (just calcualte for now)
Then get analytical sample 
Then set up indiviudal regression for outcome and run a loop (set outcomes in own data set and run outcome on that data set)
Same for comparisons

```{r}
library(prettyR)
library(rstanarm)
setwd("P:/Evaluation/TN Lives Count_Connect/Databases")
tlc_data = read.csv("TLCConnect_10_1_2019.csv", header = TRUE, na.strings = c(-6,-7,-8,-9))
head(tlc_data)
## Change youth id 
colnames(tlc_data)[1] = "YouthID"
head(tlc_data)

```
Now grab the variables that you want
YouthID, Age, Gender, HispanicLatino, RaceEthnicity, SexualOrientation, RAS_B_D's, INQ_B_D's, SSMI_B_D's, SIS_B_D's, PHQ9_1 and PHQ9_4, CSSRS1, and CSSRS4 (probably get rid of this var)

ReferralsEngaged, AttendFirstAppointment, SuicidalityWarrentingCrisisVisit
```{r}
describe.factor(tlc_data$CSSRS1)
describe.factor(tlc_data$CSSRS4)
describe.factor(tlc_data$AttemptSuicide)
head(tlc_data)
tlc_data_analysis = tlc_data[,c(1,2,5:9, 11, 13:56, 69:112,118,124)]
tlc_data_analysis = data.frame(tlc_data_analysis, HoursPsychotherapy = tlc_data$HoursPsychotherapy, CurrentlyEngaged  =  tlc_data$CurrentlyEngaged, ReferralsEngaged = tlc_data$ReferralsEngaged, Attend75Referrals = tlc_data$Attend75Referrals, ReferralsProvided = tlc_data$ReferralsProvided, CrisisPlan80Time = tlc_data$CrisisPlan80Time)
head(tlc_data_analysis)
```
Check all variables are within the ranges
```{r}
apply(tlc_data_analysis, 2, function(x){describe.factor(x)})
head(tlc_data_analysis)
```
Create average scores.  Use average scores so you can keep if one out of total is missing and have a accurate assessment
```{r}
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
sum(is.na(PHQ9_diff))
head(tlc_data_analysis)
#### Create new data with average scores
#apply(tlc_data_analysis, 2, function(x){describe.factor(x)})
tlc_data_analysis_average = data.frame(tlc_data_analysis[,c(2,4:8, 99:104)], RAS_1_diff ,RAS_2_diff, RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SSMI_diff, SIS_1_diff, SIS_2_diff, PHQ9_diff, RAS_b_1_average, RAS_b_2_average, RAS_b_3_average, RAS_b_5_average, INQ_b_1_average, INQ_b_2_average, SSMI_b_average, SIS_b_1_average, SIS_b_2_average, PHQ9_b = tlc_data_analysis$PHQ9_1,RAS_d_1_average, RAS_d_2_average, RAS_d_3_average, RAS_d_5_average, INQ_d_1_average, INQ_d_2_average, SSMI_d_average, SIS_d_1_average, SIS_d_2_average,PHQ9_d = tlc_data_analysis$PHQ9_4)
head(tlc_data_analysis_average)

# 
```
Evaluate missing data
Get percentage of missing data for each variable
Test missing assumption
Get rid of missing data
```{r}
library(MissMech)
library(naniar)
## Ok to delete data
#TestMCARNormality(tlc_data_analysis_average)
dim(tlc_data_analysis_average)
miss_var_summary(tlc_data_analysis_average)
tlc_complete = na.omit(tlc_data_analysis_average)
1- (dim(tlc_complete)[1]/dim(tlc_data_analysis_average)[1])
dim(tlc_complete)[1]
```
Descriptive statistics

Who provided (program staff) what services (modality, type, intensity, duration), to whom (individual characteristics), in what context (system, community)?

 (1) a minimum of 5,660  youth will receive all components of the enhanced post-crisis follow-up intervention by the discharge survey 
 
 (3) a minimum of  5,660  youth enrolled in the enhanced post-crisis follow-up intervention will complete a safety plan and successfully implement all aspects of the plan at least 80% of the time;
```{r}
head(tlc_complete)
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
describe.factor(tlc_complete$CrisisPlan80Time)
head(tlc_complete)
```
Look at means and sds over time 
```{r}
head(tlc_complete)
describe(tlc_complete[,23:42])
apply(tlc_complete[,23:42], 2, range)
p_change = data.frame(describe(tlc_complete[,23:42]))
write.csv(p_change, "p_change.csv", row.names = TRUE)
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
library(forecast)
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned == 1)
outcomes_t1 = tlc_complete_t1[,13:22]
describe(outcomes_t1)

results_t1 = list()
model_t1 = list()
con_t1 = list()
resisuals_check_t1 = list()
for(i in 1:length(outcomes_t1)){
  model_t1[[i]] = lm(outcomes_t1[[i]] ~ 1, data = outcomes_t1)
  results_t1[[i]] = summary(model_t1[[i]])
  results_t1[[i]] = results_t1[[i]][[4]][1:2]
  con_t1[[i]] = confint(model_t1[[i]])
  resisuals_check_t1[[i]] = checkresiduals(model_t1[[i]])
}
results_t1
results_t1 = unlist(results_t1)
results_t1 = matrix(results_t1, ncol = 2, byrow = TRUE)
write.csv(results_t1, "results_t1.csv", row.names = FALSE)
results_t1
con_t1 = data.frame(con_t1)
con_t1 = matrix(con_t1, ncol = 2, byrow = TRUE)
write.csv(con_t1, "con_t1.csv", row.names = FALSE)


tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned == 2)
outcomes_t2 = tlc_complete_t2[,13:22]
describe(outcomes_t2)

results_t2 = list()
model_t2 = list()
con_t2 = list()
resisuals_check_t2 = list()
for(i in 1:length(outcomes_t2)){
  model_t2[[i]] = lm(outcomes_t2[[i]] ~ 1, data = outcomes_t2)
  results_t2[[i]] = summary(model_t2[[i]])
  results_t2[[i]] = results_t2[[i]][[4]][1:2]
  con_t2[[i]] = confint(model_t2[[i]])
  resisuals_check_t2[[i]] = checkresiduals(model_t2[[i]])
}
results_t2
results_t2 = unlist(results_t2)
results_t2 = matrix(results_t2, ncol = 2, byrow = TRUE)
write.csv(results_t2, "results_t2.csv", row.names = FALSE)
results_t2
con_t2 = data.frame(con_t2)
con_t2 = matrix(con_t2, ncol = 2, byrow = TRUE)
write.csv(con_t2, "con_t2.csv", row.names = FALSE)

tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned == 3)
outcomes_t3 = tlc_complete_t3[,13:22]
describe(outcomes_t3)

results_t3 = list()
model_t3 = list()
con_t3 = list()
resisuals_check_t3 = list()
for(i in 1:length(outcomes_t3)){
  model_t3[[i]] = lm(outcomes_t3[[i]] ~ 1, data = outcomes_t3)
  results_t3[[i]] = summary(model_t3[[i]])
  results_t3[[i]] = results_t3[[i]][[4]][1:2]
  con_t3[[i]] = confint(model_t3[[i]])
  resisuals_check_t3[[i]] = checkresiduals(model_t3[[i]])
}

results_t3 = unlist(results_t3)
results_t3 = matrix(results_t3, ncol = 2, byrow = TRUE)
write.csv(results_t3, "results_t3.csv", row.names = FALSE)
results_t3
con_t3 = data.frame(con_t3)
con_t3 = matrix(con_t3, ncol = 2, byrow = TRUE)
write.csv(con_t3, "con_t3.csv", row.names = FALSE)

```

Now comparison models for each 
Need to figure out how to grab the effects and compare them
Contrasts are asking whether t3-t2

•	Does the effect vary by mode of intervention (i.e., phone, phone and caring texts, phone and face-to-face contacts)

(8) Youth’s outcomes will not vary by mode of treatment (i.e., phone, phone & face-to-face, phone & caring texts). 
```{r}
#### Regular
library(multcomp)
### Try linear regression version and see if contrasts are similar
outcomes_freq = tlc_complete[,13:22]
outcomes_freq_stand = data.frame(apply(outcomes_freq, 2, function(x){scale(x)}))
outcomes_freq_results = list()
outcomes_freq_sum = list()
outcomes_freq_con = list()
outcomes_freq_results_conf = list()  
t = list()
t_sum = list()
t_conf = list()
for(i in 1:length(outcomes_freq)){
  outcomes_freq_results[[i]] = lm(outcomes_freq[[i]] ~   factor(TXPackageAssigned), data = tlc_complete)
  outcomes_freq_sum[[i]] = summary(outcomes_freq_results[[i]])
  outcomes_freq_sum[[i]] = outcomes_freq_sum[[i]][[4]][2:3,1:2]
  outcomes_freq_results_conf[[i]] = confint(outcomes_freq_results[[i]])
  outcomes_freq_results_conf[[i]] = outcomes_freq_results_conf[[i]][2:3,]
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
outcomes_freq_sum = data.frame(outcomes_freq_sum)
outcomes_freq_sum = data.frame(estimate_1 = outcomes_freq_sum$X1, se_1 = outcomes_freq_sum$X3, estimate_2 = outcomes_freq_sum$X2, se_2 = outcomes_freq_sum$X4)
write.csv(outcomes_freq_sum, "outcomes_freq_sum.csv", row.names = FALSE)

### Now for ci of outcomes
outcomes_freq_results_conf
outcomes_freq_results_conf = unlist(outcomes_freq_results_conf)
write.csv(outcomes_freq_results_conf, "outcomes_freq_results_conf.csv", row.names = FALSE)
outcomes_freq_results_conf = read.csv("outcomes_freq_results_conf.csv", header = TRUE)
outcomes_freq_results_conf
outcomes_freq_results_conf = matrix(outcomes_freq_results_conf$x, ncol = 4, byrow = TRUE)
outcomes_freq_results_conf = data.frame(outcomes_freq_results_conf)
outcomes_freq_results_conf = data.frame(upper_1 = outcomes_freq_results_conf$X1, lower_1 = outcomes_freq_results_conf$X3, upper_2 = outcomes_freq_results_conf$X2, lower_2 = outcomes_freq_results_conf$X4)
write.csv(outcomes_freq_results_conf, "outcomes_freq_results_conf.csv", row.names = FALSE)
t_sum
t_conf
### standardized
### Try linear regression version and see if contrasts are similar
library(multcomp)
### Try linear regression version and see if contrasts are similar
outcomes_freq = tlc_complete[,13:22]
outcomes_freq_stand = data.frame(apply(outcomes_freq, 2, function(x){scale(x)}))
outcomes_freq_results = list()
outcomes_freq_sum = list()
outcomes_freq_con = list()
outcomes_freq_results_conf = list()  
t = list()
t_sum = list()
t_conf = list()
for(i in 1:length(outcomes_freq)){
  outcomes_freq_results[[i]] = lm(outcomes_freq_stand[[i]] ~   factor(TXPackageAssigned), data = tlc_complete)
  outcomes_freq_sum[[i]] = summary(outcomes_freq_results[[i]])
  outcomes_freq_sum[[i]] = outcomes_freq_sum[[i]][[4]][2:3,1:2]
  outcomes_freq_results_conf[[i]] = confint(outcomes_freq_results[[i]])
  outcomes_freq_results_conf[[i]] = outcomes_freq_results_conf[[i]][2:3,]
  K = matrix(c(0, 1,-1), ncol = 3, nrow = 1, byrow = TRUE)
  t[[i]] = glht(outcomes_freq_results[[i]], linfct = K)
  t_sum[[i]] = summary(t[[i]])
  t_conf[[i]] = confint(t[[i]])
}
### For outcomes
outcomes_freq_sum
outcomes_freq_sum = unlist(outcomes_freq_sum)
write.csv(outcomes_freq_sum, "outcomes_freq_sum_stand.csv", row.names = FALSE)
outcomes_freq_sum = read.csv("outcomes_freq_sum_stand.csv", header = TRUE)
outcomes_freq_sum
outcomes_freq_sum = matrix(outcomes_freq_sum$x, ncol = 4, byrow = TRUE)
outcomes_freq_sum = data.frame(outcomes_freq_sum)
outcomes_freq_sum = data.frame(estimate_1 = outcomes_freq_sum$X1, se_1 = outcomes_freq_sum$X3, estimate_2 = outcomes_freq_sum$X2, se_2 = outcomes_freq_sum$X4)
write.csv(outcomes_freq_sum, "outcomes_freq_sum_stand.csv", row.names = FALSE)

### Now for ci of outcomes
outcomes_freq_results_conf
outcomes_freq_results_conf = unlist(outcomes_freq_results_conf)
write.csv(outcomes_freq_results_conf, "outcomes_freq_results_conf_stand.csv", row.names = FALSE)
outcomes_freq_results_conf = read.csv("outcomes_freq_results_conf_stand.csv", header = TRUE)
outcomes_freq_results_conf
outcomes_freq_results_conf = matrix(outcomes_freq_results_conf$x, ncol = 4, byrow = TRUE)
outcomes_freq_results_conf = data.frame(outcomes_freq_results_conf)
outcomes_freq_results_conf = data.frame(upper_1 = outcomes_freq_results_conf$X1, lower_1 = outcomes_freq_results_conf$X3, upper_2 = outcomes_freq_results_conf$X2, lower_2 = outcomes_freq_results_conf$X4)
write.csv(outcomes_freq_results_conf, "outcomes_freq_results_conf.csv", row.names = FALSE)
outcomes_freq_results_conf
t_sum
t_conf

```
Try testing whether the inclusion of HoursPsychotherapy, CurrentlyEngaged makes a difference

•	What program/contextual factors are associated with which outcomes?
```{r}

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
