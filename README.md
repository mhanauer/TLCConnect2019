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
tlc_data = read.csv("TLCConnect9_23_19.csv", header = TRUE, na.strings = c(-6,-7,-8,-9))
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
tlc_data_analysis = data.frame(tlc_data_analysis, HoursPsychotherapy = tlc_data$HoursPsychotherapy, CurrentlyEngaged  =  tlc_data$CurrentlyEngaged, ReferralsEngaged = tlc_data$ReferralsEngaged, Attend75Referrals = tlc_data$Attend75Referrals)
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

RAS_b_average = tlc_data_analysis[,9:28]
RAS_b_average = apply(RAS_b_average, 1, mean, na.rm = TRUE)

INQ_b_average = tlc_data_analysis[,29:40]
INQ_b_average = apply(INQ_b_average, 1, mean, na.rm = TRUE)

SSMI_b_average = tlc_data_analysis[,41:45]
SSMI_b_average =  apply(SSMI_b_average, 1, mean, na.rm = TRUE)

SIS_b_average = tlc_data_analysis[,46:52]
SIS_b_average = apply(SIS_b_average, 1, mean, na.rm =TRUE)

RAS_d_average = tlc_data_analysis[53:72]
RAS_d_average = apply(RAS_d_average, 1, mean, na.rm = TRUE)

INQ_d_average = tlc_data_analysis[,73:84]
INQ_d_average = apply(INQ_d_average, 1, mean, na.rm = TRUE)

SSMI_d_average = tlc_data_analysis[,85:89]
SSMI_d_average = apply(SSMI_d_average, 1, mean, na.rm = TRUE)

SIS_d_average = tlc_data_analysis[,90:96]
SIS_d_average = apply(SIS_d_average, 1, mean, na.rm = TRUE)

### Create difference scores
RAS_diff = RAS_d_average - RAS_b_average
INQ_diff = INQ_d_average - INQ_b_average
SSMI_diff = SSMI_d_average-SSMI_b_average
sum(is.na(SSMI_diff))
SIS_diff = SIS_d_average-SIS_b_average
sum(is.na(SIS_diff))
PHQ9_diff = tlc_data_analysis$PHQ9_4 - tlc_data_analysis$PHQ9_1
sum(is.na(PHQ9_diff))
head(tlc_data_analysis)
#### Create new data with average scores
#apply(tlc_data_analysis, 2, function(x){describe.factor(x)})
tlc_data_analysis_average = data.frame(tlc_data_analysis[,c(2,4:8, 99:102)], RAS_diff, INQ_diff, SSMI_diff, SIS_diff, PHQ9_diff)
head(tlc_data_analysis_average)
```
Evaluate missing data
Get percentage of missing data for each variable
Test missing assumption
Get rid of missing data
```{r}
library(MissMech)
library(naniar)
## Ok to delete data
TestMCARNormality(tlc_data_analysis_average)
dim(tlc_data_analysis_average)
miss_var_summary(tlc_data_analysis_average)
tlc_complete = na.omit(tlc_data_analysis_average)
1- (dim(tlc_complete)[1]/dim(tlc_data_analysis_average)[1])
dim(tlc_complete)[1]
```
Descriptive statistics
```{r}
head(tlc_complete)
describe.factor(tlc_complete$TXPackageAssigned)
describe.factor(tlc_complete$Gender)
describe.factor(tlc_complete$HispanicLatino)
describe.factor(tlc_complete$RaceEthnicity)
describe.factor(tlc_complete$SexualOrientation)
describe.factor(tlc_complete$HoursPsychotherapy)
describe.factor(tlc_complete$CurrentlyEngaged)
head(tlc_complete)
```
Figure out this question
referrals will be retained at least 50% of the time among youth enrolled in the post-crisis follow-up intervention and follow-through with appointments will occur at least 75% of the time
```{r}
tlc_complete$ReferralsEngaged_binary = ifelse(tlc_complete$ReferralsEngaged > 0,1,0)
describe.factor(tlc_complete$ReferralsEngaged_binary)
describe.factor(tlc_complete$Attend75Referrals)
```


Indiviudal treatment models
Put together model for each of the outcomes.  Then run loop on the outcomes
```{r}
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned == 1)
outcomes_t1 = tlc_complete_t1[,9:13]

results_t1 = list()
for(i in 1:length(outcomes_t1)){
  results_t1[[i]] = summary(stan_glm(outcomes_t1[[i]] ~ 1, data = outcomes_t1))
}
results_t1

tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned == 2)
outcomes_t2 = tlc_complete_t2[,9:13]

results_t2 = list()
for(i in 1:length(outcomes_t2)){
  results_t2[[i]] = summary(stan_glm(outcomes_t2[[i]] ~ 1, data = outcomes_t2))
}
results_t2

tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned == 3)
outcomes_t3 = tlc_complete_t3[,9:13]

results_t3 = list()
for(i in 1:length(outcomes_t3)){
  results_t3[[i]] = summary(stan_glm(outcomes_t3[[i]] ~ 1, data = outcomes_t3))
}
results_t3

```

Now comparison models for each 
Need to figure out how to grab the effects and compare them
Contrasts are asking whether t3-t2
```{r}
outcomes_all = tlc_complete[,9:13]
results_all = list()
contrasts_all = list()
for(i in 1:length(outcomes_all)){
  results_all[[i]] = summary(stan_glm(outcomes_all[[i]] ~ factor(TXPackageAssigned), data = tlc_complete))
  contrasts_all[[i]] = as.data.frame(results_all[[i]])
  contrasts_all[[i]] = data.frame(contrasts_all[[i]][,3]-contrasts_all[[i]][,2])
  contrasts_all[[i]] = summary(stan_glm(contrasts_all[[i]][,1] ~ 1, data = contrasts_all[[i]]))
}
contrasts_all
results_all

```
Try testing whether the inclusion of HoursPsychotherapy, CurrentlyEngaged makes a difference
```{r}
outcomes_all = tlc_complete[,9:13]
results_all = list()
contrasts_all = list()
for(i in 1:length(outcomes_all)){
  results_all[[i]] = summary(stan_glm(outcomes_all[[i]] ~ factor(TXPackageAssigned) +HoursPsychotherapy + CurrentlyEngaged, data = tlc_complete))
  contrasts_all[[i]] = as.data.frame(results_all[[i]])
  contrasts_all[[i]] = data.frame(contrasts_all[[i]][,3]-contrasts_all[[i]][,2])
  contrasts_all[[i]] = summary(stan_glm(contrasts_all[[i]][,1] ~ 1, data = contrasts_all[[i]]))
}
contrasts_all
results_all
```
Try testing whether the inclusion of demos makes a difference
```{r}
describe.factor(tlc_complete$RaceEthnicity)
tlc_complete$RaceEthnicity_binary = ifelse(tlc_complete$RaceEthnicity == 3, 1, 0)
describe.factor(tlc_complete$SexualOrientation)
tlc_complete$SexualOrientation_binary = ifelse(tlc_complete$SexualOrientation == 5, 1, 0)
describe.factor(tlc_complete$Gender)


outcomes_all = tlc_complete[,9:13]
results_all = list()
contrasts_all = list()
for(i in 1:length(outcomes_all)){
  results_all[[i]] = summary(stan_glm(outcomes_all[[i]] ~ factor(TXPackageAssigned) + Age +RaceEthnicity_binary + Gender + SexualOrientation_binary, data = tlc_complete))
  contrasts_all[[i]] = as.data.frame(results_all[[i]])
  contrasts_all[[i]] = data.frame(contrasts_all[[i]][,3]-contrasts_all[[i]][,2])
  contrasts_all[[i]] = summary(stan_glm(contrasts_all[[i]][,1] ~ 1, data = contrasts_all[[i]]))
}
contrasts_all
results_all
```










