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

RAS_b_average = tlc_data_analysis[,9:28]
RAS_b_average = apply(RAS_b_average, 1, mean, na.rm = TRUE)

INQ_b_1_average = tlc_data_analysis[,29:34]
INQ_b_1_average = apply(INQ_b_1_average, 1, mean, na.rm = TRUE)

INQ_b_2_average = tlc_data_analysis[,35:40]
INQ_b_2_average = apply(INQ_b_2_average, 1, mean, na.rm = TRUE)

SSMI_b_average = tlc_data_analysis[,41:45]
SSMI_b_average =  apply(SSMI_b_average, 1, mean, na.rm = TRUE)

SIS_b_average = tlc_data_analysis[,46:52]
SIS_b_average = apply(SIS_b_average, 1, mean, na.rm =TRUE)

RAS_d_average = tlc_data_analysis[53:72]
RAS_d_average = apply(RAS_d_average, 1, mean, na.rm = TRUE)

INQ_d_1_average = tlc_data_analysis[,73:78]
INQ_d_1_average = apply(INQ_d_1_average, 1, mean, na.rm = TRUE)

INQ_d_2_average = tlc_data_analysis[,79:84]
INQ_d_2_average = apply(INQ_d_2_average, 1, mean, na.rm = TRUE)


SSMI_d_average = tlc_data_analysis[,85:89]
SSMI_d_average = apply(SSMI_d_average, 1, mean, na.rm = TRUE)

SIS_d_average = tlc_data_analysis[,90:96]
SIS_d_average = apply(SIS_d_average, 1, mean, na.rm = TRUE)

### Create difference scores
RAS_diff = RAS_d_average - RAS_b_average
INQ_1_diff = INQ_d_1_average - INQ_b_1_average
INQ_2_diff = INQ_d_2_average - INQ_b_2_average
SSMI_diff = SSMI_d_average-SSMI_b_average
sum(is.na(SSMI_diff))
SIS_diff = SIS_d_average-SIS_b_average
sum(is.na(SIS_diff))
PHQ9_diff = tlc_data_analysis$PHQ9_4 - tlc_data_analysis$PHQ9_1
sum(is.na(PHQ9_diff))
head(tlc_data_analysis)
#### Create new data with average scores
#apply(tlc_data_analysis, 2, function(x){describe.factor(x)})
tlc_data_analysis_average = data.frame(tlc_data_analysis[,c(2,4:8, 99:104)], RAS_diff, INQ_1_diff, INQ_2_diff, SSMI_diff, SIS_diff, PHQ9_diff)
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

Who provided (program staff) what services (modality, type, intensity, duration), to whom (individual characteristics), in what context (system, community)?

 (1) a minimum of 5,660  youth will receive all components of the enhanced post-crisis follow-up intervention by the discharge survey 
 
 (3) a minimum of  5,660  youth enrolled in the enhanced post-crisis follow-up intervention will complete a safety plan and successfully implement all aspects of the plan at least 80% of the time;
```{r}
head(tlc_complete)
describe.factor(tlc_complete$TXPackageAssigned)
describe.factor(tlc_complete$Gender)
describe.factor(tlc_complete$HispanicLatino)
describe.factor(tlc_complete$RaceEthnicity)
describe.factor(tlc_complete$SexualOrientation)
describe.factor(tlc_complete$HoursPsychotherapy)
describe.factor(tlc_complete$CurrentlyEngaged)
describe.factor(tlc_complete$ReferralsProvided)
describe.factor(tlc_complete$CrisisPlan80Time)

head(tlc_complete)
```
Look at means and sds over time 
```{r}
head(tlc_complete)
describe(tlc_complete[,13:18])
apply(tlc_complete[,13:18], 2, range)
```


Figure out this question
referrals will be retained at least 50% of the time among youth enrolled in the post-crisis follow-up intervention and follow-through with appointments will occur at least 75% of the time
```{r}
tlc_complete$ReferralsEngaged_binary = ifelse(tlc_complete$ReferralsEngaged > 0,1,0)
describe.factor(tlc_complete$ReferralsEngaged_binary)
describe.factor(tlc_complete$Attend75Referrals)
```
 •	What are the effects of the interventions on participants?

Indiviudal treatment models
Put together model for each of the outcomes.  Then run loop on the outcomes

(4) self-stigma of mental illness, thwarted belongingness, and perceived burdensomeness will each decrease 30% among youth enrolled in enhanced post-crisis follow-up by the discharge assessment; and 
(5) scores on the suicidal ideation scale will decrease by 40% among youth enrolled in the enhanced post-crisis follow-up intervention, 
(6) Scores on the recovery assessment scale will increase by 35% among youth enrolled in the enhanced post-crisis follow-up intervention. 

```{r}
tlc_complete_t1 = subset(tlc_complete, TXPackageAssigned == 1)
outcomes_t1 = tlc_complete_t1[,13:17]

results_t1 = list()
for(i in 1:length(outcomes_t1)){
  results_t1[[i]] = summary(stan_glm(outcomes_t1[[i]] ~ 1, data = outcomes_t1))
}
results_t1

tlc_complete_t2 = subset(tlc_complete, TXPackageAssigned == 2)
outcomes_t2 = tlc_complete_t2[,13:17]

results_t2 = list()
for(i in 1:length(outcomes_t2)){
  results_t2[[i]] = summary(stan_glm(log(outcomes_t2)[[i]] ~ 1, data = outcomes_t2))
}
results_t2

tlc_complete_t3 = subset(tlc_complete, TXPackageAssigned == 3)
outcomes_t3 = tlc_complete_t3[,13:17]

results_t3 = list()
for(i in 1:length(outcomes_t3)){
  results_t3[[i]] = summary(stan_glm(log(outcomes_t3)[[i]] ~ 1, data = outcomes_t3))
}
results_t3

```

Now comparison models for each 
Need to figure out how to grab the effects and compare them
Contrasts are asking whether t3-t2

•	Does the effect vary by mode of intervention (i.e., phone, phone and caring texts, phone and face-to-face contacts)

(8) Youth’s outcomes will not vary by mode of treatment (i.e., phone, phone & face-to-face, phone & caring texts). 
```{r}
outcomes_all = tlc_complete[,13:17]
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

•	What program/contextual factors are associated with which outcomes?
```{r}
outcomes_all = tlc_complete[,13:17]
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

•	What individual factors were associated with outcomes, including race/ethnicity/sexual identity (sexual orientation/gender identity)?
```{r}
describe.factor(tlc_complete$RaceEthnicity)
tlc_complete$RaceEthnicity_binary = ifelse(tlc_complete$RaceEthnicity == 3, 1, 0)
describe.factor(tlc_complete$SexualOrientation)
tlc_complete$SexualOrientation_binary = ifelse(tlc_complete$SexualOrientation == 5, 1, 0)
describe.factor(tlc_complete$Gender)


outcomes_all = tlc_complete[,13:17]
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
Pyschometrics
Test confirmatory factor because we have support that should be one factor
Then do invar and see if related to any factors that you included
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
measure_invar$SexualOrientation = ifelse(measure_invar$SexualOrientation == 5, 0,1)
measure_invar$Age = as.numeric(measure_invar$Age)
measure_invar$Age = ifelse(measure_invar$Age > mean(measure_invar$Age, na.rm = TRUE), 1, 0)
measure_invar$Gender = ifelse(measure_invar$Gender == 1, 1, 0)

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

## Need over time
```


Get reliability for two factors, test-retest 
```{r}
inq12_b_fac1 = tlc_data_analysis[,29:34]
inq12_b_fac2 = tlc_data_analysis[,35:40]

inq12_d_fac1 = tlc_data_analysis[,73:78]
inq12_b_fac2 = tlc_data_analysis[,79:84]

summary(omega(inq12_b_fac1))
summary(omega(inq12_b_fac2))


inq12_b_fac1_retest = inq12_b_fac1
inq12_b_fac2_retest = inq12_b_fac2
inq12_b_fac1_retest$time = rep(1,dim(inq12_b_fac1_retest)[1])
inq12_b_fac2_retest$time = rep(2,dim(inq12_b_fac2_retest)[1])

colnames(inq12_b_fac1_retest) = colnames(inq12_b_fac2_retest)
inq12_b_d_fac1 = rbind(inq12_b_fac1_retest, inq12_b_fac2_retest)
inq12_b_d_fac1_complete = na.omit(inq12_b_d_fac1)
testRetest(inq12_b_d_fac1)
```










