# Target-TLC_2019
---
title: "Enhanced Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#########################
Target Data Cleaning
##########################
```{r, include=FALSE}
setwd("P:/Evaluation/TN Lives Count_Writing/4_Target1_EnhancedCrisisFollow-up/3_Data & Data Analyses")
datPreAdult = read.csv("Target1EnhancedBaseAdult.csv", header = TRUE)
datPostAdult = read.csv("Target1EnhancedPostAdult.csv", header = TRUE)
datAdultTreat = read.csv("AdultTreatments.csv", header = TRUE)

library(prettyR)


head(datPreAdult)
# subset the variables that you want
datPreAdult =datPreAdult[c(1, 3:4, 6,8,10, 12:13, 15:34, 36:45, 47:51, 53:59)]
dim(datPreAdult)
head(datPostAdult)
datPostAdult = datPostAdult[c(1, 3:22,24:33, 35:39, 41:47)]
head(datPostAdult)
dim(datPostAdult)
# Rename added variables otherwise everything else should be the same
colnames(datPreAdult)[colnames(datPreAdult) == "Added.V2..Thinking.of.Ways.to.Kill.Self"] = "Added"   

### Need to deal with ID problems in here first
describe.factor(datPreAdult$Adult.ID)


dim(datPreAdult)
### Now merge all data with baseline to assess missingness
datAdult = merge(datPreAdult, datPostAdult, all.x = TRUE, by = "Adult.ID")
dim(datAdult)
dim(datPreAdult)
### merge with treatment
datAdult = merge(datAdult, datAdultTreat, all.x = TRUE, by = "Adult.ID")
dim(datAdult)

#### Get the missing ids for Rachel
target_id_treat = data.frame(id = datAdult$Adult.ID, treat = datAdult$Treatment)
target_id_treat
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
whole_number =  is.wholenumber(target_id_treat$id)
target_id_treat$whole_number = whole_number
dim(target_id_treat)
target_id_treat = subset(target_id_treat, whole_number == TRUE)
dim(target_id_treat)
target_id_treat$whole_number = NULL
target_id_treat$na_true = is.na(target_id_treat$treat)
target_id_treat = subset(target_id_treat, na_true == TRUE)
dim(target_id_treat)
write.csv(target_id_treat, "target_id_treat.csv", row.names = FALSE)



##########
#### Three double ids need to get rid of 1272, 1280, 1131 
datAdult = datAdult[-c(259, 262,208),] 
describe.factor(datAdult$Adult.ID)

### Now get rid of .1's
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
whole_number =  is.wholenumber(datAdult$Adult.ID)
datAdult$whole_number = whole_number
dim(datAdult)
datAdult = subset(datAdult, whole_number == TRUE)
dim(datAdult)
datAdult$whole_number = NULL

colnames(datAdult) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "RAS1_b", "RAS2_b", "RAS3_b", "RAS4_b", "RAS5_b", "RAS6_b", "RAS7_b", "RAS8_b", "RAS9_b", "RAS10_b", "RAS11_b", "RAS12_b", "RAS13_b", "RAS14_b", "RAS15_b", "RAS16_b", "RAS17_b", "RAS18_b", "RAS19_b", "RAS20_b", "INQ1_b", "INQ2_b", "INQ3_b", "INQ4_b", "INQ5_b", "INQ6_b", "INQ7_b", "INQ8_b", "INQ9_b", "INQ10_b", "SSMI1_b", "SSMI2_b", "SSMI3_b", "SSMI4_b", "SSMI5_b", "SIS1_b", "SIS2_b", "SIS3_b", "SIS4_b", "SIS5_b", "SIS6_b", "SIS7_b", "RAS1_d", "RAS2_d", "RAS3_d", "RAS4_d", "RAS5_d", "RAS6_d", "RAS7_d", "RAS8_d", "RAS9_d", "RAS10_d", "RAS11_d", "RAS12_d", "RAS13_d", "RAS14_d", "RAS15_d", "RAS16_d", "RAS17_d", "RAS18_d", "RAS19_d", "RAS20_d", "INQ1_d", "INQ2_d", "INQ3_d", "INQ4_d", "INQ5_d", "INQ6_d", "INQ7_d", "INQ8_d", "INQ9_d", "INQ10_d", "SSMI1_d", "SSMI2_d", "SSMI3_d", "SSMI4_d", "SSMI5_d", "SIS1_d", "SIS2_d", "SIS3_d", "SIS4_d", "SIS5_d", "SIS6_d", "SIS7_d", "Treatment")
describe.factor(datAdult$Treatment)

# Two treatments have B with space first so try and recode those as just B's
datAdult$Treatment = ifelse(datAdult$Treatment == "A", 1, ifelse(datAdult$Treatment =="B", 2, ifelse(datAdult$Treatment == " B", 2, ifelse(datAdult$Treatment == "C", 3, datAdult$Treatment)))) 
describe.factor(datAdult$Treatment)

### Werid B changed to 6 so changing it back
datAdult$Treatment = ifelse(datAdult$Treatment == 6 , 2, datAdult$Treatment)
describe.factor(datAdult$Treatment)
# Three items are reversed scored: f = 6, g = 7, j = 10
datAdult$INQ6_b = 8-datAdult$INQ6_b
datAdult$INQ7_b = 8-datAdult$INQ7_b
datAdult$INQ10_b = 8-datAdult$INQ10_b

datAdult$INQ6_d = 8-datAdult$INQ6_d
datAdult$INQ7_d = 8-datAdult$INQ7_d
datAdult$INQ10_d = 8-datAdult$INQ10_d

#### Check for goofy answers looks good
apply(datAdult, 2, function(x){describe.factor(x)})

### Now get the means
head(datAdult)
# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RAS_b_1_average = datAdult[,c(14:21, 28)]
RAS_b_1_average = apply(RAS_b_1_average,1,mean, na.rm = TRUE)

# Subscale two q = 17, r= 18, s= 19
RAS_b_2_average = datAdult[,c(25:27)]
RAS_b_2_average = apply(RAS_b_2_average, 1, mean, na.rm = TRUE)

# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
RAS_b_3_average = datAdult[,9:13]
RAS_b_3_average = apply(RAS_b_3_average, 1, mean, na.rm = TRUE)

# Subscale five: n = 14, o = 15, p = 16
RAS_b_5_average = datAdult[,22:24]
RAS_b_5_average = apply(RAS_b_5_average, 1, mean, na.rm = TRUE)

#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQ_b_1_average = datAdult[,29:33]
INQ_b_1_average = apply(INQ_b_1_average, 1, mean, na.rm = TRUE)

#Subscale 2 for INQ: f-j: 6-10
INQ_b_2_average = datAdult[,35:38]
INQ_b_2_average = apply(INQ_b_2_average, 1, mean, na.rm = TRUE)

#Subscale 1 for SIS: a-d: 1:4
SIS_b_1_average  = datAdult[,44:47]
SIS_b_1_average  = apply(SIS_b_1_average , 1, mean, na.rm = TRUE)


### SSMI
SSMI_b_average = datAdult[,39:43]
SSMI_b_average = apply(SSMI_b_average, 1, mean, na.rm = TRUE)
########
# Now discharge
#########
# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RAS_d_1_average = datAdult[,c(57:63, 70)]
RAS_d_1_average = apply(RAS_d_1_average,1,mean, na.rm = TRUE)

# Subscale two q = 17, r= 18, s= 19
RAS_d_2_average = datAdult[,c(67:69)]
RAS_d_2_average = apply(RAS_d_2_average, 1, mean, na.rm = TRUE)

# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
RAS_d_3_average = datAdult[,51:55]
RAS_d_3_average = apply(RAS_d_3_average, 1, mean, na.rm = TRUE)

# Subscale five: n = 14, o = 15, p = 16
RAS_d_5_average = datAdult[,64:66]
RAS_d_5_average = apply(RAS_d_5_average, 1, mean, na.rm = TRUE)

#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQ_d_1_average = datAdult[,71:75]
INQ_d_1_average = apply(INQ_d_1_average, 1, mean, na.rm = TRUE)

#Subscale 2 for INQ: f-j: 6-10
INQ_d_2_average = datAdult[,76:80]
INQ_d_2_average = apply(INQ_d_2_average, 1, mean, na.rm = TRUE)

#Subscale 1 for SIS: a-d: 1:4
SIS_d_1_average  = datAdult[,86:89]
SIS_d_1_average  = apply(SIS_d_1_average , 1, mean, na.rm = TRUE)

### SSMI
SSMI_d_average =datAdult[,81:85]
SSMI_d_average = apply(SSMI_d_average, 1, mean, na.rm = TRUE)
#################
# Clean up demographics
datAdult
## Gender female = 1 versus male = 0
describe.factor(datAdult$Gender)
female = ifelse(datAdult$Gender == 1, 0,1)
## Race non-white = 1 versus white = 0
describe.factor(datAdult$Race)
non_white = ifelse(datAdult$Race == 7,0,1)
### Single =1 versus non-single 0
describe.factor(datAdult$RelationshipStatus)
single = ifelse(datAdult$RelationshipStatus == 1, 1, 0)
### Sexual orientation
### sexual_minority != 3, hetero = 3
describe.factor(datAdult$SexualOrientation)
sexual_minority = ifelse(datAdult$SexualOrientation != 3, 1, 0)

### edu high school or greater 2 or above
describe.factor(datAdult$Edu)
high_school_greater = ifelse(datAdult$Edu > 1, 1, 0)

#### employement 2,3 employed and all else not employed
describe.factor(datAdult$Employment)
employed = ifelse(datAdult$Employment == 2, 1, ifelse(datAdult$Employment == 3, 1, 0))


### treatment
treatment =  datAdult$Treatment
describe.factor(treatment)
########## 
# Put together Target dat set
#################
target_dat = data.frame(ID = datAdult$ID, treatment, age = datAdult$Age, female, non_white, single, sexual_minority, high_school_greater, employed, RAS_b_1_average, RAS_b_2_average, RAS_b_3_average, RAS_b_5_average, INQ_b_1_average, INQ_b_2_average, SIS_b_1_average,SSMI_b_average, RAS_d_1_average, RAS_d_2_average, RAS_d_3_average, RAS_d_5_average, INQ_d_1_average, INQ_d_2_average, SIS_d_1_average,SSMI_d_average)

target_dat

```
##############
Target
Assess missing data
#####################
```{r}
library(MissMech)
library(naniar)
#TestMCARNormality(dat_pre_post_adult[,10:92])
dim(target_dat)
var_missing =  miss_var_summary(target_dat)
var_missing = data.frame(var_missing)
var_missing
full_n = dim(target_dat)[1]
full_n
############## Getting rid of anybody who doesn't have a follow-up that meet the criteria
quasi_itt = apply(target_dat[,18:25], 1, function(x){sum(is.na(x))})
quasi_itt
n_drop = round(.7*length(target_dat[,18:25]),0)
quasi_itt_dat = data.frame(target_dat,quasi_itt)
quasi_itt_dat
quasi_itt_dat = subset(quasi_itt_dat, quasi_itt < n_drop)
dim(quasi_itt_dat)
quasi_itt_dat$quasi_itt = NULL
quasi_itt_n = dim(quasi_itt_dat)[1]
### Percentage of drop for quasi itt
quasi_itt_drop_out_rate = 1-(dim(quasi_itt_dat)[1]/dim(target_dat)[1])
quasi_itt_drop_out_rate
quasi_itt_missing_percent = prop_miss_case(quasi_itt_dat)
quasi_tot_dat =  quasi_itt_dat 
quasi_tot_dat = na.omit(quasi_tot_dat)
quasi_tot_n = dim(quasi_tot_dat)[1]
quasi_tot_drop_out_rate = 1-(dim(quasi_tot_dat)[1]/dim(tlc_data_analysis_average)[1])
###
missing_results = data.frame(full_n, quasi_itt_n, quasi_tot_n, quasi_itt_drop_out_rate, quasi_tot_drop_out_rate)
missing_results = round(missing_results, 3)
missing_results = t(missing_results)
colnames(missing_results)= "n_percent"
#### Add a column with explainations for each of them
explain = c("Total number of participants. Anyone who assigned an ID is included that was not .1. Excluded if not assigned a treatment", "Total number of participants who completed at least 70% of a discharge. This data set still contains missing values.", "Total number of complete cases.", "Percentage of clients who did not complete at least 70% of discharge.", "Percentage of missing data.")
missing_results = data.frame(missing_results, explain)

write.csv(missing_results, "missing_results.csv")
target_dat_complete = na.omit(target_dat)

```
####################
Target Descriptives
###################
```{r}
library(psych)
des_cat_target_dat_complete = target_dat_complete[,c(2,4:9)]
des_cat_target_dat_complete = apply(des_cat_target_dat_complete, 2, function(x){describe.factor(x, decr.order = FALSE)})
des_cat_target_dat_complete = data.frame(des_cat_target_dat_complete)
des_cat_target_dat_complete = t(des_cat_target_dat_complete)
des_cat_target_dat_complete

des_con_target_dat_complete = target_dat_complete[,-c(1,2,4:9)]
des_con_target_dat_complete = describe(des_con_target_dat_complete)
des_con_target_dat_complete = des_con_target_dat_complete[,c(3,4,8,9)]
des_con_target_dat_complete = round(des_con_target_dat_complete, 3)
range = paste0(des_con_target_dat_complete$min, sep = ",", des_con_target_dat_complete$max)
des_con_target_dat_complete = data.frame(des_con_target_dat_complete, range)
des_con_target_dat_complete[,3:4] = NULL
des_con_target_dat_complete
############# Range

```
#############
Target Impute
#############
```{r}
library(Amelia)
impute_dat = quasi_itt_dat
dim(impute_dat)
impute_dat$SIS_d_1_average
a.out = amelia(x = impute_dat, m = 5, noms = c("treatment" ,"female", "single", "non_white", "sexual_minority", "high_school_greater", "employed"))
compare.density(a.out, var = "SIS_d_1_average")
impute_dat_loop = a.out$imputations
impute_dat_loop
dim(impute_dat_loop$imp1)
```


##################
Target within ITT
##################
```{r}
#### T1 within change
target_within_t1_base_d1 = subset(impute_dat_loop[[1]][,c(2,10:17)], treatment == 1)
target_within_t1_dis_d1 = subset(impute_dat_loop[[1]][,c(2,18:25)], treatment == 1)
target_within_t1_base_d1$treatment = NULL
target_within_t1_dis_d1$treatment = NULL
target_within_t1_d1_results = list()
for(i in 1:length(target_within_t1_base_d1)){
  target_within_t1_d1_results[[i]] = cohen.d(target_within_t1_dis_d1[[i]], target_within_t1_base_d1[[i]], paired = TRUE)
  target_within_t1_d1_results[[i]] = target_within_t1_d1_results[[i]][c(3,5)]
  
}
target_within_t1_d1_results
target_within_t1_d1_results =  unlist(target_within_t1_d1_results)
target_within_t1_d1_results = matrix(target_within_t1_d1_results, ncol = 3, byrow = TRUE)
target_within_t1_d1_results = data.frame(target_within_t1_d1_results)
target_within_t1_d1_results = round(target_within_t1_d1_results, 3)
colnames(target_within_t1_d1_results) = c("cohen_d", "lower", "upper")
target_within_t1_d1_results

target_within_t1_base_d2 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 1)
target_within_t1_dis_d2 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 1)
target_within_t1_base_d2$treatment = NULL
target_within_t1_dis_d2$treatment = NULL
target_within_t1_d2_results = list()
for(i in 1:length(target_within_t1_base_d2)){
  target_within_t1_d2_results[[i]] = cohen.d(target_within_t1_dis_d2[[i]], target_within_t1_base_d2[[i]], paired = TRUE)
  target_within_t1_d2_results[[i]] = target_within_t1_d2_results[[i]][c(3,5)]
  
}
target_within_t1_d2_results
target_within_t1_d2_results =  unlist(target_within_t1_d2_results)
target_within_t1_d2_results = matrix(target_within_t1_d2_results, ncol = 3, byrow = TRUE)
target_within_t1_d2_results = data.frame(target_within_t1_d2_results)
target_within_t1_d2_results = round(target_within_t1_d2_results, 3)
colnames(target_within_t1_d2_results) = c("cohen_d", "lower", "upper")
target_within_t1_d2_results

target_within_t1_base_d3 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 1)
target_within_t1_dis_d3 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 1)
target_within_t1_base_d3$treatment = NULL
target_within_t1_dis_d3$treatment = NULL
target_within_t1_d3_results = list()
for(i in 1:length(target_within_t1_base_d3)){
  target_within_t1_d3_results[[i]] = cohen.d(target_within_t1_dis_d3[[i]], target_within_t1_base_d3[[i]], paired = TRUE)
  target_within_t1_d3_results[[i]] = target_within_t1_d3_results[[i]][c(3,5)]
  
}
target_within_t1_d3_results
target_within_t1_d3_results =  unlist(target_within_t1_d3_results)
target_within_t1_d3_results = matrix(target_within_t1_d3_results, ncol = 3, byrow = TRUE)
target_within_t1_d3_results = data.frame(target_within_t1_d3_results)
target_within_t1_d3_results = round(target_within_t1_d3_results, 3)
colnames(target_within_t1_d3_results) = c("cohen_d", "lower", "upper")
target_within_t1_d3_results

target_within_t1_base_d4 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 1)
target_within_t1_dis_d4 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 1)
target_within_t1_base_d4$treatment = NULL
target_within_t1_dis_d4$treatment = NULL
target_within_t1_d4_results = list()
for(i in 1:length(target_within_t1_base_d4)){
  target_within_t1_d4_results[[i]] = cohen.d(target_within_t1_dis_d4[[i]], target_within_t1_base_d4[[i]], paired = TRUE)
  target_within_t1_d4_results[[i]] = target_within_t1_d4_results[[i]][c(3,5)]
  
}
target_within_t1_d4_results
target_within_t1_d4_results =  unlist(target_within_t1_d4_results)
target_within_t1_d4_results = matrix(target_within_t1_d4_results, ncol = 3, byrow = TRUE)
target_within_t1_d4_results = data.frame(target_within_t1_d4_results)
target_within_t1_d4_results = round(target_within_t1_d4_results, 3)
colnames(target_within_t1_d4_results) = c("cohen_d", "lower", "upper")
target_within_t1_d4_results

target_within_t1_base_d5 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 1)
target_within_t1_dis_d5 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 1)
target_within_t1_base_d5$treatment = NULL
target_within_t1_dis_d5$treatment = NULL
target_within_t1_d5_results = list()
for(i in 1:length(target_within_t1_base_d5)){
  target_within_t1_d5_results[[i]] = cohen.d(target_within_t1_dis_d5[[i]], target_within_t1_base_d5[[i]], paired = TRUE)
  target_within_t1_d5_results[[i]] = target_within_t1_d5_results[[i]][c(3,5)]
  
}
target_within_t1_d5_results
target_within_t1_d5_results =  unlist(target_within_t1_d5_results)
target_within_t1_d5_results = matrix(target_within_t1_d5_results, ncol = 3, byrow = TRUE)
target_within_t1_d5_results = data.frame(target_within_t1_d5_results)
target_within_t1_d5_results = round(target_within_t1_d5_results, 3)
colnames(target_within_t1_d5_results) = c("cohen_d", "lower", "upper")
target_within_t1_d5_results

target_within_t1_cohen_d = data.frame(cohen_d1 = target_within_t1_d1_results$cohen_d, cohen_d2 = target_within_t1_d2_results$cohen_d, cohen_d3 = target_within_t1_d3_results$cohen_d, cohen_d4 = target_within_t1_d4_results$cohen_d,cohen_d5 = target_within_t1_d5_results$cohen_d)
target_within_t1_cohen_d = rowMeans(target_within_t1_cohen_d)
target_within_t1_cohen_d

target_within_t1_lower = data.frame(lower1 = target_within_t1_d1_results$lower, lower2 = target_within_t1_d2_results$lower, lower3 = target_within_t1_d3_results$lower, lower4 = target_within_t1_d4_results$lower,lower5 = target_within_t1_d5_results$lower)
target_within_t1_lower = rowMeans(target_within_t1_lower)
target_within_t1_lower

target_within_t1_upper = data.frame(upper1 = target_within_t1_d1_results$upper, upper2 = target_within_t1_d2_results$upper, upper3 = target_within_t1_d3_results$upper, upper4 = target_within_t1_d4_results$upper,upper5 = target_within_t1_d5_results$upper)
target_within_t1_upper = rowMeans(target_within_t1_upper)
target_within_t1_upper

target_within_t1_results = data.frame(cohen_d = target_within_t1_cohen_d, lower = target_within_t1_lower, upper = target_within_t1_upper)
target_within_t1_results = round(target_within_t1_results, 3)
target_within_t1_results
target_within_t1_results$cohen_d = ifelse(target_within_t1_results$lower < 0 & target_within_t1_results$upper > 0, target_within_t1_results$cohen_d, paste0(target_within_t1_results$cohen_d, "*"))
target_within_t1_results
target_within_t1_results$ci_95 = paste0(target_within_t1_results$lower, sep = ",", target_within_t1_results$upper)
target_within_t1_results[,2:3] = NULL
target_within_t1_results

########### T2
#### T1 within change
target_within_t2_base_d1 = subset(impute_dat_loop[[1]][,c(2,10:17)], treatment == 2)
target_within_t2_dis_d1 = subset(impute_dat_loop[[1]][,c(2,18:25)], treatment == 2)
target_within_t2_base_d1$treatment = NULL
target_within_t2_dis_d1$treatment = NULL
target_within_t2_d1_results = list()
for(i in 1:length(target_within_t2_base_d1)){
  target_within_t2_d1_results[[i]] = cohen.d(target_within_t2_dis_d1[[i]], target_within_t2_base_d1[[i]], paired = TRUE)
  target_within_t2_d1_results[[i]] = target_within_t2_d1_results[[i]][c(3,5)]
  
}
target_within_t2_d1_results
target_within_t2_d1_results =  unlist(target_within_t2_d1_results)
target_within_t2_d1_results = matrix(target_within_t2_d1_results, ncol = 3, byrow = TRUE)
target_within_t2_d1_results = data.frame(target_within_t2_d1_results)
target_within_t2_d1_results = round(target_within_t2_d1_results, 3)
colnames(target_within_t2_d1_results) = c("cohen_d", "lower", "upper")
target_within_t2_d1_results

target_within_t2_base_d2 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 2)
target_within_t2_dis_d2 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 2)
target_within_t2_base_d2$treatment = NULL
target_within_t2_dis_d2$treatment = NULL
target_within_t2_d2_results = list()
for(i in 1:length(target_within_t2_base_d2)){
  target_within_t2_d2_results[[i]] = cohen.d(target_within_t2_dis_d2[[i]], target_within_t2_base_d2[[i]], paired = TRUE)
  target_within_t2_d2_results[[i]] = target_within_t2_d2_results[[i]][c(3,5)]
  
}
target_within_t2_d2_results
target_within_t2_d2_results =  unlist(target_within_t2_d2_results)
target_within_t2_d2_results = matrix(target_within_t2_d2_results, ncol = 3, byrow = TRUE)
target_within_t2_d2_results = data.frame(target_within_t2_d2_results)
target_within_t2_d2_results = round(target_within_t2_d2_results, 3)
colnames(target_within_t2_d2_results) = c("cohen_d", "lower", "upper")
target_within_t2_d2_results

target_within_t2_base_d3 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 2)
target_within_t2_dis_d3 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 2)
target_within_t2_base_d3$treatment = NULL
target_within_t2_dis_d3$treatment = NULL
target_within_t2_d3_results = list()
for(i in 1:length(target_within_t2_base_d3)){
  target_within_t2_d3_results[[i]] = cohen.d(target_within_t2_dis_d3[[i]], target_within_t2_base_d3[[i]], paired = TRUE)
  target_within_t2_d3_results[[i]] = target_within_t2_d3_results[[i]][c(3,5)]
  
}
target_within_t2_d3_results
target_within_t2_d3_results =  unlist(target_within_t2_d3_results)
target_within_t2_d3_results = matrix(target_within_t2_d3_results, ncol = 3, byrow = TRUE)
target_within_t2_d3_results = data.frame(target_within_t2_d3_results)
target_within_t2_d3_results = round(target_within_t2_d3_results, 3)
colnames(target_within_t2_d3_results) = c("cohen_d", "lower", "upper")
target_within_t2_d3_results

target_within_t2_base_d4 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 2)
target_within_t2_dis_d4 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 2)
target_within_t2_base_d4$treatment = NULL
target_within_t2_dis_d4$treatment = NULL
target_within_t2_d4_results = list()
for(i in 1:length(target_within_t2_base_d4)){
  target_within_t2_d4_results[[i]] = cohen.d(target_within_t2_dis_d4[[i]], target_within_t2_base_d4[[i]], paired = TRUE)
  target_within_t2_d4_results[[i]] = target_within_t2_d4_results[[i]][c(3,5)]
  
}
target_within_t2_d4_results
target_within_t2_d4_results =  unlist(target_within_t2_d4_results)
target_within_t2_d4_results = matrix(target_within_t2_d4_results, ncol = 3, byrow = TRUE)
target_within_t2_d4_results = data.frame(target_within_t2_d4_results)
target_within_t2_d4_results = round(target_within_t2_d4_results, 3)
colnames(target_within_t2_d4_results) = c("cohen_d", "lower", "upper")
target_within_t2_d4_results

target_within_t2_base_d5 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 2)
target_within_t2_dis_d5 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 2)
target_within_t2_base_d5$treatment = NULL
target_within_t2_dis_d5$treatment = NULL
target_within_t2_d5_results = list()
for(i in 1:length(target_within_t2_base_d5)){
  target_within_t2_d5_results[[i]] = cohen.d(target_within_t2_dis_d5[[i]], target_within_t2_base_d5[[i]], paired = TRUE)
  target_within_t2_d5_results[[i]] = target_within_t2_d5_results[[i]][c(3,5)]
  
}
target_within_t2_d5_results
target_within_t2_d5_results =  unlist(target_within_t2_d5_results)
target_within_t2_d5_results = matrix(target_within_t2_d5_results, ncol = 3, byrow = TRUE)
target_within_t2_d5_results = data.frame(target_within_t2_d5_results)
target_within_t2_d5_results = round(target_within_t2_d5_results, 3)
colnames(target_within_t2_d5_results) = c("cohen_d", "lower", "upper")
target_within_t2_d5_results

target_within_t2_cohen_d = data.frame(cohen_d1 = target_within_t2_d1_results$cohen_d, cohen_d2 = target_within_t2_d2_results$cohen_d, cohen_d3 = target_within_t2_d3_results$cohen_d, cohen_d4 = target_within_t2_d4_results$cohen_d,cohen_d5 = target_within_t2_d5_results$cohen_d)
target_within_t2_cohen_d = rowMeans(target_within_t2_cohen_d)
target_within_t2_cohen_d

target_within_t2_lower = data.frame(lower1 = target_within_t2_d1_results$lower, lower2 = target_within_t2_d2_results$lower, lower3 = target_within_t2_d3_results$lower, lower4 = target_within_t2_d4_results$lower,lower5 = target_within_t2_d5_results$lower)
target_within_t2_lower = rowMeans(target_within_t2_lower)
target_within_t2_lower

target_within_t2_upper = data.frame(upper1 = target_within_t2_d1_results$upper, upper2 = target_within_t2_d2_results$upper, upper3 = target_within_t2_d3_results$upper, upper4 = target_within_t2_d4_results$upper,upper5 = target_within_t2_d5_results$upper)
target_within_t2_upper = rowMeans(target_within_t2_upper)
target_within_t2_upper

target_within_t2_results = data.frame(cohen_d = target_within_t2_cohen_d, lower = target_within_t2_lower, upper = target_within_t2_upper)
target_within_t2_results = round(target_within_t2_results, 3)
target_within_t2_results
target_within_t2_results$cohen_d = ifelse(target_within_t2_results$lower < 0 & target_within_t2_results$upper > 0, target_within_t2_results$cohen_d, paste0(target_within_t2_results$cohen_d, "*"))
target_within_t2_results
target_within_t2_results$ci_95 = paste0(target_within_t2_results$lower, sep = ",", target_within_t2_results$upper)
target_within_t2_results[,2:3] = NULL
target_within_t2_results

###### T3 
#### T1 within change
target_within_t3_base_d1 = subset(impute_dat_loop[[1]][,c(2,10:17)], treatment == 3)
target_within_t3_dis_d1 = subset(impute_dat_loop[[1]][,c(2,18:25)], treatment == 3)
target_within_t3_base_d1$treatment = NULL
target_within_t3_dis_d1$treatment = NULL
target_within_t3_d1_results = list()
for(i in 1:length(target_within_t3_base_d1)){
  target_within_t3_d1_results[[i]] = cohen.d(target_within_t3_dis_d1[[i]], target_within_t3_base_d1[[i]], paired = TRUE)
  target_within_t3_d1_results[[i]] = target_within_t3_d1_results[[i]][c(3,5)]
  
}
target_within_t3_d1_results
target_within_t3_d1_results =  unlist(target_within_t3_d1_results)
target_within_t3_d1_results = matrix(target_within_t3_d1_results, ncol = 3, byrow = TRUE)
target_within_t3_d1_results = data.frame(target_within_t3_d1_results)
target_within_t3_d1_results = round(target_within_t3_d1_results, 3)
colnames(target_within_t3_d1_results) = c("cohen_d", "lower", "upper")
target_within_t3_d1_results

target_within_t3_base_d2 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 3)
target_within_t3_dis_d2 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 3)
target_within_t3_base_d2$treatment = NULL
target_within_t3_dis_d2$treatment = NULL
target_within_t3_d2_results = list()
for(i in 1:length(target_within_t3_base_d2)){
  target_within_t3_d2_results[[i]] = cohen.d(target_within_t3_dis_d2[[i]], target_within_t3_base_d2[[i]], paired = TRUE)
  target_within_t3_d2_results[[i]] = target_within_t3_d2_results[[i]][c(3,5)]
  
}
target_within_t3_d2_results
target_within_t3_d2_results =  unlist(target_within_t3_d2_results)
target_within_t3_d2_results = matrix(target_within_t3_d2_results, ncol = 3, byrow = TRUE)
target_within_t3_d2_results = data.frame(target_within_t3_d2_results)
target_within_t3_d2_results = round(target_within_t3_d2_results, 3)
colnames(target_within_t3_d2_results) = c("cohen_d", "lower", "upper")
target_within_t3_d2_results

target_within_t3_base_d3 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 3)
target_within_t3_dis_d3 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 3)
target_within_t3_base_d3$treatment = NULL
target_within_t3_dis_d3$treatment = NULL
target_within_t3_d3_results = list()
for(i in 1:length(target_within_t3_base_d3)){
  target_within_t3_d3_results[[i]] = cohen.d(target_within_t3_dis_d3[[i]], target_within_t3_base_d3[[i]], paired = TRUE)
  target_within_t3_d3_results[[i]] = target_within_t3_d3_results[[i]][c(3,5)]
  
}
target_within_t3_d3_results
target_within_t3_d3_results =  unlist(target_within_t3_d3_results)
target_within_t3_d3_results = matrix(target_within_t3_d3_results, ncol = 3, byrow = TRUE)
target_within_t3_d3_results = data.frame(target_within_t3_d3_results)
target_within_t3_d3_results = round(target_within_t3_d3_results, 3)
colnames(target_within_t3_d3_results) = c("cohen_d", "lower", "upper")
target_within_t3_d3_results

target_within_t3_base_d4 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 3)
target_within_t3_dis_d4 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 3)
target_within_t3_base_d4$treatment = NULL
target_within_t3_dis_d4$treatment = NULL
target_within_t3_d4_results = list()
for(i in 1:length(target_within_t3_base_d4)){
  target_within_t3_d4_results[[i]] = cohen.d(target_within_t3_dis_d4[[i]], target_within_t3_base_d4[[i]], paired = TRUE)
  target_within_t3_d4_results[[i]] = target_within_t3_d4_results[[i]][c(3,5)]
  
}
target_within_t3_d4_results
target_within_t3_d4_results =  unlist(target_within_t3_d4_results)
target_within_t3_d4_results = matrix(target_within_t3_d4_results, ncol = 3, byrow = TRUE)
target_within_t3_d4_results = data.frame(target_within_t3_d4_results)
target_within_t3_d4_results = round(target_within_t3_d4_results, 3)
colnames(target_within_t3_d4_results) = c("cohen_d", "lower", "upper")
target_within_t3_d4_results

target_within_t3_base_d5 = subset(impute_dat_loop[[2]][,c(2,10:17)], treatment == 3)
target_within_t3_dis_d5 = subset(impute_dat_loop[[2]][,c(2,18:25)], treatment == 3)
target_within_t3_base_d5$treatment = NULL
target_within_t3_dis_d5$treatment = NULL
target_within_t3_d5_results = list()
for(i in 1:length(target_within_t3_base_d5)){
  target_within_t3_d5_results[[i]] = cohen.d(target_within_t3_dis_d5[[i]], target_within_t3_base_d5[[i]], paired = TRUE)
  target_within_t3_d5_results[[i]] = target_within_t3_d5_results[[i]][c(3,5)]
  
}
target_within_t3_d5_results
target_within_t3_d5_results =  unlist(target_within_t3_d5_results)
target_within_t3_d5_results = matrix(target_within_t3_d5_results, ncol = 3, byrow = TRUE)
target_within_t3_d5_results = data.frame(target_within_t3_d5_results)
target_within_t3_d5_results = round(target_within_t3_d5_results, 3)
colnames(target_within_t3_d5_results) = c("cohen_d", "lower", "upper")
target_within_t3_d5_results

target_within_t3_cohen_d = data.frame(cohen_d1 = target_within_t3_d1_results$cohen_d, cohen_d2 = target_within_t3_d2_results$cohen_d, cohen_d3 = target_within_t3_d3_results$cohen_d, cohen_d4 = target_within_t3_d4_results$cohen_d,cohen_d5 = target_within_t3_d5_results$cohen_d)
target_within_t3_cohen_d = rowMeans(target_within_t3_cohen_d)
target_within_t3_cohen_d

target_within_t3_lower = data.frame(lower1 = target_within_t3_d1_results$lower, lower2 = target_within_t3_d2_results$lower, lower3 = target_within_t3_d3_results$lower, lower4 = target_within_t3_d4_results$lower,lower5 = target_within_t3_d5_results$lower)
target_within_t3_lower = rowMeans(target_within_t3_lower)
target_within_t3_lower

target_within_t3_upper = data.frame(upper1 = target_within_t3_d1_results$upper, upper2 = target_within_t3_d2_results$upper, upper3 = target_within_t3_d3_results$upper, upper4 = target_within_t3_d4_results$upper,upper5 = target_within_t3_d5_results$upper)
target_within_t3_upper = rowMeans(target_within_t3_upper)
target_within_t3_upper

target_within_t3_results = data.frame(cohen_d = target_within_t3_cohen_d, lower = target_within_t3_lower, upper = target_within_t3_upper)
target_within_t3_results = round(target_within_t3_results, 3)
target_within_t3_results
target_within_t3_results$cohen_d = ifelse(target_within_t3_results$lower < 0 & target_within_t3_results$upper > 0, target_within_t3_results$cohen_d, paste0(target_within_t3_results$cohen_d, "*"))
target_within_t3_results
target_within_t3_results$ci_95 = paste0(target_within_t3_results$lower, sep = ",", target_within_t3_results$upper)
target_within_t3_results[,2:3] = NULL
target_within_t3_results

target_within_results = rbind(target_within_t1_results, target_within_t2_results, target_within_t3_results)
target_within_results

```
###################
Between ITT Target
##################
```{r}
out_diff_dat = list()
for(i in 1:length(impute_dat_loop)){
  out_diff_dat[[i]] = impute_dat_loop[[i]][18:25]-impute_dat_loop[[i]][10:17]
  colnames(out_diff_dat[[i]]) = c("RAS_1_diff", "RAS_2_diff", "RAS_3_diff", "RAS_5_diff", "INQ_1_diff", "INQ_2_diff", "SSMI_diff", "SIS_1_diff")
  out_diff_dat[[i]] = scale(out_diff_dat[[i]])
  out_diff_dat[[i]] =cbind(impute_dat_loop[[i]], out_diff_dat[[i]])
}
out_diff_dat

impute_target_between_results = list()
impute_target_between_results_sum = list()
se_con = list()
t = list()
for(i in 1:length(out_diff_dat)){
  impute_target_between_results[[i]]=lm(cbind(RAS_1_diff, RAS_2_diff,RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SSMI_diff, SIS_1_diff) ~ factor(treatment), data = out_diff_dat[[i]])
}


impute_target_between_results_1 = summary(impute_target_between_results[[1]])
impute_target_between_results_2 = summary(impute_target_between_results[[2]])
impute_target_between_results_3 = summary(impute_target_between_results[[3]])
impute_target_between_results_4 = summary(impute_target_between_results[[4]])
impute_target_between_results_5 = summary(impute_target_between_results[[5]])
impute_target_between_results_1

coefs_1 = list()
ses_1 = list()
for(i in 1:length(impute_target_between_results_1)){
  coefs_1[[i]] = impute_target_between_results_1[[i]]$coefficients[2:3,1]
  ses_1[[i]] = impute_target_between_results_1[[i]]$coefficients[2:3,2]
}
coefs_1
coefs_1 = unlist(coefs_1)
coefs_1 = matrix(coefs_1, ncol = 16)
coefs_1

ses_1
ses_1 = unlist(ses_1)
ses_1 = matrix(ses_1, ncol = 16)
ses_1

coefs_2 = list()
ses_2 = list()
for(i in 1:length(impute_target_between_results_2)){
  coefs_2[[i]] = impute_target_between_results_2[[i]]$coefficients[2:3,1]
  ses_2[[i]] = impute_target_between_results_2[[i]]$coefficients[2:3,2]
}
coefs_2
coefs_2 = unlist(coefs_2)
coefs_2 = matrix(coefs_2, ncol = 16)
coefs_2

ses_2
ses_2 = unlist(ses_2)
ses_2 = matrix(ses_2, ncol = 16)
ses_2

coefs_3 = list()
ses_3 = list()
for(i in 1:length(impute_target_between_results_3)){
  coefs_3[[i]] = impute_target_between_results_3[[i]]$coefficients[2:3,1]
  ses_3[[i]] = impute_target_between_results_3[[i]]$coefficients[2:3,2]
}
coefs_3
coefs_3 = unlist(coefs_3)
coefs_3 = matrix(coefs_3, ncol = 16)
coefs_3

ses_3
ses_3 = unlist(ses_3)
ses_3 = matrix(ses_3, ncol = 16)
ses_3

coefs_4 = list()
ses_4 = list()
for(i in 1:length(impute_target_between_results_4)){
  coefs_4[[i]] = impute_target_between_results_4[[i]]$coefficients[2:3,1]
  ses_4[[i]] = impute_target_between_results_4[[i]]$coefficients[2:3,2]
}
coefs_4
coefs_4 = unlist(coefs_4)
coefs_4 = matrix(coefs_4, ncol = 16)
coefs_4

ses_4
ses_4 = unlist(ses_4)
ses_4 = matrix(ses_4, ncol = 16)
ses_4

coefs_5 = list()
ses_5 = list()
for(i in 1:length(impute_target_between_results_5)){
  coefs_5[[i]] = impute_target_between_results_5[[i]]$coefficients[2:3,1]
  ses_5[[i]] = impute_target_between_results_5[[i]]$coefficients[2:3,2]
}
coefs_5
coefs_5 = unlist(coefs_5)
coefs_5 = matrix(coefs_5, ncol = 16)
coefs_5

ses_5
ses_5 = unlist(ses_5)
ses_5 = matrix(ses_5, ncol = 16)
ses_5

coefs_all = rbind(coefs_1, coefs_2, coefs_3, coefs_4, coefs_5)
ses_all = rbind(ses_1, ses_2, ses_3, ses_4, ses_5)
coefs_ses =  mi.meld(coefs_all,ses_all)
t_stats = coefs_ses$q.mi / coefs_ses$se.mi

dim(impute_dat_loop$imp1)
# n = 113 minus 4 for parameters
p_values = round(2*pt(-abs(t_stats), df = 109),3)
#Critica t
critical_ts= abs(qt(0.05/2, 199))
critical_ts
upper = round(coefs_ses$q.mi+(critical_ts*coefs_ses$se.mi),3)
lower = round(coefs_ses$q.mi-(critical_ts*coefs_ses$se.mi),3)
ci_95 = paste0(lower, sep=",", upper)

target_between_impute_results = data.frame(t(coefs_ses$q.mi), t(coefs_ses$se.mi), t(p_values), ci_95)
colnames(target_between_impute_results) = c("parameter_estimate", "se", "p_value", "ci_95")
target_between_impute_results[,1:2] = round(target_between_impute_results[,1:2], 3)
target_between_impute_results$parameter_estimate = ifelse(target_between_impute_results$p_value < .05, paste0(target_between_impute_results$parameter_estimate, "*"), target_between_impute_results$parameter_estimate)
target_between_impute_results
```
#############################
Target Between Contrasts
############################
```{r}
se_con_between_d1 = list()
mean_con_bewteen_d1 = list()
for(i in 1:length(impute_target_between_results_1)){
  se_con_between_d1[[i]] = vcov(impute_target_between_results_1[[i]])
  se_con_between_d1[[i]] = sqrt((se_con_between_d1[[i]][,2:3][2]+se_con_between_d1[[i]][,2:3][6])-2*se_con_between_d1[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d1[[i]] = impute_target_between_results_1[[i]]$coefficients[2:3,1]
  mean_con_bewteen_d1[[i]] = data.frame(t(mean_con_bewteen_d1[[i]]))
  mean_con_bewteen_d1[[i]] = mean_con_bewteen_d1[[i]]$factor.treatment.2-mean_con_bewteen_d1[[i]]$factor.treatment.3
}
mean_con_bewteen_d1 = unlist(mean_con_bewteen_d1)
se_con_between_d1 = unlist(se_con_between_d1)


se_con_between_d2 = list()
mean_con_bewteen_d2 = list()
for(i in 1:length(impute_target_between_results_2)){
  se_con_between_d2[[i]] = vcov(impute_target_between_results_2[[i]])
  se_con_between_d2[[i]] = sqrt((se_con_between_d2[[i]][,2:3][2]+se_con_between_d2[[i]][,2:3][6])-2*se_con_between_d2[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d2[[i]] = impute_target_between_results_2[[i]]$coefficients[2:3,1]
  mean_con_bewteen_d2[[i]] = data.frame(t(mean_con_bewteen_d2[[i]]))
  mean_con_bewteen_d2[[i]] = mean_con_bewteen_d2[[i]]$factor.treatment.2-mean_con_bewteen_d2[[i]]$factor.treatment.3
}
mean_con_bewteen_d2 = unlist(mean_con_bewteen_d2)
se_con_between_d2 = unlist(se_con_between_d2)

se_con_between_d3 = list()
mean_con_bewteen_d3 = list()
for(i in 1:length(impute_target_between_results_3)){
  se_con_between_d3[[i]] = vcov(impute_target_between_results_3[[i]])
  se_con_between_d3[[i]] = sqrt((se_con_between_d3[[i]][,2:3][2]+se_con_between_d3[[i]][,2:3][6])-2*se_con_between_d3[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d3[[i]] = impute_target_between_results_3[[i]]$coefficients[2:3,1]
  mean_con_bewteen_d3[[i]] = data.frame(t(mean_con_bewteen_d3[[i]]))
  mean_con_bewteen_d3[[i]] = mean_con_bewteen_d3[[i]]$factor.treatment.2-mean_con_bewteen_d3[[i]]$factor.treatment.3
}
mean_con_bewteen_d3 = unlist(mean_con_bewteen_d3)
se_con_between_d3 = unlist(se_con_between_d3)

se_con_between_d4 = list()
mean_con_bewteen_d4 = list()
for(i in 1:length(impute_target_between_results_4)){
  se_con_between_d4[[i]] = vcov(impute_target_between_results_4[[i]])
  se_con_between_d4[[i]] = sqrt((se_con_between_d4[[i]][,2:3][2]+se_con_between_d4[[i]][,2:3][6])-2*se_con_between_d4[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d4[[i]] = impute_target_between_results_4[[i]]$coefficients[2:3,1]
  mean_con_bewteen_d4[[i]] = data.frame(t(mean_con_bewteen_d4[[i]]))
  mean_con_bewteen_d4[[i]] = mean_con_bewteen_d4[[i]]$factor.treatment.2-mean_con_bewteen_d4[[i]]$factor.treatment.3
}
mean_con_bewteen_d4 = unlist(mean_con_bewteen_d4)
se_con_between_d4 = unlist(se_con_between_d4)

se_con_between_d5 = list()
mean_con_bewteen_d5 = list()
for(i in 1:length(impute_target_between_results_5)){
  se_con_between_d5[[i]] = vcov(impute_target_between_results_5[[i]])
  se_con_between_d5[[i]] = sqrt((se_con_between_d5[[i]][,2:3][2]+se_con_between_d5[[i]][,2:3][6])-2*se_con_between_d5[[i]][,2:3][3])
  ## Now get difference
  mean_con_bewteen_d5[[i]] = impute_target_between_results_5[[i]]$coefficients[2:3,1]
  mean_con_bewteen_d5[[i]] = data.frame(t(mean_con_bewteen_d5[[i]]))
  mean_con_bewteen_d5[[i]] = mean_con_bewteen_d5[[i]]$factor.treatment.2-mean_con_bewteen_d5[[i]]$factor.treatment.3
}
mean_con_bewteen_d5 = unlist(mean_con_bewteen_d5)
se_con_between_d5 = unlist(se_con_between_d5)

mean_con_bewteen = rbind(mean_con_bewteen_d1, mean_con_bewteen_d2, mean_con_bewteen_d3, mean_con_bewteen_d4, mean_con_bewteen_d5)

se_con_between = rbind(se_con_between_d1, se_con_between_d2, se_con_between_d3, se_con_between_d4, se_con_between_d5)


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
```










##############
TOT
#############


#################
Within Study Target
Checking assumptions for t-tests
Not normal 
#########################
```{r}
within_target_norm = target_dat_complete[,10:25]
log_within_target_norm = log(within_target_norm)
results_hist_norm = list()
results_stat_norm = list()
log_hist_norm = list()
apply(log_within_target_norm, 2, hist)
apply(log_within_target_norm, 2, )

for(i in 1:length(within_target_norm)){
  results_hist_norm[[i]] = hist(within_target_norm[[i]])
  log_hist_norm[[i]] = hist(log_within_target_norm[[i]])
  results_stat_norm[[i]] = shapiro.test(within_target_norm[[i]])
}
log_hist_norm
```
###################
Run robust regression with time as covariate  
Check if R^2 changes
```{r}
target_dat_complete_long = reshape(target_dat_complete, varying = list(c("RAS_b_1_average","RAS_d_1_average"),c("RAS_b_2_average", "RAS_d_2_average"), c("RAS_b_3_average", "RAS_d_3_average"), c("RAS_b_5_average", "RAS_d_5_average"), c("INQ_b_1_average", "INQ_d_1_average"), c("INQ_b_2_average", "INQ_d_2_average"), c("SIS_b_1_average" ,"SIS_d_1_average"), c("SSMI_b_average", "SSMI_d_average")), direction = "long", times =c(0,1))

target_dat_complete_long

```
#################################################
Generate regression format for excel pasting
#################################################
```{r}
library(gvlma)
### Create three data sets 
target_dat_complete_long_t1 = subset(target_dat_complete_long, treatment == 1)
target_dat_complete_long_t2 = subset(target_dat_complete_long, treatment == 2)
target_dat_complete_long_t3 = subset(target_dat_complete_long, treatment == 3)
outcomes_within_target_t1 = target_dat_complete_long_t1[,11:18]
results_within_target_t1 = list()
results_within_target_t1_sum = list() 
results_within_target_t1_check = list()
results_within_target_t1_confin = list()
results_within_target_t1_f_2 = list()
results_within_target_t1_sum_pars = list()
library(MASS)

library(forecast)
for(i in 1:length(outcomes_within_target_t1)){
  results_within_target_t1[[i]] = lm(outcomes_within_target_t1[[i]] ~ time, data = target_dat_complete_long_t1)
  results_within_target_t1_sum[[i]] = summary(results_within_target_t1[[i]])
  results_within_target_t1_sum_pars[[i]] =results_within_target_t1_sum[[i]][[4]][2,c(1:2,4)]
  results_within_target_t1_check[[i]] = gvlma(results_within_target_t1[[i]])
  results_within_target_t1_confin[[i]] = confint(results_within_target_t1[[i]]) 
  results_within_target_t1_confin[[i]] = results_within_target_t1_confin[[i]][2,]
  results_within_target_t1_f_2[[i]] = results_within_target_t1_sum[[i]]$adj.r.squared/(1-results_within_target_t1_sum[[i]]$adj.r.squared)
}
summary(results_within_target_t1_check[[1]])

### Figure out how to get them into a format for excel with variable, parameter estimate, se, confint, and f^2
results_within_target_t1_sum_pars = unlist(results_within_target_t1_sum_pars)
results_within_target_t1_sum_pars = matrix(results_within_target_t1_sum_pars, ncol= 3, byrow = TRUE)
colnames(results_within_target_t1_sum_pars) = c("par_est", "se", "p_value")
results_within_target_t1_sum_pars = round(results_within_target_t1_sum_pars, 3)
results_within_target_t1_sum_pars = data.frame(results_within_target_t1_sum_pars)
results_within_target_t1_sum_pars

###### Clean up the confin 
results_within_target_t1_confin= unlist(results_within_target_t1_confin)
results_within_target_t1_confin = matrix(results_within_target_t1_confin, ncol = 2, byrow = TRUE)
results_within_target_t1_confin = data.frame(results_within_target_t1_confin)
results_within_target_t1_confin = round(results_within_target_t1_confin, 3)
results_within_target_t1_confin$ci = paste0(results_within_target_t1_confin$X1, sep=",", results_within_target_t1_confin$X2)
results_within_target_t1_confin = results_within_target_t1_confin[,3]
results_within_target_t1_confin = data.frame(ci = results_within_target_t1_confin)
results_within_target_t1_confin


### Add f^2
results_within_target_t1_f_2 = unlist(results_within_target_t1_f_2)
results_within_target_t1_f_2 = matrix(results_within_target_t1_f_2, ncol = 1, byrow = TRUE)
results_within_target_t1_f_2 = round(results_within_target_t1_f_2, 3)
colnames(results_within_target_t1_f_2) = "f_2"
results_within_target_t1_f_2 = data.frame(results_within_target_t1_f_2)
results_within_target_t1_f_2


#### All data
all_results_within_target_1 = data.frame(results_within_target_t1_sum_pars, ci = results_within_target_t1_confin$ci, f_2 = results_within_target_t1_f_2$f_2)
all_results_within_target_1

```
Within target 1 Standardized version
Between target 1 regular
Between target 1 standard
SIS variance explained version

Now try compare all treatments
```{r}
outcomes_within_target = target_dat_complete_long[,11:18]

results_within_target = list()
results_within_target_sum = list() 
results_within_target_t1_check = list()
results_within_target_t1_confin = list()
results_within_target_t1_f_2 = list()
results_within_target_t1_sum_pars = list()

for(i in 1:length(outcomes_within_target_t1)){
  results_within_target[[i]] = lm(outcomes_within_target[[i]]~ treatment*time, data = target_dat_complete_long)
  results_within_target_sum[[i]] = summary(results_within_target[[i]])
  results_within_target_sum[[i]] = results_within_target_sum[[i]]$coefficients[c(5:6), c(1,2,4)]
}
results_within_target_sum
```
###############
Code for se in contrasts works
```{r}
test_dat =  out_diff_dat[[1]]
test_dat = dummy_cols(test_dat, select_columns = "treatment")
test_model =lm(RAS_1_diff ~ factor(treatment), data = test_dat)
K = matrix(c(0, 1,-1), ncol = 3, nrow = 1, byrow = TRUE)
t= glht(test_model, linfct = K)
summary(t)
test_se = test_model_sum$cov.unscaled
sqrt((test_se[,2:3][2]+test_se[,2:3][6])-2*test_se[,2:3][3])
### sqrt(p1_var + p2_var-2*covar(p1,p2))
```

