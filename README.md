# SurveyStratifiedSampleSelection
---
title: "SurveyMissingData"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

First we need to library all of the packages.
```{r}
library(sampling)
library(PracTools)
```
The first thing we are going to do is select a stratified random sample from the dataset nhis.  Nhis has the a variable called stratum that has already placed people into the correct stratum.  We first order the stratum and we can see that there are 3911 unique stratum.   

Get a stratified random sample.  First order your variables by the stratum variable, which in this dataset in the stratum variable.  Then we decide how many stratum we want to select from and from those stratum  

Size  = rep(3,4) says pick three people per stratum.  This should work but doesn't.  It should give the probability of selection ie the weight which the number of people that you selected divided by the total number of people in that stratum.  

If we had two stratum, then it would grab a particular value from each of the cells for the two stratums.  For example, if we had state and region it would select the specfied amount from the first state in the first region then move on the next region in the state and so on for all of the combinations of states and regions.
```{r}
attach(nhis)
nhisTest = nhis[order(stratum), ]
length(nhisTest$stratum)

length(nhisTest$educ_r)

library(sampling)
stsam = strata(data = nhisTest, stratanames = "educ_r", size=c(rep(3,4)), method = "srswor", description = TRUE)

stsam
# To create the base weights you take the inverse probability

wts = 1/stsam$Prob
sum(wts)
# Should equal the total population size.
```
Using PPS sampling here.  PPS is the size of the auxility variable in relative to the number of units being selected.  So here it is number of beds for that row which I think is a hosptial and which ten is then divided by that number to a probaiblity of selection.

This is example is if you did not use a probability same for selection.

Then you need to create the base weight by dividing 1 by the probaiblity.


```{r}
require(PracTools)
require(sampling)
data("smho98")
size = smho98$BEDS
#Recode anything as zero as something else, because it will have zero chance of being selected.
size[size<=5] = 5
# It calculates 10 times the number of beds in this case, 
pk = inclusionprobabilities(size, n = 10)

summary(pk)

set.seed(12345)
# Creating a vector of zeros and ones with one stating that you are in the sample.
sam = UPsystematic(pk)

#Getting the sample data related to the sam variable.
samdat = getdata(smho98, sam)
head(samdat)
# Sam equals one, because we only want to include those units that are included in the sample.
samdat = cbind(samdat, wt = 1/pk[sam==1])
head(samdat)
```
Next multistage sampling.  Use sampling to select first stage to select the clusters.  Multiply the base weight by the nonresponse rate, which the a logisitc regression on factors including
```{r}
require(PracTools)
data(nhis)
# If the survey data has weights can use weighted in type
# Numcl creates the number of classes for propensities
out = pclass(formula = resp ~ age + as.factor(sex) + as.factor(hisp) + as.factor(race), data = nhis, type = "unwtd", link = "logit", numcl = 5)
# Gives you the range of propensities in each class and count.
table(out$p.class, useNA = "always")

summary(out$propensities)

```
Multiply the input weights by inverse cell responses propensities for each group have groups, because model probability isn't that accurate.
```{r}
round(cbind("mean" = by(data = out$propensities, INDICES = out$p.class, FUN = mean)), 3)

# Here we have a weighted resposne rate
round(cbind(by(data.frame(resp = nhis[,"resp"], wt = nhis[,"svywt"]), out$p.class, function(x){weighted.mean(x$resp, x$wt)})), 3)
```
Calibration and Poststrationfication starting with input weights which are base weights adjusted for non-reponses.

Use covariates but not need population totals for auxaility variables.  Take the base weight times the ratio of actual total over estimted total and it can adjust for over or under coverage apply these after the sample selected. 

Use poststratification if you want weight by indicators that you cannot control such as gender or ethnicity.  Like if you do not get a good sample and want it to be representative of what you want.
```{r}
require(survey)
data(api)
dclus1 = svydesign(id = ~ dnum, weights = ~ pw, data = apiclus1, fpc = ~ fpc)
# dnum = is the clustering number or clustering class.
# fpc = population finite correction
# Total for the population covariates.  These are each for school type.
pop.types = data.frame(stype= c("E", "H", "M"), Freq = c(4421, 755, 1018))

dclus1p = postStratify(dclus1, ~stype, pop.types)
rbind(summary(weights(dclus1)), summary(weights(dclus1p)))

svymean(~enroll, dclus1p)

```
Missing data section.  When we weight we are covering completely missing values, because we are weighting up to those values.  Find item with least missing impute, then go to next item and impute using the new imputted values. 

Mean imputtation maybe add a random error with mean zero and variance equal to whatever the variance is.

Stanrard regression for mising much have all values for each item to predict a value.  So you build a model and then plug in the values for the covirates for the missing y value and predict it.

Predictive mean matching is pmm in mice for continous outcome

logreg for binary outcome variable
```{r}
library(mice)
require(mice)
data("nhanes2")
head(nhanes2)

nhances2.imp = mice(nhanes2, seed = 12345)

# how to run regression with these values

# We use the round function to pool the values from the five different data sets
fit = with(nhances2.imp, lm(chl ~ age + bmi))
round(summary(pool(fit)),2)


```
Says the imputation method used for each of the variables.
VisistSequences = impute bmi first and then move to the next one

Predictor matrix = tells us what covariates were used to impute each variable.

FMI = fraction of missing information.
