## 4625 - R Programming
## Problem Set 5
## March 10
## David Flasterstein

rm(list=ls())
set.seed(12435)
options(stringsAsFactors=F)
library(foreign)

## read in data
anes <- read.dta("~/Documents/PS5/anes_timeseries_2012_stata12.dta")

## model Obama's feeling thermometer score as function
## of Clinton's feeling thermometer score
model1 <- lm(ft_dpc ~ ft_hclinton, anes)

## make a prediction for a single observation with
## hypothetical clinton score of 77
predict(model1, data.frame(ft_hclinton=77))
## we would expect a Obama score of 71.7


## Question 1
## randomly subset the data into two partitions
## use "training set" to build at least three models 
## of Obama's feeling thermometer score
## document carefully how you deal with missingness

# create a data frame of variables of interest as numerics
# ftgr_welfare: rate people on welfare 
# ftgr_tea: how do people feel about the tea party
# ftgr_muslims: how do people feel about muslims
data <- data.frame(anes$caseid, anes$ft_dpc, anes$ftgr_welfare, 
                   anes$ftgr_tea, anes$ftgr_muslims)
colnames(data) <-c("caseid","obama_feelings","weflare","tea","muslims")

# randomly choose which rows to be in training sample
subset <- sample(5914, size=5914/2,replace=FALSE)
# split training and testing groups
train <- data[subset,]
test <- data[-subset,]

