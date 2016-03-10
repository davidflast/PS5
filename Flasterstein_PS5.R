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

## Question 1

## Create and subset the data frame with variables of interest
# all ftgr statistics ask respondents to rate groups on a scale from
# 1 to 100
data <- data.frame(anes$caseid, anes$ft_dpc, anes$ftgr_welfare,
                   anes$ftgr_tea, anes$ftgr_liberals,anes$ftgr_congress,
                   anes$ftgr_muslims,anes$ftgr_catholics, anes$ftgr_mormons,
                   anes$ftgr_atheists)
colnames(data) <-c("caseid","obama_feelings","welfare",
                   "tea","liberals", "congress","muslims", "catholics",
                   "mormons", "athiests")
# replaces all -6 and -7 (which mean no response) with NA
# in the functions to calculate fit, the NAs are removed
for(i in 1:10) {
  data[i]<-replace(data[i],data[i]==-6 | data[i]==-7, NA)
}
# randomly choose which rows to be in training sample
subset <- sample(5914, size=5914/2,replace=FALSE)
# split training and testing groups
train <- data[subset,]
test <- data[-subset,]
test_obama <- test$obama_feelings

## Models
# linear model of opinions on welfare recipients
welfare_lm <- lm(obama_feelings ~ welfare, train)
# linear model of opinions on multiple religious minorities
religion_lm <- lm(obama_feelings ~ muslims + catholics + mormons + athiests, train)
# linear model of the conservative trifecta of hating liberals and congress
# and loving the tea party
cons_trifecta_lm <- lm(obama_feelings ~ tea + liberals + congress, train)

## Question 2

# Welfare model predictions
welfare_prediction <- as.numeric(predict(welfare_lm, test))
# Religion model predictions
religion_prediction <- as.numeric(predict(religion_lm, test))
# Conservative model predictions
cons_prediction <- as.numeric(predict(cons_trifecta_lm, test))

## Organize predictions into a matrix
prediction_matrix <- cbind(welfare_prediction,religion_prediction,
                           cons_prediction)

##Questions 4 and 5

## Individual fit statistic functions
# These are helper functions for the main function
# each takes in the vectors of observed and predicted outcomes
# and outputs the corresponding statistic
RMSE_f <- function(pred, obs){
  abs_error <- abs(pred - obs)
  rmse <- sqrt(mean(abs_error^2, na.rm=T))
  return(rmse)
}
MAD_f <- function(pred, obs){
  abs_error <- abs(pred - obs)
  mad <- median(abs_error, na.rm=T)
  return(mad)
}
RMSLE_f <- function(pred, obs){
  ln_pred <- log(abs(pred + 1))
  ln_obs <- log(abs(obs + 1))
  rmsle <- sqrt(mean(ln_pred - ln_obs, na.rm=T))
  return(rmsle)
}
MAPE_f <- function(pred, obs){
  abs_error <- abs(pred - obs)
  abs_per_error <- (abs_error/abs(pred))* 100
  mape <- mean(abs_per_error, na.rm=T)
  return(mape)
}
MEAPE_f <- function(pred, obs){
  abs_error <- abs(pred - obs)
  abs_per_error <- (abs_error/abs(pred))* 100
  meape <- median(abs_per_error, na.rm=T)
  return(meape)
}


# Function for checking fit statistics
#
# This function takes in observed and predicted data and
# returns the chosen fit statistics
# @param y vector of observed values
# @param P vector of corresponding predictions by models as columns
# @param do_(Testname) if true, function will do the fit statistic
#
# @return a matrix with the test values for each model, with 
#         the models as rows
#
# @author David P. Flasterstein
check_fit <- function(y, P, do_RMSE=T, do_MAD=T, do_RMSLE=T,
                      do_MAPE=T, do_MEAPE=T){
  return_matrix <- matrix(nrow=ncol(P))
  if(!is.matrix(P)){
    return("P needs to be a matrix of values")
  }
  if(!is.vector(y)){
    return("y needs to be a vector of values")
  }
  if(do_RMSE){
   RMSE <- apply(P, 2, function(p, o) RMSE_f(pred=p,obs=o), o=y)
   return_matrix <- cbind(return_matrix, RMSE)
  }
  if(do_MAD){
    MAD <- apply(P, 2, function(p, o) MAD_f(pred=p,obs=o), o=y)
    return_matrix <- cbind(return_matrix, MAD)
  }
  if(do_RMSLE){
    RMSLE <- apply(P, 2, function(p, o) RMSLE_f(pred=p,obs=o), o=y)
    return_matrix <- cbind(return_matrix, RMSLE)
  }
  if(do_MAPE){
    MAPE <- apply(P, 2, function(p, o) MAPE_f(pred=p,obs=o), o=y)
    return_matrix <- cbind(return_matrix, MAPE)
  }
  if(do_MEAPE){
    MEAPE <- apply(P, 2, function(p, o) MEAPE_f(pred=p,obs=o), o=y)
    return_matrix <- cbind(return_matrix, MEAPE)
  }
  return(return_matrix)
}




## Question 5
# check how my models did with each statistic
check_fit(y=test_obama, P=prediction_matrix) 
  
  
  
  


