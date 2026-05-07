#Instructions:

# The beta.rel.threshold function estimates the probability that a provider’s performance 
# rate is below a chosen threshold. It takes 6 arguments:
# 1) The variable and data.frame containing the cluster ID (e.g., df$ClinicID)
# 2) The variable and data.frame containing the count of service incidents (e.g., df$yes)
# 3) The variable and data.frame containing the count of patients (e.g., df$total)
# 4) The minimum cluster size (i.e. number of patients) used in the histogram that eliminates small clusters.
# 5) The quality threshold
# 6) The level of confidence needed for classification

# The output of the beta.rel.threshold function is a data.frame with three variables, the 
# cluster id, the reliability statistic, and the threshold classification probability. This 
# data.frame only contains records of clusters that have more then zero patients. Thus, it 
# may not contain every observation in the original data set. For this reason, the output 
# from the function should be merged with the original data set each time the function is 
# run. 

# install packages, only run the first time
# install.packages("openxlsx")                                           
# install.packages("plyr")                                               
# install.packages('dplyr')                                          
# install.packages("lme4")                                                
# install.packages("haven")                                              
# install.packages("gsubfn")                                              
# install.packages("VGAM")                                                

# load libraries

library(openxlsx)                                                       
library(plyr)                 
library(dplyr)
library(lme4)
library(haven)
library(gsubfn)
library(VGAM)

# read in dataset
df <- read.xlsx('G://Shared drives/_projects/x_fh_special/Methods paper related/Github files/Example data.xlsx')

beta.rel.threshold <- function(id,y,n,sm,threshold,conf){

  # remove clusters with zero patients
  no_zero <- which(n>0)
  
  y <- y[no_zero]
  n <- n[no_zero]
  id <-id[no_zero]
  
  fit <- vglm(cbind(y,n-y) ~ 1, betabinomial)  #estimate beta-binomial model
  parms <- coef(fit, matrix = TRUE) #extract logit(mu) and logit(gamma) 
  
  #inverse logit link
  mu <- exp(coef(fit, matrix = TRUE)[1])/(1+exp(coef(fit, matrix = TRUE)[1])) 
  gamma <-exp(coef(fit, matrix = TRUE)[2])/(1+exp(coef(fit, matrix = TRUE)[2])) 
  theta <- gamma/(1-gamma)

  
  #Derive Beta parameters
  alpha=mu/theta;
  beta=(1-mu)/theta;
  
  #reliability
  rel <- n/(alpha+beta+n)

  a <- alpha + y
  b <- beta + (n - y)
  
  #Threshold probability
  p_below <- pbeta(threshold,a,b,lower.tail=TRUE) 
  
  
  result <- as.data.frame( cbind(id,round(rel,3),round(p_below,3))) 
  names(result) <- c("id","rel","p_below")
  
  result
}

# calculate probability of below threshold for each clinic using the beta.rel.threshold function
probability <- beta.rel.threshold(df$ClinicID,df$yes,df$total,5,.35,.9)


