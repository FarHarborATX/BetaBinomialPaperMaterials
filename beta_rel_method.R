#Instructions:

# The beta_rel functions takes 5 arguments
# 1) The variable and data.frame containing the cluster ID (e.g., df$ClinicID)
# 2) The variable and data.frame containing the count of service incidents (e.g., df$yes)
# 3) The variable and data.frame containing the total count of patients (e.g., df$total)
# 4) The user-specified cluster ID variable name for the output dataset
# 5) The user-specified reliability variable name for the output dataset
# The output of the beta_rel function is a data.frame with two variables, the cluster ID and the reliability 
# statistic. This data.frame only contains records of clusters that have more then zero patients.  
# Thus, it may not contain every observation in the original data set.  
# For this reason, the output from the function should be merged with the original data set each
# time the function is run. 

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

# round rates to 3 digits
df$Rate <- round(df$Rate, digits = 3) 

# function to calculate reliability
beta_rel <- function(id,y,n,id_name,rel_name){
  
  # remove clusters with zero patients
  no_zero <- which(n>0)
  
  y <- y[no_zero]
  n <- n[no_zero]
  id <-id[no_zero]
  
  fit <- vglm(cbind(y,n-y) ~ 1, betabinomial)  #estimate beta-binomial model
  parms <- coef(fit, matrix = TRUE) #extract logit(mu) and logit(gamma) 
  
  # inverse logit link
  mu <- exp(coef(fit, matrix = TRUE)[1])/(1+exp(coef(fit, matrix = TRUE)[1])) 
  gamma <-exp(coef(fit, matrix = TRUE)[2])/(1+exp(coef(fit, matrix = TRUE)[2])) 
  theta <- gamma/(1-gamma)
  
  # derive Beta parameters
  alpha=mu/theta;
  beta=(1-mu)/theta;
  
  # calculate reliability
  rel <- n/(alpha+beta+n)
  
  result <- as.data.frame(cbind(id,round(rel,3)))
  names(result) <- c(id_name,rel_name)
  
  result
}

# calculate reliability using beta_rel function developed above
calc_rel <- beta_rel(df$ClinicID, 
                    df$yes,
                    df$total,
                    "ClinicID", "Reliability")

# output dataset with calculated reliability
outdata <- merge(df[,c("ClinicID","yes","total","Rate")],
                calc_rel,
                by.x="ClinicID",all=TRUE)
