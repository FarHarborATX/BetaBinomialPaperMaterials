#### This program produces the figures in the Beta-Binomial Reliability manuscript ####

# load libraries
library(openxlsx)                                                       
library(plyr)                 
library(dplyr)
library(lme4)
library(haven)
library(gsubfn)
library(VGAM)

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


# read in example data
contraceptive <- read_excel("G:/Shared drives/_projects/x_fh_special/Methods paper related/Github files/Figure test data.xlsx")


#### Figure 1 #######

y <- contraceptive$yes_mostmod
n <- contraceptive$total
id <- contraceptive$ClinicID
rel_name <- "MostMod"
id_name <- "ID"
sm <- 5

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
M <- 1/gamma - 1

#Derive Beta parameters
alpha=mu/theta;
beta=(1-mu)/theta;


x <- seq(0,1,by=.01)
beta_density <- dbeta(x,alpha,beta)


plot(x,beta_density,lwd=2,type="l",cex=.1,lty=2,
     xlab="Performance measure rate",ylab="Density",
     main="Most-Mod Service Quality")
hist(y/n,probability =TRUE,
     density=20,angle=90+45,add=TRUE)


#### Figure 2 #######

y <- contraceptive$yes_larc
n <- contraceptive$total
id <- contraceptive$ClinicID
rel_name <- "LARC"
id_name <- "ID"
sm <- 5

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
M <- 1/gamma - 1

#Derive Beta parameters
alpha=mu/theta;
beta=(1-mu)/theta;


x <- seq(0,1,by=.001)
beta_density <- dbeta(x,alpha,beta)
plot(x,beta_density,lwd=2,type="l",cex=.1,lty=2, xlim=c(0,.3),
     xlab="Performance measure rate",ylab="Density",
     main="Long-acting reversible contraception (LARC) measure")
hist(y/n,probability =TRUE,
     density=20,angle=90+45,add=TRUE)



#### Figure 3 #######

y <- contraceptive$yes_mostmod
n <- contraceptive$total
id <- contraceptive$ClinicID
rel_name <- "MostMod"
id_name <- "ID"
sm <- 5

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
M <- 1/gamma - 1

#Derive Beta parameters
alpha=mu/theta;
beta=(1-mu)/theta;


p <- y/n
id <- contraceptive$ClinicID

plot.dat <- as.data.frame(cbind(id[order(p)],y[order(p)],n[order(p)],p[order(p)]))
names(plot.dat) <- c("id","y","n","p")


examples <- c("A","B","C","D")
cnty <- c(1,37,42,49)


x <- seq(0,1,by=.001)
beta_density <- dbeta(x,alpha,beta)
plot(x,beta_density,lwd=4,type="o",cex=.1,lty=2,ylim=c(-4,18),xlim=c(.1,.65),
     xlab="Performance measure rate",ylab="Density",
     main="Most or moderately effective contraceptive method (Most-Mod) measure")


for(i in c(1,4)){
  
  x <- seq(0,1,by=.001)
  a <- alpha + plot.dat$y[cnty[i]]
  b <- beta + (plot.dat$n[cnty[i]] - plot.dat$y[cnty[i]])
    
  beta_density <- dbeta(x,a,b)
  
  points(x,beta_density,type="o",lwd=1,cex=.1)
  abline(v= a/(a+b),lty=3)
  
  
  abline(v=plot.dat$p[cnty[i]],lwd=3,lty=2)
  
  arrows(plot.dat$p[cnty[i]],-2,a/(a+b),-2,length=.1,lwd=3,code=2)
  
}

text(.3,5,"Prior distribution",srt=(50))

text(.264,17,"County #1")
text(.254,6,"PPD")

text(.485,13.75,"County #2")
text(.480,6,"PPD")

text(.24,-3,"shrinkage",cex=.65)
text(.54,-3,"shrinkage",cex=.65)

plot.dat$n[1]
plot.dat$n[49]



#### Figure 4 #######

y <- contraceptive$yes_mostmod
n <- contraceptive$total
id <- contraceptive$ClinicID
rel_name <- "MostMod"
id_name <- "ID"
sm <- 5

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
M <- 1/gamma - 1

#Derive Beta parameters
alpha=mu/theta;
beta=(1-mu)/theta;

var_beta <- alpha*beta/((alpha+beta)^2*(alpha+beta+1))
var_error <- ((y/n)*(1- (y/n)))/n
lgt_p <- log((y/n)/(1-(y/n)))
p <- (y/n)

q_25<- quantile(p)[2]
q_75<- quantile(p)[4]


p.char <- ifelse((y/n) > q_25 & (y/n) < q_75,1,8)

rel_bb <- n/(M+n)
rel_adams <- var_beta/(var_beta +var_error)


cbind(rel_bb,rel_adams)

plot(rel_adams,rel_bb,pch=p.char,cex=log(n)-3,
     xlab="Adams (2009) formulation reliability",ylab="Alternative beta-binomial reliability",
     main="Most or moderately effective contraceptive method (Most-Mod) measure")
abline(a=0,b=1)
legend("bottomright", c("within interquartile range","outside interquartile range"),pch=c(1,8))



#### Figure 5 #######

y <- contraceptive$yes_larc
n <- contraceptive$total
id <- contraceptive$ClinicID
rel_name <- "LARC"
id_name <- "ID"
sm <- 5

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
M <- 1/gamma - 1

#Derive Beta parameters
alpha=mu/theta;
beta=(1-mu)/theta;

var_beta <- alpha*beta/((alpha+beta)^2*(alpha+beta+1))
var_error <- ((y/n)*(1- (y/n)))/n
lgt_p <- log((y/n)/(1-(y/n)))
p <- (y/n)

q_25<- quantile(p)[2]
q_75<- quantile(p)[4]


p.char <- ifelse((y/n) > q_25 & (y/n) < q_75,1,8)

rel_bb <- n/(M+n)
rel_adams <- var_beta/(var_beta +var_error)


cbind(rel_bb,rel_adams)
plot(rel_adams,rel_bb,pch=p.char,cex=log(n)-3,
     xlab="Adams (2009) formulation reliability",ylab="Alternative beta-binomial reliability",
     main="Long-acting reversible contraception (LARC) measure")
abline(a=0,b=1)
legend("topleft", c("within interquartile range","outside interquartile range"),pch=c(1,8))



#### Supplemental Figure 1
#selecting the measures

y <- contraceptive$yes_mostmod
n <- contraceptive$total
id <- contraceptive$ClinicID
rel_name <- "MostMod"
id_name <- "ID"
sm <- 5

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
M <- 1/gamma - 1

#Derive Beta parameters
alpha=mu/theta;
beta=(1-mu)/theta;


### PPD parameters

p <- round(y/n,3)
id <- contraceptive$ClinicID
a <- alpha + y
b <- beta + (n - y)
rel <- round(n/(alpha + beta + n),3)
p_below <- round(pbeta(.35,a,b,lower.tail=TRUE),3)


plot.dat <- as.data.frame(cbind(id,y,n,p,p_below,a,b,rel))
names(plot.dat) <- c("id","y","n","p","p_below","a","b","rel")


cnty <- c(33,42)  #selecting two example counties.  


### ploting the prior density

x <- seq(0,1,by=.001)
beta_density <- dbeta(x,alpha,beta)

plot(x,beta_density,lwd=4,type="l",cex=.1,lty=2,ylim=c(-4,30),xlim=c(.2,.60),
     xlab="Quality Measure rate",ylab="Density",
     #main="Most-Mod Service Quality",
     sub=expression(paste(alpha,"= 31.53; ",beta,"=47.53")))


# selecting the first county,  this code allows you to plot multiple counties simoultaneously.

for(i in 1){
  
  x <- seq(0,1,by=.001)
  a <- alpha + plot.dat$y[cnty[i]]
  b <- beta + (plot.dat$n[cnty[i]] - plot.dat$y[cnty[i]])
  beta_density <- dbeta(x,a,b)
  
  points(x,beta_density,lwd=2,cex=.1,type="l")   ### Thes plots the PPD
  
  x <- seq(0,.35,by=.001)
  beta_density <- dbeta(x,a,b)
  polygon(c(.3,x,.35),c(0,beta_density,0),density=20,angle=90+45)  ## This shades the PPD
  
  abline(v= .35,lty=3)  #Threshold
  abline(v=a/(a+b)) #EB estimate
  
  abline(v=plot.dat$p[cnty[i]],lwd=3,lty=4) #observed score
  
  arrows(plot.dat$p[cnty[i]],-2,a/(a+b),-2,length=.1,lwd=3,code=2)  #shrinkage arrow
  
}


text(.275,5.5,"PPD",srt=60)
text(.47,5,"Prior",srt=320)
text(.31,-3.5,"shrinkage",cex=.65)

legend("topright",c("Observed rate (0.310)","Empirical Bayes estimate","Threshold (0.35)"),lwd=c(3,1,1),lty=c(4,1,3))  


p_below <- pbeta(.35,a,b,lower.tail=TRUE)  #probability that the county is below the threshold.

n[cnty[i]] #county size
y[cnty[i]] #incident count
p[cnty[i]] #observed score
n[cnty[i]]/(alpha+beta+n[cnty[i]]) #reliability


#### Supplemental Figure 2


x <- seq(0,1,by=.001)
beta_density <- dbeta(x,alpha,beta)

plot(x,beta_density,lwd=4,type="l",cex=.1,lty=2,ylim=c(-4,30),xlim=c(.2,.60),
     xlab="Quality Measure rate",ylab="Density")


for(i in 2){
  
  x <- seq(0,1,by=.001)
  a <- alpha + plot.dat$y[cnty[i]]
  b <- beta + (plot.dat$n[cnty[i]] - plot.dat$y[cnty[i]])
  beta_density <- dbeta(x,a,b)
  
  points(x,beta_density,lwd=2,cex=.1,type="l")
  
  x <- seq(0,.35,by=.001)
  beta_density <- dbeta(x,a,b)
  polygon(c(.3,x,.35),c(0,beta_density,0),density=20,angle=90+45)
  
  abline(v= .35,lty=3)
  abline(v=a/(a+b))
  
  abline(v=plot.dat$p[cnty[i]],lwd=3,lty=4)
  
  arrows(plot.dat$p[cnty[i]],-2,a/(a+b),-2,length=.1,lwd=3,code=2)
  
}

text(.25,7.5,"PPD",srt=75)
text(.47,5,"Prior",srt=320)
text(.3,-3.5,"shrinkage",cex=.65)

legend("topright",c("Observed rate (0.295)","Empirical Bayes estimate","Threshold (0.35)"),lwd=c(3,1,1),lty=c(4,1,3))  


p_below <- pbeta(.35,a,b,lower.tail=TRUE)  #probability that the county is below the threshold.

n[cnty[i]]
y[cnty[i]]
p[cnty[i]]
n[cnty[i]]/(alpha+beta+n[cnty[i]])





