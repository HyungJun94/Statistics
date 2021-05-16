### codes for Regression 

## 1. Linear Regression 

# toy data
x1 <- rnorm(20)
x2 <- rnorm(20, mean=2, sd=2)
y <- 2+3*x1+4.5*x2 + rnorm(20)

# R built-in function---------------------------------
reg <- lm(y~x1+x2)
summary(reg)

# from the scratch regression--------------------------
X <- cbind(1,x1,x2)

fitted_coef <- solve(t(X)%*%X)%*%t(X)%*%y 
fitted_coef  #coefficients

fitted_val <- X %*% fitted_coef
fitted_val   #fitted value

SSE <- sum((y-fitted_val)^2)
MSE <- SSE/17
MSE  # Mean Squared Error (degree of freedom = 20-3)
sqrt(MSE)  # Residual Standard Error

SSR <- sum((fitted_val-mean(y))^2)
MSR <- SSR/2 
MSR # Mean Sqaured Regression (degree of freedom = 3-1)

SST <- sum((y-mean(y))^2)
MST <- SST/19
MST # Mean Squared Total (degree of freedom = 20-1)

R_squared <- SSR/SST
R_squared  # R squared index

adj_R_squared <- 1-(1-R_squared)*(20-1)/(20-2-1)
adj_R_squared  # adjusted R squared

F_statistic <- MSR/MSE 
F_statistic  # F statistic (degree of freedom: 2, 17)

p_val <- 1-pf(F_statistic, 2, 17)
p_val  # p value 



## 2. Logistic Regression 

# example
imprison <- rep(c('yes','no'),4)  # whether imprisoned 
resid <- rep(c('no', 'yes'),c(4,4))  # whether from the resident  
arrest <- rep(rep(c('yes','no'),c(2,2)),2) # whether has been arrested in the past 
count <- c(42,109,17,75,33,175,53,359)
data1 <- data.frame(count, resid, arrest, imprison)

# [GLM] logistic regression  R built-in function------------------------
model <- glm(imprison ~ resid+arrest, weights=count, 
             family=binomial(link='logit'), data=data1)
model
summary(model)



## 3. Logistic Regression with Ordinal data 

# example data 
mental <- rep(4:1, c(12,12,7,9))
status <- c(1,1,1,1,0,1,0,1,1,1,0,0,1,0,1,0,1,1,0,1,
            1,0,1,1,0,1,0,0,1,0,0,1,1,1,0,0,0,1,0,0)
event <- c(1,9,4,3,2,0,1,3,3,7,1,2,5,6,3,1,8,2,5,5,
           9,3,3,1,0,4,3,9,6,4,3,8,2,7,5,4,4,8,8,9)
data2 <- data.frame(mental, status, event)

library('MASS')
model2<- polr(factor(mental)~factor(status)+event, data=data2 ,Hess=F)
summary(model2)

# Odds Ratio for each parameter
exp(coef(model2))
exp(cbind(OR = coef(model2), confint(model2)))



## 4. Log-linear model 

# In this model, every variable is responsive variable 
# We only check the interactions, as well as the dependencies, among the variables
# Usually used for modeling more than two categorical variables. 

count = c(659,270,532,347,432,532,269,552)
dead = rep(c(1,0),4)
outwind =rep(rep(c(1,0),c(2,2)),2)
seatbelt = rep(c(1,0),c(4,4))

data3 <- data.frame(count, dead, outwind, seatbelt)

#saturated model 
model3 <- glm(count~dead*outwind*seatbelt,family=poisson,data=data3)
summary(model3)
#3interaction remove
model31 <- glm(count~dead*outwind+dead*seatbelt+outwind*seatbelt,family=poisson,data=data3)
summary(model31)
#dead*outwind remove 
model32 <- glm(count~dead*seatbelt+outwind*seatbelt,family=poisson,data=data3)
summary(model32)
#dead*seatbelt remove 
model33 <- glm(count~dead*outwind+outwind*seatbelt,family=poisson,data=data3)
summary(model33)
#outwind*seatbelt remove
model34 <- glm(count~dead*outwind+dead*seatbelt,family=poisson,data=data3)
summary(model34)
#dead*outwind only
model35 <- glm(count~seatbelt+dead*outwind,family=poisson,data=data3)
summary(model35)
#dead*seatbelt only
model36 <- glm(count~outwind+dead*seatbelt,family=poisson,data=data3)
summary(model36)
#outwind*seatbelt only
model37 <- glm(count~dead+outwind*seatbelt,family=poisson,data=data3)
summary(model37)
#no interaction
model39 <- glm(count~dead+outwind+seatbelt,family=poisson,data=data3)
summary(model39)

# It is also possible to fit ordinary logistic regression on this data.
logitM <- glm(dead~outwind+seatbelt,family=binomial(link=logit), weights=count, data=data3)
summary(logitM)











