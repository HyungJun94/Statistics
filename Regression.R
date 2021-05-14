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












