## Building A Burned Forest Area Predictor with Forest Fires data

## Table of Contents
# Abstract 
# EDA
# Fitting various models
# Reorganizing the data with Data reduction technic PCA
# Comparison
# Discussion & Conclusion 

# data structure: 517X13
#1. X - x-axis spatial coordinate within the Montesinho park map (lattitude) : 1 to 9
#2. Y - y-axis spatial coordinate within the Montesinho park map (longitude) : 2 to 9
#3. month(factor) - month of the year: "jan" to "dec" 
#4. day(factor) - day of the week: "mon" to "sun"
#5. FFMC - fine fuel moisture code(moisture of forest litter fuels under the shade of a forest canopy): 18.7 to 96.20
#6. DMC - duff moisture code(fuel moisture of decomposed organic material underneath the litter): 1.1 to 291.3 
#7. DC - drought code(drying deep into the soil): 7.9 to 860.6 
#8. ISI - initial spread index(fire spread potential): 0.0 to 56.10
#9. temp - temperature in Celsius degrees: 2.2 to 33.30
#10. RH - relative humidity in %: 15.0 to 100
#11. wind - wind speed in km/h: 0.40 to 9.40 
#12. rain - outside rain in mm/m2 : 0.0 to 6.4 
#13. area[TARGET variable] - the burned area of the forest (in ha): 0.00 to 1090.84 >> log(area+1) transformed

# fine fuel: ignition components in forest that easily started to burn (leaves, twig etc.)

## data generation ------------------------------------------------------------------------------------------
forest_fires <- read.csv('C:/Users/hlim1/OneDrive/문서/KOREA UNIV/2020-2/통계분석방법론/forestfires.csv', header=T)

## EDA ------------------------------------------------------------------------------------------
str(forest_fires)
plot(forest_fires)

# visualize coordinates >> no use
plot(x=forest_fires$X, y=forest_fires$Y)

# tabulate seasonal frequency
mon_freq <- table(forest_fires$month)
barplot(mon_freq, main='monthly forest fires')
day_freq <- table(forest_fires$day)
barplot(day_freq, main='daily forest fires')

# FWI(Fire Weather Index)
par(mfrow=c(2,2))
hist(forest_fires$FFMC, nclass=30, main='FMC')
hist(forest_fires$DMC, nclass=30, main='DMC')
hist(forest_fires$DC, nclass=30, main='DC')
hist(forest_fires$ISI, nclass=30, main='ISI')


# weather conditions
par(mfrow=c(1,2))
hist(forest_fires$temp, main='Temperature')
hist(forest_fires$RH, main='Humidity')
tab_rain <- table(forest_fires$rain!=0)
row.names(tab_rain)=c('No Rain', 'Rain')
tab_rain
forest_fires$rain[forest_fires$rain!=0]

# skewd variable: area
hist(forest_fires$area, nclass=100, ylim=c(0,500), main='area')
points(quantile(forest_fires$area, probs=c(0.5,0.85,0.99)),rep(0,3), col=c(4,4,4))

log_area <-log(forest_fires$area +1) 
hist(log_area, nclass=20, main='log transformed area')
points(quantile(log_area, probs=c(0.25,0.5,0.75)),rep(0,3), col=c(4,4,4))

# missing -> no missing
for(i in 1:13){
  print(sum(is.na(forest_fires[,i])))
}

# variable transformation
forest_fires$log_area <- log(forest_fires$area +1)

# create season variable >> replace month
N <- nrow(forest_fires)
season <- rep(0,N)
for(i in 1:N){
  if(forest_fires$month[i] %in% c('dec','jan','feb')){
    season[i] <- 'winter'
  }else if(forest_fires$month[i] %in% c('mar','apr','may')){
    season[i] <- 'spring'
  }else if(forest_fires$month[i] %in% c('jun','jul','aug')){
    season[i] <- 'summer'
  }else{
    season[i] <- 'autumn'
  }
}
forest_fires$season <- factor(season)
plot(forest_fires$season, forest_fires$log_area,main='season') 


# create week variable >> replace day
week <- rep(0,N)
for(i in 1:N){
  if(forest_fires$day[i] %in% c('fri','sat','sun')){
    week[i] <- 'weekend'
  }else{
    week[i] <- 'weekday'
  }
}
forest_fires$week <- factor(week)
plot(forest_fires$week, forest_fires$log_area,main='Week') 



# Relationship between variables 
attach(forest_fires)

season <- factor(season); week <- factor(week)

par(mfrow=c(3,4))
e1 <- rnorm(nrow(forest_fires), sd=0.5)
plot(X+e1, log_area,main='X') # with jittering
plot(Y+e1, log_area,main='Y') # with jittering
plot(season, log_area,main='month') # instead of month 
plot(week, log_area,main='week') # instead of day
plot(FFMC, log_area,main='FFMC')
plot(DMC, log_area,main='DMC')
plot(DC, log_area,main='DC')
plot(ISI, log_area,main='ISI')
plot(temp, log_area,main='temp')
plot(RH, log_area,main='RH')
plot(wind, log_area,main='wind')
plot(rain, log_area,main='rain')



# check normal assumption for continuous variables
par(mfrow=c(2,5))
qqnorm(X, main='X')
qqnorm(Y, main='Y')
qqnorm(FFMC, main='FFMC')
qqnorm(DMC, main='DMC')
qqnorm(DC, main='DC')
qqnorm(ISI, main='ISI')
qqnorm(temp, main='temp')
qqnorm(RH, main='RH')
qqnorm(wind, main='wind')
qqnorm(rain, main='rain')

# kolmogorov test for normality
e <-rnorm(517,0.1^100) # jittering
ks.test(scale(X+e), "pnorm", mean=0, sd=1, alternative="two.sided") # approximately normal
ks.test(scale(Y+e), "pnorm", mean=0, sd=1, alternative="two.sided")
ks.test(scale(FFMC+e), "pnorm", mean=0, sd=1, alternative="two.sided")
ks.test(scale(DMC+e), "pnorm", mean=0, sd=1, alternative="two.sided")
ks.test(scale(ISI+e), "pnorm", mean=0, sd=1, alternative="two.sided")
ks.test(scale(temp+e), "pnorm", mean=0, sd=1, alternative="two.sided") # approximately normal
ks.test(scale(RH+e), "pnorm", mean=0, sd=1, alternative="two.sided")
ks.test(scale(wind+e), "pnorm", mean=0, sd=1, alternative="two.sided") # approximately normal 
ks.test(scale(rain+e), "pnorm", mean=0, sd=1, alternative="two.sided")

# between continous & target [area]
plot(forest_fires[,-c(3,4,13)])    

cor.test(X, log_area, method='pearson')
cor.test(Y, log_area, method='kendall')
cor.test(FFMC, log_area, method='kendall')
cor.test(DMC, log_area, method='kendall') #
cor.test(DC, log_area, method='kendall')
cor.test(ISI, log_area, method='kendall')
cor.test(temp, log_area, method='pearson') 
cor.test(RH, log_area, method='kendall')
cor.test(wind, log_area, method='pearson')
cor.test(rain, log_area, method='kendall')


# between categorical & target 
par(mfrow=c(1,2))
plot(season, log_area, main='Monthly burned area')    
plot(week, log_area, main='daily burned area')

kruskal.test(log_area ~ season) #significant            #not normal just by looking at it
kruskal.test(log_area ~ week) #not significant


# Only wind variable is approximately normal 
# Thus we have DMC and saeson relatively significantly related with target variable


## Splitting data into train and test set ----------------------------------------------------------
N <- nrow(forest_fires)
test_index <- sample(1:N,N/5)
test_data <- forest_fires[test_index,]
train_data <- forest_fires[-test_index,]

## Cross Validation Set

test_set <- list()
train_set <- list()
test_index <- sample(1:5,N,replace=T) #5-fold CV
table(test_index)

for(i in 1:5){
  test_set[[i]] <- forest_fires[test_index==i,]
  train_set[[i]] <- forest_fires[test_index!=i,]
}

## -------------------------------------------------------------------------------------------------
## Model fitting -----------------------------------------------------------------------------------


#linear regression 

lin_reg <- lm(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=forest_fires)
summary(lin_reg)
anova(lin_reg)

# cross validation & compare mean accuracy 
lin_reg_train_pred <- list(); lin_reg_train_mse  <- c()
lin_reg_test_pred <- list(); lin_reg_test_mse <- c()
for(i in 1:5){
  lin_reg <- lm(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=train_set[[i]])
  
  lin_reg_train_pred[[i]] <- predict(lin_reg, train_set[[i]])
  lin_reg_train_mse[i] <- sum((train_set[[i]]$log_area - lin_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  lin_reg_test_pred[[i]] <- predict(lin_reg, test_set[[i]])
  lin_reg_test_mse[i] <- sum((test_set[[i]]$log_area - lin_reg_test_pred[[i]])^2)/nrow(test_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',lin_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',lin_reg_test_mse,']')

cv_result_lin_reg <- list(Train_prediction_MSE=mean(lin_reg_train_mse),Test_prediction_MSE=mean(lin_reg_test_mse))
cv_result_lin_reg

# robust M regression

library(MASS)
m_reg <- rlm(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=forest_fires)
summary(m_reg)
anova(m_reg)

# cross validation & compare mean accuracy 
m_reg_train_pred <- list(); m_reg_train_mse  <- c()
m_reg_test_pred <- list(); m_reg_test_mse <- c()
for(i in 1:5){
  m_reg <- rlm(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=train_set[[i]])
  
  m_reg_train_pred[[i]] <- predict(m_reg, train_set[[i]])
  m_reg_train_mse[i] <- sum((train_set[[i]]$log_area - m_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  m_reg_test_pred[[i]] <- predict(m_reg, test_set[[i]])
  m_reg_test_mse[i] <- sum((test_set[[i]]$log_area - m_reg_test_pred[[i]])^2)/nrow(test_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',m_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',m_reg_test_mse,']')

cv_result_m_reg <- list(Train_prediction_MSE=mean(m_reg_train_mse),Test_prediction_MSE=mean(m_reg_test_mse))
cv_result_m_reg



# robust LMS regression 

library(MASS)
lms_reg <- lqs(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=forest_fires,method='lms')
summary(lms_reg)

# cross validation & compare mean accuracy 
lms_reg_train_pred <- list(); lms_reg_train_mse  <- c()
lms_reg_test_pred <- list(); lms_reg_test_mse <- c()
for(i in 1:5){
  lms_reg <- lqs(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=train_set[[i]],method='lms')
  
  lms_reg_train_pred[[i]] <- predict(lms_reg, train_set[[i]])
  lms_reg_train_mse[i] <- sum((train_set[[i]]$log_area - lms_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  lms_reg_test_pred[[i]] <- predict(lms_reg, test_set[[i]])
  lms_reg_test_mse[i] <- sum((test_set[[i]]$log_area - lms_reg_test_pred[[i]])^2)/nrow(test_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',lms_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',lms_reg_test_mse,']')

cv_result_lms_reg <- list(Train_prediction_MSE=mean(lms_reg_train_mse),Test_prediction_MSE=mean(lms_reg_test_mse))
cv_result_lms_reg


# robust LTS regression

lts_reg <- lqs(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=forest_fires,method='lts')
summary(lts_reg)

# cross validation & compare mean accuracy 
lts_reg_train_pred <- list(); lts_reg_train_mse  <- c()
lts_reg_test_pred <- list(); lts_reg_test_mse <- c()
for(i in 1:5){
  lts_reg <- lqs(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=train_set[[i]],method='lts')
  
  lts_reg_train_pred[[i]] <- predict(lts_reg, train_set[[i]])
  lts_reg_train_mse[i] <- sum((train_set[[i]]$log_area - lts_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  lts_reg_test_pred[[i]] <- predict(lts_reg, test_set[[i]])
  lts_reg_test_mse[i] <- sum((test_set[[i]]$log_area - lts_reg_test_pred[[i]])^2)/nrow(test_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',lts_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',lts_reg_test_mse,']')

cv_result_lts_reg <- list(Train_prediction_MSE=mean(lts_reg_train_mse),Test_prediction_MSE=mean(lts_reg_test_mse))
cv_result_lts_reg


# quantile regression: median 

library(quantreg)
quant_reg <- rq(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain,tau=0.50, data=forest_fires)
summary(quant_reg)

# cross validation & compare mean accuracy 
quant_reg_train_pred <- list(); quant_reg_train_mse  <- c()
quant_reg_test_pred <- list(); quant_reg_test_mse <- c()
for(i in 1:5){
  quant_reg <- rq(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain,tau=0.50, data=train_set[[i]])
  
  quant_reg_train_pred[[i]] <- predict(quant_reg, train_set[[i]])
  quant_reg_train_mse[i] <- sum((train_set[[i]]$log_area - quant_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  quant_reg_test_pred[[i]] <- predict(quant_reg, test_set[[i]])
  quant_reg_test_mse[i] <- sum((test_set[[i]]$log_area - quant_reg_test_pred[[i]])^2)/nrow(test_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',quant_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',quant_reg_test_mse,']')

cv_result_quant_reg <- list(Train_prediction_MSE=mean(quant_reg_train_mse),Test_prediction_MSE=mean(quant_reg_test_mse))
cv_result_quant_reg


# LOcal polynomial regrESSion (defualt)---------------------------------------------------------------

loess_reg <- loess(log_area ~ X+DMC+wind,data=forest)# default[degree=2,span=0.75]
summary(loess_reg)

# cross validation & compare mean accuracy 
loess_reg_train_pred <- list(); loess_reg_train_mse  <- c()
loess_reg_test_pred <- list(); loess_reg_test_mse <- c()
for(i in 1:5){
  loess_reg <- loess(log_area ~ X+DMC+wind,data=train_set[[i]])# default[degree=2,span=0.75]
  
  loess_reg_train_pred[[i]] <- predict(loess_reg, train_set[[i]])
  loess_reg_train_mse[i] <- sum((train_set[[i]]$log_area - loess_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  loess_reg_test_pred[[i]] <- predict(loess_reg, test_set[[i]])
  if(sum(is.na(loess_reg_test_pred[[i]]))>0){
    na_index <- (1:nrow(test_set[[i]]))[is.na(loess_reg_test_pred[[i]])]
  }else{
    na_index <-10000
  }
  loess_reg_test_mse[i] <- sum((test_set[[i]]$log_area[-na_index] - loess_reg_test_pred[[i]][-na_index])^2)/nrow(test_set[[i]][-na_index])
}

cat(' 5-fold train prediction MSEs are [',loess_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',loess_reg_test_mse,']')

cv_result_loess_reg <- list(Train_prediction_MSE=mean(loess_reg_train_mse),Test_prediction_MSE=mean(loess_reg_test_mse))
cv_result_loess_reg

# LOcal polynomial regrESSion (adjusted)
 
loess2_reg <- loess(log_area ~ X+DMC+wind,data=forest_fires,degree=1, span=1) #linear fit with wider span
summary(loess2_reg)

# cross validation & compare mean accuracy 
loess2_reg_train_pred <- list(); loess2_reg_train_mse  <- c()
loess2_reg_test_pred <- list(); loess2_reg_test_mse <- c()
for(i in 1:5){
  loess2_reg <- loess(log_area ~ X+DMC+wind,data=train_set[[i]],degree=1, span=1)
  
  loess2_reg_train_pred[[i]] <- predict(loess2_reg, train_set[[i]])
  loess2_reg_train_mse[i] <- sum((train_set[[i]]$log_area - loess2_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  loess2_reg_test_pred[[i]] <- predict(loess2_reg, test_set[[i]])
  if(sum(is.na(loess2_reg_test_pred[[i]]))>0){
    na_index <- (1:nrow(test_set[[i]]))[is.na(loess2_reg_test_pred[[i]])]
  }else{
    na_index <-10000
  }
  loess2_reg_test_mse[i] <- sum((test_set[[i]]$log_area[-na_index] - loess2_reg_test_pred[[i]][-na_index])^2)/nrow(test_set[[i]][-na_index])
}

cat(' 5-fold train prediction MSEs are [',loess2_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',loess2_reg_test_mse,']')

cv_result_loess2_reg <- list(Train_prediction_MSE=mean(loess2_reg_train_mse),Test_prediction_MSE=mean(loess2_reg_test_mse))
cv_result_loess2_reg


# Genralized Additive Model ------------------

#install.packages('mgcv')
library(mgcv)

# plot area
par(mfrow=c(1,1))
hist(area+1, nclass=200, xlim=c(0,200)) # non-negative with gamma shape 

# semi-parametric gamma regression model  
gam_reg <- gam((area+1) ~ s(X,k=9)+s(Y,k=6)+season+week+s(FFMC)+s(DMC)+s(DC)+s(ISI)+s(temp)+s(RH)+s(wind)+s(rain,k=4),
               family=Gamma(link='log'),data=forest_fires)
summary(gam_reg)

# cross validation & compare mean accuracy 
gam_reg_train_pred <- list(); gam_reg_train_mse  <- c()
gam_reg_test_pred <- list(); gam_reg_test_mse <- c()
for(i in 1:5){
  gam_reg <- gam((area+1) ~ s(X,k=9)+s(Y,k=6)+season+week+s(FFMC)+s(DMC)+s(DC)+s(ISI)+s(temp)+s(RH)+s(wind)+s(rain,k=4),
                 family=Gamma(link='log'),data=train_set[[i]])  
  gam_reg_train_pred[[i]] <- predict(gam_reg, train_set[[i]])
  gam_reg_train_mse[i] <- sum((train_set[[i]]$log_area - gam_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  gam_reg_test_pred[[i]] <- predict(gam_reg, test_set[[i]])
  gam_reg_test_mse[i] <- sum((test_set[[i]]$log_area - gam_reg_test_pred[[i]])^2)/nrow(test_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',gam_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',gam_reg_test_mse,']')

cv_result_gam_reg <- list(Train_prediction_MSE=mean(gam_reg_train_mse),Test_prediction_MSE=mean(gam_reg_test_mse))
cv_result_gam_reg


# semi-parametric regression model  
gam2_reg <- gam(log_area ~ s(X,k=9)+s(Y,k=6)+season+week+s(FFMC)+s(DMC)+s(DC)+s(ISI)+s(temp)+s(RH)+s(wind)+s(rain,k=4),
               data=forest_fires)
summary(gam2_reg)

# cross validation & compare mean accuracy 
gam2_reg_train_pred <- list(); gam2_reg_train_mse  <- c()
gam2_reg_test_pred <- list(); gam2_reg_test_mse <- c()
for(i in 1:5){
  gam2_reg <- gam(log_area ~ s(X,k=9)+s(Y,k=6)+season+week+s(FFMC)+s(DMC)+s(DC)+s(ISI)+s(temp)+s(RH)+s(wind)+s(rain,k=4),
                 data=train_set[[i]])
  gam2_reg_train_pred[[i]] <- predict(gam2_reg, train_set[[i]])
  gam2_reg_train_mse[i] <- sum((train_set[[i]]$log_area - gam2_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  gam2_reg_test_pred[[i]] <- predict(gam2_reg, test_set[[i]])
  gam2_reg_test_mse[i] <- sum((test_set[[i]]$log_area - gam2_reg_test_pred[[i]])^2)/nrow(test_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',gam2_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',gam2_reg_test_mse,']')

cv_result_gam2_reg <- list(Train_prediction_MSE=mean(gam2_reg_train_mse),Test_prediction_MSE=mean(gam2_reg_test_mse))
cv_result_gam2_reg



# support vector regression 

library(e1071)
svm_reg <- svm(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain,kernel='radial',gamma=0.15,cost=0.75,data=train_data) #Gaussian kernel 
summary(svm_reg)

# cross validation & compare mean accuracy 
svm_reg_train_pred <- list(); svm_reg_train_mse  <- c()
svm_reg_test_pred <- list(); svm_reg_test_mse <- c()
for(i in 1:5){
  svm_reg <- svm(log_area ~ X+Y+season+week+FFMC+DMC+DC+ISI+temp+RH+wind+rain,kernel='radial',gamma=0.15,cost=0.75,data=train_set[[i]])
  svm_reg_train_pred[[i]] <- predict(svm_reg, train_set[[i]])
  svm_reg_train_mse[i] <- sum((train_set[[i]]$log_area - svm_reg_train_pred[[i]])^2)/nrow(train_set[[i]])
  
  svm_reg_test_pred[[i]] <- predict(svm_reg, test_set[[i]])
  svm_reg_test_mse[i] <- sum((test_set[[i]]$log_area - svm_reg_test_pred[[i]])^2)/nrow(test_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',svm_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',svm_reg_test_mse,']')

cv_result_svm_reg <- list(Train_prediction_MSE=mean(svm_reg_train_mse),Test_prediction_MSE=mean(svm_reg_test_mse))
cv_result_svm_reg


## ---------------------------------------------------------------------------------------------------------------
## Reorganizing the variable with PCA ----------------------------------------------------------------------------

library(stats)
conti_var <- forest_fires[,c('X','Y','FFMC','DMC','DC','ISI','temp','RH','wind','rain')]
pc_conti_var <- prcomp(conti_var, scale=T)
summary(pc_conti_var)
names(pc_conti_var)

# scree plot
var_explained <- pc_conti_var$sdev^2/sum(pc_conti_var$sdev^2)
plot(var_explained, type='line')
points(var_explained)

# pc scores 
pc_scores <- pc_conti_var$x 
sum(var_explained[1:5]) # 5 PC explain up to 80% of total variability

# new data
pc_var <- pc_scores[,1:5]
pc_var <- cbind(pc_var, forest_fires[,c('season','week','log_area','area')])

# test train split
N <- nrow(forest_fires)
test_index <- sample(1:N,N/5)
test_pc_data <- pc_var[test_index,]
train_pc_data <- pc_var[-test_index,]

# Cross Validation Set
test_pc_set <- list()
train_pc_set <- list()

for(i in 1:5){
  test_pc_set[[i]] <- pc_var[test_index==i,]
  train_pc_set[[i]] <- pc_var[test_index!=i,]
}

#linear regression w. PC

lin_pc_reg <- lm(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week, data=pc_var)
summary(lin_pc_reg)
anova(lin_pc_reg)

# cross validation & compare mean accuracy 
lin_pc_reg_train_pred <- list(); lin_pc_reg_train_mse  <- c()
lin_pc_reg_test_pred <- list(); lin_pc_reg_test_mse <- c()
for(i in 1:5){
  lin_pc_reg <- lm(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week, data=train_pc_set[[i]])
  
  lin_pc_reg_train_pred[[i]] <- predict(lin_pc_reg, train_pc_set[[i]])
  lin_pc_reg_train_mse[i] <- sum((train_pc_set[[i]]$log_area - lin_pc_reg_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  lin_pc_reg_test_pred[[i]] <- predict(lin_reg, test_set[[i]])
  lin_pc_reg_test_mse[i] <- sum((test_pc_set[[i]]$log_area - lin_pc_reg_test_pred[[i]])^2)/nrow(test_pc_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',lin_pc_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',lin_pc_reg_test_mse,']')

cv_result_lin_pc_reg <- list(Train_prediction_MSE=mean(lin_pc_reg_train_mse),Test_prediction_MSE=mean(lin_pc_reg_test_mse))
cv_result_lin_pc_reg


# robust M regression w. PC

library(MASS)
m_pc_reg <- rlm(log_area ~PC1+PC2+PC3+PC4+PC5+season+week, data=pc_var)
summary(m_pc_reg)
anova(m_pc_reg)

# cross validation & compare mean accuracy 
m_pc_reg_train_pred <- list(); m_pc_reg_train_mse  <- c()
m_pc_reg_test_pred <- list(); m_pc_reg_test_mse <- c()
for(i in 1:5){
  m_pc_reg <- rlm(log_area ~PC1+PC2+PC3+PC4+PC5+season+week, data=train_pc_set[[i]])
  
  m_pc_reg_train_pred[[i]] <- predict(m_pc_reg, train_pc_set[[i]])
  m_pc_reg_train_mse[i] <- sum((train_pc_set[[i]]$log_area - m_pc_reg_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  m_pc_reg_test_pred[[i]] <- predict(m_reg, test_set[[i]])
  m_pc_reg_test_mse[i] <- sum((test_pc_set[[i]]$log_area - m_pc_reg_test_pred[[i]])^2)/nrow(test_pc_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',m_pc_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',m_pc_reg_test_mse,']')

cv_result_m_pc_reg <- list(Train_prediction_MSE=mean(m_pc_reg_train_mse),Test_prediction_MSE=mean(m_pc_reg_test_mse))
cv_result_m_pc_reg


# robust LMS regression w. PC

library(MASS)
lms_pc_reg <- lqs(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week, data=pc_var,method='lms')
summary(lms_pc_reg)

# cross validation & compare mean accuracy 
lms_pc_reg_train_pred <- list(); lms_pc_reg_train_mse  <- c()
lms_pc_reg_test_pred <- list(); lms_pc_reg_test_mse <- c()
for(i in 1:5){
  lms_pc_reg <- lqs(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week, data=train_pc_set[[i]],method='lms')
  
  lms_pc_reg_train_pred[[i]] <- predict(lms_pc_reg, train_pc_set[[i]])
  lms_pc_reg_train_mse[i] <- sum((train_pc_set[[i]]$log_area - lms_pc_reg_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  lms_pc_reg_test_pred[[i]] <- predict(lms_reg, test_set[[i]])
  lms_pc_reg_test_mse[i] <- sum((test_pc_set[[i]]$log_area - lms_pc_reg_test_pred[[i]])^2)/nrow(test_pc_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',lms_pc_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',lms_pc_reg_test_mse,']')

cv_result_lms_pc_reg <- list(Train_prediction_MSE=mean(lms_pc_reg_train_mse),Test_prediction_MSE=mean(lms_pc_reg_test_mse))
cv_result_lms_pc_reg

# robust LTS regression w.pc

lts_pc_reg <- lqs(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week, data=pc_var,method='lts')
summary(lts_pc_reg)

# cross validation & compare mean accuracy 
lts_pc_reg_train_pred <- list(); lms_pc_reg_train_mse  <- c()
lts_pc_reg_test_pred <- list(); lms_pc_reg_test_mse <- c()
for(i in 1:5){
  lts_pc_reg <- lqs(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week, data=train_pc_data,method='lts')
  
  lts_pc_reg_train_pred[[i]] <- predict(lts_pc_reg, train_pc_set[[i]])
  lts_pc_reg_train_mse[i] <- sum((train_pc_set[[i]]$log_area - lts_pc_reg_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  lts_pc_reg_test_pred[[i]] <- predict(lts_reg, test_set[[i]])
  lts_pc_reg_test_mse[i] <- sum((test_pc_set[[i]]$log_area - lts_pc_reg_test_pred[[i]])^2)/nrow(test_pc_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',lts_pc_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',lts_pc_reg_test_mse,']')

cv_result_lts_pc_reg <- list(Train_prediction_MSE=mean(lts_pc_reg_train_mse),Test_prediction_MSE=mean(lts_pc_reg_test_mse))
cv_result_lts_pc_reg


# quantile regression: median w. pc

library(quantreg)
quant_pc_reg <- rq(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week,tau=0.50, data=pc_var)
summary(quant_pc_reg)

# cross validation & compare mean accuracy 
quant_pc_reg_train_pred <- list(); quant_pc_reg_train_mse  <- c()
quant_pc_reg_test_pred <- list(); quant_pc_reg_test_mse <- c()
for(i in 1:5){
  quant_pc_reg <- rq(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week,tau=0.50, data=train_pc_set[[i]])

  quant_pc_reg_train_pred[[i]] <- predict(quant_pc_reg, train_pc_set[[i]])
  quant_pc_reg_train_mse[i] <- sum((train_pc_set[[i]]$log_area - quant_pc_reg_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  quant_pc_reg_test_pred[[i]] <- predict(quant_pc_reg, test_pc_set[[i]])
  quant_pc_reg_test_mse[i] <- sum((test_pc_set[[i]]$log_area - quant_pc_reg_test_pred[[i]])^2)/nrow(test_pc_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',quant_pc_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',quant_pc_reg_test_mse,']')

cv_result_quant_pc_reg <- list(Train_prediction_MSE=mean(quant_pc_reg_train_mse),Test_prediction_MSE=mean(quant_pc_reg_test_mse))
cv_result_quant_pc_reg



# LOESS w. PC (defualt)

loess_pc_reg <- loess(log_area ~ PC1+PC2+PC3,data=pc_var)# default[degree=2,span=0.75]
summary(loess_pc_reg)

# cross validation & compare mean accuracy 
loess_pc_reg_train_pred <- list(); loess_pc_reg_train_mse  <- c()
loess_pc_reg_test_pred <- list(); loess_pc_reg_test_mse <- c()
for(i in 1:5){
  loess_pc_reg <- loess(log_area ~  PC1+PC2+PC3,data=train_pc_set[[i]])# default[degree=2,span=0.75]
  
  loess_pc_reg_train_pred[[i]] <- predict(loess_pc_reg, train_pc_set[[i]])
  loess_pc_reg_train_mse[i] <- sum((train_pc_set[[i]]$log_area - loess_pc_reg_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  loess_pc_reg_test_pred[[i]] <- predict(loess_pc_reg, test_pc_set[[i]])
  if(sum(is.na(loess_pc_reg_test_pred[[i]]))>0){
    na_index <- (1:nrow(test_pc_set[[i]]))[is.na(loess_pc_reg_test_pred[[i]])]
  }else{
    na_index <-10000
  }
  loess_pc_reg_test_mse[i] <- sum((test_pc_set[[i]]$log_area[-na_index] - loess_pc_reg_test_pred[[i]][-na_index])^2)/nrow(test_pc_set[[i]][-na_index])
}

cat(' 5-fold train prediction MSEs are [',loess_pc_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',loess_pc_reg_test_mse,']')

cv_result_loess_pc_reg <- list(Train_prediction_MSE=mean(loess_pc_reg_train_mse),Test_prediction_MSE=mean(loess_pc_reg_test_mse))
cv_result_loess_pc_reg

# LOESS w. PC (adjusted)

loess_pc_reg2 <- loess(log_area ~ PC1+PC2+PC3,degree=1,span=1,data=pc_var)# default[degree=2,span=0.75]
summary(loess_pc_reg2)

# cross validation & compare mean accuracy 
loess_pc_reg2_train_pred <- list(); loess_pc_reg2_train_mse  <- c()
loess_pc_reg2_test_pred <- list(); loess_pc_reg2_test_mse <- c()
for(i in 1:5){
  loess_pc_reg2 <- loess(log_area ~  PC1+PC2+PC3,degree=1,span=1,data=train_pc_set[[i]])# default[degree=2,span=0.75]
  
  loess_pc_reg2_train_pred[[i]] <- predict(loess_pc_reg2, train_pc_set[[i]])
  loess_pc_reg2_train_mse[i] <- sum((train_pc_set[[i]]$log_area - loess_pc_reg2_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  loess_pc_reg2_test_pred[[i]] <- predict(loess_pc_reg2, test_pc_set[[i]])
  if(sum(is.na(loess_pc_reg2_test_pred[[i]]))>0){
    na_index <- (1:nrow(test_pc_set[[i]]))[is.na(loess_pc_reg2_test_pred[[i]])]
  }else{
    na_index <-10000
  }
  loess_pc_reg2_test_mse[i] <- sum((test_pc_set[[i]]$log_area[-na_index] - loess_pc_reg2_test_pred[[i]][-na_index])^2)/nrow(test_pc_set[[i]][-na_index])
}

cat(' 5-fold train prediction MSEs are [',loess_pc_reg2_train_mse,']','\n',
    '5-fold test prediction MSEs are [',loess_pc_reg2_test_mse,']')

cv_result_loess_pc_reg2 <- list(Train_prediction_MSE=mean(loess_pc_reg2_train_mse),Test_prediction_MSE=mean(loess_pc_reg2_test_mse))
cv_result_loess_pc_reg2

# GAM w. PC
library(mgcv)

# semi-parametric gamma regression model  
gam_pc_reg <- gam((area+1) ~ s(PC1)+s(PC2)+s(PC3)+s(PC4)+s(PC5)+season+week,family=Gamma(link='log'),data=pc_var)
summary(gam_pc_reg)

# cross validation & compare mean accuracy 
gam_pc_reg_train_pred <- list(); gam_pc_reg_train_mse  <- c()
gam_pc_reg_test_pred <- list(); gam_pc_reg_test_mse <- c()
for(i in 1:5){
  gam_pc_reg <- gam((area+1) ~ s(PC1)+s(PC2)+s(PC3)+s(PC4)+s(PC5)+season+week,
                 family=Gamma(link='log'),data=train_pc_set[[i]])  
  
  gam_pc_reg_train_pred[[i]] <- predict(gam_pc_reg, train_pc_set[[i]])
  gam_pc_reg_train_mse[i] <- sum((train_pc_set[[i]]$log_area - gam_pc_reg_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  gam_pc_reg_test_pred[[i]] <- predict(gam_pc_reg, test_pc_set[[i]])
  gam_pc_reg_test_mse[i] <- sum((test_pc_set[[i]]$log_area - gam_pc_reg_test_pred[[i]])^2)/nrow(test_pc_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',gam_pc_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',gam_pc_reg_test_mse,']')

cv_result_gam_pc_reg <- list(Train_prediction_MSE=mean(gam_pc_reg_train_mse),Test_prediction_MSE=mean(gam_pc_reg_test_mse))
cv_result_gam_pc_reg

# semi-parametric regression model  
gam2_pc_reg <- gam(log_area ~ s(PC1)+s(PC2)+s(PC3)+s(PC4)+s(PC5)+season+week,data=pc_var)
summary(gam2_pc_reg)

# cross validation & compare mean accuracy 
gam2_pc_reg_train_pred <- list(); gam2_pc_reg_train_mse  <- c()
gam2_pc_reg_test_pred <- list(); gam2_pc_reg_test_mse <- c()
for(i in 1:5){
  gam2_pc_reg <- gam(log_area ~ s(PC1)+s(PC2)+s(PC3)+s(PC4)+s(PC5)+season+week,data=train_pc_set[[i]]) 
  
  gam2_pc_reg_train_pred[[i]] <- predict(gam2_pc_reg, train_pc_set[[i]])
  gam2_pc_reg_train_mse[i] <- sum((train_pc_set[[i]]$log_area - gam2_pc_reg_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  gam2_pc_reg_test_pred[[i]] <- predict(gam2_pc_reg, test_pc_set[[i]])
  gam2_pc_reg_test_mse[i] <- sum((test_pc_set[[i]]$log_area - gam2_pc_reg_test_pred[[i]])^2)/nrow(test_pc_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',gam2_pc_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',gam2_pc_reg_test_mse,']')

cv_result_gam2_pc_reg <- list(Train_prediction_MSE=mean(gam2_pc_reg_train_mse),Test_prediction_MSE=mean(gam2_pc_reg_test_mse))
cv_result_gam2_pc_reg


# support vector regression 

library(e1071)
svm_pc_reg <- svm(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week,kernel='radial',gamma=0.15,cost=0.75,data=pc_var) #Gaussian kernel 
summary(svm_pc_reg)

# cross validation & compare mean accuracy 
svm_pc_reg_train_pred <- list(); svm_pc_reg_train_mse  <- c()
svm_pc_reg_test_pred <- list(); svm_pc_reg_test_mse <- c()
for(i in 1:5){
  svm_pc_reg <- svm(log_area ~ PC1+PC2+PC3+PC4+PC5+season+week,kernel='radial',gamma=0.15,cost=0.75,data=train_pc_set[[i]])
  
  svm_pc_reg_train_pred[[i]] <- predict(svm_pc_reg, train_pc_set[[i]])
  svm_pc_reg_train_mse[i] <- sum((train_pc_set[[i]]$log_area - svm_pc_reg_train_pred[[i]])^2)/nrow(train_pc_set[[i]])
  
  svm_pc_reg_test_pred[[i]] <- predict(gam2_pc_reg, test_pc_set[[i]])
  svm_pc_reg_test_mse[i] <- sum((test_pc_set[[i]]$log_area - svm_pc_reg_test_pred[[i]])^2)/nrow(test_pc_set[[i]])
}

cat(' 5-fold train prediction MSEs are [',svm_pc_reg_train_mse,']','\n',
    '5-fold test prediction MSEs are [',svm_pc_reg_test_mse,']')

cv_result_svm_pc_reg <- list(Train_prediction_MSE=mean(svm_pc_reg_train_mse),Test_prediction_MSE=mean(svm_pc_reg_test_mse))
cv_result_svm_pc_reg


## Comparison
Result_data <- cbind(cv_result_lin_reg, cv_result_m_reg, cv_result_lms_reg, cv_result_lts_reg,cv_result_quant_reg,
                     cv_result_loess_reg,cv_result_loess2_reg,cv_result_gam_reg,cv_result_gam2_reg, cv_result_svm_reg)
colnames(Result_data) <- c('Linear','M-reg.','LMS-reg.','LTS-reg.','Quantile','LOESS','LOESS(adjusted)','GAM(Gamma)','GAM','SVM')

Result_PC <- cbind(cv_result_lin_pc_reg, cv_result_m_pc_reg, cv_result_lms_pc_reg, cv_result_lts_pc_reg,cv_result_quant_pc_reg,
                   cv_result_loess_pc_reg,cv_result_loess_pc_reg2,cv_result_gam_pc_reg,cv_result_gam2_pc_reg, cv_result_svm_pc_reg)
colnames(Result_PC) <- c('Linear','M-reg.','LMS-reg.','LTS-reg.','Quantile','LOESS','LOESS(adjusted)','GAM(Gamma)','GAM','SVM')

Result_data
Result_PC

## Discussion point & Conclusion 



