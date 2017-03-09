dir <- choose.dir()
setwd(dir)
getwd()

#Linear Regression
#It is a way of finding a relationship between a single, continuous variable 
#called Dependent or Target variable and one or more other variables 
#(continuous or not) called Independent Variables.

#Assumptions of Linear Regression Analysis 

#1. Linear Relationship : Linear regression needs a linear relationship 
#between the dependent and independent variables.

#2. Normality of Residual : Linear regression requires residuals should 
#be normally distributed.

#3. Homoscedasticity :  Linear regression assumes that residuals are 
#approximately equal for all predicted dependent variable values.

#4. No Outlier Problem

#5. Multicollinearity : It means there is a high correlation between 
#independent variables. The linear regression model MUST NOT be faced
#with problem of multicollinearity.

#6. Independence of error terms - No Autocorrelation

#It states that the errors associated with one observation are not 
#correlated with the errors of any other observation. It is a problem
#when you use time series data. Suppose you have collected data from labors 
#in eight different districts. It is likely that the labors within each district
#will tend to be more like one another that labors from different districts,
#that is, their errors are not independent.


#Measures of Model Performance

#1. R-squared
#It measures the proportion of the variation in your dependent 
#variable explained by all of your independent variables in the model.
#It assumes that every independent variable in the model helps to explain
#variation in the dependent variable. In reality, some variables don't 
#affect dependent variable and they don't help building a good model.

#Higher the R-squared, the better the model fits your data. 
#In psychological surveys or studies, we generally found low R-squared
#values lower than 0.5. It is because we are trying to predict human 
#behavior and it is not easy to predict humans. In these cases, 
#if your R-squared value is low but you have statistically significant
#independent variables (aka predictors), you can still generate insights
#about how changes in the predictor values are associated with changes in
#the response value.


#2. Adjusted R-squared
#It measures the proportion of variation explained by only those independent 
#variables that really affect the dependent variable. It penalizes you for
#adding independent variable that do not affect the dependent variable.

#Every time you add a independent variable to a model, the R-squared increases,
#even if the independent variable is insignificant. It never declines. 
#Whereas Adjusted R-squared increases only when independent variable is 
#significant and affects dependent variable


#3. RMSE ( Root Mean Square Error)
#It explains how close the actual data points are to the model's predicted values.

#The RMSE for your training and your test sets should be very similar if you 
#have built a good model. If the RMSE for the test set is much higher than that
#of the training set, it is likely that you've badly over fit the data, 
#i.e. you've created a model that works well in sample, but has little predictive
#value when tested out of sample.

#Lower values of RMSE indicate better fit. RMSE is a good measure of how 
#accurately the model predicts the response, and is the most important 
#criterion for fit if the main purpose of the model is prediction.

#R-squared is a relative measure of fit, RMSE is an absolute measure of fit.


#The code below covers the assumption testing and evaluation of model performance :
#Data Preparation
#Testing of Multicollinearity
#Treatment of Multicollinearity
#Checking for Autocorrelation
#Checking for Outliers
#Checking for Heteroscedasticity
#Testing of Normality of Residuals
#Forward, Backward and Stepwise Selection
#Calculating RMSE
#Box Cox Transformation of Dependent Variable
#Calculating R-Squared and Adj, R-squared manually
#Calculating Residual and Predicted values
#Calculating Standardized Coefficient

library(ggplot2)
library(car)
library(caret)


#Loading data
data(mtcars)

#Converting categorical variables to factor
mtcars$am   = as.factor(mtcars$am)
mtcars$cyl  = as.factor(mtcars$cyl)
mtcars$vs   = as.factor(mtcars$vs)
mtcars$gear = as.factor(mtcars$gear)

#Dropping dependent variable
mtcars_a = subset(mtcars, select = -c(mpg))

#Identifying numeric variables
numericData <- mtcars_a[sapply(mtcars_a, is.numeric)]

#Calculating Correlation
descrCor <- cor(numericData)
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.6)#CARET

#Identifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

#Print highly correlated attributes
highlyCorCol

#Remove highly correlated variables and create a new dataset
dat3 <- mtcars[, -which(colnames(mtcars) %in% highlyCorCol)]
dim(dat3)

#Build Linear Regression Model
fit = lm(mpg ~ ., data=dat3)

#Check Model Performance
summary(fit)

#Extracting Coefficients
summary(fit)$coeff

#Extracting Rsquared value
summary(fit)$r.squared

#Extracting Adj. Rsquared value
summary(fit)$adj.r.squared

#Stepwise Selection based on AIC
library(MASS)
step <- stepAIC(fit, direction="both")
summary(step)

#Backward Selection based on AIC
step <- stepAIC(fit, direction="backward")
summary(step)

#Forward Selection based on AIC
step <- stepAIC(fit, direction="forward")
summary(step)

#Stepwise Selection with BIC
n = dim(dat3)[1]
stepBIC = stepAIC(fit,k=log(n))
summary(stepBIC)

#R Function : Standardised coefficients
stdz.coff <- function (regmodel)
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
sy <- sapply(regmodel$model[1], sd)
beta <- b * sx / sy
return(beta)
}
std.Coeff = data.frame(Standardized.Coeff = stdz.coff(stepBIC))
std.Coeff = cbind(Variable = row.names(std.Coeff), std.Coeff)
row.names(std.Coeff) = NULL 

#Testing for Multicollinearity
#Check VIF of all the variables
vif(stepBIC)

#Autocorrelation Test
durbinWatsonTest(stepBIC)

#Normality Of Residuals (Should be > 0.05)
res=residuals(stepBIC,type="pearson")
shapiro.test(res)

#Testing for heteroscedasticity (Should be > 0.05)
ncvTest(stepBIC)

#Outliers - Bonferonni test
outlierTest(stepBIC)

#See Residuals
resid <- residuals(stepBIC)

#Relative Importance
library(relaimpo)
calc.relimp(stepBIC)

#See Predicted Value
pred = predict(stepBIC,dat3)

#See Actual vs. Predicted Value
finaldata = cbind(mtcars,pred)
print(head(subset(finaldata, select = c(mpg,pred))))

#Calculating RMSE
rmse <- sqrt(mean((dat3$mpg - pred)^2))
print(rmse)

#Calculating Rsquared manually
y = dat3[,c("mpg")]
R.squared = 1 - sum((y-pred)^2)/sum((y-mean(y))^2)
print(R.squared)

#Calculating Adj. Rsquared manually
n = dim(dat3)[1]
p = dim(summary(stepBIC)$coeff)[1] - 1
adj.r.squared = 1 - (1 - R.squared) * ((n - 1)/(n-p-1))
print(adj.r.squared)


#Box Cox Transformation
library(lmSupport)
modelBoxCox(stepBIC)
