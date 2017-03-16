

################\
# Qn 1

n <- 82 #no of student

# y = 15.3 + 0.72x 

beta0 <- 15.3
beta1 <- 0.72
std.error <- 0.38


df <- n-2 # degree of freedom


#Test Requirement

#1.The dependent variable Y has a linear relationship to the independent variable X.
#2.For each value of X, the probability distribution of Y has the same standard deviation ??.
#3.For any given value of X,
#a.The Y values are independent.
#b.The Y values are roughly normally distributed. A little skewness is ok if the sample size is large.


#Hyptheses
#H0: slope = 0
#HA: slope !=0

t <- beta1/std.error# test statistic
t#1.895 

#for two tail - t = lower than -1.895 and greater than 1.895

pvalue <- pt(t,df=df, lower.tail = F)*2 #for two tail
pvalue# 0.0617 is greater than alpha ie 0.05
#HO can't be rejected for given data for two tailed

#b
pvalue <- pt(t,df=df, lower.tail = F) #for one tail
pvalue# 0.0308 is smaller than alpha
#HO is rejected 



##########################
# Qn 2
##########################

getdata = function(x) read.table(file=paste ("http://www.umich.edu/~dnoll/BME503/",x,sep=""), header=T)
ibi = getdata("ibi.txt")
head(ibi)

attach(ibi)

# a.
summary(ibi) # ur method is ok

#graphical method
par(mfrow = c(1,3)) # adjust the graphical plotting slides
hist(IBI)# Left Skew
boxplot(IBI)#No outlier
qqnorm(IBI)
qqline(IBI,col=2,lwd=3,pch=19,datax=FALSE)
#there are some values at higher end of tail stir away from normality 
#Normally distributed skewed to left
library(e1071) 
skewness(IBI) # -0.543



#for Area
hist(Area)
boxplot(Area)#2outlier
qqnorm(Area)#Nearly Normally distributed
qqline(Area,col=2,lwd=3,pch=19,datax=FALSE) 
#there are some values at higher end of tail stir away from normality
#Normally distributed skewed to right 
skewness(Area)#0.676



#b. - Ur version is totally wrong you have to plot scatter plot taking Area as 
      #explanctory variable in X axis and IBI as response in Y axis
par(mfrow = c(1,1))
plot(IBI~Area, col = 4, pch = 18) #seems to be in linear relation ship
cor(Area,IBI)#0.446  - IBI is moderately correlated with Area
#There are many potential outliers for lower Area 

#c
#Relation between IBI and Area can be explained by simple linear regression
#Assumption:
  #1. Linearity: Relation between Area (explanatory var) and IBI(response var) should be linear
  #2. Nearly normal residual 
  #3. Constant Variability - Homoscedasticity
model <- lm(IBI~Area)
plot(IBI~Area, col = 4, pch = 18)
abline(model, col = "red", lwd = 2)


#d
#HO: There is no relationship between Area and IBI and hence slope is equal to 0
#HA: there is significant linear relationship hence slope is not equal to 0


#e
model <- lm(IBI~Area)
model #intercept/beta0 = 52.923, beta1 = 0.4602
# IBI = 52.923 + 0.4602xArea 

summary(model)
#p-value - 0.001322  - significant (HO rejected)
#R squared - 0.1988  - 19.88% of variability in IBI is explained by Area by this model


#f
res <- model$residuals
plot(res~Area, col = "red", pch = 16) #Assumption of Homoscedasticity is violated


#g
qqnorm(res)
qqline(res, col="red", lwd = 2)
#Aproximate normally distributed with some values at top and bottom stirring away from narmality


#h
#Can consider the assumption is reasonable
#1. Linearity - (Can be checked in scatterplot of IBI and Area and residula plot)
plot(IBI~Area) #Linearity is ok
plot(res)#Linearity is ok

#2. Nearly Normal  residual
#Satisfied - from g

#3. Constant Variability - Homoscedasticity
plot(res~Area)
abline(h=0)
#Assumption of Homoscedasticity is somewhat seems to violated in lower half but 
#can be ignored



##################################
#    Qn 3
#################################

tornado = getdata("tornadoes.txt")
tornado$Year <- as.numeric(tornado$Year)
View(tornado)

summary(tornado)

attach(tornado)
#a
par(mfrow = c(2,3))
hist(Tornadoes)#Nearly normal distribution with Right Skew
boxplot(Tornadoes)#one outlier
qqnorm(Tornadoes)
qqline(Tornadoes, col = "red", lwd = 2)

hist(Population) 
boxplot(Population)
qqnorm(Population)#Normally distributed but heavy tailed
qqline(Population, col = "red", lwd = 2)

par(mfrow = c(1,1))
#b
#i Tornado vs year
mod1 <- lm(Tornadoes~Year)
summary(mod1)
#p-value: 4.279e-14 - Significant
#R-squared:  0.6553 - 65.53% of variability in Tornadoes is explained by Year
plot(Year, Tornadoes)
abline(mod1, lwd = 2 , col = "red")

plot(mod1$residuals~Year)#Homoscedasticity is ok

qqnorm(mod1$residuals)
qqline(mod1$residuals, col = "red", lwd = 2) #Nearly Normal 


#ii. Tornado vs Population
mod2 <- lm(Tornadoes~Population)
summary(mod2)
#p-value: 1.404e-14 - Significant
#R-squared:  0.6691 - 66.91% of variability in Tornadoes is explained by Population
plot(Population, Tornadoes)
abline(mod2, lwd = 2 , col = "red")

plot(mod2$residuals~Year)#Homoscedasticity is ok

qqnorm(mod2$residuals)
qqline(mod2$residuals, col = "red", lwd = 2) #Nearly Normal 


#i Population vs year
mod3 <- lm(Population~Year)
summary(mod3)
#p-value: 2.2e-16 - Significant
#R-squared:  0.9962 - 99.62% of variability in Tornadoes is explained by Year
plot(Year, Population)
abline(mod3, lwd = 2 , col = "red") #Highly corelated

plot(mod3$residuals~Year)#Homoscedasticity is violated

qqnorm(mod3$residuals)
qqline(mod3$residuals, col = "red", lwd = 2) #Nearly Normal with some value at stir aay from normality


#c
model <- lm(Tornadoes~Population+Year)
summary(model)
#p-value: 6.512e-14 - Significant
#R-squared:  0.682 - 68.2% of variability in Tornadoes is explained by the Model
plot(model)
# Tornadoes = 63680 + 0.01912xPopulation - 33.91xYear

#d
res <- model$residuals

hist(res)

qqnorm(res)
qqline(res, col="red", lwd = 2)# Nearly Normal

plot(res~Population) #nearly Normal

plot(res~Year)  #nearly Normal
#Each plot explains the normality of residuals


#e
#HO: There is no relationship between Number of Tornadoes and Population and Year and hence slope is equal to 0
#HA: there is significant linear relationship hence slope is not equal to 0

summary(model)#find t value and p value
#Tornadoes are significantly related with Population but not significant with Year 
# in a multiple regression model

#From  Mod 1 - it is evident that number of tornadoes increased over time
#
#From Mod 2 - it is evident that number of tornadoes increased with increase with population

#From Mod 3 - Population increases over time (High relation)

#In final model - relationship between tornadoes and population is significant whereas not so significant with year

#So it is concluded that no of tornadoes increased over time is due to increase in population over time which result in more sighting of tornadoes
