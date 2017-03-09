#==============================================================================
#      SABYASACHI SAHU  --------   sab21.sahu@gmail.com   --- 3rd Feb'17
#==============================================================================
#           Case Study No 1 - Credit Card Segmentation
#==============================================================================

# Clear environment
rm(list = ls())

#Set Directory
dir <- choose.dir()
setwd(dir)
getwd()


#Loading Dataset
data1 <- read.csv("CC GENERAL.csv", stringsAsFactors = FALSE)

#Data Summary Overview
library(dplyr)
glimpse(data1)
View(data1)
head(data1, 3)
str(data1)
dim(data1)
summary(data1)

#Check Missing Values
colSums(is.na(data1))

# missing values dataset
missingData <- data[!complete.cases(data),]
write.csv(missingData, "missing.csv")

#------------------------------------------------------------------------------

# user written function for creating descriptive statistics

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  len <- length(boxplot.stats(x)$out)
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,
           min = min, pct1=p1,pct5=p5,pct10=p10,qnt1=q1,qnt2=q2,qnt3=q3,pct90=p90,
           pct95=p95,pct99=p99,max=max, UC=UC, LC=LC, outliersCount = len ))
}


data_stats <- t(data.frame(apply(data1[,-1], 2, mystats)))
write.csv(data_stats, "statsnew.csv") #importing stats into excel to visualize

#------------------------------------------------------------------
#Found 1 Missing Value in CREDIT_LIMIT, and 313 missing values in MINIMUM_PAYMENTS
#Imputing both by Median
data1$CREDIT_LIMIT[is.na(data1$CREDIT_LIMIT)] <- median(data1$CREDIT_LIMIT,na.rm = T)
data1$MINIMUM_PAYMENTS[is.na(data1$MINIMUM_PAYMENTS)] <- median(data1$MINIMUM_PAYMENTS,na.rm = T)
dataNew <- data1
#Or Impute it by user defined function 

#Missing data imputation user defined function - 
fMissing <- function(x){
  x <- as.numeric(as.character(x)) #first convert factor variable to numeric
  x[is.na(x)] <- median(x,na.rm = TRUE)
  x
}
 
dataNew <- data.frame(apply(data1,2,fMissing)) #Calling Function to impute missing
dataNew$CUST_ID <- data1$CUST_ID #Cust_ID coerced to NA as it was class charachter
summary(dataNew)
str(dataNew)
#--------------------------------------------------------------------------

  
#Importing data sets for data preparation in Excel
write.csv(dataNew,"dataNew.csv")

#Advanced Data Preparation (KPI)

#1. Usage Limit = Balance to Credit Limit Ratio
dataNew$CREDIT_LIMIT_USAGE <- dataNew$BALANCE/dataNew$CREDIT_LIMIT

#2 AVG_AMOUNT_PER_PURCHASE
dataNew$AVG_AMOUNT_PER_PURCHASE_TRX <- ifelse(dataNew$PURCHASES_TRX==0,0,
                                          dataNew$PURCHASES/dataNew$PURCHASES_TRX)
#3AVG_CASH_ADV_AMOUNT_PER_TRANSACTION
dataNew$AVG_CASH_ADV_AMOUNT_PER_TRX <- ifelse(dataNew$CASH_ADVANCE_TRX==0,0,
                                          dataNew$CASH_ADVANCE/dataNew$CASH_ADVANCE_TRX)
#4MONTHLY_AVG_PURCHASE
dataNew$MONTHLY_AVG_PURCHASE <- dataNew$PURCHASES/dataNew$TENURE

#5MONHTLY_AVG_CASH_ADV
dataNew$MONHTLY_AVG_CASH_ADV <- dataNew$CASH_ADVANCE/dataNew$TENURE

#6 PAYMENT_TO_MINIMUM_PAYMENT_RATIO
dataNew$PAYMENT_TO_MINIMUM_PAYMENT_RATIO <- dataNew$PAYMENTS/dataNew$MINIMUM_PAYMENTS

#7 One Off Customers Flag
dataNew$ONEOFF_FLAG <- ifelse(dataNew$ONEOFF_PURCHASES>0,1,0)

#8 INSTALLMENT_FLAG
dataNew$INSTALLMENT_FLAG <- ifelse(dataNew$INSTALLMENTS_PURCHASES>0,1,0)

#9 CASH_ADV_FLAG
dataNew$CASH_ADV_FLAG <- ifelse(dataNew$CASH_ADVANCE>0,1,0)


#10 Customer Category - Oneoff, Installment, Cash Advance
dataNew$CUSTOMER_CATEGORY <- ifelse(dataNew$ONEOFF_FLAG==1&dataNew$CASH_ADV_FLAG==0&
                                     dataNew$INSTALLMENT_FLAG==0,1, #Category 1 - Oneoff Only
                             ifelse(dataNew$ONEOFF_FLAG==0&dataNew$CASH_ADV_FLAG==0&
                                     dataNew$INSTALLMENT_FLAG==1,2, #Category 2 - Installment Only
                             ifelse(dataNew$ONEOFF_FLAG==0&dataNew$CASH_ADV_FLAG==1&
                                     dataNew$INSTALLMENT_FLAG==0,3, #Category 3 - Cash Adv Only
                             ifelse(dataNew$ONEOFF_FLAG==1&dataNew$CASH_ADV_FLAG==1
                                     &dataNew$INSTALLMENT_FLAG==1,5,#Category 5 - All
                             ifelse(dataNew$ONEOFF_FLAG==0&dataNew$CASH_ADV_FLAG==0&
                                     dataNew$INSTALLMENT_FLAG==0,0,4))))) #Category 0 - None
                                                                    #Category 4 - Others
dim(dataNew)

aggregate(PAYMENTS~CUSTOMER_CATEGORY, data = dataNew, sum)
aggregate(PAYMENTS~CUSTOMER_CATEGORY, data = dataNew, mean)
aggregate(PAYMENTS~CUSTOMER_CATEGORY, data = dataNew, length)
aggregate(PURCHASES~CUSTOMER_CATEGORY, data = dataNew, mean)
aggregate(CASH_ADVANCE~CUSTOMER_CATEGORY, data = dataNew, mean)
names(dataNew)
aggregate(CREDIT_LIMIT_USAGE~CUSTOMER_CATEGORY, data = dataNew, mean)


#Visualizing Customer Category
cat <- table(factor(dataNew$CUSTOMER_CATEGORY))
bplot <- barplot(cat, main = "Customer Category",xlab="Category", beside=T, horiz = F)
# variable 'bpl0t' is now a matrix of vertical bar positions on the x-axis
boxplot(dataNew$BALANCE~dataNew$CUSTOMER_CATEGORY)
boxplot(dataNew$BALANCE)
boxplot.stats(dataNew$BALANCE)$out
x <- subset(dataNew,dataNew$CUSTOMER_CATEGORY==2)
(boxplot.stats(x$PURCHASES)$out)


text(x=bplot, y=(cat*.5), labels = as.character(cat), xpd=T)
# Needed to use xpd=TRUE because the xlimits were too narrow.

plot(dataNew$USAGE_LIMIT)
with(dataNew, plot(CREDIT_LIMIT_USAGE,PURCHASES))
max(dataNew$CREDIT_LIMIT_USAGE)

dataNew[which.max(dataNew$CREDIT_LIMIT_USAGE),]

#----------------------------------------

# Initial Variable Deletion for analysis
names(dataNew)
#Variable Identified which are not significant
  #"CUST_ID" - Character Variable
  #"CUSTOMER_CATEGORY"
vars <- c("CUST_ID", "CUSTOMER_CATEGORY")
dataNew[,vars] <- NULL
names(dataNew)



#------------------------------------------------------------------------------
#Checking Outliers
#------------------------------------------------------------------------------
stat2 <- t(as.data.frame(apply(dataNew[,-1],2,mystats)))
write.csv(stat2, "stats2.csv")

names(dataNew)
#Variables need to be capped - 
  #"BALANCE" "BALANCE_FREQUENCY" "PURCHASES" "ONEOFF_PURCHASES" "INSTALLMENTS_PURCHASES" "CASH_ADVANCE"        
  #"CASH_ADVANCE_FREQUENCY" - Needs to be capped to 1 if it's greater than 1
  #"CASH_ADVANCE_TRX" "PURCHASES_TRX"  "CREDIT_LIMIT" "PAYMENTS" "MINIMUM_PAYMENTS"                              
  #"AVG_AMOUNT_PER_PURCHASE" "AVG_CASH_ADV_AMOUNT_PER_TRANSACTION" "MONTHLY_AVG_PURCHASE"               
  #"MONHTLY_AVG_CASH_ADV" "PAYMENT_TO_MINIMUM_PAYMENT_RATIO" "CREDIT_LIMIT_USAGE"

vars <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
          "CASH_ADVANCE","CASH_ADVANCE_TRX", "PURCHASES_TRX" ,"CREDIT_LIMIT","PAYMENTS",
          "MINIMUM_PAYMENTS","AVG_AMOUNT_PER_PURCHASE", "AVG_CASH_ADV_AMOUNT_PER_TRANSACTION", 
          "MONTHLY_AVG_PURCHASE","MONHTLY_AVG_CASH_ADV", "PAYMENT_TO_MINIMUM_PAYMENT_RATIO", 
          "CREDIT_LIMIT_USAGE")

#------------------------------------------------------------------
#For "CASH_ADVANCE_FREQUENCY",  capping to 1 if it's greater than 1
dataNew$CASH_ADVANCE_FREQUENCY[which(dataNew$CASH_ADVANCE_FREQUENCY>1)] <- 1
cashAdvFrq <- dataNew$CASH_ADV_FLAG
names(dataNew)

#-----------------------------------------------------------


#OUTLIER User defined function
outlier <- function(x, q){
  q <- capping(a)
  cap <- quantile(x,q)
  out <- which(x>cap)
  x[out] <- cap
  x
}

capping <- function(a){a <- cap_percentile;return(a)}

#---------------------------------
#Select percentile for capping
cap_percentile <- .95
#---------------------------------

#data100 <- dataNew[,-1] #AS IS DATA (Only deleting CustID as it is class Character)
#data99 <- data.frame(apply(dataNew[,-1],2,outlier)) # Capped at 99 percentile
data95 <- data.frame(apply(dataNew,2,outlier)) # Capped at 95 percentile
#data90 <- data.frame(apply(dataNew[,-1],2,outlier)) # Capped at 90 percentile



#Capping Correction in following variable
data95$CASH_ADVANCE_FREQUENCY <- cashAdvFrq
summary(data95)
#-------------------------------------------------------------------------------
#Correlation Matrix
corrm <- cor(dataNew[,-1])
write.csv(corrm,"corr1.csv")


#  Variable Reduction
names(dataNew)
#Variable Identified which highly corelated
#"MONTHLY_AVG_PURCHASE" - highly correlated with  PURCHASES
#"MONHTLY_AVG_CASH_ADV" - highly correlation with CASH_ADVANCE
vars <- c("MONTHLY_AVG_PURCHASE", "MONHTLY_AVG_CASH_ADV")
data95[,vars] <- NULL
names(dataNew)

#Variable Selection
vars_org <- colnames(dataNew)
vars_org
#-------------------------------------------------------------------------------
#Principal Component Analysis & Factor Analysis
#-------------------------------------------------------------------------------

#PCA of untreated outlier data (excluding customer category)
pc100 <- prcomp(data100[,-27], scale. = T)
summary(pc100) # Total 8 components eigen value greater than 1 and 74.7% Variance explained
screeplot(pc100, type = "line") #Scree Plot recommends for 3 factors explaining 50% variance
pc100$rotation #VARIMAX
pc100$sdev^2 #Eigen Value
#?prcomp
#?princomp
#?principal
#?factanal
#?fa

library(psych)
fa100 <- fa(cor(data100[,-27]), nfactors =8, rotate = "varimax", fm = "ml")
summary(fa100)
fa100
fa100_sort <- fa.sort(fa100)
fa100_sort$loadings
#factanal(x=data100, factors = 8) # unable to optimize from this starting value


pc99 <- prcomp(data99, scale. = T) #PCA of data capped at 99th Percentile
summary(pc99) # Total 7 components eigen value greater than 1 and 75.9% Variance explained
screeplot(pc99, type = "line") #Scree Plot recommends for 3 factors explaining 50% variance
pc99$rotation




pc95 <- prcomp(data95, scale. = T) #PCA of data capped at 95th Percentile
summary(pc95) #1 Total 6 components eigen value greater than 1 and 75.76% Variance explained
              
screeplot(pc95, type = "line") #Scree Plot recommends for 3 factors explaining 50% variance
pc95rot <- pc95$rotation
write.csv(pc95rot,"pc95rot.csv")
biplot(pc95, cex = .5)



library(psych)
describe((data95))

fa95 <- fa(cor(data95), nfactors =6, rotate = "varimax", fm = "pa")
summary(fa95)
fa95
fa95_sort <- fa.sort(fa95)
load95 <- fa95_sort$loadings
write.csv(load95,"FAload95_pa1.csv")
fa95$e.values
fa95$weights
plot(fa95)
#Variable Reduction 1
#1 PURCHASES - Related with Cash Adv  and Oneoff purchase  and loaded in 3 factors above .5
#2. TENURE - No sigficant loading in any of 6 factors
#3 BALANCE - Loaded in 3 factors significantly above .5
#4. PURCHASES_TRX - Loaded in 2 factors above .5

vars <- c("PURCHASES","TENURE", "BALANCE", "PURCHASES_TRX")
data95[vars] <- NULL

pc95 <- prcomp(data95, scale. = T) #PCA of data capped at 95th Percentile
summary(pc95) #2 Total 5 components eigen value greater than 1 and 72.9% Variance explained

fa95 <- fa(cor(data95), nfactors =5, rotate = "varimax", fm = "pa")
fa95_sort <- fa.sort(fa95)
load95 <- fa95_sort$loadings
write.csv(load95,"FAload95_pa2.csv")


summary(data95)
#PERFORM FACTOR ANALYSIS AGAIN

#-----------------------------------------------------------------------------------
##################### K Mean - Clustering
#=---------------------------------------------------------------------------------------

dat95_final <- scale(data95)##standardizing the data


#building clusters using k-means clustering 
cluster_three <- kmeans(dat95_final,3)
cluster_four <- kmeans(dat95_final,4)
cluster_five <- kmeans(dat95_final,5)
cluster_six <- kmeans(dat95_final,6)

data95New<-cbind(data95,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,
                 km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster)
View(data95New)

#Graph based on k-means 
require(cluster)

clusplot(dat95_final, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2, # Labels clusters and cases
         col.p = cluster_five$cluster,
         cex=.5
)

#library(gmodels)
#with(data95New, CrossTable(km_clust_3,km_clust_4,km_clust_5,km_clust_6))

#data95New%>%
 # group_by(km_clust_3)%>%
  #summarise_all(mean)

segment <- with(data95New, c(table(km_clust_3)/length(km_clust_3),
                  table(km_clust_4)/length(km_clust_3),
                  table(km_clust_5)/length(km_clust_3),
                  table(km_clust_6)/length(km_clust_3)))#segemnt proportion
length(segment)#18


#?clusplot

names(data95New)
data95Profile <- rbind(aggregate(.~km_clust_3, data = data95New, mean), 
aggregate(.~km_clust_4, data = data95New, mean),
aggregate(.~km_clust_5, data = data95New, mean),
aggregate(.~km_clust_6, data = data95New, mean))

rownames(data95Profile) <- c("KM3_1" ,"KM3_2" ,"KM3_3", 
                             "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,
                             "KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", 
                             "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6")
data95Profile[,c("km_clust_3","km_clust_4", "km_clust_5", "km_clust_6")] <- NULL

#data95Profile <- NULL
dim(data95Profile)

#colnames(data95)
#dim(data95Profile)
#View(data95New)  
#colnames(data95Profile)

overallAvg <-  colMeans(data95)#Overall Average of each variable
length(overallAvg)#20



View(data95Profile)
data95Profile <- cbind(data95Profile, Segment = segment)
data95Profile <- rbind(data95Profile, Overall = overallAvg)
data95Profile[nrow(data95Profile), ncol(data95Profile)] <- NA#removing coercion
data95Profile <- t(data95Profile)

#str(data95Profile)
#head(data95Profile, 24)
#tail(data95Profile)
#library(dplyr)
data95New%>%
  group_by(km_clust_3)%>%
  summarise_all(mean)%>%
  #summarise_each(funs(mean), km_clust_3 )%>%
  str()



write.csv(data95Profile, "Profiling_95.csv")

#Alternative Profiling
vars <- colnames(data95)
require(tables)
tt<-cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
                  ~ Heading()*length*All(data95["PAYMENTS"]), data=data95New),
          tabular( 1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
                   ~ Heading()*mean*All(data95[vars]), data=data95New))
tt1<-as.data.frame.matrix(tt)
#View(tt1)

rownames(tt1)<-c("ALL", "KM3_1" ,"KM3_2" ,"KM3_3", "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,"KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6")
colnames(tt1)<-c("Segment_Size",vars)
cluster_profiling<-t(tt1)

write.csv(cluster_profiling, "Cluster_profiling1.csv") 







library(MASS)
#install.packages("clusterGeneration")
library(clusterGeneration)


cov.mat<-genPositiveDefMat(data100,covMethod="unifcorrmat")$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)


#FOR data capping 90 percintile

#Select percentile for capping
cap_percentile <- .90
#---------------------------------

data90 <- data.frame(apply(dataNew,2,outlier)) # Capped at 90 percentile



#Capping Correction in following variable
data90$CASH_ADVANCE_FREQUENCY <- cashAdvFrq
summary(data95)
#-------------------------------------------------------------------------------
#  Variable Reduction
names(dataNew)
#Variable Identified which highly corelated
#"MONTHLY_AVG_PURCHASE" - highly correlated with  PURCHASES
#"MONHTLY_AVG_CASH_ADV" - highly correlation with CASH_ADVANCE
vars <- c("MONTHLY_AVG_PURCHASE", "MONHTLY_AVG_CASH_ADV")
data90[,vars] <- NULL
names(dataNew)

#-------------------------------------------------------------------------------
#Principal Component Analysis & Factor Analysis
#-------------------------------------------------------------------------------


pc90 <- prcomp(data90, scale. = T) #PCA of data capped at 90th Percentile
summary(pc90) #1 Total 5 components eigen value greater than 1 and 74.08% Variance explained

#screeplot(pc95, type = "line") #Scree Plot recommends for 3 factors explaining 50% variance
#pc95rot <- pc95$rotation
#write.csv(pc95rot,"pc95rot.csv")
#biplot(pc95, cex = .5)



fa90 <- fa(cor(data90), nfactors =5, rotate = "varimax", fm = "pa")
summary(fa90)
fa90
fa90_sort <- fa.sort(fa90)
load90 <- fa90_sort$loadings
write.csv(load90,"FAload90_pa1.csv")
#fa95$e.values
#fa95$weights
#plot(fa95)
#Variable Reduction 1
#1 PURCHASES - Related with Cash Adv  and Oneoff purchase  and loaded in 3 factors above .5
#2. TENURE - No sigficant loading in any of 6 factors
#3 BALANCE - Loaded in 3 factors significantly above .5
#4. PURCHASES_TRX - Loaded in 2 factors above .5

vars <- c("PURCHASES","TENURE", "BALANCE", "PURCHASES_TRX")
data90[vars] <- NULL

pc90 <- prcomp(data90, scale. = T) #PCA of data capped at 90th Percentile
summary(pc90) #2 Total 4 components eigen value greater than 1 and 70.5% Variance explained

fa90 <- fa(cor(data90), nfactors =4, rotate = "varimax", fm = "pa")
fa90_sort <- fa.sort(fa90)
load90 <- fa90_sort$loadings
write.csv(load90,"FAload90_pa2.csv")


summary(data95)
#PERFORM FACTOR ANALYSIS AGAIN

#-----------------------------------------------------------------------------------
##################### K Mean - Clustering
#=---------------------------------------------------------------------------------------

dat90_final <- scale(data90)##standardizing the data


#building clusters using k-means clustering 
cluster_three <- kmeans(dat90_final,3)
cluster_four <- kmeans(dat90_final,4)
cluster_five <- kmeans(dat90_final,5)
cluster_six <- kmeans(dat90_final,6)

data90New<-cbind(data90,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,
                 km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster)
View(data90New)

#Graph based on k-means 
require(cluster)

clusplot(dat90_final, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2, # Labels clusters and cases
         col.p = cluster_five$cluster,
         cex=.5
)

#library(gmodels)
#with(data95New, CrossTable(km_clust_3,km_clust_4,km_clust_5,km_clust_6))

#data95New%>%
# group_by(km_clust_3)%>%
#summarise_all(mean)

segment <- with(data90New, c(table(km_clust_3)/length(km_clust_3),
                             table(km_clust_4)/length(km_clust_3),
                             table(km_clust_5)/length(km_clust_3),
                             table(km_clust_6)/length(km_clust_3)))#segemnt proportion
length(segment)#18


#?clusplot

names(data90New)
data90Profile <- rbind(aggregate(.~km_clust_3, data = data90New, mean), 
                       aggregate(.~km_clust_4, data = data90New, mean),
                       aggregate(.~km_clust_5, data = data90New, mean),
                       aggregate(.~km_clust_6, data = data90New, mean))

rownames(data90Profile) <- c("KM3_1" ,"KM3_2" ,"KM3_3", 
                             "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,
                             "KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", 
                             "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6")
data90Profile[,c("km_clust_3","km_clust_4", "km_clust_5", "km_clust_6")] <- NULL

#data95Profile <- NULL
dim(data90Profile)

#colnames(data95)
#dim(data95Profile)
#View(data95New)  
#colnames(data95Profile)

overallAvg <-  colMeans(data90)#Overall Average of each variable
length(overallAvg)#20



View(data95Profile)
data90Profile <- cbind(data90Profile, Segment = segment)
data90Profile <- rbind(data90Profile, Overall = overallAvg)
data90Profile[nrow(data90Profile), ncol(data90Profile)] <- NA#removing coercion
data90Profile <- t(data90Profile)

#str(data95Profile)
#head(data95Profile, 24)
#tail(data95Profile)
#library(dplyr)
data95New%>%
  group_by(km_clust_3)%>%
  summarise_all(mean)%>%
  #summarise_each(funs(mean), km_clust_3 )%>%
  str()



write.csv(data90Profile, "Profiling_90.csv")


#------------------------------------------------------------
#Km_clust_5 is selected as optimum 
#Visualizing km_clust_5 with original data set
data95NewClust <- cbind(dataNew, km_clust_5 = cluster_five$cluster)


segment <- with(data95NewClust, c(table(km_clust_5)/length(km_clust_5)))
                            #segemnt proportion
length(segment)#18





dat95 <- aggregate(.~km_clust_5, data = data95NewClust, mean)
                       

rownames(dat95) <- c( "KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5")
                             
#data90Profile[,c("km_clust_3","km_clust_4", "km_clust_5", "km_clust_6")] <- NULL

#data95Profile <- NULL
dim(dat95)

#colnames(data95)
#dim(data95Profile)
#View(data95New)  
#colnames(data95Profile)

overallAvg <-  colMeans(dataNew)#Overall Average of each variable
length(overallAvg)#20



View(dat95)
dat95<- cbind(dat95, Segment = segment)
dat95 <- rbind(dat95, Overall = overallAvg)
dat95[nrow(dat95), ncol(dat95)] <- NA#removing coercion
dat95 <- t(dat95)

#str(data95Profile)
#head(data95Profile, 24)
#tail(data95Profile)
#library(dplyr)

pur <-dataNew$CASH_ADVANCE+dataNew$PURCHASES
plot(pur, dataNew$PAYMENTS)

write.csv(dat95, "RAw DATA Profiling.csv")
