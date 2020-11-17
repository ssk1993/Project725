
rm(list = ls(all = TRUE))

#loading up libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(glmnet)
library(knitr)
library(caret)
library(Metrics)
library(foreign)
library(lubridate)
library(olsrr)#package for backward selection 
library(corrplot)


#setting up work directory 
setwd("C:/Users/sugan/Desktop/725/project/auction")

#loading a dataset

df2 <- read.dta("ebaydatafinal.dta")

#dimension of dataframe
dim(df2)

#renaming the logbid to revenue 
names(df2)[names(df2) == "logbid1"] <- "revenue"


#summary of dependent variable
summary(df2$revenue)

#dropping the missing values in dependent variable
df2=df2[!is.na(df2$revenue),]

#checking columnwise null values 
colSums(is.na(df2))

#keeping columns which have no null values

df2<- df2[ , colSums(is.na(df2)) == 0]

#summary of dependent variable 
summary(df2$revenue)

#check if revenue has any null values 
sum(is.na(df2$revenue))

#charachter variables in dataset 
Charcol <- names(df2[,sapply(df2, is.character)])
cat('There are', length(Charcol), 'remaining columns with character values')

#factorising the maker variable
df2$maker <- as.factor(df2$maker)
table(df2$maker)

#factorising the interior variable
df2$interior <- as.factor(df2$interior)
table(df2$interior)

#factorising the exterior variable
df2$exterior <- as.factor(df2$exterior)
table(df2$exterior)


#factorising doors
df2$doors <- as.factor(df2$doors)

#factorising cylinders
df2$cyl <- as.factor(df2$cyl)

#dealing with date variables

#converting strings into date format 
df2$startdate <- parse_date_time(df2$startdate, orders="mdy HMS")
df2$enddate <- parse_date_time(df2$enddate, orders="mdy HMS")

#extracting months and day from dates 
df2$months <-  month(df2$startdate)
df2$days <- day(df2$startdate)
df2$monthe <-  month(df2$enddate)
df2$daye <- day(df2$enddate)

#factorising month and dates 

df2$months <- as.factor(df2$months)
df2$days <- as.factor(df2$days)
df2$monthe <- as.factor(df2$monthe)
df2$daye <- as.factor(df2$daye)
df2$week <- as.factor(df2$week)


#checking variables and their association with dependent variable



#density of revenue
d <- density(df2$revenue) # returns the density data
plot(d)

#density of number of bids
df2 <- df2[df2$numbids < 111 ,]
d2 <- density(df2$numbids) # returns the density data
plot(d2)



#finding highly correlated numeric variables 
#getting names of numeric variables 
numvar<- names(df2[,sapply(df2, is.numeric)])

#creating dataset of only numerical variables
df_numVar <- df2[, numvar]

#pairwise correlation 
cor_numVar <- cor(df_numVar, use="pairwise.complete.obs")

#sorting variables according to their correlation with dependent variable
cor_sorted <- as.matrix(sort(cor_numVar[,'revenue'], decreasing = TRUE))

#highly correlated variables
head(cor_sorted , 50)

#negatively correlated variables
tail(cor_sorted , 50)

#keeping highly correlated variables only 

CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.2)))

#matrix of high correlated variables 
cor_numVar <- cor_numVar[CorHigh, CorHigh]

#correlation plot 
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#keeping high ocrrelated variables only in dataset 
df2 <- df2[,CorHigh]


# backward selection of variables 

alias( lm(revenue ~ ., data = df2) )

#removing aliases
df2<-df2[, names(df2)!='biddy1']
df2<-df2[, names(df2)!='reserve']
df2<-df2[, names(df2)!='html']
df2<-df2[, names(df2)!='_merge']


model <- lm(revenue ~ ., data = df2)
k <- ols_step_backward_p(model, details = TRUE)
#plot(k)



#Creating train and test dataset
set.seed(0)
sample_size = floor(0.5*nrow(df2))


# randomly split data in r
picked = sample(seq_len(nrow(df2)),size = sample_size)
train =df2[picked,]
test = df2[-picked,]

dim(train)
dim(test)



#lasso and ridge MSE
#credit to creg 
cv_loop =function(data, a) {
  folds = 10
  foldindtest <- seq(1,nrow(train),length.out=folds+1)
  foldind <- floor(seq(1,nrow(train),length.out=folds+1))
  
  lambdas <- seq(0,1,by = .001)
  mse_totals <- rep(0, times = length(lambdas))
  
  #lasso
  a <- 1 
  for (i in 1:folds){
    
    # We train the data on everything BUT what's in our fold,
    # and test on what is in the fold
    start <- foldind[i]
    end <- foldind[i+1]
    foldtest <- start:end
    
    # divide up into our (K-1/K)*N to train, and N/K to test
    f_traindata <- train[-foldtest,]
    f_testdata <- train[foldtest,]
    
    #' remember that GLMNet has issues with data tables.
    # f_train_x <- as.matrix(f_traindata[,temp := NULL]) -> will remove a column in place!
    f_train_x <- data.matrix(f_traindata[,2:15])
    f_train_y <- data.matrix(f_traindata[,"revenue"])
    
    f_test_x <- data.matrix(f_testdata[,2:15])
    f_test_y <- data.matrix(f_testdata[,"revenue"])
    
    # regress with ridge/lasso
    fit = glmnet(f_train_x,f_train_y,alpha=a,lambda=lambdas)
    
    # track the MSE differences in each lambda
    #' Don't forget to cite me if you use this loop.
    for (j in 1:length(lambdas)){
      mse_totals[j] = mse_totals[j] + sum((predict.glmnet(fit,f_test_x,s=lambdas[j]) - f_test_y)^2)
    }
  }
  # choose the lambda that returns minimum MSE
  return(lambdas[which.min(mse_totals)])
}

ridge_min <-cv_loop(train, a = 0)
final_ridge <-glmnet(data.matrix(test[,2:15]), test$revenue,alpha = 0, lambda = ridge_min)
mse_ridge = mean((predict(final_ridge,data.matrix(test[,2:15]),s=ridge_min) - test$revenue)^2)

lasso_min <-cv_loop(train, a = 1)
final_lasso <-glmnet(as.matrix(test[,2:15]), test$revenue,alpha = 1, lambda = lasso_min)

mse_lasso = mean((predict(final_lasso,data.matrix(test[,2:15]),s=lasso_min) - test$revenue)^2)


print(paste("Ridge MSE with 50% data and without noise", mse_ridge))
print(paste("Lasso MSE with 50% data and without noise", mse_lasso))



