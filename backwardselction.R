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
df <- read.dta("ebaydatafinal.dta")
#dimension of dataframe
dim(df)
summary(df$biddy1)

#deleting null values from highest bid
df=df[!is.na(df$biddy1),]
#dimension of dataframe
dim(df)
summary(df$bookvalue)


#keeping columns which have no null values
df<- df[ , colSums(is.na(df)) == 0]

#renaming the logbid to revenue 
names(df)[names(df) == "logbid1"] <- "revenue"
summary(df$revenue)


#deleting unnecessary columns

df = df[,!grepl("^ding_",names(df))]
df = df[,!grepl("^scratch_",names(df))]
df = df[,!grepl("^dent_",names(df))]
df = df[,!grepl("^broken_",names(df))]
df = df[,!grepl("^crack_",names(df))]
df = df[,!grepl("^problem_",names(df))]
df = df[,!grepl("^rust_",names(df))]
df<-df[, names(df)!='_merge']
df<-df[, names(df)!='itemnum']

## Dealing with date variables
#converting strings into date format 
df$startdate <- parse_date_time(df$startdate, orders="mdy HMS")
df$enddate <- parse_date_time(df$enddate, orders="mdy HMS")

#extracting months and day from dates 
df$months <-  month(df$startdate)
df$days <- day(df$startdate)
df$monthe <-  month(df$enddate)
df$daye <- day(df$enddate)

#factorising month and dates 
df$months <- as.factor(df$months)
df$days <- as.factor(df$days)
df$monthe <- as.factor(df$monthe)
df$daye <- as.factor(df$daye)
df$week <- as.factor(df$week)


## Charachter Variables

charnames <- names(df[,sapply(df, is.character)])

chardf <- df[,charnames]

dim(chardf)

names(chardf)

#dropping name variables in original dataset
df = df[,!grepl("^name",names(df))]
df = df[,!grepl("^bidder",names(df))]
df<-df[, names(df)!='highbiddername']
df<-df[, names(df)!='sellername']
df<-df[, names(df)!='vin']
df<-df[, names(df)!='caradphotos']

#deleting date variables
df<-df[, names(df)!='membersince']
df<-df[, names(df)!='enddate']
df<-df[, names(df)!='startdate']

#factorising charachter variables
df$maker <- as.factor(df$maker)
df$interior <- as.factor(df$interior)
df$exterior <- as.factor(df$exterior)
df$software <- as.factor(df$software)


#factorising variables
df$doors <- as.factor(df$doors)
df$cyl <- as.factor(df$cyl)
df<-df[, names(df)!='biddy1']


#correlation
numnames <- names(df[,sapply(df, is.numeric)])
numdf <- df[,numnames]
dim(numdf)
names(numdf)




#pairwise correlation 
cor_numVar <- cor(numdf, use="pairwise.complete.obs")

cor_sorted <- as.matrix(sort(cor_numVar[,'revenue'], decreasing = TRUE))


#highly correlated variables 

CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))

#matrix of high correlated variables 
cor_numVar <- cor_numVar[CorHigh, CorHigh]

options(repr.plot.width=10, repr.plot.height=10)
corrplot.mixed(cor_numVar,lower.col = "black", tl.col="black", tl.pos = "lt", number.cex=0.65,  tl.cex=0.7)

#deleting highly correlated variables
numdf<-numdf[, names(numdf)!='html']
numdf<-numdf[, names(numdf)!='text']

df<-df[, names(df)!='html']
df<-df[, names(df)!='text']

#checking if there are any aliases

alias( lm(revenue ~ ., data = numdf) )

#there are three more aliases 
#deleting aliases


numdf<-numdf[, names(numdf)!='endingdate']
numdf<-numdf[, names(numdf)!='sellerborn']
numdf<-numdf[, names(numdf)!='startingdate']
numdf<-numdf[, names(numdf)!='endingdate']
numdf<-numdf[, names(numdf)!='reserve']



#checking aliases again 
alias( lm(revenue ~ ., data = numdf) )


##Dealing with factor variables 

## backward selection of factor variables
factnames <- names(df[,sapply(df, is.factor)])
factdf <- df[,factnames]
names(factdf)

unique(factdf$maker)
## I havent included factor variables in backward selection yet.

##Backward selection for numerical variables 

set.seed(0)
sample_size = floor(0.5*nrow(df))


# randomly split data in r
picked = sample(seq_len(nrow(numdf)),size = sample_size)
train = numdf[picked,]
test = numdf[-picked,]

dim(train)
dim(test)

library(leaps)
regfit.full=regsubsets(revenue ~ . , data = train,nvmax = 20, method="backward")

#I am using nvmax as 20 , you can try higher numbers or less
reg.summary=summary(regfit.full)


data.frame(ADJr2 = which.max(reg.summary$adjr2), BIC =
             which.min(reg.summary$bic), CP = which.min(reg.summary$cp))

test.mat=model.matrix(revenue ~ . ,data = test,nvmax = 20, method="backward")

val.errors = rep(NA,20)
for (i in 1:20){
  coefi = coef(regfit.full, id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((test$revenue-pred)^2)
}

# Print out the mse's 
adjr2.mse = val.errors[which.max(reg.summary$adjr2)]
bic.mse = val.errors[which.min(reg.summary$bic)]




print(paste("ADJR2 MSE :", adjr2.mse))
print(paste("BIC MSE :",bic.mse))


#which model is good?
which.min(val.errors)

#coeffecients of the final model
coef(regfit.full ,19)

#these are the variables will be used for lasso and ridge .


