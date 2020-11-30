#setting up directory and reading the dataset 
rm(list = ls(all = TRUE))

#libraries

#loading up libraries
library(data.table)
library(plyr)
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
library(knitr)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)


setwd("C:/Users/sugan/Desktop/725/project/auction")
df <- read.csv("AuctionData.csv", na.strings = ".")

#deleting unnecessory columns as we have used group variables for dent , ding
#rust ,scratch , crack ,broken,  problem etc. 
df <- df[-c(80:415)]

df <- df[-c(213:261)]

#deleting bidder names 

df <- df[!grepl("^biddername",names(df))]

dim(df)
summary(df$biddy1)

#deleting missing values from highest bid
df=df[!is.na(df$biddy1),]

names(df)[names(df) == "logbid1"] <- "revenue"

## Dealing with date variables
#converting strings into date format 
df$startdate <- parse_date_time(df$startdate, orders="mdy HMS")
df$enddate <- parse_date_time(df$enddate, orders="mdy HMS")
df$membersince <- parse_date_time(df$membersince, orders="mdy")
#extracting months and day from dates 
df$months <-  month(df$startdate)
df$days <- day(df$startdate)
df$monthe <-  month(df$enddate)
df$daye <- day(df$enddate)

#extracting year from member since 
df$memberyr <- year(df$membersince)

# #factorising month and dates and year 
# df$months <- as.factor(df$months)
# df$days <- as.factor(df$days)
# df$monthe <- as.factor(df$monthe)
# df$daye <- as.factor(df$daye)
# df$week <- as.factor(df$week)
# df$memberyr <- as.factor(df$memberyr)

#deleting the original date vars 
drop <- c("membersince","startdate","enddate")
df = df[,!(names(df) %in% drop)]

#null values
colSums(is.na(df))
#deleting columns with high missing values
df<- df[ , colSums(is.na(df)) < 16000 ,]


df <- na.omit(df)

#creating factor variable from some numeric variables 
unique(df$cyl)
df$cyl <- as.factor(df$cyl)
unique(df$doors)
df$doors <- as.factor(df$doors)
unique(df$problem_group)
df$ding_group <- as.factor(df$ding_group)
df$scratch_group <- as.factor(df$scratch_group)
df$crack_group <- as.factor(df$crack_group)
df$broken_group <- as.factor(df$broken_group)
df$dent_group <- as.factor(df$dent_group)
df$problem_group <- as.factor(df$problem_group)
df$rust_group <- as.factor(df$rust_group)

#dealing with factor variables 

factnames <- names(df[,sapply(df, is.factor)])
factdf <- df[,factnames]
dim(factdf)
names(factdf)


#webpage binary
factdf$webpage <- ifelse(factdf$webpage=="ebayhosting",1,0)

#Changing interior and exterior variables to be 1 if a 'normal' color and 0 otherwise.

extnormcolor <- list("Black", "Blue", "Brown", "Green",
                     "Orange", "Silver", "Tan", "Yellow",
                     "Burgundy","Gold","Gray","Red", "White")

factdf$exterior <- ifelse(factdf$exterior %in% extnormcolor,1,0)

intnormcolor <- list("Black","Blue","Brown","Gray",
                     "Gray","Red","Tan","White")

factdf$interior <- ifelse(factdf$interior %in% intnormcolor,1,0)

#creating big three 

big3 <- list("Chevrolet","Dodge","Ford")
factdf$BigThree <- ifelse(factdf$maker %in% big3, 1,0)

#sports car and others
sportscar <- list("Camaro","Corvette","Mustang","Thunderbird")
midsize <- list("Civic","Accord","Corolla","Camry","Maxima","Altima","3-Series")
truck <- list("F-250","F-150","Tacoma","F-350","Silverado 1500","Silverado 2500","F-100")

factdf$sportscar <- ifelse(factdf$model %in% sportscar,1,0)
factdf$midsize <- ifelse(factdf$model %in% midsize,1,0)
factdf$truck <- ifelse(factdf$model %in% truck,1,0)


#deleting maker and model 
drop <- c("maker","model","name","highbiddername","sellername","location","caradphotos")
factdf = factdf[,!(names(factdf) %in% drop)]





#dealing with numeric variables
numnames <- names(df[,sapply(df, is.numeric)])
numdf <- df[,numnames]




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
drop <- c("html","text","miles","age","age2","itemnum")
numdf = numdf[,!(names(numdf) %in% drop)]

#Finding aliases and deleting them 
alias( lm(revenue ~ ., data = numdf) )

#dropping columns which are aliases 
drop <- c("startingdate","year","endingdate","sellerborn","biddy1","reserve","X_merge")
numdf = numdf[,!(names(numdf) %in% drop)]
# 
# 
# numdf <- numdf[numdf$photos!= "-Inf" ,]
# numdf <- numdf[numdf$highbidderfdback!= "-Inf" ,]


#finding skew and normalisation 





names(numdf)

# 
# 
# for(i in 1:ncol(numdf)){
#   if (abs(skew(numdf[,i]))>0.8){
#     numdf[,i] <- log(numdf[,i] +1)
#   }
# }
# 
# #normalising the data
# PreNum <- preProcess(numdf, method=c("center", "scale"))
# print(PreNum)
# 
# 
# DFnorm <- predict(PreNum, numdf)
# dim(DFnorm)
# 

#hot encoding for factor variables
library(mltools)
factdf <- one_hot(as.data.table(factdf))



#cbind 

finaldata <- cbind(numdf, factdf)

finaldata <- na.omit(finaldata)

names(finaldata)

finaldata<-finaldata[finaldata$sell == 1,]


set.seed(0)
sample_size = floor(0.5*nrow(finaldata))


# randomly split data in r
picked = sample(seq_len(nrow(finaldata)),size = sample_size)
train = finaldata[picked,]
test = finaldata[-picked,]

dim(train)
dim(test)

trainx = train[,!(names(train) %in% "revenue")]
trainy = train[,(names(train) %in% "revenue")]

testx = test[,!(names(test) %in% "revenue")]
testy = test[,(names(test) %in% "revenue")]


set.seed(2020)
my_control <-trainControl(method="cv", number=10)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=trainx, y=trainy, method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune

min(lasso_mod$results$RMSE)
max(lasso_mod$results$Rsquared)


lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')

LassoPred <- predict(lasso_mod, testx)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)

#making revenue as first column
finaldata<- finaldata %>% select(revenue, everything())

set.seed(0)
sample_size = floor(0.5*nrow(finaldata))


# randomly split data in r
picked = sample(seq_len(nrow(finaldata)),size = sample_size)
train = finaldata[picked,]
test = finaldata[-picked,]



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
    f_train_x <- data.matrix(f_traindata[,2:138])
    f_train_y <- data.matrix(f_traindata[,"revenue"])
    
    f_test_x <- data.matrix(f_testdata[,2:138])
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
final_ridge <-glmnet(data.matrix(test[,2:138]), test$revenue,alpha = 0, lambda = ridge_min)
mse_ridge = mean((predict(final_ridge,data.matrix(test[,2:138]),s=ridge_min) - test$revenue)^2)

lasso_min <-cv_loop(train, a = 1)
final_lasso <-glmnet(as.matrix(test[,2:138]), test$revenue,alpha = 1, lambda = lasso_min)

mse_lasso = mean((predict(final_lasso,data.matrix(test[,2:138]),s=lasso_min) - test$revenue)^2)


print(paste("Ridge MSE is", mse_ridge))
print(paste("Lasso MSE is", mse_lasso))




#lasso using ps code

lassoVarImp<-varImp(final_lasso, lambda = lasso_min)

lassoImportance <- lassoVarImp$Overall

varsSelected <- length(which(lassoImportance!=0))
varsNotSelected <- length(which(lassoImportance==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')


#deleting columns which has 0 importance 
#dropping columns which are aliases 


names(finaldata) <- make.names(names(finaldata))

set.seed(0)
sample_size = floor(0.5*nrow(finaldata))


# randomly split data in r
picked = sample(seq_len(nrow(finaldata)),size = sample_size)
train = finaldata[picked,]
test = finaldata[-picked,]







library(randomForest)
require(caTools)
#wait for 20 minutes to run this 
model1 <- randomForest(revenue ~ ., data = train,ntree = 100, importance = TRUE)
model1
plot(model1)


imp_RF<-importance(model1)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")





varImpPlot(model1)
rfmse <- mean((test$revenue - predict(model1, newdata=test))^2)

print(rfmse)
