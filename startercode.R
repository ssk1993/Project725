
#Libraries for the project
library(tidyverse)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(knitr)
library(foreign)
library(ggcorrplot)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(glmnet)
library(psych)
library(xgboost)
library(ggthemes)


#setting up a working directory
setwd("C:/Users/sugan/Desktop/725/project/auction")

#loading a dataset

df <- read.dta("ebaydatafinal.dta")

#dimensions of dataset

dim(df)

#looking at first 11 variables
str(df[,c(1:10, 548)])

#summary for the highest bid
summary(df$biddy1)

#missing values in each column
colSums(is.na(df))

#numeric variables
numericVars <- which(sapply(df, is.numeric)) 
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

#char/string variables
Charcol <- which(sapply(df, is.character))
CharVarNames <- names(Charcol)
cat('There are', length(CharVarNames), 'numeric variables')

#Density of the biddy1 (I guess its the revenue in the dataset)

p1 <-ggplot(data=df[!is.na(df$biddy1),], aes(x= biddy1))+
  geom_histogram(fill="blue", binwidth = 1500)+
  ggtitle('Highest bid density ') 

p1

#Lets try limiting the bid values 
df2 <- df[df$biddy1 < 50000 ,]
p2 <-ggplot(data=df2[!is.na(df2$biddy1),], aes(x= biddy1))+
  geom_histogram(fill="blue", binwidth = 1500)+
  ggtitle('Highest bid density ') + theme_stata()
p2






