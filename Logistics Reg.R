#Perform a logistic regression on the binary dataset
str(binary)

#convert from integer to factor
binary$admit <- as.factor(binary$admit)
binary$rank <- as.factor(binary$rank)


#check  if there are NA values in the dataset
xtabs(~admit + rank, data = binary)


#xtabs() is the numerical version of barchartGC(). You use it when you want to study
#the distribution of one factor variable;
#the relationship between two factor variables.


#partition dataset
library(caret)
library(caTools)

sample_ <- sample.split(binary, SplitRatio = 0.80)
train <- subset(binary, sample_ == T)
test <- subset(binary, sample_ == F)

#build logistic regression model
mymodel <- glm(admit~., family = "binomial", data = train)


summary(mymodel)
#the nmore stars we have the more significant the values are
#remove gre and remake model
mymodel1 <- glm(admit~ gpa + rank, data = train, family = "binomial")

summary(mymodel1)

#create prediction
pred <- predict(mymodel1, train, type = "response")
head(pred)
p1 <- ifelse(pred > 0.5, 1, 0)
head(p1)
head(train)

#create matrix table
tab1 <-table(p1, train$admit)

#calculate for prediction values manually
y <- -4.0376 + (1.1318*3.61) + (1 *-1.0951 )
exp(y)/(exp(y) + 1)

#misclassification error
1-sum(diag(tab1)/sum(tab1))

#create prediction for test
pred2 <- predict(mymodel1, test, type = "response")
p2 <- ifelse(pred2 > 0.5,1,0)


#misclassification error
tab1 <- table(p2, test$admit)


#Goodness of fit test

#This is the second part, where we aim to implement a logreg model on the dividend dataset
library(tidyverse)
#laod in dividend dataset
dividend <- "https://github.com/MGCodesandStats/statsmodels-sklearn-linear-regression/raw/master/logistic%20r/dividendinfo.csv" %>% 
  read.csv()


#examine structure of dataset
str(dividend)


#convert dividend  column to factor class
dividend$dividend <- as.factor(dividend$dividend)



#partition dataset?
mymodel_ <- lm(data = dividend)
mymodel <- glm(data = dividend, dividend~., family = "binomial")

#check summary of model
summary(mymodel)

#predict
pred1 <- predict(mymodel, dividend, type = "response")
p1 <- ifelse(pred1 > 0.5, 1, 0)
table(p1, dividend$dividend)


