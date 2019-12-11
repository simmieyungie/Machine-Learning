#Sentiment analysis on kaggle dataset

#load in kaggle data and save in new dataframe
kaggle <- training.1600000.processed.noemoticon


#We are interested in the the last column

#packages
library(tm)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(caret)
library(caTools)

#obtain partition of data for trial
kaggle_1 <- kaggle %>% 
  sample_n(size = 1000, replace = T)


#load in the tweets column
kaggle_2 <- iconv(kaggle_1$V6, to = "UTF-8")
head(kaggle_2)

#Build corpus
corpus_kaggle <- Corpus(VectorSource(kaggle_2))
inspect(corpus_kaggle[1:5])

#convert all to lowercase
corpus_kaggle <- tm_map(corpus_kaggle, tolower)
inspect(corpus_kaggle[1:5])
#remover all punctuations 
corpus_kaggle <- tm_map(corpus_kaggle, removePunctuation)
inspect(corpus_kaggle[1:5])

#remover numbers
corpus_kaggle <- tm_map(corpus_kaggle, removeNumbers)
inspect(corpus_kaggle[1:5])

#removestop words
corpus_kaggle <- tm_map(corpus_kaggle,removeWords, stopwords("english"))
inspect(corpus_kaggle[1:15])

#remove http
#create function to remove stopword
removeurl <- function(x) gsub("[http]", "", x)

#apply remove url function
cleanset_kaggle <- tm_map(corpus_kaggle, content_transformer(removeURL))
inspect(cleanset_kaggle[1:15])


#term document matrix
tdm_kaggle <- DocumentTermMatrix(cleanset_kaggle)
tdm_kaggle

#remove sparse terms
tdm_kaggle <- removeSparseTerms(tdm_kaggle, 0.996)
tdm_kaggle
inspect(tdm_kaggle)
#convert to a matrix and then dataframe
tdm_kaggle_matrix <- as.data.frame(as.matrix(tdm_kaggle))

tdm_kaggle_matrix1 <- cbind.data.frame(tdm_kaggle_matrix, label = factor(kaggle_1$V1))

#split data into training and test dataset
sample1 <- sample.split(tdm_kaggle_matrix1, SplitRatio = 0.80)
train <- subset(tdm_kaggle_matrix1, sample1 == T)
test <- subset(tdm_kaggle_matrix1, sample1 == F)


#Append _c to names of colums
colnames(train) <- paste(colnames(train), "_c", sep = "")
colnames(test) <- paste(colnames(test), "_c", sep = "")

#build ranadom forest model
library(randomForest)
model_kaggle <- randomForest(train$label_c~., data = train, 
                             mtry = 4,
                             ntree = 300,
                             proximity = T)

kaggle_pred <- predict(model_kaggle, train)
confusionMatrix(kaggle_pred, train$label_c)


kaggle_pred2 <- predict(model_kaggle, test)
confusionMatrix(kaggle_pred2, test$label_c)



# Train the model using rf
model_rf_kaggle = train(make.names(label_c)~., data=train, method='rf', tuneLength=1, trControl = fitControl)
model_rf_kaggle

kaggle_pred3 <- predict(model_rf_kaggle, test)
table(kaggle_pred3, make.names(test$label_c))





