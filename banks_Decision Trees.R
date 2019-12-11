#Bank decision tree
#https://www.kaggle.com/rouseguy/bankbalanced/downloads/bank.csv/1

#Load in datasets

str(bank)

#It appears dataset classes are correct after checking classes
#Load in the labels 
labels <- read.table("labels.txt", sep = ":")

library(tidyverse)

#Range for age
ggplot(data = bank, aes(x = "", y = age)) +
  geom_boxplot(fill = "gold") + coord_flip() +
  ggtitle("Age Range") + xlab("Age") + ylab("Range")


##use a histogram
ggplot(data = bank, aes(age)) +
  geom_histogram(bins = 100)


#examine avereage  duration of 
ggplot(data = bank, aes(x = "", y = duration)) +
  geom_boxplot(fill = "gold") + coord_flip() +
  xlab("Age") + ylab("Count") +
  ggtitle("Distribution of Ages")

##
ggplot(data = bank, aes(duration)) +
  geom_histogram(bins = 100, fill = "darkgray")+
  ggtitle("Distribution of Ages")


#Examine level of education that being used most
bank %>% 
  select(education) %>% 
  group_by(education) %>% 
  count() #Apparently secondary school leavers take more loans followed by tertiary


#examine roles
most_roles <- bank %>% 
  select(job) %>% 
  group_by(job) %>% 
  count()

ggplot(data = most_roles, aes(x = job, y = n)) +
  geom_bar(stat = "identity") + coord_flip()


#Examine people who made deposits (yes) versus job category
yes_vs_cat <- bank %>% 
  select(job, deposit) %>% 
  filter(deposit == "yes") %>% 
  group_by(deposit, job) %>% 
  count(deposit)

ggplot(data = yes_vs_cat, aes(x = reorder(job, n), y = n, fill = n)) +
  geom_bar(stat = "identity") + coord_flip()



##Poutcome 
bank %>% 
  select(poutcome) %>% 
  group_by(poutcome) %>% 
  count() #%>% 
  geom_bar(aes(x = poutcome, y = n), stat = "identity")

#combine unknown and other as they really do not seem different
bank$poutcome <- gsub("other", "unknown", bank$poutcome)

#examaine if all strings of other have been replaced with unknown
bank %>% 
  select(poutcome) %>% 
  group_by(poutcome) %>% 
  count() 

#remap values for default yes = 1, no = 0
library(plyr) #use revalue for plyr
#mapvalues(x, from = c("beta", "gamma"), to = c("two", "three"))
#map values can also be used as seen above 

bank$default_cat <-  revalue(bank$default, c("yes" =1, "no"= 0))

#remap for housing
bank$housing_cat <- revalue(bank$housing, c("yes" =1, "no"= 0))


#remap values for deposit
bank$deposit_cat <- revalue(bank$deposit, c("yes" =1, "no" = 0))

#remove contact, day and month etc
bank_1 <- bank %>% 
  select(-day, -month, -contact, -housing, -default, -deposit)



# pdays: number of days that passed by after the client was last contacted from a previous campaign
#-1 means client was not previously contacted
# Map padys=-1 into a large value (10000 is used) to indicate that it is so far 
#in the past that it has no effect
bank_1$recent_pdays <- gsub(-1, 10000, bank_1$pdays)


#remove pdays
bank_1 <<- bank_1 %>% 
  select(-pdays)


bank_2 <- bank_1 %>% mutate(edu_sec = 
                    case_when(education == "secondary" ~ 1,
                                                  TRUE ~ 0)) %>% 
           mutate(edu_ter =
                    case_when(education == "tertiary" ~ 1,
                              TRUE ~ 0)) %>% 
           mutate(edu_pri =
                    case_when(education == "primary"  ~ 1,
                              TRUE ~ 0)) %>% 
          mutate(edu_unk =
                   case_when(education == "unknown" ~ 1,
                             TRUE ~ 0)) %>% 
           mutate(mari_div =
                    case_when(marital == "divorced" ~ 1,
                              TRUE ~ 0)) %>% 
           mutate(mari_mari =
                    case_when(marital == "married" ~ 1, 
                              TRUE ~ 0)) %>% 
          mutate(mari_sing =
                   case_when(marital == "single" ~ 1,
                               TRUE ~ 0)) %>% 
          mutate(pout_unk =
                   case_when(poutcome == "unknown" ~ 1,
                             TRUE ~ 0)) %>% 
          mutate(pout_succ =
                   case_when(poutcome == "success" ~ 1,
                             TRUE ~ 0)) %>% 
          mutate(pout_fail =
                   case_when(poutcome == "failure" ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(loan_cat = 
           case_when(loan == "yes" ~ 1,
                     TRUE ~ 0)) %>% 
  select(-poutcome, -marital, -education, -job, -loan)
   

summary(bank_2)

#sactter plot of age and balance
ggplot(data = bank_2, aes(age, balance)) +
  geom_point(col = "blue")

#historgram of duration and poutcome
ggplot(data = bank_2, aes(pout_succ, duration)) +
  geom_histogram(stat = "identity", bins = 5000)



#In order to determine correlation between heterogeneous variables, 
#you can use hetcor() function from the polycor
#install.packages("polycor", method = "pearson")
library(polycor)

#convert character column of recent_pdays to class numeric 
bank_2$recent_pdays <- as.numeric(bank_2$recent_pdays)

###perform correlation
?hetcor
hetcor(bank_2, std.err = F)


# People signed up to a term deposite having a personal loan (loan_cat) and housing loan (housing_cat)
bank_2 %>% 
  filter(deposit_cat == 1, loan_cat == 1, housing_cat == 1) %>% 
  nrow()


# People signed up to a term deposite with a credit default 
bank_2 %>% 
  filter(deposit_cat == 1, default_cat == 1) %>% 
  nrow()




# Bar chart of job Vs deposite
ggplot(data = bank_1, aes(job, deposit_cat, fill = job)) +
  geom_bar(stat = "identity") + coord_flip()


# Bar chart of "previous outcome" Vs "call duration"
ggplot(data = bank_1, aes(poutcome, duration, fill = poutcome)) +
  geom_bar(stat = "identity")

#Build decision tree
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(party)
library(caret)

#create test and train data sets

sample_ <- sample.split(bank_2, SplitRatio = 0.75)
train <- subset(bank_2, sample_ == T)
test <- subset(bank_2, sample_ == F)

tree <- rpart(data = train, deposit_cat~.)
rpart.plot(tree, extra = 2)


#predict for train
prediction <- predict(tree, train, type = "class")

tab <- table(prediction, train$deposit_cat)
tab

confusionMatrix(prediction, train$deposit_cat)

#Accuracy : 0.791 for train data set

#for test
prediction_1 <- predict(tree, test, type = "class")
confusionMatrix(prediction_1, test$deposit_cat)

#Accuracy : 0.8054 for test dataset

