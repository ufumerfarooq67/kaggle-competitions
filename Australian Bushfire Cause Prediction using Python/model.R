library(tidyverse)
library(tidymodels)
library(caret)
library(randomForest)
library(rpart)

################################################################################

# Reading the training and testing dataset
train <- read.csv("student_training_release.csv")
test <- read.csv("student_predict_x_release.csv")

#1
head(train)

#2
summary(train)

#3 
dim(train)
dim(test)

#4 
str(test)

colnames(train)

# Function to remove NA values.
removeNAValues <- function(dataset,ignore) 
{
  ignore
  dataset[c(ignore)] <- as.numeric(unlist(dataset[c(ignore)]))
  
  columns_null <- dataset[,colSums(is.na(dataset)) > 0]
  
  names <- colnames(columns_null)
  
  
  for(name in names)
  {
    
    if(class(unlist(columns_null[name])) == "character")
    {
      columns_null[,name] <- columns_null[,name] %>% 
        replace_na(tail(names(sort(table(columns_null[,name]))), 1)) 
    }
    
    else
    {
      
      columns_null[,name] <- columns_null[,name] %>% 
        replace_na(mean(columns_null[,name],na.rm=TRUE))
    }
    
  }
  
  dataset[,colnames(columns_null)] <- columns_null
  
  return (dataset)
  
}

# Function to select highly correlated features
highlyCorrelatedfeaturesMethod <- function(dataset)
{
  
  corr <- cor(dataset)
  correlation_salePrice <-corr[,"cause"]
  
  correlation_salePrice <- data.frame(t(correlation_salePrice))
  dim(correlation_salePrice)
  
  correlation_salePrice <- correlation_salePrice[,colSums(correlation_salePrice) > 0.1]
  correlation_salePrice <- data.frame(t(correlation_salePrice))
  dim(correlation_salePrice)
  strong_relation_names <- rownames(correlation_salePrice)
  strong_relation_names
  
  
  return (strong_relation_names[-19])
}


######################################################################################
# Data Cleaning
#####################################################################################

# Removing NA values from train and convert to numeric except character and factor
ignore <- c(-1,-5,-6,-56)
train <- removeNAValues(train,ignore)


# Remove NA values from test and convert to numeric except character and factor
ignore <- c(-4,-6,-7)
test <- removeNAValues(test,ignore)

# Converting label to factor and delecting date column
train <- train %>% mutate(cause = factor(cause))
train <- train %>% mutate(FOR_TYPE = factor(FOR_TYPE))
train <- train %>% mutate(FOR_CAT = factor(FOR_CAT))
train["FOR_TYPE"]<-as.numeric(train$FOR_TYPE)
train["FOR_CAT"]<-as.numeric(train$FOR_CAT)
train <- train[,-1]
str(train)

# Converting label to factor and deleting date column
test <- test %>% mutate(FOR_TYPE = factor(FOR_TYPE))
test <- test %>% mutate(FOR_CAT = factor(FOR_CAT))
test["FOR_TYPE"]<-as.numeric(test$FOR_TYPE)
test["FOR_CAT"]<-as.numeric(test$FOR_CAT)
test <- test[,-4]
str(test)


#####################################################################################
# Model 1:
# Random Forest (0.7176 better than provided model.)
#####################################################################################

set.seed(3000)
split <- initial_split(train, 3/4, strata = "cause")
fires_tr <- training(split)
fires_ts <- testing(split)

colnames(fires_tr)
model <- rand_forest(mtry=10) %>%
  set_engine("randomForest",importance=TRUE, proximity=TRUE) %>%
  set_mode("classification") %>%
  fit(cause ~
      dist_road +
      dist_cfa +
      lon +
      dist_camp +
      month + lat +
      amaxt720 +
      ws +
      amint14 +
      ase180,
    data=fires_tr)

model


# Make predictions and saving to csv file
predictions <- test %>%
  mutate(cause_p = predict(model, test)$.pred_class) %>%
  select(id, cause_p) %>%
  rename(Id = id, Category = cause_p)

write_csv(predictions, file="predictions_2021-05-04.csv")



#####################################################################################
# Model 2
# Decision Tree (Highly COrrelated features) (0.67 : Low than random forest model)
# and improved version)
#####################################################################################

backupTrain <- train
backupTest <- test[,c(-1)]

dim(backupTrain)
dim(backupTest)


# Let's convert characters to numeric
temp <- backupTrain
temp["cause"]<-as.numeric(temp$cause)
features <- highlyCorrelatedfeaturesMethod(temp)
features

backupTrain <- backupTrain[,c(features,"cause")]

model <- rpart(cause~., data = backupTrain, method = 'class')


# Make predictions and saving to csv file
predictions <- test %>%
  mutate(cause_p = predict(model, test,type='class')) %>%
  select(id, cause_p) %>%
  rename(Id = id, Category = cause_p)

write_csv(predictions, file="predictions_2021-05-04.csv")

