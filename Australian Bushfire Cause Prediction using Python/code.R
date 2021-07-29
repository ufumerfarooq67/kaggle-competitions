library(tidyverse)
library(tidymodels)
library(caret)
library(randomForest)
library(FSinR)
library(ggplot2)
library(rpart)
library(nnet)
library(mlr)
library(gbm)
library(glmnet)
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

# Remove outliers and replace it with median.
replaceOutliersWithMedian <- function(x) {
  
  names <- colnames(x)
  names = names[!(names %in% "cause")]
  
  for(name in names)
  {
    if(length(boxplot(x[name])$out) != 0)
    {
      lower_bound <- median(x[,name]) - 3 * mad(x[,name], constant = 1)
      lower_bound
      
      upper_bound <- median(x[,name]) + 3 * mad(x[,name], constant = 1)
      upper_bound
      
      outlier_ind <- which(x[,name] < lower_bound | x[,name] > upper_bound)
      
      
      
      x[outlier_ind,name] <- median(x[-outlier_ind,name])
      
      
    }
  }
  
  return (x)
  
}

# Function to return the names of features other than outliers.
removeOutlierFeatures <- function(x) {
  
  names <- colnames(x)
  names = names[!(names %in% "cause")]
  selected = names
  for(name in names)
  {
    if(length(boxplot(x[name])$out) != 0)
    {
      
      #x <- x[outlier_ind,name] <- new
      selected = selected[!(selected %in% name)]
      #x[outlier_ind,name] <- median(x[-outlier_ind,name])
      
      
    }
  }
  
  return (selected)
  
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
# Decision Tree (Without columns the has outliers.) (0.2 very bad score)
#####################################################################################


temp <- train
temp["cause"]<-as.numeric(temp$cause)
features <- highlyCorrelatedfeaturesMethod(temp)
features


train <- train[,c(features)]
str(train)

set.seed(3000)
split <- initial_split(train, 3/4, strata = "cause")
fires_tr <- training(split)
fires_ts <- testing(split)


colnames(fires_tr)
model <- rand_forest(mtry=18) %>%
  set_engine("randomForest",importance=TRUE, proximity=TRUE) %>%
  set_mode("classification") %>%
  fit(cause ~.,
      data=fires_tr)

model


# Make predictions and saving to csv file
predictions <- test %>%
  mutate(cause_p = predict(model, test)$.pred_class) %>%
  select(id, cause_p) %>%
  rename(Id = id, Category = cause_p)

write_csv(predictions, file="predictions_2021-05-04.csv")

#####################################################################################
# Model 3
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

#####################################################################################
# Model 4
# Multinomial Logstic Regression (0.50 (bad))
#####################################################################################

backupTrain <- train
backupTest <- test[,c(-1)]

# Training the multinomial model
model <- multinom(cause ~ . , data = backupTrain)


# Make predictions and saving to csv file
predictions <- test %>%
  mutate(cause_p = predict(model,test,type='class')) %>%
  select(id, cause_p) %>%
  rename(Id = id, Category = cause_p)

write_csv(predictions, file="predictions_2021-05-04.csv")


#####################################################################################
# Model 5
# Multi-nomial Logistic Regression (Detecting and changing Outliers with median) 
# (0.54 : Still not better than 0.67 : Decision Tree)
#####################################################################################

backupTrain <- train
backupTest <- test[,c(-1)]

backupTrain <- replaceOutliersWithMedian(backupTrain)
dim(backupTrain)

backupTest <- replaceOutliersWithMedian(backupTest)
dim(backupTest)


# Training the multinomial model
model <- multinom(cause ~ . , data = backupTrain, maxit=1000)


# Make predictions and saving to csv file
predictions <- test %>%
  mutate(cause_p = predict(model,test,type='class')) %>%
  select(id, cause_p) %>%
  rename(Id = id, Category = cause_p)

write_csv(predictions, file="predictions_2021-05-04.csv")


#####################################################################################
# Variable Selection
# Running t-test to detect important variables 
#####################################################################################

# Running the independent t-test
temp <- train

features <- removeOutlierFeatures(temp)
temp <- temp[,c(features,"cause")]

names <- colnames(temp)
names = names[!(names %in% "cause")]

for(name in names)
{
  print(t.test(temp[,name], temp[,"cause"]))
}

# t.test shows is not giving us which variables are important.


#####################################################################################
# Variable Selection
# Running information gain to detect important variables 

library(FSelector)
information.gain(cause~., train) > 0.1

features <- c("lon","dist_camp","aws_m1","aws_m3","aws_m6","aws_m12","aws_m24","year","month")

# Decision Tree on above selected variables. 
# 0.51 score (no improvement)
#####################################################################################

backupTrain <- train
backupTest <- test[,c(-1)]

backupTrain <- backupTrain[,c(features,"cause")]
backupTest <- backupTest[,c(features)]

model <- rpart(cause~., data = backupTrain, method = 'class')


# Make predictions and saving to csv file
predictions <- test %>%
  mutate(cause_p = predict(model, test,type='class')) %>%
  select(id, cause_p) %>%
  rename(Id = id, Category = cause_p)

write_csv(predictions, file="predictions_2021-05-04.csv")


#############################################################
# Decision Tree
# Using LASSO to select important features and then apply
# 0.2 Score (Very bad)
#############################################################

backupTrain <- train
backupTest <- test[,c(-1)]

backupTrain["cause"] <- as.numeric(backupTrain$cause)
str(train)

# Using LASSO for variable selection
feat_mod_select <- cv.glmnet(as.matrix(backupTrain[ , -55]) , backupTrain[ ,55], standardize = TRUE, alpha =1)

# Checking coefficients with the minimum cross-validation error
as.matrix(coef(feat_mod_select, feat_mod_select$lambda.min))

features <- c("FOR_CODE",
              "FOR_CAT",
              "HEIGHT",
              "arf7",
              "arf28",
              "arf90",
              "arf720",
              "se",
              "ase14",
              "ase90",
              "ase180",
              "ase720",
              "amaxt7",
              "amaxt14",
              "amaxt90",
              "amaxt360",
              "amint7",
              "amint28",
              "amint60",
              "amint90",
              "amint720",
              "dist_camp",
              "aws_m24",
              "year",
              "month"
)

backupTrain <- train
backupTest <- test[,c(-1)]

backupTrain <- backupTrain[,c(features,"cause")]
backupTest <- backupTest[,c(features)]

model <- rpart(cause~., data = backupTrain, method = 'class')


# Make predictions and saving to csv file
predictions <- test %>%
  mutate(cause_p = predict(model, test,type='class')) %>%
  select(id, cause_p) %>%
  rename(Id = id, Category = cause_p)

write_csv(predictions, file="predictions_2021-05-04.csv")

