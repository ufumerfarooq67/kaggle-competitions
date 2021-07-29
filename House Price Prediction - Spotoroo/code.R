library(dplyr)
library(tidyr)
library(ModelMetrics)
library(Metrics)
library(corrplot)
library(caTools)  # loading caTools library
library(ggplot2)
library(scales)
library(glmnet)
library(caret)
library(xgboost)
library(randomForest)
library(stats)
# Reading the data
training <- read.csv("train.csv")
testing <- read.csv("test.csv")
sample_submission <- read.csv("sample_submission.csv")


# Combining both the dataset for data exploration and pre-processing.
output_train <- training['SalePrice']
data <-  rbind(within(training, rm('SalePrice')), testing)


# Dimension of dataset
dim(data)

# Structure of dataset
str(data)


# Let find out if we have got null values.
any(is.na(data))



highlyCorrelatedfeaturesMethod <- function(dataset,val)
{
  train_lasso<- dataset[,sapply(dataset,is.numeric) | sapply(dataset,is.integer)]
  corr <- cor(train_lasso)
  correlation_salePrice <-tail(corr,1)
  correlation_salePrice <- correlation_salePrice[,colSums(correlation_salePrice) > val]
  correlation_salePrice <- data.frame(t(correlation_salePrice))
  correlation_salePrice
  strong_relation_names <- colnames(correlation_salePrice)
  strong_relation_names <- strong_relation_names[-15]
  strong_relation_names
  
  return (strong_relation_names)
}

# Cleaning, Pre-processing and Casting
preprocessing <- function(dataset)
{
  
  print("Cleaning the dataset and Pre-processing")
  # Let find those columns with null values.
  columns_null <- dataset[,colSums(is.na(dataset)) > 0]
  colnames(columns_null)
  
  # Categorical Columns with NA as None (No Type)
  columns_null$PoolQC <- columns_null$PoolQC %>% 
    replace_na("None")
  
  columns_null$MiscFeature <- columns_null$MiscFeature %>% 
    replace_na("None")
  
  columns_null$Alley <- columns_null$Alley %>% 
    replace_na("None")
  
  columns_null$Fence <- columns_null$Fence %>% 
    replace_na("None")
  
  columns_null$FireplaceQu <- columns_null$FireplaceQu %>% 
    replace_na("None")
  
  columns_null$GarageFinish <- columns_null$GarageFinish %>% 
    replace_na("None")
  
  columns_null$GarageQual <- columns_null$GarageQual %>% 
    replace_na("None")
  
  columns_null$GarageCond <- columns_null$GarageCond %>% 
    replace_na("None")
  
  columns_null$GarageType <- columns_null$GarageType %>% 
    replace_na("None")
  
  columns_null$BsmtCond <- columns_null$BsmtCond %>% 
    replace_na("None")
  
  columns_null$BsmtExposure <- columns_null$BsmtExposure %>% 
    replace_na("None")
  
  columns_null$BsmtQual <- columns_null$BsmtQual %>% 
    replace_na("None")
  
  columns_null$BsmtFinType2 <- columns_null$BsmtFinType2 %>% 
    replace_na("None")
  
  columns_null$BsmtFinType1 <- columns_null$BsmtFinType1 %>% 
    replace_na("None")
  
  columns_null$MasVnrType <- columns_null$MasVnrType %>% 
    replace_na("None")
  
  
  # Categorical Columns with no clear NA meaning (replacing with value having highest frequency)
  columns_null$MSZoning <- columns_null$MSZoning %>% 
    replace_na(tail(names(sort(table(columns_null$MSZoning))), 1))
  
  
  columns_null$Utilities <- columns_null$Utilities %>% 
    replace_na(tail(names(sort(table(columns_null$Utilities))), 1))
  
  
  columns_null$Functional <- columns_null$Functional %>% 
    replace_na(tail(names(sort(table(columns_null$Functional))), 1))
  
  
  columns_null$Exterior1st <- columns_null$Exterior1st %>% 
    replace_na(tail(names(sort(table(columns_null$Exterior1st))), 1))
  
  
  columns_null$Exterior2nd <- columns_null$Exterior2nd %>% 
    replace_na(tail(names(sort(table(columns_null$Exterior2nd))), 1))
  
  
  columns_null$Electrical <- columns_null$Electrical %>% 
    replace_na(tail(names(sort(table(columns_null$Electrical))), 1))
  
  
  columns_null$KitchenQual <- columns_null$KitchenQual %>% 
    replace_na(tail(names(sort(table(columns_null$KitchenQual))), 1))
  
  columns_null$SaleType <- columns_null$SaleType %>% 
    replace_na(tail(names(sort(table(columns_null$SaleType))), 1))
  
  
  
  #########################################
  
  
  # Numerical Columns (Replace NA with mean)
  
  columns_null$LotFrontage <- columns_null$LotFrontage %>% 
    replace_na(mean(columns_null$LotFrontage,na.rm=TRUE))
  
  
  columns_null$GarageYrBlt <- columns_null$GarageYrBlt %>% 
    replace_na(round(mean(columns_null$GarageYrBlt,na.rm = TRUE)))
  
  columns_null$GarageCars <- columns_null$GarageCars %>% 
    replace_na(round(mean(columns_null$GarageCars,na.rm = TRUE)))
  
  columns_null$GarageArea <- columns_null$GarageArea %>% 
    replace_na(round(mean(columns_null$GarageArea,na.rm = TRUE)))
  
  
  columns_null$BsmtFullBath <- columns_null$BsmtFullBath %>% 
    replace_na(round(mean(columns_null$BsmtFullBath,na.rm = TRUE)))
  
  
  columns_null$BsmtHalfBath <- columns_null$BsmtHalfBath %>% 
    replace_na(round(mean(columns_null$BsmtHalfBath,na.rm = TRUE)))
  
  
  columns_null$BsmtFinSF1 <- columns_null$BsmtFinSF1 %>% 
    replace_na(round(mean(columns_null$BsmtFinSF1,na.rm = TRUE)))
  
  
  columns_null$BsmtFinSF2 <- columns_null$BsmtFinSF2 %>% 
    replace_na(round(mean(columns_null$BsmtFinSF2,na.rm = TRUE)))
  
  
  columns_null$BsmtUnfSF <- columns_null$BsmtUnfSF %>% 
    replace_na(round(mean(columns_null$BsmtUnfSF,na.rm = TRUE)))
  
  
  columns_null$TotalBsmtSF <- columns_null$TotalBsmtSF %>% 
    replace_na(round(mean(columns_null$TotalBsmtSF,na.rm = TRUE)))
  
  
  columns_null$MasVnrArea <- columns_null$MasVnrArea %>% 
    replace_na(round(mean(columns_null$MasVnrArea,na.rm = TRUE)))
  
  
  # Let's replace new columns without any NA values.
  dataset[,colnames(columns_null)] <- columns_null
  
  columns_null <- dataset[,colSums(is.na(dataset)) > 0]
  colnames(columns_null)
  # No, null values
  
  any(is.na(dataset))
  # False mean, not even a single value is null.
  
  
  
  #####################################################################
  
  ########## CHANGING THE TYPES OF PARTICULAR COLUMNS
  
  ####################################################################
  
  # Changing categorical with factor type to numeric
  
  #Categorical factors that should be numeric
  dataset$ExterQual <- as.numeric(factor(dataset$ExterQual))
  dataset$ExterCond <- as.numeric(factor(dataset$ExterCond))
  dataset$BsmtQual <- as.numeric(factor(dataset$BsmtQual))
  dataset$BsmtCond <- as.numeric(factor(dataset$BsmtCond))
  dataset$BsmtExposure <- as.numeric(factor(dataset$BsmtExposure))
  dataset$BsmtFinType1 <- as.numeric(factor(dataset$BsmtFinType1))
  dataset$BsmtFinType2 <- as.numeric(factor(dataset$BsmtFinType2))
  dataset$HeatingQC <- as.numeric(factor(dataset$HeatingQC))
  dataset$KitchenQual <- as.numeric(factor(dataset$KitchenQual))
  dataset$FireplaceQu <- as.numeric(factor(dataset$FireplaceQu))
  dataset$GarageQual <- as.numeric(factor(dataset$GarageQual))
  dataset$GarageCond <- as.numeric(factor(dataset$GarageCond))
  dataset$PoolQC <- as.numeric(factor(dataset$PoolQC))
  dataset$Condition2 <- as.numeric(factor(dataset$Condition2))
  dataset$Heating <- as.numeric(factor(dataset$Heating))

  
  # Changing few numeric to factor
  dataset$MoSold <- factor(dataset$MoSold)
  dataset$MSSubClass <- factor(dataset$MSSubClass)
  
  
  
  # New Feature Generation
  ###Combining existing features###
  dataset$OverallGrade <- dataset$OverallQual * dataset$OverallCond
  dataset$GarageGrade <- dataset$GarageQual * dataset$GarageCond
  dataset$ExterGrade <- dataset$ExterQual * dataset$ExterCond
  dataset$KitchenScore <- dataset$KitchenAbvGr * dataset$KitchenQual
  dataset$FireplaceScore <- dataset$Fireplaces * dataset$FireplaceQu
  dataset$GarageScore <- dataset$GarageArea * dataset$GarageQual
  dataset$PoolScore <- dataset$PoolArea * dataset$PoolArea
  dataset$TotalBath <- dataset$BsmtFullBath + (0.5 * dataset$BsmtHalfBath) + dataset$FullBath + (0.5 * dataset$HalfBath)
  dataset$TotalSF <- dataset$TotalBsmtSF + dataset$X1stFlrSF + dataset$X2ndFlrSF
  dataset$TotalPorchSF <- dataset$OpenPorchSF + dataset$EnclosedPorch + dataset$X3SsnPorch + dataset$ScreenPorch

  
  return (dataset)
}


#################################
dataset = preprocessing(data)


n <- nrow(testing)

train <- dataset[1:nrow(training),]
test <- dataset[nrow(training)+1:n,]

train <- cbind(train,output_train)


# XGBOOST
################################
head(train)
train_xgboost <- train[,c("GrLivArea","SalePrice")]

set.seed(123)   
sample = sample.split(train_xgboost,SplitRatio = 0.70) 
newTrain =subset(train_xgboost,sample == TRUE) 
newTest=subset(train_xgboost, sample == FALSE)

param <- list(colsample_bytree = 1,
              subsample = .6,
              booster = "gbtree",
              max_depth = 8,
              eta = 0.05,
              min_child_weight = 2,
              eval_metric = "rmse",
              objective="reg:linear",
              gamma = 0.01)


dtrain = xgb.DMatrix(as.matrix(sapply(newTrain, as.numeric)), label=newTrain$SalePrice)
dtest = xgb.DMatrix(as.matrix(sapply(newTest, as.numeric)), label=newTest$SalePrice)

model <- xgb.train(params = param,
            data = dtrain,
            nrounds = 4000,
            watchlist = list(train = dtrain),
            verbose = TRUE,
            print_every_n = 50,
            nthread = 2)


prediction <- predict(model, dtest)

prediction <- as.data.frame(as.matrix(prediction))
colnames(prediction) <- "prediction"

model_output <- cbind(newTest[2], prediction)

cor(model_output)

plot(prediction-newTest$SalePrice)

ggplot(data=model_output, aes(x = SalePrice, y = prediction, size = SalePrice-prediction)) +
  geom_point() +
  scale_x_continuous(breaks= seq(0, 1000000, by=100000),labels=comma) +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)



# XGBOOST on highly correlated features
# Applying on original data
########################################

features <-highlyCorrelatedfeaturesMethod(train,0.5)
# Training on whole dataset and test on testing dataset


train_xbg <- train[,c(features,"SalePrice")]
test_xbg <- test[,features]
test_xbg$SalePrice <- 0
colnames(train_xbg)
colnames(test_xbg)

param <- list(colsample_bytree = 1,
              subsample = .6,
              booster = "gbtree",
              max_depth = 8,
              eta = 0.08,
              min_child_weight = 2,
              eval_metric = "rmse",
              objective="reg:linear",
              gamma = 0.01)


dtrain = xgb.DMatrix(as.matrix(sapply(train_xbg, as.numeric)), label=train_xbg$SalePrice)
dtest = xgb.DMatrix(as.matrix(sapply(test_xbg, as.numeric)))

model <- xgb.train(params = param,
                   data = dtrain,
                   nrounds = 2000,
                   watchlist = list(train = dtrain),
                   verbose = TRUE,
                   print_every_n = 50,
                   nthread = 2)

prediction <- predict(model, dtest)

prediction <- as.data.frame(as.matrix(prediction))
colnames(prediction) <- "prediction"

model_output <- cbind(test$Id, prediction)
colnames(model_output) <- c("Id","SalePrice")

write.csv(model_output,file="sample_submission.csv")


# Random Forest on Original Data
################################

rf <- randomForest(
  formula = SalePrice ~ ., 
  data    = train_xbg, 
  ntree   = 500,
  xtest = test_xbg[-15]
)


prediction_rf <- data.frame(rf$predicted)

model_output_rf <- cbind(test$Id, prediction_rf[-1460,])
colnames(model_output_rf) <- c("Id","SalePrice")

write.csv(model_output_rf,file="sample_submission_rf.csv")


# Lasso Regression with Training Data
#####################################

# Training on whole dataset and test on testing dataset
lasso_data <- train[,c(features,"SalePrice")]

set.seed(123)   
sample = sample.split(lasso_data,SplitRatio = 0.70) 
newTrain =subset(lasso_data,sample == TRUE) 
newTest=subset(lasso_data, sample == FALSE)


set.seed(123)
control = trainControl(method ="cv", number = 5)
Grid_la_reg = expand.grid(alpha = 1,
                          lambda = seq(0.001, 0.1, by = 0.0002))

lasso_model = train(x = newTrain[,-15],
                    y = newTrain[,15],
                    method = "glmnet",
                    trControl = control,
                    tuneGrid = Grid_la_reg
)

# mean validation score
mean(lasso_model$resample$RMSE)

# Plot
plot(lasso_model, main = "Lasso Regression")

prediction_lasso <- predict(lasso_model,newTest[,-15])
prediction_lasso <- as.data.frame(as.matrix(prediction_lasso))

model_output <- data.frame(cbind(newTest[,15], prediction_lasso))
colnames(model_output) <- c("SalePrice","prediction")
head(model_output)
cor(model_output)

ggplot(data=model_output, aes(x = SalePrice, y = prediction, size = SalePrice-prediction)) +
  geom_point() +
  scale_x_continuous(breaks= seq(0, 1000000, by=100000),labels=comma) +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)

# Lasso Regression Model with Original Data
########################
# Training lasso regression model
set.seed(123)
control = trainControl(method ="cv", number = 5)
Grid_la_reg = expand.grid(alpha = 1,
                          lambda = seq(0.001, 0.1, by = 0.0002))

lasso_model = train(x = train_xbg[,-15],
                    y = train_xbg[,15],
                    method = "glmnet",
                    trControl = control,
                    tuneGrid = Grid_la_reg
)


# mean validation score
mean(lasso_model$resample$RMSE)

# Plot
plot(lasso_model, main = "Lasso Regression")


prediction_lasso <- predict(lasso_model,test_xbg[-1460,-15])

prediction_lasso <- as.data.frame(as.matrix(prediction_lasso))
colnames(prediction_lasso) <- "prediction"


model_output_ls <- cbind(test$Id, prediction_lasso)
colnames(model_output_ls) <- c("Id","SalePrice")
write.csv(model_output_ls,file="sample_submission_ls.csv")

###################################################################
# Data Visualization and Selecting Features
##################################################################

# HIstogram of training
ggplot(data=train, aes(x=SalePrice)) +
  geom_histogram(fill="green", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)

# Scatter plot of GrLivArea vs SalePrice
ggplot(data=train, aes(x=GrLivArea,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)
# We can see a postive correlation, therefore, it is an important feature.


#  Scatter plot of OverallQual vs SalePrice
ggplot(data=train, aes(x=OverallQual,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)
# We can see a strong correlation between overallqual and saleprice.
# A very important variable in predicting the saleprice

# Scatter plot of YearBuilt vs SalePrice
ggplot(data=train, aes(x=YearBuilt,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)
# The relation is as strong as overallQual but can be considered as partially
# important.


# Scatter plot of YearRemodAdd vs SalePrice
ggplot(data=train, aes(x=YearRemodAdd,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)
# The relation is as strong as overallQual but can be considered as partially
# important.

# Scatter plot of TotalBsmtSF vs SalePrice
ggplot(data=train, aes(x=TotalBsmtSF,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)
# The relation is as strong as overallQual but can be considered as partially
# important.


ggplot(data=train, aes(x=GrLivArea,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)


# Can see that GarageCars (3) can result in the huge rise in price.
ggplot(data=train, aes(x=GarageCars,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)


# Can see that we have got strong correlation between TotalSF and price
ggplot(data=train, aes(x=TotalSF,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)

# Categorical

# MiscFeature as None is dominating feature in the market.
# Hence, we can use it as price predictor.
ggplot(data=train, aes(x=MiscFeature,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)

# Houses with PavedDrives as Y (means Yes) are more dominating in the market.
# Can be used an idea price predictor.
ggplot(data=train, aes(x=PavedDrive,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)

# House with Electical as SBrkr are more dominating.
ggplot(data=train, aes(x=Electrical,y=SalePrice)) +
  geom_point() +
  scale_y_continuous(breaks= seq(0, 1000000, by=100000),labels=comma)



features
# Features has the names of highly correlated columns with price
x  <- data.matrix(train[,features])
head(x)

library(caret)
preproc1 <- preProcess(x, method=c("center", "scale"))
norm1 <- predict(preproc1,x)
summary(norm1)
heatmap(x, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))



# Boxplots
x_new <- data.frame(data.matrix(train[,features]))
all_feature_names <- colnames(x)

for (name in all_feature_names) 
{
  n <- paste0(name,".jpg")
  jpeg(n, width = 350, height = 350)

  boxplot(x_new[,name])
  
  dev.off()
}


newData <- train[,c(features,"SalePrice")]

set.seed(123)   
sample = sample.split(newData,SplitRatio = 0.70) 
newTrain =subset(newData,sample == TRUE) 
newTest=subset(newData, sample == FALSE)
lmodel <- lm(data=newTrain,formula = SalePrice ~ .)

colnames(newTest)
prediction <- predict(lmodel,newTest[-15])
plot(lmodel$residuals)
abline(lm(data=newData,SalePrice ~ .))

