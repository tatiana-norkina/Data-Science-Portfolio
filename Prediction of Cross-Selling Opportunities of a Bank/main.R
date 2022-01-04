############### Course Data Mining in Marketing ###############################################

####################### Main Algorithm: GRADIENT BOOSTING - XGBoost ###########################

####################### Data preparation ######################################################

# clear workspace

rm(list = ls())

#### Load the dataset ####
wd <- "C:\\your_directory_here" # modify path depending on where you saved the data set
setwd(wd)

xsell <- read.csv("xsell.csv")

xsell$X <- NULL ### csv file contains X variable that should be deleted

# transform variables into factor variables

xsell$gender <- as.factor(xsell$gender) 
xsell$marital <- as.factor(xsell$marital)
xsell$occupation <- as.factor(xsell$occupation)
xsell$pref_device <- as.factor(xsell$pref_device)

xsell$ppower <- as.factor(xsell$ppower)
xsell$avg_res_dur <- as.factor(xsell$avg_res_dur)
xsell$pop_km <- as.factor(xsell$pop_km)
xsell$car_seg <- as.factor(xsell$car_seg)

summary(xsell)

# split data set into training and validation

set.seed(12345) # fix random number generator seed for reproducibility
train_id <- sample(1:nrow(xsell), size = floor(0.8 * nrow(xsell)), replace=FALSE) 

# split in Training and Test (80/20)

xsell_train<-xsell[train_id,]
xsell_valid<-xsell[-train_id,]

####################### Algorithm XGBOOST ######################################################

library(xgboost)
library(Matrix)

options(na.action='na.pass') # Returns the NA object unchanged, if not changed, NA would be dropped

# prepare matrix for XGBoost algorithm, including all variables

train_m <-model.matrix(xsell ~ .-1, data = xsell_train)
valid_m <-model.matrix(xsell ~ .-1, data = xsell_valid)

train_label <- xsell_train$xsell

######### Step 1: build the XGBoost model using default parameters ############################

# run the model

set.seed(1234)
xgb <- xgboost(data=train_m,label=train_label,max.depth=3,eta=0.01,subsample=0.5,
               nrounds=100,objective="binary:logistic", verbose=1)

print(xgb,verbose=TRUE)
xgb

# make predictions
xsell_valid$pred_xgb <- predict(xgb, newdata = valid_m)

xsell_valid$pred_xgb_factor <- factor(ifelse(xsell_valid$pred_xgb>.5,1,0),labels=c("No xsell","xsell"))

# check accuracy with the confusion matrix 
library(caret)

# non-tuned XGBoost model
confusionMatrix(xsell_valid$pred_xgb_factor,factor(xsell_valid$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))

############################
#### Accuracy valid: 0.6515#
############################


# predictions on test data set

xsell_train$pred_xgb <- predict(xgb, newdata = train_m)

xsell_train$pred_xgb_factor <- factor(ifelse(xsell_train$pred_xgb>.5,1,0),labels=c("No xsell","xsell"))

# check accuracy with the confusion matrix 
library(caret)

# non-tuned XGBoost model
confusionMatrix(xsell_train$pred_xgb_factor,factor(xsell_train$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))


############################
#### Accuracy train: 0.648 #
############################




########## Step 2: build the XGBoost model using tuning and feature engineering ####################################################

# split in Training and Test for tuning (80/20)

xsell_train_tuned<-xsell[train_id,]
xsell_valid_tuned<-xsell[-train_id,]

train_m_tuned <-model.matrix(xsell ~ .-1, data = xsell_train_tuned)
valid_m_tuned <-model.matrix(xsell ~ .-1, data = xsell_valid_tuned)

train_label_tuned <- xsell_train_tuned$xsell

dtrain <- xgb.DMatrix(data = train_m_tuned, label=xsell_train_tuned$xsell)

### tuning via grid

library(xgboost)
library(Matrix)

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6), 
                                colsample_bytree = c(0.5, 0.6),
                                max_depth = c(3, 4),
                                min_child = seq(1), 
                                eta = c(0.01,0.03)
)

ntrees <- 300

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = ntrees, nfold = 5, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))}))


output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames
head(output)


# run the tuned model

set.seed(1234)
xgb_tuned <- xgboost(data=train_m_tuned,label=train_label_tuned,max.depth=4,eta=0.01,min_child=1, subsample=0.6,
               colsample_bytree = 0.5,
               nrounds=300,objective="binary:logistic", verbose=1)

print(xgb,verbose=TRUE)
xgb_tuned

# make predictions
xsell_valid_tuned$pred_xgb <- predict(xgb_tuned, newdata = valid_m_tuned)

xsell_valid_tuned$pred_xgb_factor <- factor(ifelse(xsell_valid_tuned$pred_xgb>.5,1,0),labels=c("No xsell","xsell"))

# check accuracy with the confusion matrix 
library(caret)

# non-tuned XGBoost model
confusionMatrix(xsell_valid_tuned$pred_xgb_factor,factor(xsell_valid_tuned$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))


############################
#### Accuracy valid: 0.6545#
############################


# make predictions
xsell_train_tuned$pred_xgb <- predict(xgb_tuned, newdata = train_m_tuned)

xsell_train_tuned$pred_xgb_factor <- factor(ifelse(xsell_train_tuned$pred_xgb>.5,1,0),labels=c("No xsell","xsell"))

# check accuracy with the confusion matrix 
library(caret)

# non-tuned XGBoost model
confusionMatrix(xsell_train_tuned$pred_xgb_factor,factor(xsell_train_tuned$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))


############################
#### Accuracy train: 0.6811#
############################



# feature engineering
# feature performance and relevance to get to know what variables to drop 

importance_matrix <- xgb.importance(model = xgb_tuned)
print(importance_matrix)
xgb.plot.importance(importance_matrix, cex=1.2)

xsell_train_tuned$fixed_acc <- NULL
xsell_valid_tuned$fixed_acc <- NULL

xsell_train_tuned$complaints <- NULL
xsell_valid_tuned$complaints <- NULL


train_m_tuned_new <-model.matrix(xsell ~ .-1, data = xsell_train_tuned)
valid_m_tuned_new <-model.matrix(xsell ~ .-1, data = xsell_valid_tuned)

train_label_tuned_new <- xsell_train_tuned$xsell

dtrain <- xgb.DMatrix(data = train_m_tuned_new, label=xsell_train_tuned$xsell)

# run the tuned model

set.seed(1234)
xgb_tuned_new <- xgboost(data=train_m_tuned_new,label=train_label_tuned_new,max.depth=4,eta=0.01,min_child=1, subsample=0.6,
                     colsample_bytree = 0.5,
                     nrounds=300,objective="binary:logistic", verbose=1)

print(xgb_tuned_new,verbose=TRUE)
xgb_tuned_new

# make predictions
xsell_valid_tuned$pred_xgb <- predict(xgb_tuned_new, newdata = valid_m_tuned_new)

xsell_valid_tuned$pred_xgb_factor <- factor(ifelse(xsell_valid_tuned$pred_xgb>.5,1,0),labels=c("No xsell","xsell"))

# check accuracy with the confusion matrix 
library(caret)

# non-tuned XGBoost model
confusionMatrix(xsell_valid_tuned$pred_xgb_factor,factor(xsell_valid_tuned$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))

# after deleting fixed_acc and complaints variables accuracy of the model did not change

############################
#### Accuracy valid: 0.6545#
############################

# make predictions
xsell_train_tuned$pred_xgb <- predict(xgb_tuned_new, newdata = train_m_tuned_new)

xsell_train_tuned$pred_xgb_factor <- factor(ifelse(xsell_train_tuned$pred_xgb>.5,1,0),labels=c("No xsell","xsell"))

# check accuracy with the confusion matrix 
library(caret)

# non-tuned XGBoost model
confusionMatrix(xsell_train_tuned$pred_xgb_factor,factor(xsell_train_tuned$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))

############################
#### Accuracy train: 0.6794#
############################


######################## Random Forest ###################################################################

# split data set into training and validation ####

set.seed(12345) # fix random number generator seed for reproducibility
train_id <- sample(1:nrow(xsell), size = floor(0.8 * nrow(xsell)), replace=FALSE) 

# split in Training and Test (80/20)

xsell_train_rf<-xsell[train_id,]
xsell_valid_rf<-xsell[-train_id,]

# build Random Forest Model
library(randomForest)

# create a model
model <- xsell ~ .

# recode churn variable into a factor 
xsell_train_rf$xsell <- factor(xsell_train_rf$xsell)

# model the Random Forest with default parameters: ntree = 100, mtry = 6
set.seed(12345) # fix random number generator seed for reproducibility
rf <- randomForest(model, # Model (churn ~ . gives the algorithm all indep. vars. and lets it do the selection)
                   data=xsell_train_rf, # dataset without missings
                   ntree=100,       # Number of trees (more trees can improve performance, but takes more time to run)
                   mtry=6,          # m parameter: number of randomly selected variables the tree can consider for each split
                   nodesize=100,    # minimum number of observations in leaf nodes 
                   maxnodes=10,     # max amount of nodes
                   replace=TRUE,    # Sampling with replacement (Y/N). If FALSE, then there's no bagging, you're just slicing your dataset
                   sampsize=4000,   # size of each sample
                   na.action=na.roughfix) # impute missing values by median/mode

# check the results
rf

# generate predictions
# type="response" produces a binary prediction; "prob" the corresponding likelihood
pred_rf_response <- predict(rf,newdata=xsell_valid_rf, type="response")
pred_rf_prob <- predict(rf,newdata=xsell_valid_rf, type="prob")

# Add prediction to validation data
xsell_valid_rf$pred_rf <-pred_rf_prob[,2]

### Compare models via confusion matrix ####

xsell_valid_rf$pred_rf_factor<-factor(ifelse(xsell_valid_rf$pred_rf>.5,1,0),labels=c("No xsell","xsell"))

library(caret)
confusionMatrix(xsell_valid_rf$pred_rf_factor,factor(xsell_valid_rf$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))

############################
#### Accuracy valid: 0.6458#
############################

# generate predictions
# type="response" produces a binary prediction; "prob" the corresponding likelihood
pred_rf_response <- predict(rf,data=xsell_train_rf, type="response")
pred_rf_prob <- predict(rf,data=xsell_train_rf, type="prob")

# Add prediction to validation data
xsell_train_rf$pred_rf <-pred_rf_prob[,2]

### Compare models via confusion matrix ####

xsell_train_rf$pred_rf_factor<-factor(ifelse(xsell_train_rf$pred_rf>.5,1,0),labels=c("No xsell","xsell"))

library(caret)
confusionMatrix(xsell_train_rf$pred_rf_factor,factor(xsell_train_rf$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))

############################
#### Accuracy train : 0.6361 
############################


### For the paper it is better to use default parameters as a benchmark 
### Of course, the parameters of RF can be tuned for producing better predictions


model1 <- randomForest(xsell ~ ., data = xsell_train, importance = TRUE,na.action=na.roughfix)
model1

pred_rf_response <- predict(model1,newdata=xsell_valid_rf, type="response")
pred_rf_prob <- predict(model1,newdata=xsell_valid_rf, type="prob")

# Add prediction to validation data
xsell_valid_rf$pred_rf <-pred_rf_prob[,2]

### Compare models via confusion matrix ####

xsell_valid_rf$pred_rf_factor<-factor(ifelse(xsell_valid_rf$pred_rf>.5,1,0),labels=c("No xsell","xsell"))

library(caret)
confusionMatrix(xsell_valid_rf$pred_rf_factor,factor(xsell_valid_rf$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))

############################
#### Accuracy valid : 0.6399  
############################

pred_rf_response <- predict(model1,data=xsell_train_rf, type="response")
pred_rf_prob <- predict(model1,data=xsell_train_rf, type="prob")

# Add prediction to validation data
xsell_train_rf$pred_rf <-pred_rf_prob[,2]

### Compare models via confusion matrix ####

xsell_train_rf$pred_rf_factor<-factor(ifelse(xsell_train_rf$pred_rf>.5,1,0),labels=c("No xsell","xsell"))

library(caret)
confusionMatrix(xsell_train_rf$pred_rf_factor,factor(xsell_train_rf$xsell,labels=c("No xsell","xsell")), 
                positive="xsell",dnn = c("Prediction", "Actual Data"))

############################
#### Accuracy train: 0.503
############################
