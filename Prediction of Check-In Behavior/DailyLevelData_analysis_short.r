rm(list = ls()) #clear workspace
library(dplyr)
library(doParallel)
library(caret)
library(smotefamily)

#setwd #set working directory

source("DailyLevelData_analysis_functions.r")

# ----
# Loading data "do not run", unless you know what you are doing.
if(0){
  #load data
  yelp_data <- read.csv("DailyLevel_data_Imputed.csv",header=TRUE,skipNul = T) #read csv file
  yelp_data$date <- as.Date(yelp_data$date)
  yelp_data$X=NULL
  
  #---- read the temperature data
  wear=extractweather(yelp_data)

  # take the averages across stations for each coordinate
  weather=weardailyavg(wear)
  
  
  
  dates=sort(unique(yelp_data$date))
  weatherstations=as.data.frame(t(sapply(weather,function(x){colMeans(x$range)})))

  # adding weather data to yelp_data
  if(0){
    stations_by=t(apply(yelp_data[,c("business_lat","business_long")],1,
                        function(x){a=sort((x[1]-weatherstations$rangelat)^2+
                                             (x[2]-weatherstations$rangelong)^2,index.return=T)
                        return(a$ix[1:50])})) # finding the 50 closest stations
    
    # add for example, temperature forecasts to the weather data
    for(i in 1:length(weather)){
      if(nrow(weather[[i]]$data)==0)
        next
      store_weather=weather[[i]]$data
      store_weather$TOBS_1=c(store_weather$TOBS[2:nrow(store_weather)],NA)
      store_weather$TOBS_2=c(store_weather$TOBS[3:nrow(store_weather)],NA,NA)
      store_weather$TOBS_3=c(store_weather$TOBS[4:nrow(store_weather)],NA,NA,NA)
      store_weather$TOBS_4=c(store_weather$TOBS[5:nrow(store_weather)],NA,NA,NA,NA)
      weather[[i]]$data=store_weather
    }
    weatherinf=colnames(store_weather)[-1] # which weather variables are available?
    
    yelp_data_weather=NULL
    for(i in 1:length(weather)){
      k=1 # start with the closest station
      stores_in=stations_by[,k]==i
      if(sum(stores_in)==0)
        next
      store_weather=weather[[i]]$data
      
      temp=yelp_data[stores_in,]
      temp=merge(temp,store_weather,by.x="date",by.y="DATE",all.x=T)
      yelp_data_weather=rbind(yelp_data_weather,temp)
      print(i)
    }
    
    # now deal with the missings, by going to the next possible station
    temp_indx=is.na(yelp_data_weather[,"TOBS"])|is.na(yelp_data_weather[,"PRCP"])
    k_changed=NULL
    for(i in which(temp_indx)){
      temp_date=yelp_data_weather[i,]$date
      for(k in 2:ncol(stations_by)){
        temp=weather[[stations_by[i,k]]]$data
        if(!is.na(as.numeric(temp[temp$DATE==temp_date,"TOBS"]))&!is.na(as.numeric(temp[temp$DATE==temp_date,"PRCP"])))
          break
      }
      k_changed=c(k_changed,k)
      
      yelp_data_weather[i,weatherinf]=temp[temp$DATE==temp_date,-1]
      #print(i)
    }
    
    # add weekends and quarters
    temp=weekdays(yelp_data_weather$date,abbreviate = T)
    yelp_data_weather$WE=temp=="Sa"|temp=="So"
    
    yelp_data_weather$Quarter=as.factor(quarters(yelp_data_weather$date))
    
    #save(file="yelp_data_weather.RData",list=c("yelp_data_weather"))
    #write.csv(yelp_data_weather,file="yelp_data_weather.csv")
    
  }
  
}
# END OF:Loading data "do not run", unless you know what you are doing.

# ----
# Importing and adjusting the yelp-data + weather data
yelp_data_weather=read.csv(file="yelp_data_weather.csv")
#load("yelp_data_weather.RData")

# some adjustments to the imported data
yelp_data=yelp_data_weather

yelp_data$date = as.Date(yelp_data$date)
yelp_data$ch_in_string[yelp_data$ch_in>=1]="ch_in"
yelp_data$ch_in_string[yelp_data$ch_in==0]="Noch_in"
yelp_data$ch_in_string <- as.factor(yelp_data$ch_in_string)
yelp_data$ch_in_string <- relevel(yelp_data$ch_in_string,ref="ch_in") # since the performance evaluations are mainly made
                                                                      # to check for the minority class - in our case Noch_in


yelp_data$business_park=as.factor(yelp_data$business_park)
yelp_data$business_open=as.factor(yelp_data$business_open)
yelp_data$business_cat=as.factor(yelp_data$business_cat)
yelp_data$WE=as.factor(yelp_data$WE)
yelp_data$Quarter=as.factor(yelp_data$Quarter)


# ----
# some simply analysis using aggregated on daily level data (some descriptive analysis)
yelp_data_daily= yelp_data %>%
  group_by(date) %>%
  summarize(ch_ins=sum(ch_in,na.rm=T),business_price=mean(business_price,na.rm=T),
            n_photo=mean(n_photo,na.rm=T),cum_n_tips=mean(cum_n_tips,na.rm=T),cum_max_friends=mean(cum_max_friends,na.rm=T),
            cum_max_u_elite=mean(cum_max_u_elite,na.rm=T),cum_max_us_fans=mean(cum_max_us_fans,na.rm=T),
            cum_max_us_rev=mean(cum_max_us_rev,na.rm = T),male=mean(male,na.rm=T),female=mean(female,na.rm=T),
            TMAX=mean(TMAX,na.rm=T),PRCP=mean(PRCP,na.rm=T),SNOW=mean(SNOW,na.rm=T),SNWD=mean(SNWD,na.rm=T),
            TOBS=mean(TOBS,na.rm=T),
            TOBS_1=mean(TOBS_1,na.rm=T),TOBS_2=mean(TOBS_2,na.rm=T),TOBS_3=mean(TOBS_3,na.rm=T),
            TOBS_4=mean(TOBS_4,na.rm=T))

# some analyses on daily level data
formula1=as.formula(ch_ins~business_price+PRCP+SNOW+TOBS+n_photo+female+male+cum_max_u_elite+cum_max_us_fans)
formula2=as.formula(ch_ins~PRCP+SNOW+TOBS+n_photo+female+male+cum_max_u_elite+cum_max_us_fans) # without avg price
model1=lm(formula1, yelp_data_daily)
#car::vif(model1)
summary(model1)

# Example: Are the coefficients for different price categories different? 
by(yelp_data,yelp_data$business_price,function(x){
  x_daily_cat= x %>%
    group_by(date) %>%
    summarize(ch_ins=sum(ch_in,na.rm=T),business_price=mean(business_price,na.rm=T),
              n_photo=mean(n_photo,na.rm=T),cum_n_tips=mean(cum_n_tips,na.rm=T),cum_max_friends=mean(cum_max_friends,na.rm=T),
              cum_max_u_elite=mean(cum_max_u_elite,na.rm=T),cum_max_us_fans=mean(cum_max_us_fans,na.rm=T),
              cum_max_us_rev=mean(cum_max_us_rev,na.rm = T),male=mean(male,na.rm=T),female=mean(female,na.rm=T),
              TMAX=mean(TMAX,na.rm=T),PRCP=mean(PRCP,na.rm=T),SNOW=mean(SNOW,na.rm=T),SNWD=mean(SNWD,na.rm=T),
              TOBS=mean(TOBS,na.rm=T),
              TOBS_1=mean(TOBS_1,na.rm=T),TOBS_2=mean(TOBS_2,na.rm=T),TOBS_3=mean(TOBS_3,na.rm=T),
              TOBS_4=mean(TOBS_4,na.rm=T))
  model1=lm(formula2, x_daily_cat)
  summary(model1)
})


# ----
# predictiv models
# Split randomly
set.seed(66)
yelp_data_na=yelp_data
# list of variables in your model
varsin=c("ch_in_string","ch_in","WE","Quarter","business_price","business_cat","business_park","TOBS","PRCP","n_photo","female","male","cum_n_tips","cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_us_rev")
yelp_data=subset(yelp_data,select=varsin)
datasetsize=nrow(yelp_data)/10
x <- yelp_data[sample(1:datasetsize, datasetsize, replace = F),]
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]

BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))

# create dummies
x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))

# class imbalance check.
temp=table(x.train[,"ch_in_string"])
# if yes, maybe you want do random over-sampling:
if(0){
  oversampled=x.train[x.train$ch_in_string==names(temp)[sort.int(temp,index.return=T,decreasing = T)$ix[1]],]
  minclass=names(temp)[sort.int(temp,index.return=T)$ix[1]]
  for(m in 1:(length(temp)-1)){
    minchclass=names(temp)[sort.int(temp,index.return=T)$ix[m]]
    minclassdat=x.train[x.train$ch_in_string==minchclass,]
    minclassdat=minclassdat[sample(1:nrow(minclassdat), sort(temp,decreasing = T)[1] , replace = T),]
    oversampled=rbind(oversampled,minclassdat)
  }
  x.train=oversampled
}
# or better, do SMOTE:
if(1){
  x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
  names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
  x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
  x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
  x.traindum=x.traindum_smote
  rm(x.traindum_smote)
  }


############ Data for Heuristic machine learning methods
# normalize data (very important for ML techniques, but not for logistic regression)
x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)

# adjust Baseformulea to the dummy version of the data
varsin_dum=varsin[1:2]
for(i in 3:length(varsin)){
  if(!is.null(levels(x[,varsin[i]]))){
    for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
      varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
    }
  }else{
    varsin_dum=c(varsin_dum,varsin[i])
  }
}

BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))


# set threshold probability: usually .5, but better is to set it to the portion of 1's. 
probthres=1-mean(x.traindum$ch_in)

# ----
# the analyses
######### LOGIT
ptm <- proc.time()
x.modelLogit <- glm(BaseFormula_dum , data = x.traindum, family = "binomial") # estimating the probability of "no-checkin"

summary(x.modelLogit)

x.evaluate$predictionlogit <- predict(x.modelLogit, newdata=x.evaluatedum, type = "response")
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>probthres] <- "Noch_in"
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=probthres] <- "ch_in"

x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))
LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")

TimeAux <- proc.time() - ptm 
#LogitOutput$summary=summary(x.modelLogit)
LogitOutput$TimeElapsed <- TimeAux[3]
LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100
Logitconfmatrix <- table(x.evaluate$predictionlogitclass,x.evaluate$ch_in_string)
rm(TimeAux)

############ SVM
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(parallelSVM)
ptm <- proc.time()
# fast trainer
#x.modelSVM <- train(BaseFormula, data = x.train, method="svmRadial", cachesize=12000, tolerance=.01)

# faster trainer
x.modelSVM <- parallelSVM(BaseFormula_dum , data = x.trainnorm, probability=T, 
                          cachesize=12000, tolerance=.005, kernel="radial")

x.evaluate$predictionSVM <- predict(x.modelSVM, newdata=x.evaluatenorm, probability = T)


x.evaluate$predictionSVMclass[attr(x.evaluate$predictionSVM,"probabilities")[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionSVMclass[attr(x.evaluate$predictionSVM,"probabilities")[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctSVM <- x.evaluate$predictionSVMclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))

# for fast trainer you can also get the variable importance
#print(varImp(x.modelSVM))

# Extract the class probabilities.
x.evaluate$predictionSVM <- attr(x.evaluate$predictionSVM,"probabilities")[,'Noch_in']

SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")

TimeAux <- proc.time() - ptm 
SVMOutput$TimeElapsed <- TimeAux[3]
SVMOutput$PercCorrect <- mean(x.evaluate$correctSVM)*100
SVMconfmatrix <- table(x.evaluate$predictionSVMclass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)

########## Neural network
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(NeuralNetTools) # required for plotting
# fast trainer using parallel computations
ptm <- proc.time()
mlp_grid = expand.grid(layer1 = 5,
                       layer2 = 0,
                       layer3 = 0)
x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid=mlp_grid) 

x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluatenorm, type="prob")

x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$correctNNet <- x.evaluate$predictionNNetclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))

print(varImp(x.modelNNet))
# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet$finalModel)
}
x.evaluate$predictionNNet <- x.evaluate$predictionNNet[,"Noch_in"]

NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")

TimeAux <- proc.time() - ptm 
#NNetOutput$summary=varImp(x.modelNNet)
NNetOutput$TimeElapsed <- TimeAux[3]
NNetOutput$PercCorrect <- mean(x.evaluate$correctNNet)*100
NNetconfmatrix <- table(x.evaluate$predictionNNetclass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)
########## TREE
# fast model using parallel computation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree') 


x.evaluate$predictionTree <- predict(x.modelTree, newdata = x.evaluatenorm, type = "prob")

x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionTreeClass <- factor(x.evaluate$predictionTreeClass, levels=c("ch_in","Noch_in"))

x.evaluate$correctTree <- x.evaluate$predictionTreeClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctTree)))

x.evaluate$predictionTree <- x.evaluate$predictionTree[,"Noch_in"]

# to see the importance of the variables
print(varImp(x.modelTree))

# plot tree 
if(0){
  plot(x.modelTree$finalModel)
}

TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")

TimeAux <- proc.time() - ptm 
#TreeOutput$summary <- varImp(x.modelTree)
TreeOutput$TimeElapsed <- TimeAux[3]
TreeOutput$PercCorrect <- mean(x.evaluate$correctTree)*100
Treeconfmatrix <- table(x.evaluate$predictionTreeClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)
############ Bagging
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# fast training using parallel computation
x.modelBagging  <- train(BaseFormula_dum, data=x.trainnorm, method="treebag",importance=T)

# Use the model to predict the evaluation.
x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluatenorm, type="prob")

x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBaggingClass <- factor(x.evaluate$predictionBaggingClass, levels=c("ch_in","Noch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBagging <- x.evaluate$predictionBaggingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))

# Extract the class probabilities.
x.evaluate$predictionBagging <- x.evaluate$predictionBagging[,"Noch_in"]

# to see the importance of the variables
print(varImp(x.modelBagging))

BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")

TimeAux <- proc.time() - ptm
#BaggingOutput$summary <- varImp(x.modelBagging)
BaggingOutput$TimeElapsed <- TimeAux[3]
BaggingOutput$PercCorrect <- mean(x.evaluate$correctBagging)*100
Baggingconfmatrix <- table(x.evaluate$predictionBaggingClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)

############ Boosting
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using boosting ensemble algorithms
# fast trainer using parallel computation
x.modelBoosting  <- train(BaseFormula_dum, data=x.trainnorm, method = 'blackboost')#,  method = 'bstTree')

# Use the model to predict the evaluation.
x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluatenorm,type="prob")

x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBoostingClass <- factor(x.evaluate$predictionBoostingClass, levels=c("ch_in","Noch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBoosting <- x.evaluate$predictionBoostingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))

# Extract the class probabilities.
x.evaluate$predictionBoosting <- x.evaluate$predictionBoosting[,"Noch_in"]

# to see the importance of the variables
print(varImp(x.modelBoosting))

# Make a lift curve
BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")

TimeAux <- proc.time() - ptm 
#BoostingOutput$summary <- varImp(x.modelBoosting)
BoostingOutput$TimeElapsed <- TimeAux[3]
BoostingOutput$PercCorrect <- mean(x.evaluate$correctBoosting)*100
Boostingconfmatrix <- table(x.evaluate$predictionBoostingClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)

############ RANDOM FOREST
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using "random forest and bagging ensemble algorithms
# a fast trainer using parallel computation
x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF") 

# Use the model to predict the evaluation.
x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluatenorm, type = "prob")

x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionRFClass <- factor(x.evaluate$predictionRFClass, levels=c("ch_in","Noch_in"))


# Calculate the overall accuracy.
x.evaluate$correctRF <- x.evaluate$predictionRFClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))

# Extract the class probabilities.
x.evaluate$predictionRF <- x.evaluate$predictionRF[,"Noch_in"]

# to see the importance of the variables
print(varImp(x.modelRF))

RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")

TimeAux <- proc.time() - ptm 
#RFOutput$summary <- varImp(x.modelRF)
RFOutput$TimeElapsed <- TimeAux[3]
RFOutput$PercCorrect <- mean(x.evaluate$correctRF)*100
RFconfmatrix <- table(x.evaluate$predictionRFClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)

# save(file=paste0("results",format(Sys.time(),"%b-%d-%y"),".RData"),
#      list=c("LogitOutput","SVMOutput","NNetOutput","TreeOutput","BaggingOutput","BoostingOutput","RFOutput",
#                                          "x.train","x.evaluate","x.modelBagging","x.modelBoosting","x.modelNNet",
#                                          "x.modelRF","x.modelTree","x.modelLogit","x.modelSVM","BaseFormula","BaseFormula1"))

# load(file=paste0("results",format(Sys.time(),"%b-%d-%y"),".RData"))

# SOME Summarizing plots:

#barplot(c(LogitOutput$TDL,naiveBayesOutput$TDL, SVMOutput$TDL,TreeOutput$TDL,BaggingOutput$TDL,BoostingOutput$TDL,RFOutput$TDL,NNetOutput$TDL), names.arg = c("Logit","Naive Bayes","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), main="Top Decile Lifts of the models")
#barplot(c(LogitOutput$GINI,naiveBayesOutput$GINI, SVMOutput$GINI,TreeOutput$GINI,BaggingOutput$GINI,BoostingOutput$GINI,RFOutput$GINI,NNetOutput$GINI), names.arg = c("Logit","Naive Bayes","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), main="GINI coefficients of the models")

OverallTDL <- c(LogitOutput$TDL,SVMOutput$TDL,TreeOutput$TDL,BaggingOutput$TDL,BoostingOutput$TDL,RFOutput$TDL,NNetOutput$TDL)
OverallGINI <- c(LogitOutput$GINI,SVMOutput$GINI,TreeOutput$GINI,BaggingOutput$GINI,BoostingOutput$GINI,RFOutput$GINI,NNetOutput$GINI)

ForGraph <- data.frame(OverallTDL,OverallGINI)

myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)

ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)

op <- par(mar = c(5,4,4,4) + 0.1)

barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), beside = TRUE, yaxt = "n", names.arg = c("Logit","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), ylab =	"Top Decile Lift", legend = c("TDL","GINI"), main="Performance of the Machine Learning Algorithms")

axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)

axis(4, at = myRightAxisAt, labels = myRightAxisLabs)

mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))

mtext(c(paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
        paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
        paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20))
mtext(c(paste(round(LogitOutput$PercCorrect,digits=0),"%"),
        paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=0),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
        paste(round(RFOutput$PercCorrect,digits=0),"%"),
        paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20))

mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)


lift_obj=lift(ch_in_string~predictionBagging+predictionBoosting+predictionTree+predictionNNet+predictionSVM+predictionlogit,data=x.evaluate,class="Noch_in")

ggplot(lift_obj)
