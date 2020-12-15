#**************MSBA 434 - Data Mining & Visualization******************
#***********************Direct-Maling Project**************************
#****************************Louie Tran********************************


#*****MODEL 1: K-NN MODEL*******************************************************
#Importing data
set.seed(12345)
fundraising <- read.csv("/Users/louietran/Downloads/Fundraising.csv")

#Test with logistic regression to see which variables to use in this project
#I do this 20 times with every predictor variables
mylogit <- glm(TARGET_B~totalmonths, data = fundraising, family = 'binomial')
summary(mylogit)

#Subsetting a dataset with the variables I want to use
fsub <- fundraising[c('TARGET_B','totalmonths','LASTGIFT','AVGGIFT','NUMCHLD','NUMPROM','INCOME','homeowner.dummy')]

#Create variable for number of columns for testing trial and errors purpose
col <- ncol(fsub)

#Partioning 60-40
train.index <- sample(row.names(fsub), 0.6*dim(fsub)[1])  
valid.index <- setdiff(row.names(fsub), train.index)  
train <- fsub[train.index, ]
valid <- fsub[valid.index, ]

# Standardize Data
train.norm <- train
valid.norm <- valid
fsub.norm <- fsub
library(caret)
norm.values <- preProcess(train[,2:col], method=c("center", "scale"))
train.norm[, 2:col] <- predict(norm.values, train[, 2:col])
valid.norm[, 2:col] <- predict(norm.values, valid[, 2:col])
fsub.norm[, 2:col] <- predict(norm.values, fsub[,2:col])

# use knn() to compute knn. 
library(FNN)
nn <- knn(train = train.norm[, 2:col], test = valid.norm[, 2:col], cl = train.norm[,1], k = 43)


#Accuracy Test
library(caret)
#initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 44, 1), accuracy = rep(0, 44))

#Compute knn for different k on validation.
for(i in 1:44) {
  knn.pred <- knn(train.norm[, 2:col],valid.norm[, 2:col], 
                  cl = factor(train.norm$TARGET_B), k = i)
  accuracy.df[i, 2] <- confusionMatrix(as.factor(knn.pred), as.factor(valid.norm$TARGET_B))$overall[1]
}
accuracy.df

#Test on train
pred1 <- knn(fsub.norm[, 2:col], train.norm[, 2:col], cl = fsub.norm[,1], k = 13, prob = TRUE)
tpred1 <- table(pred1)
ttrain1 <- table(train.norm[1])

#Net profit calculation
#Net profit calculated for donors
np_1 = (13-0.68)/((0.5/0.051))
#Net profit calculated for non-donors
np_0 = (0-0.68)/((0.5/0.949))
#NET PROFIT’s formula: If TARGET_B = 1, then profit is $1.25664, otherwise -$1.29064
#validation net profit
act_netprofit = ttrain1[2] * np_1 + ttrain1[1] * np_0
act_netprofit
pred_netprofit = tpred1[2] * np_1 + tpred1[1] * np_0
pred_netprofit


#*****LIFT CHARTS COMPONENTS

#Add predicted values to training data set
train['pred1_train'] <- pred1
#Create net profit variables for actual data and predicted data
train[,'NetProfit_pred'] <- NA
train[,'NetProfit_act'] <- NA

#Use for loops to fill in net profit values based on the the predicted values
for (i in 1:nrow(train)) {
  if(train$pred1_train[i] == 1){ 
    train$NetProfit_pred[i]= np_1
  }
  else {
    train$NetProfit_pred[i]= np_0
  }
}
#Use for loops to fill in net profit values based on the the actual values
for (i in 1:nrow(train)) {
  if(train$TARGET_B[i] == 1){ 
    train$NetProfit_act[i]= np_1
  }
  else {
    train$NetProfit_act[i]= np_0
  }
}

#Use gains libraryy to create lift charts later
library(gains)
gain1 <- gains(as.numeric(train$NetProfit_act ), as.numeric(train$NetProfit_pred))
netprofit1 <- train$NetProfit_pred 




#*****MODEL 2: NAIVE BAYES MODEL***************************************
#Importing Data
library(e1071)
library('OneR')
set.seed(12345)
fundraising2 <- read.csv("/Users/louietran/Downloads/Fundraising.csv")
fundraising2['Avggift'] = fundraising2$AVGGIFT
fundraising2$AVGGIFT <- NULL
fsub2 <- fundraising2[c('TARGET_B','totalmonths','LASTGIFT','Avggift','NUMCHLD','NUMPROM','INCOME','homeowner.dummy')]

#Transform Data by binning numerical variables and convert them to factors
fsub2$NUMPROM <- bin(fsub2$NUMPROM, nbins = 3, labels = c("Low","Medium","High"))
fsub2$MAXRAMNT <- bin(fsub2$MAXRAMNT, nbins = 3, labels = c("Low","Medium","High"))
fsub2$LASTGIFT <- bin(fsub2$LASTGIFT, nbins = 3, labels = c("Low","Medium","High"))
fsub2$Avggift <- bin(fsub2$Avggift, nbins = 3, labels = c("Low","Medium","High"))
fsub2$INCOME <- bin(fsub2$INCOME, nbins = 3, labels = c("Low","Medium","High"))

#Create Year variable
fsub2$YEAR <-round(fsub2$totalmonths/12)
fsub2$YEAR <- factor(fsub2$YEAR)
fsub2$totalmonths <- NULL

#Convert factor variables
fsub2$homeowner.dummy <- factor(fsub2$homeowner.dummy)
fsub2$NUMCHLD <- factor(fsub2$NUMCHLD)
fsub2$TARGET_B <- factor(fsub2$TARGET_B)


#Partitioning Data 60-40
train2.index <- sample(row.names(fsub2), 0.6*dim(fsub2)[1])  
valid2.index <- setdiff(row.names(fsub2), train2.index)  
train2 <- fsub2[train2.index, ]
valid2 <- fsub2[valid2.index, ]


# Run Naive Bayes
fsub2.nb <- naiveBayes(TARGET_B ~ .,  data = train2)

## predict probabilities
pred2.prob <- predict(fsub2.nb, newdata = valid2, type = "raw")
## predict class membership
pred2.class <-predict(fsub2.nb, newdata = valid2)

##A summary table
df2 <- data.frame(actual = valid2$TARGET_B, predicted = pred2.class, pred2.prob)

library(caret)

# training
pred2.train.class <- predict(fsub2.nb, newdata = train2)
confusionMatrix(pred2.train.class, train2$TARGET_B)

pred2.valid.class <- predict(fsub2.nb, newdata = valid2)
confusionMatrix(pred2.valid.class, valid2$TARGET_B)

#*****LIFT CHARTS FOR BOTH MODELS

#Add predicted values to training data set
train2['pred2_train'] <- pred2.train.class
#Create net profit variables for actual data and predicted data
train2[,'NetProfit_pred'] <- NA
train2[,'NetProfit_act'] <- NA

#Use for loops to fill in net profit values based on the the predicted values
for (i in 1:nrow(train2)) {
  if(train2$pred2_train[i] == 1){ 
    train2$NetProfit_pred[i]= np_1
  }
  else {
    train2$NetProfit_pred[i]= np_0
  }
}
#Use for loops to fill in net profit values based on the the actual values
for (i in 1:nrow(train2)) {
  if(train2$TARGET_B[i] == 1){ 
    train2$NetProfit_act[i]= np_1
  }
  else {
    train2$NetProfit_act[i]= np_0
  }
}

#Use gains to make lift charts
library(gains)
gain2 <- gains(as.numeric(train2$NetProfit_act ), as.numeric(train2$NetProfit_pred))
netprofit2 <- train2$NetProfit_pred 

#Create lift charts for both models
plt2 <-plot(c(0,gain2$cume.pct.of.total*sum(netprofit2))~c(0,gain2$cume.obs), ylim = c(-800,0),
     xlab="Cumulative Observations", ylab="Cumulative Net Profit", main="", type="l",)
lines(c(0,gain1$cume.pct.of.total*sum(netprofit1))~c(0,gain1$cume.obs), type ='l',col ='gray')
legend(500,50,
       legend = c("Naive Bayes", "KNN"), 
       col = c("black",'gray'), 
       lty = 1,
       lwd = 2,
       bty = "n", 
       pt.cex = 1:2, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


##TEST NEW DATA WITH K-NN
library(caret)
#Import new dataset
future <- read.csv("/Users/louietran/Downloads/FutureFundraising.csv")

#Subsetting data with same variables used in the two methods above
futuresub2 <- future[c('Row.Id','TARGET_B','totalmonths','LASTGIFT','AVGGIFT','NUMCHLD','NUMPROM','INCOME','homeowner.dummy')]

#Standardize data
futuresub2.norm <- futuresub2
norm.values2 <- preProcess(futuresub2[,3:9], method=c("center", "scale"))
futuresub2.norm[, 3:9] <- predict(norm.values2, futuresub2[,3:9])

#Use K-NN k = 13 to predict the dataset
futurepred <- knn(train = train.norm[, 2:8], test = futuresub2.norm[, 3:9], cl = train.norm[,1], k = 13,prob = TRUE)

#Add probability to the dataset
futuresub2['Probability'] <- attributes(futurepred)$prob

#Sort the data set in descending order of probability
futuresub2_sorted <- futuresub2[order(futuresub2$Probability, decreasing = TRUE),]

#List of probability of potential donors
donors_prob <- futuresub2_sorted[c('Row.Id','Probability')]





