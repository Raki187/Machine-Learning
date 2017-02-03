dataset <- read.table("D:/Machine Learning/Assignment_4/dataset.txt",sep=",",stringsAsFactors = FALSE,header=FALSE)
names(dataset)	<- c("DGN","PRE4",	"PRE5",	"PRE6",	"PRE7",	"PRE8",	"PRE9",	"PRE10",	"PRE11","PRE14",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE", "Risk1Y")
cols <- sapply(dataset, is.logical)
dataset[,cols] <- lapply(dataset[,cols], as.numeric)
dataset<-subset(dataset,select=-c(DGN,PRE6,PRE14))
dataset$Risk1Y <- factor(dataset$Risk1Y)

trainIndex	<- sample(1:nrow(dataset),	0.8	*	nrow(dataset))
train	<- dataset[trainIndex,	]
test	<- dataset[-trainIndex,	]
library(ipred)
library(rpart)
for(i in 0:9)
{
  x=i*folds+1
  y=x+folds-1
  test<-dataset[x:y,]
  train<-dataset[-(x:y),]
  fit <- bagging(Risk1Y ~.,data=train,mfinal=15,control=rpart.control(maxdepth=7, minsplit=22))
  library(e1071)
  library(caret)

  #Accuracy for training data
  train$pred.class <- predict(fit,train)
  t_pred_train<-confusionMatrix(data=factor(train$pred.class),reference=train$Risk1Y,positive='1')
  conf_mat <- t_pred_train$table
  accuracy_train <- sum(diag(conf_mat))/sum(conf_mat)
  accuracy_train<-100*accuracy_train
  cat(paste("\nAccuracy on Training dataset",accuracy_train))

  #Accuracy for test data
  test$pred.class <- predict(fit,test)
  t_pred_test<-confusionMatrix(data=factor(test$pred.class),reference=test$Risk1Y,positive='1')
  conf_mat <-  t_pred_test$table
  accuracy_test <- sum(diag(conf_mat))/sum(conf_mat)
  accuracy_test<-100*accuracy_test
  cat(paste("\nAccuracy on Testing dataset",accuracy_test))

  #Precision and Recall
  tp<-conf_mat[1,1]
  tn<-conf_mat[2,2]
  fn<-conf_mat[1,2]
  fp<-conf_mat[2,1]
  pr<-tp/(tp+fp)
  re<-tp/(tp+fn)
  cat(paste("\nPrecision:",pr))
  cat(paste("\nRecall   :",re))
}

