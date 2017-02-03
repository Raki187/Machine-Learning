library(caret)
library(rpart)
library(gbm)
library(adabag)
adadata <- read.table("D:/Machine Learning/Assignment_4/dataset.txt",sep=",",stringsAsFactors = FALSE,header=FALSE)
names(adadata)	<- c("DGN","PRE4",	"PRE5",	"PRE6",	"PRE7",	"PRE8",	"PRE9",	"PRE10",	"PRE11","PRE14",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE", "Risk1Y")
cols <- sapply(adadata, is.logical)
adadata[,cols] <- lapply(adadata[,cols], as.numeric)
adadata<-subset(adadata,select=-c(DGN,PRE6,PRE14))
trainIndex	<- sample(1:nrow(adadata),	0.8	*	nrow(adadata))
train	<- adadata[trainIndex,	]
test	<- adadata[-trainIndex,	]
train$Risk1Y <- as.factor(train$Risk1Y)
test$Risk1Y <- as.factor(test$Risk1Y)
fit1 <- boosting(Risk1Y ~ ., data = train, boos = TRUE, mfinal = 13,control = rpart.control(cp = -1))

#Accuracy for training data
t_pred_train = predict(fit1,train,type="class")
conf_mat <- t_pred_train$confusion
accuracy_train <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_train<-100*accuracy_train
cat(paste("Accuracy on Training dataset",accuracy_train))

#Accuracy for test data
t_pred_test = predict(fit1,test)
conf_mat <-  t_pred_test$confusion
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
