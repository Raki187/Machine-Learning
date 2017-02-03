#importing dataset
library(randomForest)
dataset <- read.table("D:/Machine Learning/Assignment_4/dataset.txt",sep=",",stringsAsFactors = FALSE,header=FALSE)
#assigning names to columns
names(dataset)	<- c("DGN","PRE4",	"PRE5",	"PRE6",	"PRE7",	"PRE8",	"PRE9",	"PRE10",	"PRE11","PRE14",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE", "Risk1Y")
cols <- sapply(dataset, is.logical)
dataset[,cols] <- lapply(dataset[,cols], as.numeric)
dataset<-subset(dataset,select=-c(DGN,PRE6,PRE14))
trainIndex	<- sample(1:nrow(dataset),	0.8	*	nrow(dataset))
train	<- dataset[trainIndex,	]
test	<- dataset[-trainIndex,	]
rffit <- randomForest(factor(Risk1Y) ~.,data=train,ntree=100,nodesize=17,mtry=9)
varImpPlot(rffit)

#Accuracy for training data
t_pred_train = predict(rffit,train,type="class")
conf_mat <- table(train$Risk1Y,t_pred_train)
accuracy_train <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_train<-100*accuracy_train
cat(paste("Accuracy on Training dataset",accuracy_train))

#Accuracy for test data
t_pred_test = predict(rffit,test, type = "class")
conf_mat <- table(test$Risk1Y,t_pred_test)
accuracy_test <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_test<-100*accuracy_test
cat(paste("\nAccuracy on Testing dataset",accuracy_test))
library(pROC)

#Are under ROC
predictions <- as.numeric(predict(rffit,test, type = 'response'))
auc<-multiclass.roc(test$Risk1Y, predictions)
cat(paste("\nArea under the ROC curve : ",auc[7]))
#print(auc)
