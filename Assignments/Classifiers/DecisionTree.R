#importing dataset
dataset <- read.table("D:/Machine Learning/Assignment_2/dataset.txt",sep=",")
#assigning names to columns
names(dataset)	<- c("DGN",		"PRE4",	"PRE5",	"PRE6",	"PRE7",	"PRE8",	"PRE9",	"PRE10",	"PRE11",	
              "PRE14",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE", "Risk1Y")

trainIndex	<- sample(1:nrow(dataset),	0.8	*	nrow(dataset))

train	<- dataset[trainIndex,	]
test	<- dataset[-trainIndex,	]

library(rpart)

fit <- rpart(Risk1Y~., method="class", data=train,minsplit=11, minbucket=3)

#Accuracy for training data
t_pred_train = predict(fit,train, type = "class")
conf_mat <- table(train$Risk1Y,t_pred_train)
accuracy_train <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_train<-100*accuracy_train
accuracy_train

#Accuracy for training data
t_pred_test = predict(fit,test, type = "class")
conf_mat <- table(test$Risk1Y,t_pred_test)
accuracy_test <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_test<-100*accuracy_test
accuracy_test
