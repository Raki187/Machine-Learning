#IMPORTING DATASET
dataset <- read.table("D:/Machine Learning/Assignment_2/dataset.txt",sep=",")
# NAMING  THE COLUMNS
names(dataset)	<- c("DGN",		"PRE4",	"PRE5",	"PRE6",	"PRE7",	"PRE8",	"PRE9",	"PRE10","PRE11","PRE14",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE", "Risk1Y")
#SPLITTING DATASET INTO TRAIN AND TEST 
trainIndex	<- sample(1:nrow(dataset),	0.8	*	nrow(dataset))
train	<- dataset[trainIndex,	]
test	<- dataset[-trainIndex,	]
#LIBRARY FOR MODELLING SEPARATE VECTOR MACHINES
library(e1071)
svm.model <- svm(factor(Risk1Y) ~ ., data = train,kernel="polynomial" ,cost = 100, gamma = 1)

#ACCURACY FOR TRAIN
svmtrainpred <- predict(svm.model,train)
conf_mat <- table(train$Risk1Y,svmtrainpred)
accuracy_train <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_train<-100*accuracy_train
accuracy_train

#ACCURACY FOR TEST
svmtestpred <- predict(svm.model,test)
conf_mat <- table(test$Risk1Y,svmtestpred)
accuracy_test <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_test<-100*accuracy_test
accuracy_test    