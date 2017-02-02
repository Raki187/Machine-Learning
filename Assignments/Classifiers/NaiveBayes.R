#IMPORTING DATASET
dataset <- read.table("D:/Machine Learning/Assignment_2/dataset.txt",sep=",")
# NAMING  THE COLUMNS
names(dataset)	<- c("DGN",		"PRE4",	"PRE5",	"PRE6",	"PRE7",	"PRE8",	"PRE9",	"PRE10","PRE11","PRE14",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE", "Risk1Y")
#SPLITTING DATASET INTO TRAIN AND TEST 
trainIndex	<- sample(1:nrow(dataset),	0.8	*	nrow(dataset))
train	<- dataset[trainIndex,	]
test	<- dataset[-trainIndex,	]
#LIBRARY FOR MODELLING NAIVE BAYES

library(e1071)

nbmodel <- naiveBayes(factor(Risk1Y) ~ factor(DGN)+factor(PRE4)+factor(PRE5)+factor(PRE6)+factor(PRE7)+factor(PRE8)+factor(PRE9)+factor(PRE10)+factor(PRE11)+factor(PRE14)+factor(PRE17)+factor(PRE19)+factor(PRE25)+factor(PRE30)+factor(PRE32)+ factor(AGE), data = train)

#ACCURACY FOR TRAIN DATASET
trainnbpred <- predict(nbmodel,as.data.frame(train))
conf_mat <- table(train$Risk1Y,trainnbpred)
accuracy_train <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_train<-100*accuracy_train
accuracy_train

#ACCURACY FOR TEST DATASET
testnbpred <- predict(nbmodel,as.data.frame(test))
conf_mat <- table(test$Risk1Y,testnbpred)
accuracy_test <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_test<-100*accuracy_test
accuracy_test
