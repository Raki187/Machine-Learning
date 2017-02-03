dataset <- read.table("D:/Machine Learning/Assignment_4/dataset.txt",sep=",",stringsAsFactors = FALSE,header=FALSE)
names(dataset)	<- c("DGN","PRE4",	"PRE5",	"PRE6",	"PRE7",	"PRE8",	"PRE9",	"PRE10",	"PRE11","PRE14",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE", "Risk1Y")
folds<-nrow(dataset)/10
cols <- sapply(dataset, is.logical)
dataset[,cols] <- lapply(dataset[,cols], as.numeric)
dataset<-subset(dataset,select=-c(DGN,PRE6,PRE14))
for(i in 0:9)
{
  x=i*folds+1
  y=x+folds-1
  test<-dataset[x:y,]
  train<-dataset[-(x:y),]
  model <- glm(Risk1Y ~.,family=binomial(link='logit'),data=train)
  
  t_pred_train = ifelse(predict(model,type='response')>0.5,1,0) 
  conf_mat <- table(t_pred_train,train$Risk1Y)
  accuracy_train <- sum(diag(conf_mat))/sum(conf_mat)
  accuracy_train=train*100 
  cat(paste("\nAccuracy on Training dataset",accuracy_train))
  
  t_pred_test = ifelse(predict(model,type='response')>0.5,1,0) 
  conf_mat <- table(t_pred_test,train$Risk1Y)
  accuracy_test <- sum(diag(conf_mat))/sum(conf_mat)
  accuracy_test<-100*accuracy_test
  cat(paste("\nAccuracy on Testing dataset",accuracy_test))
  
  library(gplots)
  library(ROCR)
  
  pr <- prediction(t_pred_train, train$Risk1Y)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  cat(paste("\nArea under the ROC curve :",auc*100))
  
}

