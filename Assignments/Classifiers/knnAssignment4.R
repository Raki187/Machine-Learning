dataset <- read.table("D:/Machine Learning/Assignment_4/dataset.txt",sep=",",stringsAsFactors = FALSE,header=FALSE)
names(dataset)	<- c("DGN","PRE4",	"PRE5",	"PRE6",	"PRE7",	"PRE8",	"PRE9",	"PRE10",	"PRE11","PRE14",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE", "Risk1Y")
folds<-nrow(dataset)/10
cols <- sapply(dataset, is.logical)
dataset[,cols] <- lapply(dataset[,cols], as.numeric)
dataset<-subset(dataset,select=-c(DGN,PRE6,PRE14))
accuracy_test<-0
for(i in 0:9)
{
  x=i*folds+1
  y=x+folds-1
  test<-dataset[x:y,]
  train<-dataset[-(x:y),]
  library(class)
  normalize <- function(x) 
    {
      return ((x - min(x)) / (max(x) - min(x)))
    }
  model <- knn(train= train[,-14], test = test[,-14],cl = as.integer(train[,14]), k =, prob = TRUE)
  prob <- attr(model, "prob")
  library(gmodels)
  conf_mat <- table(test$Risk1Y,model)
  precision <- (conf_mat[1,1]) / (conf_mat[1,1]+ conf_mat[2,1])
  cat(paste("\nFold Number",i,"Precision\t",precision,"\t"))
  a_test <- sum(diag(conf_mat))/sum(conf_mat)
  a_test<-100*a_test
  accuracy_test<-accuracy_test+a_test

}
cat(paste("\nAverage accuracy on Training dataset\t",accuracy_test/10,"\n"))


