
args = commandArgs(trailingOnly=TRUE)
train <- read.table(args[1], header=TRUE)
test<- read.table(args[2], header=TRUE)
names_attributes_train <-names(train)
number_attributes <- ncol(train) - 1
c0=length(which(train[ncol(train)]==0))
c1=length(which(train[ncol(train)]==1))
c=c0+c1
cat(paste("\n\nP(",names_attributes_train[ncol(train)],"0) = ",c0/c))
tm<-data.matrix(train)
for (i in 1:number_attributes)
{
  x<-as.character(unique(unlist(train[i])))
  for (k in x)
  {
    a=0
    for(j in 1:nrow(train))
    {
      if(tm[[j,ncol(train)]]==0)
      {
        if(tm[[j,i]]==k)
          a=a+1
      }
    }
    cat(paste(" P(",names_attributes_train[i],"=",k,"|",names_attributes_train[ncol(train)],"0) = ",a/c0))
  }
}
cat(paste("\n\n\nP(",names_attributes_train[ncol(train)],"1) = ",c1/c))
for (i in 1:number_attributes)
{
  x<-as.character(unique(unlist(train[i])))
  for (k in x)
  {
    a=0
    for(j in 1:nrow(train))
    {
      if(tm[[j,ncol(train)]]==1)
      {
        if(tm[[j,i]]==k)
          a=a+1
      }
    }
    cat(paste("  P(",names_attributes_train[i],"=",k,"|",names_attributes_train[ncol(train)],"1) = ",a/c0))
  }
} 
library(e1071)
nbmodel <- naiveBayes(as.factor(train$class) ~  ., data = train)
trainnbpred <- predict(nbmodel,as.data.frame(train))
conf_mat <- table(trainnbpred, train$class) 
accuracy_train <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_train<-100*accuracy_train
cat(paste(" \n\nAccuracy on training set (",nrow(train),"instances):",accuracy_train))
testnbpred <- predict(nbmodel,as.data.frame(test))
conf_mat <- table(testnbpred, test$class)
accuracy_test <- sum(diag(conf_mat))/sum(conf_mat)
accuracy_test<-100*accuracy_test
cat(paste(" \n\nAccuracy on training set (",nrow(test),"instances):",accuracy_test))






