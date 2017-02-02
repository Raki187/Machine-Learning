library(neuralnet)
transfusion <- read.csv("perceptron.csv")
names(dataset)	<- c("PRE4",	"PRE5",	"PRE7",	"PRE8",	"PRE9",	"PRE10",	"PRE11",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE", "Risk1Y")
maxs = apply(dataset, MARGIN = 2, max)
mins = apply(dataset, MARGIN = 2, min)
scaled = as.data.frame(scale(dataset, center = mins, scale = maxs - mins))
trainIndex	<- sample(1:nrow(dataset),	0.8	*	nrow(dataset))
train <- scaled[trainIndex, ]
test <- scaled[-trainIndex, ]
nnet_train <- train
nnet_train <- cbind( nnet_train , train$Risk1Y == 1)
nnet_train <- cbind( nnet_train , train$Risk1Y == 0)
names(nnet_train)[15] <- 'ClassOne'
names(nnet_train)[16] <- 'ClassZero'
nn <- neuralnet(ClassOne + ClassZero ~ PRE4 + PRE5+PRE7 +PRE8+ PRE9+ PRE10 +PRE11 +PRE17 +PRE19+PRE25+PRE30+PRE32+AGE , data=nnet_train, hidden=3 , err.fct="sse",linear.output = FALSE ,threshold = 0.04,rep=15)
x <- train[, c( "PRE4",	"PRE5",	"PRE7",	"PRE8",	"PRE9",	"PRE10",	"PRE11",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE")]
mypredict <- compute(nn, x)$net.result
maxidx <- function(arr) 
{
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), maxidx)
prediction <- c('ClassOne', 'ClassZero')[idx]
df <- data.frame(table(actual=train$Risk1Y,predicted =prediction))
tp <- df[2,3]
tn <- df[3,3]
fp <- df[1,3]
fn <- df[4,3]
acc_train = ((tp+tn)/(tp+tn+fp+fn))*100
acc_train
x <- test[, c( "PRE4",	"PRE5",	"PRE7",	"PRE8",	"PRE9",	"PRE10",	"PRE11",	"PRE17",	"PRE19",	"PRE25", "PRE30", "PRE32", "AGE")]
mypredict <- compute(nn, x)$net.result
maxidx <- function(arr) 
{
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), maxidx)
prediction <- c('ClassOne', 'ClassZero')[idx]
df <- data.frame(table(actual=test$Risk1Y,predicted =prediction))
tp <- df[2,3]
tn <- df[3,3]
fp <- df[1,3]
fn <- df[4,3]
acc_test = ((tp+tn)/(tp+tn+fp+fn))*100
acc_test

