library(rjson)
library(stringr)
options(scipen=999)
args = commandArgs(trailingOnly=TRUE)
clusters <-args[1]
#clusters<-25
TweetsDataFile  <- args[3]
#TweetsDataFile<-"D:/Machine Learning/Assignment_5/Part II/tweets.json"
initialSeedsFile <- readLines(args[2])
#initialSeedsFile<-readLines("D:/Machine Learning/Assignment_5/Part II/initialSeedsFile.txt")
output_file<- args[4]
output_file<-"D:/Machine Learning/Assignment_5/Part II/output.txt"
data <- lapply(readLines(TweetsDataFile), fromJSON)
initialSeedsFile<-str_replace_all(initialSeedsFile, ",", " ")
initialSeedsFile<-as.numeric(initialSeedsFile)
initialSeedsFile<-as.data.frame(initialSeedsFile)
id<-c("id")
text<-("text")
for ( i in 1: length(data)) 
{
  id[i]<-c(data[[i]]$id)
  text[i]<-c(data[[i]]$text)
}
id<-as.data.frame(id)
indices<-match(initialSeedsFile$initialSeedsFile,id$id)
centers<-text[indices]
cluster<-("index")
for( i in 1:length(text))
{
  tw1 <- unlist(strsplit(text[i], " "))
  tw2 <- unlist(strsplit(centers[1], " "))
  intr<-length(intersect(tw1,tw2))
  unin<-length(union(tw1, tw2))
  dist<-1-(intr/unin)
  pos<-1
  for( j in 2:length(centers))
  {
    tw2 <- unlist(strsplit(centers[j], " "))
    intr<-length(intersect(tw1,tw2))
    unin<-length(union(tw1, tw2))
    temp<-1-(intr/unin)
    if (temp<dist)
    {
        dist<-temp
        pos<-j;
    }
  }
  cluster[i]<-c(pos)
}
sink(output_file)
for(i in 1:clusters)
{
  ind<-which(cluster==i)
  if(length(ind>0))
  {
    cat(i)
    cat('\t')
    x<-id$id[ind]
    x<-as.character(x)
    cat(x,sep = ",")
  }
  cat('\n')
}
SSE<-0
for(i in 1:length(centers))
{
  tw1 <- unlist(strsplit(centers[i], " "))
  ind<-which(cluster==i)
  if(length(ind)>0)
  {
    for(j in 1:length(ind))
    {
      tw2 <- unlist(strsplit(text[ind[j]], " "))
      intr<-length(intersect(tw1,tw2))
      unin<-length(union(tw1, tw2))
      temp<-1-(intr/unin)
      SSE<-SSE+temp
    }
  }
}
cat("\nSSE=",SSE)

