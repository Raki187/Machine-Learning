args = commandArgs(trailingOnly=TRUE)
clusters <-args[1]
input_file <- read.table(args[2], header=TRUE)
output_file<- args[3]
centroids <- sample(1:nrow(input_file),clusters)
c_x<-("x")
c_y<-("y")
for(i in 1:clusters)
{
  x<-input_file$x[centroids[i]]
  y<-input_file$y[centroids[i]]
  c_x[i]<-c(x)
  c_y[i]<-c(y)
}
c_x<-as.numeric(c_x)
c_y<-as.numeric(c_y)
answer<-("index")
for(i in 1:nrow(input_file))
{
  x<-input_file$x[i]
  y<-input_file$y[i]
  cx<-c_x[1]
  cy<-c_y[1]
  dist<-((x-cx)^2+(y-cy)^2)
  pos<-1
  for(j in 2:clusters)
  {
    cx<-c_x[j]
    cy<-c_y[j]
    temp<-((x-cx)^2+(y-cy)^2)
    if(temp<dist)
    {
      dist<-temp
      pos<-j
      c_x[j]<-(cx+x)/2
      c_y[j]<-(cy+y)/2
    }
    answer[i]<-c(pos)
  }
}
answer<-as.numeric(answer)
sink(output_file)
for(i in 1:clusters)
{
  cat(i,"\t")
  cat(which(answer %in% c(i)),"\n",sep=",")
}
SSE<-0
for(i in 1 :clusters)
{
  cx<-c_x[i]
  cy<-c_y[i]
  ind<-which(answer %in% c(i))
  if(length(ind)>0)
  {
    for(j in 1:length(ind))
    {
      x<-input_file$x[ind[j]]
      y<-input_file$y[ind[j]]
      SSE<-SSE+((x-cx)^2+(y-cy)^2)
    }
  }
}
cat("\nSSE=",SSE)

