library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(e1071)
header <- F
dataURL <- "BreastCancerData.csv"
d<-read.csv(dataURL,header = header)
c = 2
d[[c]] <- factor(d[[c]],labels = c(0,1))


for( i in 1: nrow(d)){
  for( j in 1:ncol(d)){
    if ( d[i,j] == '?'){
      print(d[i,j])
      d[i,j] <- gsub("?",0,d[i,j], fixed = TRUE)
    }
  }
}



sampleInstances<-sample(1:nrow(d),size = 0.5*nrow(d))
trainingData<-d[sampleInstances,]
testData<-d[-sampleInstances,]




cnames <- colnames(d)
y <- paste(cnames[c], "~", sep="")
count <- 1
if(c != 1) y <- paste(y,cnames[1] , sep="") else {
  y <- paste(y , cnames[2] , sep="") 
  count = 2
}

for(i in 1:length(cnames)){
  
  if(c != i && i != count){
    y <- paste(y, cnames[i], sep="+")
  }
}
y



y <- as.formula(y)


model1<- svm(y, data = trainingData, kernel = 'polynomial', degree = '2',  
             cost = '10')


testData$V2 <- as.numeric(testData$V2)






