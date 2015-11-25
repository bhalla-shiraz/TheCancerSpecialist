#import libraries
library(rpart)
library(e1071)
require(randomForest)
header <- F
dataURL <- "BreastCancerData.csv"
d<-read.csv(dataURL,header = header)


c = 2
d[[c]] <- factor(d[[c]],labels = c(0,1))


for(i in 1:ncol(d)){
  a = sapply(d[[i]],is.factor)
  if(a[1] == TRUE && i != c) d[[i]] <- as.numeric(d[[i]])
}



sampleInstances<-sample(1:nrow(d),size = 0.9*nrow(d))
trainingData<-d[sampleInstances,]
testData<-d[-sampleInstances,]
# which one is the class attribute



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
y <- as.formula(y)


model1 <- randomForest(y,data = trainingData,boos = TRUE,mfinal = 10,control = rpart.control(maxdepth = 1))

model1.pred <- predict(model1,newdata=testData)

a = model1.pred
counter = 0
correct = (a == testData[[c]])
for(i in 1:length(correct)){
  if(correct[i] == TRUE) counter = counter + 1
}
acc = counter/nrow(testData)


all <- list(acc=acc)
