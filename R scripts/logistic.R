library(caret)
#args <- commandArgs(TRUE)
#dataURL<-as.character(args[1])
#header<-as.logical(args[2])
header <- F
dataURL <- "BreastCancerData.csv"
d<-read.csv(dataURL,header = header)

#c = as.integer(args[3])
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
y



y <- as.formula(y)
model1 <- glm(y, data = trainingData, family = "binomial")
prediction <-predict(model1, testData)
# use a threshold value and anything above that, you can assign to class=1 others to class=0
threshold=0.35
prediction<-sapply(prediction, FUN=function(x) if (x>threshold) 1 else 0)

accuracy <- table(prediction, testData[[c]])
print(accuracy)

if(nrow(accuracy) == 1)acc = accuracy[1]/nrow(testData)else{
  #Accuracy given by confusionMatrix command
  confuse_mat <- confusionMatrix(accuracy)
  overal <- confuse_mat$overall
  accuracy <- overal['Accuracy']
  acc <- as.double(accuracy)
}
all <- list(acc=acc)

