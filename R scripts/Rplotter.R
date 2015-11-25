header <- F
dataURL <- "BreastCancerData.csv"
d<-read.csv(dataURL,header = header)
c = 2
d[[c]] <- as.factor(d[[c]])

for(i in 1:ncol(d)){
  a = sapply(d[[i]],is.factor)
  if(a[1] == TRUE && i != c) d[[i]] <- as.numeric(d[[i]])
}


bi = 1
b = d[1,]
for(i in 1: nrow(d)){
   if(d[i,2] == "M"){
     b[bi,] <- d[i,]
     bi = bi + 1;
   }
      
}

hist(b$V3, col = "Blue", xlab = "Radius", ylab = "Malignant cases", main = "")
png(filename="hist.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=72)
hist(b$V3, col = "Blue", xlab = "Radius", ylab = "Malignant cases", main = "")
dev.off()


png(filename="plot.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=72)
plot(d$V6, d$v2, pch=21, bg=c("blue","red")[unclass(d$V2)], main="Breast Cancer Data", xlab = "Index", ylab = "Area")
dev.off()

png(filename="pairs.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=72)
pairs(d[3:6], main = "Breast Cancer Data", pch = 21, bg = c("red", "green3", "blue")[unclass(d$V2)], labels = c("Radius","Texture","Perimeter","Area"))
dev.off()
