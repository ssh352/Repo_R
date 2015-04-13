setwd("D:/GitHub/R_project/Kaggle - Digit Recognizer")

## Read in data
train <- read.csv("train.csv", header=TRUE)
train<-as.matrix(train)

##Color ramp def.
colors<-c('white','black')
cus_col<-colorRampPalette(colors=colors)

## Plot the average image of each digit
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
all_img<-array(dim=c(10,28*28))
for(di in 0:9) {
  print(di)
  all_img[di+1,]<-apply(train[train[,1]==di,-1],2,sum)
  all_img[di+1,]<-all_img[di+1,]/max(all_img[di+1,])*255
  
  z<-array(all_img[di+1,],dim=c(28,28))
  z<-z[,28:1] ##right side up
  image(1:28,1:28,z,main=di,col=cus_col(256))
}

library(randomForest)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

labels <- as.factor(train[,1])
train <- train[,-1]

rf <- randomForest(train, labels, xtest=test, ntree=1000)
predictions <- levels(labels)[rf$test$predicted]

write(predictions, file="rf_benchmark.csv", ncolumns=1) 


