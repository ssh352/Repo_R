library(cluster)  # plot clusters
library(plyr)     # rename data frame columns
# load train and test data
setwd("D:\\Data Mining\\R\\Clusters")
# options(OutDec= ".")
data <- read.csv("cluster_ALL.csv", dec=",", header=FALSE, sep = ";", stringsAsFactors=TRUE)
data <- rename(data, c("V2"="daxi", "V3"="daxname", "V4"="summary", "V5"="order_rows", "V6"="order_count", "V7"="volume", "V8"="count"))
X <- data[,c(4,7)]
# add custom features
#X$sum_on_count <- X$summary / X$count
#X$vol_on_count <- X$volume / X$count
#X$sum_on_vol <- X$summary / X$volume
#X <- do.call(data.frame, lapply(X, function(x) replace(x, is.infinite(x),0)))
#X <- do.call(data.frame, lapply(X, function(x) replace(x, is.nan(x),0)))
dax <- data[,c(2,3)]
X_s <- scale(X)
rbu <- data[,1]
for (i in unique(rbu)) {
  ix <- which(rbu==i)
  #X_s[ix,] <- scale(X[ix,])
}
 
J = 0
J_min = 0
for (i in 1:1000) {
  print
  cl <- kmeans(X_s, 4, 1000, algorithm = "Lloyd")
  J = sum(cl$withinss)
  if (J_min == 0) {
    J_min = J
    clusters = cl$cluster
  }
  if (J < J_min) {
    J_min = J
    clusters = cl$cluster
  }
}
# Create submission dataframe and output to file
result <- data.frame(rbu = rbu, daxid = dax$daxi, daxname = dax$daxname, summary = X$summary, group = cl$cluster )
# result <- data.frame(group = cl$cluster )
write.csv(result, file = "result.csv", row.names = FALSE)
clusplot(X_s, cl$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, xlab="Summary", ylab="Volume")
