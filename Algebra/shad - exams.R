l <- 2012
l1 <- round(l / 2)
l2 <- l1 + 1
s <- sample(1:(l), l)
sort(s)
s1 <- sort(s[1:l1])
s2 <- sort(s[l2:l], decreasing = TRUE)

s1 - s2
d <- t(rbind(s1,s2))

i <- 0
d.cut <- d
while (nrow(d.cut) > 1 && i < 8) {
  i <- i + 1
  div <- round(l1 / (2^i))

  sum.up <- sum((head(d.cut[,1], div) - head(d.cut[,2], div))^2)
  sum.down <- sum((tail(d.cut[,2], div) - tail(d.cut[,1], div))^2)
  
  if (sum.up < sum.down) {
    d.cut <- head(d.cut, div)
  } else {
    d.cut <- tail(d.cut, div)
  }
  
  print(paste(i, div, nrow(d.cut)))
}
d.cut
head(d.cut)
head(d)

d.cut[,1] - d.cut[,2]

A <- diag(1:10)
A[1,3] <- 1

A %*% solve(A)

v <- sample(0:3, 10, replace = T)
n <- length(v)
k <- 3
for (i in 1:n) {
  if (i > k) {
    
  } else {
    s <- v[i]
  }
}

x <- t(as.matrix(sample(-1:1, 3)))
A <- matrix(sample(1:3, 9, replace = T), nrow = 3, ncol = 3)
x %*% t(x)
x %*% A %*% t(x)


mean(runif(100000, 0, 1))

