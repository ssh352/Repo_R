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
while (nrow(d.cut) > 1 && i < 6) {
  i <- i + 1
  div <- ceiling(l1 / (2^i))

  s.lr.down <- sum((head(d.cut[,1], div) - tail(d.cut[,2], div))^2)
  s.rl.down <- sum((head(d.cut[,2], div) - tail(d.cut[,1], div))^2)
  
  if (s.lr.down < s.rl.down) {
    min.sum <- 1
    max.sum <- 2
  } else {
    min.sum <- 2
    max.sum <- 1
  }
  
  d.cut <- cbind(
            head(d.cut[,min.sum], div),
            tail(d.cut[,max.sum], div)
          )
  
  print(paste(i, div, head(d.cut,1), tail(d.cut,1)))
}
d.cut
head(d.cut)
head(d)


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