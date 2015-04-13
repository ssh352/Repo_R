delta <- as.matrix(rbind(c(3,-2,-3,-4),c(1,2,1,-2),c(6,1,-1,3),c(3,0,2,1)))
result <- as.matrix(c(-2,4,6,5))

d <- det(delta)
d1 <- round(det(cbind(result,delta[,-1])),2)
d2 <- round(det(cbind(delta[,1],result,delta[,c(3,4)])),2)
d3 <- round(det(cbind(delta[,c(1,2)],result,delta[,c(4)])),2)
d4 <- round(det(cbind(delta[,-4],result)),2)

x1 <- d1 / d
x2 <- d2 / d
x3 <- d3 / d
x4 <- d4 / d
x <- as.matrix(c(x1,x2,x3,x4))

test <- delta %*% x

all.equal(result, test)
