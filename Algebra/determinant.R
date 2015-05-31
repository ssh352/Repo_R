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

x <- seq(0, 1, 0.01)
y <- (2 * x^2 - 3 * x - 1) / (x^4 - 1)
plot(x, y)

x <- seq(-2, 2, 0.1)
y <- 1 - x ^ 2
plot(x, y, type = "l")
abline(h=0)
abline(v=0)
integrand <- function(x) {x^2 + 1}
integrate(integrand, lower = -1, upper = 1)

x <- 2
1 / (x + 1/x^6)
(x^-1 + (x^-6)^-1)
(2 + 3 + 4)^-1
2^-1 + 3^-1 + 4^-1

x1 <- seq(sqrt(pi/3), sqrt(pi/6), by=-0.1)
y1 <- sin(x1^2)
plot(x1, y1, type="l", xlim=c(0, 1), ylim=c(0, 1))
x2 <- seq(sqrt(3)/2, 1/2, by=-0.1)
y2 <- sqrt(asin(x2))
lines(x2, y2, type="l")

y1 <- sqrt(asin(x))
lines(x, y1, type="l")

integrand <- function(x) {sin(x^2)}
integrate(integrand, lower = sqrt(pi/6), upper = sqrt(pi/3))

integrand <- function(x) {asin(x)}
integrate(integrand, lower = 1/2, upper = sqrt(3)/2)

lines(x, sqrt(asin(x)))
grid()
