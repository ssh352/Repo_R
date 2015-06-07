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

x <- seq(-3, 3, 0.1)
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

x1 <- seq(sqrt(pi/6), sqrt(pi/3), by=0.01)
y1 <- sin(x1^2)
plot(x1, y1, type="l", xlim=c(0, 1), ylim=c(0, 1))
x2 <- seq(1/2, sqrt(3)/2, by=0.01)
y2 <- sqrt(asin(x2))
lines(x2, y2, type="l")

s1 <- (sqrt(pi/6)+sqrt(pi/3)) * (sqrt(3) - 1) * 1/4
s2 <- (sqrt(3) + 1) * (sqrt(pi/3) - sqrt(pi/6)) * 1 / 4
s1+s2

1/4 * (sqrt(pi/3)*sqrt(3) - sqrt(pi/3) + sqrt(pi/6)*sqrt(3) - sqrt(pi/6))
1/4 * (sqrt(pi/3)*sqrt(3) - sqrt(pi/6)*sqrt(3) + sqrt(pi/3) - sqrt(pi/6))

1/4 * (sqrt(pi/3)*sqrt(3) - sqrt(pi/3) - sqrt(pi/6))
1/4 * (sqrt(pi/3)*sqrt(3) + sqrt(pi/3) - sqrt(pi/6))

rbind(x1, y1, x2, y2)

integrand <- function(x) {sin(x^2)}
integrate(integrand, lower = sqrt(pi/6), upper = sqrt(pi/3))

integrand <- function(x) {asin(x)}
integrate(integrand, lower = 1/2, upper = sqrt(3)/2)

lines(x, sqrt(asin(x)))
grid()

integrand1 <- function(x) {
  k <- 1
  for (j in 1:1) {k <- k * cos(j * x)}
  return(k)
}
integrate(integrand1, lower = 0, upper = 2*pi)

integrand2 <- function(x) {
  k <- 2
  for (j in 1:1) {k <- k * cos(j * x)}
  return(k)
}
integrate(integrand2, lower = 0, upper = 2*pi)

integrand3 <- function(x) {
  k <- 3
  for (j in 1:1) {k <- k * cos(j * x)}
  return(k)
}
integrate(integrand3, lower = 0, upper = 2*pi)

integrand4 <- function(x) {
  k <- 4
  for (j in 1:1) {k <- k * cos(j * x)}
  return(k)
}
integrate(integrand4, lower = 0, upper = 2*pi)


a <- 1
for (i in 1:10) {
  a <- a / (1 + i * a)
  print(paste(i, a))
}

r <- sapply(1:10000, function(x){
  a <- sample(1:10, 1)
  b <- sample(1:10, 1)
  ifelse(sum(sort(a)==sort(b))==TRUE, TRUE, FALSE)
})
c <- table(r)
c[2] / sum(c)
c[1] / sum(c)


m <- 3
n <- 9
#Сочетание
factorial(n)/(factorial(n-m) * factorial(m))
#Размещение
factorial(n)/(factorial(n-m))


factorial(9) / (factorial(6) * factorial(3))
