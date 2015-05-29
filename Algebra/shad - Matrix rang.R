
cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#########################################
#A. Предел последовательности - 0.5
#########################################
#http://kontromat.ru/?page_id=1346
options(digits = 15)
c <- numeric(0)

c <- sapply(1:400000, function(i) {
  return(as.double((0 + i - 1) * i / 2) / i^2)
})

plot(c, type="l")
grid()
tail(c)

library(rSymPy)
sympy("var('n')")
n <- Var("n")
y <- n * n
Sym("limit((", n, "-1)/(2*", n, "), ",n ,", oo)")
sympy("limit((n-1)/(2*n), n, oo)")

#########################################
#B. Предел функции - 0 (скорее всего так и есть)
#########################################
c <- numeric(0)

c <- sapply(seq(0,-7.9999,-0.0001), function(x) {
  return((sqrt(1 - x) - 3) / (2 + (x^2) ^ (1/6)))
})

plot(c, type="l")
grid()
tail(c)

#library(rSymPy)
#sympy("var('x')")
#x <- Var("x")
#sympy("limit( ( ( 1 - x )**1/2 - 3) / ( 2 + x**1/3), x, -8)")

#########################################
#C. Интеграл -  3.64547
#########################################
options(digits = 6)
x <- seq(0,1,0.1)
y <- x^2 * exp(3*x)
integrand <- function(x) {x^2 * exp(3*x)}
plot(x,y, type="l")
integrate(integrand, lower = 0, upper = 1)

#########################################
#D. Собственный вектор (6, -3, 6) - Собственное значение 9
#########################################
v1 <- c( 6, -2,  2)
v2 <- c(-2,  5,  0)
v3 <- c( 2,  0,  7)
V <- as.matrix(rbind(v1,v2,v3))

s <- eigen(V)
s

#########################################
#E. Ранг матрицы - значение а = 2
#########################################
r1 <- c(1,1,1,1,0)
r2 <- c(1,2,1,1,0)
r3 <- c(1,1,3,1,0)
r4 <- c(1,1,1,4,0)
r5 <- c(1,2,3,4,0)

R <- as.matrix(rbind(r1,r2,r3,r4,r5))
y <-qr(R)
y$rank

for(a in seq(0,30,0.01)) {
  a1 <- c( (a - 1),   (a - 1),   (a*(a - 1)) )
  a2 <- c( (2*a - 3), (3*a - 5), (a^2 - 2) )
  a3 <- c((-1),       (a - 3),   (4*a - 5) )
  A <- as.matrix(rbind(a1, a2, a3))
  y <- qr(A)
  if (y$rank == 2) {
    print(paste(a,">",y$rank))
  }
}


#########################################
#F. Подстановки
#########################################
#http://mmtb.uginfo.sfedu.ru/algebra/topic3/02/02.html
#http://mathhelpplanet.com/viewtopic.php?f=32&t=29598
A <- matrix(rbind(c(1,2,3,4,5,6,7), c(2,7,5,4,6,1,3)), nrow = 2, ncol = 7)
B <- matrix(rbind(c(1,2,3,4,5,6,7), c(1,4,7,3,6,5,2)), nrow = 2, ncol = 7)
C <- matrix(rbind(c(1,2,3,4,5,6,7), c(5,6,2,1,7,4,3)), nrow = 2, ncol = 7)

rearrange_inv <- function(X) {
  return(rbind(X[2,],X[1,]))
}

rearrange_sort <- function(X) {
  return(X[,order(X[1,])])
}

rearrange_mult <- function(X,Y) {
  sort_X <- rearrange_sort(X)
  sort_Y <- rearrange_sort(Y)
  return(rbind(sort_Y[1,],sort_X[2,sort_Y[2,]]))
}

rearrange_count <- function(X) {
  return(
    sum(sapply(1:ncol(X),function(i){
      length(which(X[2,i] < X[2,c(1:i)]))
    }))
  )
}

A_inv <- rearrange_inv(A)
B_inv <- rearrange_inv(B)

X <- rearrange_mult(rearrange_mult(A_inv, C), B_inv)
rearrange_count(X)

#########################################
#G. Солдатики - 17640
#########################################
library(gtools)
perm <- permutations(3,13,c("red","blue","green"), set=TRUE, repeats.allowed = TRUE)
ix <- sapply(1:nrow(perm), function(i) {
  r <- length(which(perm[i,] == "red"))
  b <- length(which(perm[i,] == "blue"))
  g <- length(which(perm[i,] == "green"))
  if (r == 6 & b == 3 & g == 4) return(i)
})

ix <- unlist(ix)

all_perm <- perm[ix,]
ix_if <- sapply(1:nrow(all_perm), function(i){
  if (min(diff(which(all_perm[i,]=="green"))) > 1) return(i)
})
ix_if <- unlist(ix_if)
result <- all_perm[ix_if,]


#########################################
#H. Синие дубы - 0.1625
#########################################
#пассажирские  0,75	0,2	0,15
#грузовые	0,25	0,05	0,0125
