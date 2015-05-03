r1 <- c(1,1,1,1,0)
r2 <- c(1,2,1,1,0)
r3 <- c(1,1,3,1,0)
r4 <- c(1,1,1,4,0)
r5 <- c(1,2,3,4,0)

R <- as.matrix(rbind(r1,r2,r3,r4,r5))
y <-qr(R)
y$rank

for(a in seq(0,3,0.01)) {
  a1 <- c( (a - 1),   (a - 1),   (a*(a - 1)) )
  a2 <- c( (2*a - 3), (3*a - 5), (a^2 - 2) )
  a3 <- c((-1),       (a - 3),   (4*a - 5) )
  A <- as.matrix(rbind(a1, a2, a3))
  y <- qr(A)
  if (y$rank == 2) {
    print(paste(a,">",y$rank))
  }
}

v1 <- c( 6, -2,  2)
v2 <- c(-2,  5,  0)
v3 <- c( 2,  0,  7)
V <- as.matrix(rbind(v1,v2,v3))

s <- eigen(V)
