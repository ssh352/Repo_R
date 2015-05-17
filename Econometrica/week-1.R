library(psych)
library(dplyr)
library(ggplot2)
library(GGally)

############################
# Cars
###########################

d <- mutate(cars, speed=1.67*speed, dist=0.3*dist, ratio=dist/speed)
head(d)
head(cars)

model <- lm(data=d, dist~speed)
model

beta_hat <- coef(model)
beta_hat
eps_hat <- residuals(model)
eps_hat
y <- d$dist
y_hat <- fitted(model)
y
y_hat
cbind(y,y_hat)

RSS <- deviance(model)
RSS
TSS <- sum( (y - mean(y))^2 )
TSS
ESS <- TSS - RSS
R2 <- ESS/TSS
R2
R2 <- cor(y, y_hat)^2
R2
X <- model.matrix(model)
X

nd <- data.frame(speed=c(40,60))
nd
predict(model, nd)
qplot(data=d, speed, dist) + stat_smooth(method = "lm")

############################
# Fertility
###########################
help(swiss)

t <- swiss
t
str(t)
describe(t)
ggpairs(t)

model2 <- lm(data=t, Fertility~Agriculture+Education+Catholic)
coef(model2)
fitted(model2)
residuals(model2)
deviance(model2)
report <- summary(model2)
report$r.squared

R2 <- cor(t$Fertility, fitted(model2))^2
R2

nd2 <- data.frame(Agriculture=0.5, Education=20, Catholic=0.5)
predict(model2, nd2)

#test
m <- lm(data=mtcars, mpg~cyl+hp+wt+am)
round(summary(m)$r.squared,2)

m <- lm(data=mtcars, mpg~disp+hp+wt+am)
summary(m)

m1 <- lm(data=mtcars, mpg~disp+hp+wt+am)
summary(m1)$r.squared
m2 <- lm(data=mtcars, mpg~cyl+hp+wt+am)
summary(m2)$r.squared
m3 <- lm(data=mtcars, mpg~disp+cyl+wt+am)
summary(m3)$r.squared
m4 <- lm(data=mtcars, mpg~disp+hp+cyl+am)
summary(m4)$r.squared
