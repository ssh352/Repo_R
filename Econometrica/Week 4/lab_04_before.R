# ESLI RUSSKIE BUKVI NE VIDNI --->
# File -- Reopen with encoding --- utf8 --- set as default --- ok


library("HSAUR") # из этого пакета возьмем набор данных по семиборью
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тесты для линейных моделей
library("glmnet") # LASSO + ridge
library("ggplot2") # графики
library("car") # vif
library(GGally)
#install.packages("GGally")

#4.2.1
h <- cars

qplot(data=h, speed, dist)

model <- lm(data=h, dist~speed)
summary(model)

h <- mutate(h, speed2 = speed^2, speed3 = speed^3)
model_mk <- lm(data=h, dist~speed + speed2 + speed3)
summary(model_mk)
plot(model_mk)

vif(model_mk)
X0 <- model.matrix(data=h, dist~0 + speed + speed2 + speed3)
cor(X0)

nd <- data.frame(speed=10, speed2=100, speed3=1000)
predict(model, newdata=nd, interval="prediction")
predict(model_mk, newdata=nd, interval="prediction")

confint(model)
confint(model_mk)

model_mk <- lm(data=h, dist~speed + speed3)
summary(model_mk)
vif(model_mk)

#4.2.2 - Ридж и LASSO

y <- h$dist
X0 <- model.matrix(data=h, dist~0 + speed + speed2 + speed3)
head(X0)

# LASSO
lambdas <- seq(50, 0.1, length=30)
m_lasso <- glmnet(X0, y, alpha=1, lambda=lambdas)
#Как зависит от лямбда коэффиценты
plot(m_lasso, xvar="lambda", label=TRUE)

plot(m_lasso, xvar="dev", label=TRUE)

plot(m_lasso, xvar="norm", label=TRUE)


coef(m_lasso, s=c(0.1, 1))
head(m_lasso)

#4.2.3
lambdas <- seq(50, 0.1, length=30)
m_rr <- glmnet(X0, y, alpha=0, lambda=lambdas)

# Cross validation
cv <- cv.glmnet(X0, y, alpha=1)
plot(cv)
cv$lambda.min
cv$lambda.1se

coef(cv, s="lambda.1se")
coef(cv, s="lambda.min")

# 4.2.4
#PCA
h <- heptathlon
glimpse(h)
h <- dplyr::select(h, -score)
describe(h)
describe(scale(h))

cor(h)
h.pca <- prcomp(h, scale=TRUE)
pca1 <- h.pca$x[,1]
head(pca1)
v1 <- h.pca$rotation[,1]
v1

summary(h.pca)
#Корреляция между методом главных компонент и оценкой олимпийского комитета
cor(heptathlon$score, pca1)
#Какой объем дисперсии объясняет каждая компонента
plot(h.pca)
biplot(h.pca, xlim=c(-1,1))

#TEST
#Q 8
R2.s <- 0.8
1 / (1 - R2.s)

#Q 15
h <- airquality
ggpairs(h)

#Q 16
model_mk <- lm(data=h, Ozone~Solar.R + Wind + Temp)
summary(model_mk)
round(vif(model_mk), 3)

#Q 17
h.cl <- na.omit(h)
X17 <- model.matrix(data=h.cl, Ozone~0 + Solar.R + Wind + Temp)
y <- h.cl$Ozone
lambdas <- seq(50, 0.1, length=30)
m_lasso <- glmnet(X17, y, alpha=1, lambda=lambdas)
round(coef(m_lasso), 3)
round(coef(m_lasso, s=1), 3)

#Q 18
m_rr <- glmnet(X17, y, alpha=0, lambda=lambdas)
round(coef(m_rr, s=2), 3)

#Q 20
h.pca <- prcomp(X17, scale=TRUE)
pca1 <- h.pca$x[,1]
head(pca1)
v1 <- h.pca$rotation[,1]
v1

summary(h.pca)
#Корреляция между методом главных компонент и оценкой олимпийского комитета
cor(airquality$Solar.R, pca1)
#Какой объем дисперсии объясняет каждая компонента
plot(h.pca)
biplot(h.pca, xlim=c(-1,1))
