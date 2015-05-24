# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики
library(Ecdat)
library(GGally)
library(memisc)
#install.packages("Ecdat")

h <- read.table("flats_moscow.txt", header = TRUE)
h.s <- as.data.frame(scale(h))
head(h)
head(h.s)

#Разброс данных тем выше чем больше площадь, что говорит о типичной гетероскедастичности
qplot(data=h, x=log(totsp), y=log(price))

model <- lm(data=h, price~totsp)
summary(model)

coeftest(model)
confint(model)

vcov(model)
vcovHC(model)

h <- augment(model, h)
glimpse(h)

qplot(data=h, totsp, abs(.resid))
vcovHC(model, type="HC2")

coeftest(model)
coeftest(model,vcov. = vcovHC(model))

conftable <- coeftest(model,vcov. = vcovHC(model))
ci <- data.frame(estimate=conftable[,1], se_hc=conftable[,2])
ci <- mutate(ci, left_ci=estimate - 1.96*se_hc, right_ci=estimate + 1.96*se_hc)
ci

#5.2.5
bptest(model)
bptest(model, data=h, varformula = ~ totsp + I(totsp^2))
#Тест Уайта
bptest(model, data=h, varformula = ~ poly(totsp, 2))

#Тест Голдфельда-Квандта
gqtest(model, order.by = ~totsp, data=h, fraction = 0.2)

model.log <- lm(data=h, log(price)~(totsp))
bptest(model.log, data=h, varformula = ~ poly(totsp, 2))
gqtest(model.log, order.by = ~totsp, data=h, fraction = 0.2)


#Exam
#Q 5
h <- ChickWeight
head(h)
round(aggregate(data=h, weight~Time, FUN=mean),2)

#Q 6
res <- round(aggregate(data=h, weight~Time+as.numeric(Diet), FUN=mean),2)
res[order(res$Time, res$weight), ]

#Q 7

m.ch <- lm(data=ChickWeight, weight~Time+Diet)
r <- summary(m.ch)
round(r$r.squared,2)

#Q 9
b2 <- 5
b3 <- 6
se.b2 <- 0.7
se.b3 <- 0.5
cov.b2b3 <- 0.25
H.b2b3 <- 10

t <- (b2 - b3 - H.b2b3) / (se.b2^2 + se.b3^2 - 2 * cov.b2b3)
round(t,3)

#Q 12
h <- diamonds
qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)

#Q 14
m.dm <- lm(data=diamonds, price~carat + table + x + y + depth)
summary(m.dm)
round(confint(m.dm, level=0.90)['table', '5 %'], 2)

#Q 19
h <- BudgetFood
head(h)

m.bf <- lm(data=BudgetFood, wfood~totexp+size)
nd <- data.frame(totexp=700000, size=4)
round(predict(m.bf, nd, level=0.9, interval="prediction"), 2)

#Q 20
m.bf <- lm(data=BudgetFood, wfood~totexp+size)
reset(m.bf)

#Q 21
h <- na.omit(BudgetFood)
h$sex <- as.numeric(h$sex) - 1
m.bf <- lm(data=h, wfood~totexp+size)
m.bf.sex <- lm(data=h, wfood~totexp+size+totexp:sex+totexp:sex + sex)
m.bf.sex <- lm(data=h, wfood~totexp + size + I(totexp*sex) + I(totexp*sex))
m.bf.sex <- lm(data=h, wfood~totexp + size + sex)
waldtest(m.bf, m.bf.sex)

str(h)

#Q 24
r2.s <- 0.8
r2.e <- 0.65
r2.f <- 0.7

max(1/(1 - c(r2.s, r2.e, r2.f)))

#Q 26
m.c <- lm(data=mtcars, mpg ~ disp + hp + wt)
summary(m.c)
ggpairs(mtcars[,c("mpg", "disp", "hp", "wt")])
round(max(vif(m.c)), 2)

#Q 27
pca.mc <- prcomp(mtcars[,c("disp", "hp", "wt")], scale=TRUE)
biplot(pca.mc, xlim=c(-1,1))
str(pca.mc)
round(max(pca.mc$x[,"PC1"]), 2)

#Q 28
r <- summary(pca.mc)
str(r)
r$importance[3, "PC3"] - r$importance[3, "PC2"]

pca.mc <- prcomp(mtcars[,c("disp", "hp", "wt")], scale=TRUE)
data.pca3.old <- as.data.frame(cbind(mpg=mtcars$mpg, pc1=pca.mc$x[, "PC1"], pc2=pca.mc$x[, "PC2"], pc3=pca.mc$x[, "PC3"]))
m.pca2 <- lm(data=data.pca3.old, mpg ~ pc1 + pc2)
m.pca3 <- lm(data=data.pca3.old, mpg ~ pc1 + pc2 + pc3)
mtable(m.pca2, m.pca3)
summary(m.pca3)$r.squared - summary(m.pca2)$r.squared


X0 <- model.matrix(data=mtcars, mpg~0+disp+hp+wt)
data.pca3 <- prcomp(X0, scale=TRUE)

22/28
