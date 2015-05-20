setwd("D:/GitHub/Repo_R/Econometrica/Week 3")

library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")
library(GGally)
library(psych)
#install.packages(c("vcd","knitr","pander"))
#memisc, lmtest, ggplot2, foreign, vcd, devtools, hexbin, pander, sjPlot, knitr
h <- diamonds
glimpse(h)

qplot(data=h, carat, price)
bg <- qplot(data=h, log(carat), log(price))
bg + geom_hex()

f <- read.csv("flats_moscow.txt", header = TRUE, sep="\t", dec=".")
head(f)
qplot(data=f, totsp, price)
qplot(data=f, log(totsp), log(price)) + geom_hex()

mosaic(data=f, ~walk+brick+floor, shade=TRUE)

f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)

qplot(data=f, price)
qplot(data=f, log(price))
qplot(data=f, log(price), fill=brick)
qplot(data=f, log(price), fill=brick, position = "dodge")
g2 <- qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)
g2 + facet_grid(walk~floor)
g2 + facet_grid(~floor)
g2 + facet_grid(~walk)

y.m <- mean(f$price)
model_0 <- lm(data=f, log(price)~log(totsp))
y0 <- exp(fitted(model_0))
ESSr <- sum((y0 - y.m)^2)
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))
y2 <- exp(fitted(model_2))
ESSur <- sum((y2 - y.m)^2)
model_2b <- lm(data=f, log(price)~log(totsp)*brick)
model_3 <- lm(data=f, log(price)~log(totsp)+brick:log(totsp))

mtable(model_0, model_1, model_2, model_2b, model_3)

sjp.lm(model_2)

nw <- data.frame(totsp=c(60,60), brick=factor(c(1,0)))

predict(model_2, nw)
exp(predict(model_2, nw))
exp(predict(model_2, nw, interval = "confidence"))
exp(predict(model_2, nw, interval = "prediction"))

waldtest(model_0, model_1) # H_0: true model_0 is rejected
waldtest(model_1, model_2) # H_0: true model_1 is rejected
waldtest(model_0, model_2) # H_0: true model_0 is rejected

gg0 <- qplot(data=f, log(totsp), log(price))
gg0 + stat_smooth(method = "lm") + facet_grid(~walk)
gg0 + aes(col=brick) + stat_smooth(method = "lm") + facet_grid(~walk)

f$nonbrick <- memisc::recode(f$brick, 1 <- 0, 0 <- 1)
glimpse(f)

model_wrong <- lm(data=f, log(price)~log(totsp)+brick+nonbrick)
mtable(model_0, model_1, model_2, model_2b, model_3, model_wrong)
summary(model_wrong)


resettest(model_2) # С пятипроцентной точностью, дополнительные переменные добавлять не надо

#test
# Q 2
#wageˆi=250+15schoolingi+55experiencei. ESS=125, TSS=200.
#wageˆi=250+15schoolingi+55experiencei. + mschoolingi и fschoolingi ESS=175.
# TSS = RSS + ESS
# RSS = TSS - ESS
r <- 2
n <- 25
kur <- 5
ESSr <- 105
TSS <- 200
ESSur <- 175
RSSur <- TSS - ESSur
RSSr <- TSS - ESSr
R2ur <- ESSur/TSS
R2r <- ESSr / TSS
alpha <- 0.05

F <- ((RSSr - RSSur) / r) / RSSur / (n - kur)
F
f.cr <- qf(1 - alpha, df1 = r, df2= n)
f.cr
F > f.cr #H_0 гипотеза не отвергается, новые переменные могут быть равны 0

r <- 2
n <- 25
F <- ((RSSr - RSSur) / r) / RSSur / (n - kur)
qf(0.99, df1 = r, df2= n)
#H_0 гипотеза не отвергается

#Q 3
n <- 2040
beta.1 <- -80
beta.l <- 1.9
alpha <- 0.05
sigma2 <- 1259.265
v <- matrix(c(21.9, -0.46, -0.46, 0.01), ncol=2, nrow=2)
livesp <- 150

var.st <- v[1,1] + livesp^2 * v[2,2] + 2 * livesp * v[1,2]
var.st

#Q 4
var.pr <- round(sigma2 + var.st, 3)
var.pr

#Q 11
d <- diamonds
nrow(d)

#Q 12
summary(lm(data=d, log(price)~log(carat)))
summary(lm(data=d, log(price)~carat))

#Q 13
m13 <- lm(data=d, price~carat+x)
summary(m13)
#F-statistic: 1.57e+05 on 2 and 53937 DF,  p-value: < 2.2e-16
m13 <- lm(data=d, price~carat+y)
summary(m13)
#F-statistic: 1.57e+05 on 2 and 53937 DF,  p-value: < 2.2e-16

#Q 14
m14 <- lm(data=d, price~carat)
summary(m14)

#Q 15
m15 <- lm(data=d, price~carat+depth)
mtable(m15)
m15 <- lm(data=d, price~carat+depth+cut)
mtable(m15)
m15 <- lm(data=d, price~carat)
mtable(m15)

#Q 16
m16_1 <- lm(data=d, price~carat)
m16_2 <- lm(data=d, price~carat+depth)
m16_3 <- lm(data=d, price~carat+depth+cut)
mtable(m16_1, m16_2, m16_3)

#Q 17
m17_1 <- lm(data=d, price~carat+depth)
m17_2 <- lm(data=d, price~carat+depth+cut)
waldtest(m17_1, m17_2)
m17_1 <- lm(data=d, price~carat)
m17_2 <- lm(data=d, price~carat+depth)
waldtest(m17_1, m17_2)

#Q 18
m18 <- lm(data=d, price~carat+depth+cut)
resettest(m18)
m18 <- lm(data=d, price~carat)
resettest(m18)

#Q 19
qplot(data = d, log(price), fill=color, geom = "density", alpha = 0.5) + facet_wrap(~color)

#Q 20
df <- diamonds
d <- select(df, clarity, price, cut)
head(d)
a <- aggregate(price~clarity+cut, data=d, FUN=mean)
head(a)
a[order(-a$price),]
a <- aggregate(price~clarity, data=d, FUN=mean)
a[order(-a$price),]
qplot(data=df, log(carat), log(price), color = clarity) + facet_wrap(~cut) 

#test
v <- c(1, 2, 3, 4, 5)
se <- sd(v)/sqrt(length(v))
sqrt(var(v))

#Lections
#3.1.2
f <- read.csv("flats_moscow.txt", header = TRUE, sep="\t", dec=".")
f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)

m <- lm(data=f, price~totsp)
summary(m)

v <- vcov(m)
v
c <- coef(m)
k <- length(c)
n <- nrow(f)

sigma2 <- sum(m$residuals^2)/(n-k)

nd <- data.frame(totsp=c(60))
p.price <- as.numeric(predict(m, nd))
se.y <- sd(fitted(m))/sqrt(n)

#Доверительный интервал ????
c(p.price - 2 * se.y, p.price + 2 * se.y)

#3.1.8
beta.1 <- -2.5
beta.s <- 0.6
beta.e <- 0.157
alpha <- 0.05
R.2 <- 0.09
n <- 3294
k <- 3

F <- (R.2 / (k-1)) / ((1 - R.2) / (n - k))
f.cr <- qf(1-alpha, df1=2, df2=n)
F > f.cr # Если true, гипотеза о незначимости бета отвергается
#Хотя бы один из регрессоров значим

#3.1.10
beta.1 <- -3.3
beta.s <- 0.95
beta.e <- 0.25
beta.y2 <- -0.264
beta.y3 <- 0.024
R.2ur <- 0.095
R.2r <- 0.091
n <- 3294
k <- 5
r <- 2

F <- ((R.2ur - R.2r) / r) / ((1 - R.2ur) /  (n - k))
f.cr <- qf(0.95, df1=r, df2=n-k)
F > f.cr # если true - значит есть пропущенные переменные, гипотеза Ho отвергается

