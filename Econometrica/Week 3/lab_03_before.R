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
#install.packages(c("vcd","knitr","pander"))

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

qplot(data=f, log(price))
qplot(data=f, log(price), fill=brick)
qplot(data=f, log(price), fill=brick, position = "dodge")
g2 <- qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)
g2 + facet_grid(walk~floor)
g2 + facet_grid(~floor)

model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))
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
waldtest(model_3, model_2) # H_0: true model_3 is rejected

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
# Q 1
#wageˆi=250+15schoolingi+55experiencei. ESS=125, TSS=200.
#wageˆi=250+15schoolingi+55experiencei. + mschoolingi и fschoolingi ESS=175.
# TSS = RSS + ESS
# RSS = TSS - ESS
r <- 2
n <- 40
kur <- 3
ESSr <- 125
TSSr <- 200
ESSur <- 175
RSSur <- TSSr - ESSur
RSSr <- TSSr - ESSr

F <- ((RSSr - RSSur) / r) / RSSur / (n - kur)
qf(0.99, df1 = r, df2= n)
#H_0 гипотеза не отвергается

r <- 2
n <- 25
F <- ((RSSr - RSSur) / r) / RSSur / (n - kur)
qf(0.99, df1 = r, df2= n)
#H_0 гипотеза не отвергается

#Q 11
d <- diamonds
nrow(d)

#Q 12
summary(lm(data=d, log(price)~log(carat)))

#Q 13
m13 <- lm(data=d, price~carat+x)
summary(m13)
#F-statistic: 1.57e+05 on 2 and 53937 DF,  p-value: < 2.2e-16

#Q 14
m14 <- lm(data=d, price~carat)
summary(m14)

#Q 15
m15 <- lm(data=d, price~carat+depth)
summary(m15)
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

#Q 18
m18 <- lm(data=d, price~carat+depth+cut)
resettest(m18)

#Q 19
qplot(data = d, log(price), fill=color, geom = "density", alpha = 0.5) + facet_wrap(~color)

#Q 20
qplot(data=d, log(carat), log(price), color = clarity) + facet_wrap(~cut) 
