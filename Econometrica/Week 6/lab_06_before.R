# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

library("lubridate") # работа с датами

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

library("devtools")

#install_github("dgrtwo/broom")
#install_github("cran/bstats")
#install.packages("rusquant",repos="http://r-forge.r-project.org", type="source")
#install_github("bdemeshev/sophisthse")
#install.packages("Quandl")

library("quantmod") # загрузка с finance.google.com
library("rusquant") # загрузка с finam.ru
library("sophisthse") # загрузка с sophist.hse.ru
library("Quandl") # загрузка с Quandl

x <- c("2014-04-15", "2011-08-17")
y <- ymd(x)
y + days(20)
y - years(10)
day(y)
month(y)
vignette("lubridate")

####
x <- rnorm(5)
x
y <- ymd("2014-01-01") + days(0:4)
y

ts <- zoo(x, order.by=y)
ts

lag(ts, -1) # Вчерашнее значение
lag(ts, 1) # Завтрашнее
diff(ts) # Разница между значениям

ts2 <- zooreg(x, start=as.yearqtr("2014-01"), freq=4)
ts2

ts3 <- zooreg(x, start=as.yearmon("2014-01"), freq=12)
ts3

data("Investment")
help("Investment")

start(Investment)
end(Investment)
time(Investment)
coredata(Investment)

dna <- Investment
dna[1,2] <- NA
dna[5,3] <- NA

#Зполнение пропущенных значений
na.approx(dna)[5,3]   #Линейная апроксимация между двумя значениями
na.locf(dna)[5,3]     #Подстановка предыдущего значения
Investment[5,3]

# Загрузка данных
a <- sophisthse("POPNUM_Y")
plot(a)

# quandl
b <- Quandl("FRED/GNP")
plot(b, type="l")

# finance.google.com
Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "AAPL", from="2010-01-01", to="2014-02-03", src="google")
head(AAPL)
tail(AAPL)

# finam.ru
getSymbols(Symbols = "GAZP", from="2011-01-02", to="2014-09-09", src="Finam")
head(GAZP)
tail(GAZP)

plot(GAZP)

autoplot(GAZP[,1:4])
autoplot(GAZP[,1:4], facets = NULL)

chartSeries(GAZP)

#6.2.4
d <- as.zoo(Investment)
ggpairs(Investment)
ggpairs(log(Investment))
autoplot(d[,1:2], facets = NULL)

model1 <- lm(data=d, RealInv~RealInt + RealGNP)
summary(model1)
coeftest(model1)
confint(model1)

d_aug <- augment(model1, as.data.frame(d))
glimpse(d_aug)
qplot(data=d_aug, lag(.resid), .resid) # Похоже на автокорреляцию

vcov(model1)
vcovHAC(model1)

conftable <- coeftest(model1, vcov. = vcovHAC(model1))
ci <- data.frame(estimate=conftable[,1], se_ac=conftable[,2])

ci <- mutate(ci, left_95=estimate - 1.96 * se_ac, rigth_95=estimate + 1.96 * se_ac)

#6.2.5
# Durbin-Watson
#H0: нет автокорреляции
#Ha: автокорреляция 1го порядка
dwt(model1)
res <- dwt(model1)
res$dw
res$p
res$r

#BG-test
#H0: нет автокорреляции
#Ha: автокорреляция любого порядка
bgtest(model1, order = 2) # Не отвергается
res <- bgtest(model1, order = 2)
res$statistic
res$p.value

####
model2 <- lm(data=d, RealInv~ . )
summary(model2)

model3 <- lm(data=d, RealInv~ Investment + RealGNP )
summary(model3)

d.log <- log(d)
is.na(d.log) <- NA
d.log <- na.approx(d.log)
d.log[1, 7] <- d.log[2, 7]

model4 <- lm(data=d.log, RealInv~ . )
summary(model4)

model5 <- lm(data=d.log, RealInv ~ Investment + Price )
summary(model5)

mtable(model1, model2, model3, model4, model5, summary.stats = TRUE, ddigits = 2)

#test
library(Ecdat)

#q 11
h <- Griliches
head(h)

m.g <- lm(data=h, lw80 ~ age80 + iq + school80 + expr80)
summary(m.g)
vcov(m.g)

#q 12
library(sandwich)
abs(vcov(m.g)[3,2] - vcovHC(m.g)[3,2])

#q 13
v <- c(
  vcovHC(m.g, type="HC0")[4,4],
  vcovHC(m.g, type="HC2")[4,4],
  vcovHC(m.g, type="HC4")[4,4],
  vcovHC(m.g, type="HC5")[4,4]
)
names(v) <- c("HC0","HC2","HC4","HC5")  
min(v)

#q 14
bptest(m.g, data=h, varformula = ~age80)

#q 15
gqtest(m.g, order.by = ~age80, data=h, fraction = 0.2)

#q 16
h <- Solow 
m.s <- lm(data=Solow, q ~ k + A)
summary(m.s)
abs(vcovHAC(m.s)[3,3] - vcov(m.s)[3,3])

#q 17
m.s2 <- lm(data=Solow, q ~ k)
dwt(m.s2)

#q 18
m.s3 <- lm(data=Solow, q ~ A)
bgtest(m.s3, order=3)

#q 20
Sys.setlocale("LC_TIME","C") 
getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="google") 
d <- MSFT
names(d)

ts2 <- zooreg(x, start=as.yearqtr("2014-01"), freq=4)
ts <- zooreg(d$MSFT.Close, start=as.Date(index(d)[1]))
head(ts)
head(lag(ts))

nd <- cbind(d$MSFT.Close)
m.m <- lm(data=ts, MSFT.Close ~ lag(MSFT.Close, 1) + lag(MSFT.Close, 2))
round(summary(m.m)$r.squared, 2)
