setwd("D:/GitHub/Repo_R/Econometrica/Week 9")

# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

# lab 09

library("dplyr") # манипуляции с данными
library("caret") # стандартизованный подход к регрессионным и классификационным моделям
library("AER") # инструментальные переменные 
library("ggplot2") # графики
library("sandwich") # робастные стандартные ошибки
library("ivpack") # дополнительные плющки для инструментальных переменных
library("memisc") # табличка mtable 
#install.packages(c("caret","AER","sandwich","ivpack"))

#9.2.1
h <- read.csv("flats_moscow.txt", header = TRUE, sep="\t", dec=".")
head(h)

in_train <- createDataPartition(y = h$price, p=0.75, list=FALSE)
h_train <- h[in_train, ]
h_test <- h[-in_train, ]
nrow(h_train)
nrow(h_test)

model_1 <- lm(data=h_train, log(price) ~ log(totsp) + log(kitsp) + log(livesp) )
model_2 <- lm(data=h_train, log(price) ~ log(totsp) + brick )

mtable(model_1, model_2)

y_hat_1 <- exp(predict(model_1, h_test))
y_hat_2 <- exp(predict(model_2, h_test))

y <- h_test$price
sum((y-y_hat_1)^2)
sum((y-y_hat_2)^2)

#9.2.2
#Эндогенность
data("CigarettesSW")
h <- CigarettesSW

h2 <- mutate(h, rprice = price/cpi, rincome=income/cpi/population, rtax=tax/cpi)
h3 <- filter(h2, year=="1995")
head(h3)

model_0 <- lm(data=h3, log(packs)~log(rprice))
summary(model_0)

#two stage OLS
st_1 <- lm(data=h3, log(price)~tax)
h3$log_price_hat <- fitted(st_1)

st_2 <- lm(data=h3, log(packs)~log_price_hat)
summary(st_2)

model_iv<- ivreg(data=h3, log(packs)~log(rprice)|rtax)
summary(model_iv)
mtable(model_0, st_2, model_iv)

#9.2.3
# Робастные стандартные ошибки
coeftest(model_iv, vcov=vcovHC)

# Наличие экзогенных регрессоров
model_iv_2<- ivreg(data=h3, log(packs)~log(rprice) + log(rincome)|log(rincome) + rtax)
summary(model_iv_2)
coeftest(model_iv_2, vcov=vcovHC)

# Несколько инструментальных переменных
h3 <- mutate(h3, rtax2 = (taxs - tax)/cpi)
head(h3)
model_iv_3<- ivreg(data=h3, log(packs)~log(rprice) + log(rincome)|log(rincome) + rtax + rtax2)
summary(model_iv_3)
coeftest(model_iv_3, vcov=vcovHC)
mtable(model_iv_2, model_iv_3)
