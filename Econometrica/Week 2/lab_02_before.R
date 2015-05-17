setwd("D:/GitHub/Repo_R/Econometrica/Week 2")

# if you see KRAKOZYABRY then do 
# "File" - "Reopen with encoding" - "UTF-8" - (Set as default) - OK

# lab 2

# загружаем пакеты
library("memisc") # две и более регрессий в одной табличке
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тестирование гипотез в линейных моделях
library("sjPlot") # графики
library("sgof")
library("ggplot2") # графики
library("foreign") # загрузка данных в разных форматах
library("car")
library("hexbin") # графики
library("devtools")
#devtools::install_github("bdemeshev/rlms")
library("rlms") # загрузка данных в формате rlms (spss)

#Генерируем случайные велечины
z <- rnorm(100, 5, 3)
z
describe(z)
hist(z, 20)

# Функция плотности
x <- seq(-10,15, by=0.5)
y <- dnorm(x, mean = 5, sd = 3)

qplot(x,y, geom="line")

# P(Z<3)
# P(Z<3)=F(3)

pnorm(3, mean=5, sd=3)

# P(Z in [4:9])
# P(Z<9)-P(Z<4)
pnorm(9, mean=5, sd=3)-pnorm(4, mean=5, sd=3)

# P(Z<a)=0.7 a?
qnorm(0.7, mean=5, sd=3)


# Множественная регрессия
h <- swiss
glimpse(h)

model <- lm(data=h, Fertility~Catholic+Agriculture+Examination)
summary(model)
coef(model)
coeftest(model)
confint(model)
sjp.lm(model)

# Проверка гипотезы b_Cath = b_Agri
model_aux <- lm(data=h, Fertility~Catholic+I(Catholic+Agriculture)+Examination)
sjp.lm(model_aux)
summary(model_aux)

linearHypothesis(model, "Catholic-Agriculture=0")

# Стандартизация

h_st <- mutate_each(h, "scale")
model_st <- lm(data=h_st, Fertility~Catholic+Agriculture+Examination)
summary(model_st)
sjp.lm(model_st)
sjp.lm(model_st, showStandardBeta = TRUE)

# Искусственный эксперимент
d <- matrix(nrow=100, rnorm(100*41,mean=0,sd=1))
df <- data.frame(d)
glimpse(df)

model_pusto <- lm(data=df, X1~.)
summary(model_pusto)

# Сравнить несколько моделей
model2 <- lm(data=h, Fertility~Catholic+Agriculture)
compar_12 <- mtable(model, model2)
compar_12

#Сохранение результатов
stuff <- list(data=h, model=model2)
saveRDS(stuff, file="mydata.RDS")

mylist <- readRDS(file = "mydata.RDS")

# CSV
t <- read.csv("flats_moscow.txt", header = TRUE, sep="\t", dec=".")
head(t)
mod_3 <- lm(data=t, price ~ totsp+brick)
sjp.lm(mod_3)
summary(mod_3)

t_st <- mutate_each(t, "scale")
mod_3_st <- lm(data=t_st, price ~ totsp+brick)
sjp.lm(mod_3_st)
summary(mod_3_st)


# RLMS

h <- read.rlms("r21i_os24a.sav")
saveRDS(h, "r21i_os24a.RDS")

head(h)

h2 <- dplyr::select(h, qm1, qm2, qh6, qh5)
head(h2)
describe(h2)

h3 <- dplyr::rename(h2, ves=qm1, rost=qm2, sex=qh5, b_year=qh6)
h3 <- mutate(h3, vozrast = 2015 - b_year)
head(h3)

h4 <- filter(h3, sex == "МУЖСКОЙ")

qplot(data=h4, rost, ves)
qplot(data=h4, ves)


# test
#question 10
round(pchisq(9,10),2)

#question 13
df <- diamonds
head(df)
max(df$price)

#question 14
nrow(filter(df, cut=="Premium"))

#question 16
summary(lm(data=df, price~carat+table))

#question 17
summary(lm(data=df, price~carat))

#question 18
s <- summary(lm(data=df, price~carat+x+y+table))
str(s)
round(s$coefficients["y", 4],6)

#question 19
m1 <- lm(data=df, price~carat)
m2 <- lm(data=df, price~carat+y+x)
mtable(m1,m2)

#question 20
m20 <- lm(data=df, price~carat+y+x)
summary(m20)
sjp.lm(m20)

c(126 - 2 * 27.76 * 0.9, 126 + 2 * 27.76 * 0.9)
