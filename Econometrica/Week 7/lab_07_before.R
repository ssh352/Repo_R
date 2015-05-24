# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

# lab 07

library(memisc)
library("dplyr") # манипуляции с данными
library("erer") # расчет предельных эффектов
library("vcd") # графики для качественных данных
library("ggplot2") # графики
library("reshape2") # манипуляции с данными
library("AUC") # для ROC кривой
library(GGally)


# при загрузке файлов R автоматом переделывает все строковые переменные в факторные
# эта шаманская команда просит R так не делать :)
#options(stringsAsFactors=FALSE)

# читаем данные по пассажирам Титаника
t <- read.csv("titanic3.csv", stringsAsFactors=FALSE)
t <- mutate(t,sex=as.factor(sex),pclass=as.factor(pclass),survived=as.factor(survived))

# источник и описание:
# http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html
summary(t)

head(t,3)
#Качественные переменные
mosaic(data=t, ~sex+pclass+survived, shade=TRUE)

#Количественные переменные
qplot(data=t, x=survived, y=age, geom="violin") # Распределение по возрастам почти одинаковое
qplot(data=t, x=survived, y=age, geom="boxplot")

qplot(data=t, x=survived, y=fare, geom="violin") 
qplot(data=t, x=survived, y=fare, geom="boxplot")

qplot(data=t, x=age, y=..count.., fill=survived, geom="density", position="stack")
qplot(data=t, x=age, y=..count.., fill=survived, geom="density", position="fill")

#Оценивание моделей
m_logit <- glm(data=t, survived ~ sex + age + pclass + fare, family=binomial(link="logit"), x=TRUE)
m_probit <- glm(data=t, survived ~ sex + age + pclass + fare, family=binomial(link="probit"), x=TRUE)

summary(m_logit)
summary(m_probit)

vcov(m_logit)

nd <- data.frame(age=seq(from=5, to=100, length=100), sex="male", pclass="2nd", fare=100)
pr_logit <- predict(m_logit, nd, se=TRUE)
nd.pr <- cbind(nd, pr_logit)
head(nd.pr)

new.pr <- mutate(nd.pr, prob=plogis(fit), left_ci=plogis(fit-1.96*se.fit), right_ci=plogis(fit+1.96*se.fit))
head(new.pr)

qplot(data=new.pr, x=age, y=prob,geom="line") + geom_ribbon(aes(ymin=left_ci,ymax=right_ci), alpha=0.2) + theme_bw()

t2 <- select(t, sex, age, pclass, survived, fare) %>% na.omit()
m_logit2 <- glm(data=t2, survived ~ sex + age, family=binomial(link="logit"), x=TRUE)

lrtest(m_logit, m_logit2)

mtable(m_logit, m_logit2)

#Предельные эффекты
maBina(m_logit)
maBina(m_logit, x.mean=FALSE)

# МНК
m_ols <- lm(data=t, as.numeric(survived) ~ sex + age + pclass + fare)
summary(m_ols)

pr_ols <- predict(m_ols, nd)

#7.2.5
pr_t <- predict(m_logit, t, se=TRUE)
t <- cbind(t, pr_t)
t <- mutate(t, prob=plogis(fit))
head(t)

# ROC
roc.data <- roc(t$prob, t$survived)
str(roc.data)
qplot(x=roc.data$cutoffs, y=roc.data$tpr, geom="line")
qplot(x=roc.data$cutoffs, y=roc.data$fpr, geom="line")

qplot(x=roc.data$fpr, y=roc.data$tpr, geom="line")

plot(roc.data$cutoffs, y=roc.data$tpr, type="line")
lines(roc.data$cutoffs, roc.data$fpr)

#Test
#Q 2
x <- 4
round(1 / (1 + exp(-(2 - 0.3 * x))), 2)

#Q 12
m_logit2 <- glm(data=t, survived ~ sex + age + fare + sibsp, family=binomial(link="logit"), x=TRUE)
summary(m_logit2)

#q 13
m13 <- glm(data=t, survived ~ age + I(age^2) + sex + fare + sibsp + parch, family=binomial(link="logit"), x=TRUE)
summary(m13)
A <- m13$coefficients[3]
B <- m13$coefficients[2]
age_optimal <- (-B/(2*A))
round(age_optimal, digits = 1)

#Q 14
m14 <- glm(data=t, survived ~ age + I(age^2) + sex + fare + I(fare^2) + sibsp, family=binomial(link="logit"), x=TRUE)
summary(m14)
round(confint(m14, level=0.95)["sibsp", 1], 2)

#Q 15
m15 <- glm(data=t, survived ~ age + I(age^2) + sex + fare + sibsp, family=binomial(link="logit"), x=TRUE)
summary(m15)

nd <- data.frame(age=05, sex="male", sibsp=2, fare=200)
predict(m15, nd, se = TRUE)

#Q 16
m16.fit <- predict(m15, nd, se = TRUE)$fit
m16.se <- predict(m15, nd, se = TRUE)$se.fit

round(m16.fit - 1.96 * m16.se, 2)

#q 18
m18.1 <- glm(data=t, survived ~ age + I(age^2) + sex + fare + I(fare^2) + sibsp + parch, family=binomial(link="logit"), x=TRUE)
m18.2 <- glm(data=t, survived ~ age + I(age^2) + sex + sibsp + parch, family=binomial(link="logit"), x=TRUE)

lrtest(m18.1, m18.2)

#Q 19
d <- select(t, age, sex, lass, survived, fare) %>% na.omit()
m_logit2 <- glm(data=t2, survived ~ sex + age, family=binomial(link="logit"), x=TRUE)
