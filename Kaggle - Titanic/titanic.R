#http://trevorstephens.com/post/72918760617/titanic-getting-started-with-r-part-1-booting
#Titanic: Getting Started With R - Part 1: Booting Up R
setwd("D:/GitHub/Repo_R/Kaggle - Titanic/")

library(dplyr)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Количество погибших и выживших
summary(train$Survived)
#Процентное соотношение
round(prop.table(table(train$Survived))*100)

#Смотрим количество выживших женщин и мужчин
round(prop.table(table(select(train, Sex, Survived)), 1) * 100)

#Добавляем средний возраст для пропущенных элементов
train$Age[is.na(train$Age)] <- summary(train$Age)[3]
hist(train$Age)

#Добавляем новую колонку дети
train$Child <- 0
train$Child[train$Age < 18] <- 1
round(prop.table(table(select(train, Child, Survived)), 1) * 100)

#Смотрим какие группы людей чаще выживают
#Делаем группировку по выжившим используя колонки пол и ребенок
#Функции применяются к колонке вижившие
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#Выбираем колонки и смотрим по каким у нас больше всего выживших
apply(select(train, Pclass, Sex, SibSp, Parch, Embarked, Child), 2, FUN=function(x) {
  aggregate(train$Survived ~ x, data=train, FUN=function(y) {sum(y)/length(y)})
})

#Посчитаем среднюю стоимость билета для каждого из классов
p <- aggregate(Fare ~ Pclass, data=train, FUN=mean)
#И добавим средние значения добавим по каждому классу
train[which(train$Pclass==1 & train$Fare==0),"Fare"] <- p[1,2]
train[which(train$Pclass==2 & train$Fare==0),"Fare"] <- p[2,2]
train[which(train$Pclass==3 & train$Fare==0),"Fare"] <- p[3,2]

#Сгруппируем значения по стоимости билета на несколько групп
train$Fare2 <- cut(train$Fare, c(0,10,20,30,100), include.lowest = TRUE, labels=c("<10","10-19","20-29",">30")) 
#Здесь имеет смысл учитывать только нижний сегмент
aggregate(Survived ~ train$Fare2, data=train, FUN=function(y) {sum(y)/length(y)})

#Теперь смотрим кто выжил используя три параметра
agg <- aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {c(percent=sum(x)/length(x),count=length(x)) })
agg <- cbind(select(agg, Fare2, Pclass, Sex),agg$Survived)
agg <- arrange(agg, percent)

agg <- aggregate(Survived ~ Fare2 + Pclass + Sex + SibSp + Parch + Embarked +Child, data=train, FUN=function(x) {c(percent=sum(x)/length(x),count=length(x)) })
agg <- cbind(select(agg, Fare2, Pclass, Sex, SibSp, Parch, Embarked, Child),agg$Survived)
agg <- arrange(agg, percent)

#Попробуем использовать дерево решений
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#Параметры дерева настраиваются автоматически
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)

fancyRpartPlot(fit)

#В этом случае сами настроим дерево решений
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
#Настройка дерева решений руками
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

#Займемся созданием новых параметров
#И для начала объединими датасеты
test$Survived <- NA
combi <- rbind(select(train, -Fare2, -Child), test)
#Выдерним все титулы из имен
combi$Title <- sapply(as.character(combi$Name), FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)
#Объединим французкие мадам и мадмуазель
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
#Все титутлы для богатых объединим в один
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

#Добавляем новую переменную размер семьи
combi$FamilySize <- combi$SibSp + combi$Parch + 1
#Добавляем имя семьи
combi$Surname <- sapply(as.character(combi$Name), FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
#И создаем уникальный идентификатор семьи
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)


famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)

#Таким образом попробуем предсказать недостающие возрасты людей
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

#Убираем NA значения, т.к. они будут мешать предсказаниям
summary(combi$Embarked)
combi[which(combi$Embarked == ''),]
combi$Embarked[which(combi$Embarked == '')] = "S"
combi$Embarked <- factor(combi$Embarked)

#Убираем NA значения из стоимости
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm=TRUE)

#Нужно оставть количество факторов <=32, а количество FamilyID больше
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

#install.packages('randomForest')
library(randomForest)

train <- combi[1:891,]
test <- combi[892:1309,]

#Т.к. в данном случае у нас будет получаться разные варианты, сделаем их воспроизводимыми
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, test)

#install.packages('party')
library(party)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)
