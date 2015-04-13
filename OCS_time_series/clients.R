setwd("D:/GitHub/R_project/OCS_time_series")
# options(OutDec= ".")
# use USSC
data <- read.csv("CO_all_clients_sum.csv", dec=",", header=FALSE, sep = ";", stringsAsFactors=TRUE)
#clear NA value
data[is.na(data)] <- 0
#Only shipment
shipment <- as.matrix(data[,c(3:74)])
#info about customer
info <- data[,c(1:2)]
names(info)[c(1,2)] <- c("id", "name")
#myts <- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12)
#plot(myts)
#avg <- mean(myts)
l <- ncol(shipment)
#Calculate churn
info["first_activity"] <- apply(shipment, 1, function(x) { min(which(x != 0))})
info["last_activity"] <- apply(shipment, 1, function(x) { max(which(x != 0))})
info["all_sum"] <- rowSums(shipment)
#age, time worked with
info["age_in_month"] <- sapply(1:nrow(info), function(i) { as.integer(info[i,]$last_activity) - as.integer(info[i,]$first_activity) + 1 })
info["churn"] <- sapply(1:nrow(shipment), function(i) { (l - as.integer(info[i,]$last_activity)) > 2 })

#Сначала создадим данные для тренировки
#Возьмем компании в которых продажи были в течении 12 месяцев и информации о которых больше чем на 4 квартала
#Потом на основе этих данных обучим алгоритм
#И сделаем отдельныую выборку по активным в данный момент клиентам, по которым нужно предстказание
################################
##  Выборка для тренировки    ##
################################
info_12 <- info[which(info$age_in_month >=12),]
ship_12 <- shipment[which(info$age_in_month >=12),]
ship_12 <- ship_12[which(info_12$last_activity >= 15),]
info_12 <- info_12[which(info_12$last_activity >= 15),]

#month is no activity, in age range
info_12["no_activity"] <- sapply(1:nrow(info_12), function(i) { table(ship_12[i,c(info_12[i,]$first_activity:info_12[i,]$last_activity)] == 0)["TRUE"]})

#Запомним все отгрзуки партнеров чья история больше или равна 12 месяцам
#Нормализация нужна для боле оптимальной работы алгоритмов машинного обучения
#и Нормализуем отгрузки, чтобы у всех было одинаково
ship_low <- sapply(1:nrow(info_12), function(i)  {as.vector(lowess(scale(ship_12[i,]))$y[c((info_12[i,]$last_activity-13):(info_12[i,]$last_activity-2))])})
#То же самое без нормализации
ship_scale <- sapply(1:nrow(info_12), function(i)  {  as.vector(scale(ship_12[i,])[c((info_12[i,]$last_activity-13):(info_12[i,]$last_activity-2))])})

#запоминаем инфу по последним четырем кварталам, с нормализацией и усреднением
info_12["q1"] <- sapply(1:nrow(info_12), function(i) {sum(ship_low[c(1:3),i])})
info_12["q2"] <- sapply(1:nrow(info_12), function(i) {sum(ship_low[c(4:6),i])})
info_12["q3"] <- sapply(1:nrow(info_12), function(i) {sum(ship_low[c(7:9),i])})
#Отгрузки за последний квартал, без усреднения
#Чтобы было видно насколько отличается от средних закупок
info_12["q4"] <- sapply(1:nrow(info_12), function(i) {sum(ship_scale[c(10:12),i])})

info_12[is.na(info_12)] <- 0
train <- as.data.frame(info_12[,c("age_in_month","no_activity","q1","q2","q3","q4")])
train_label <- as.factor(info_12[,c("churn")])

################################
##  Выборка для предсказания  ##
################################
test <- info[which(info$churn == FALSE),]
ship_test <- shipment[which(info$churn == FALSE),]
ship_test <- ship_test[which(test$age_in_month >= 12),]
test <- test[which(test$age_in_month >= 12),]

#month is no activity, in age range
test["no_activity"] <- sapply(1:nrow(test), function(i) { table(ship_test[i,c(test[i,]$first_activity:test[i,]$last_activity)] == 0)["TRUE"]})

#Запомним все отгрзуки партнеров чья история больше или равна 12 месяцам
#Нормализация нужна для боле оптимальной работы алгоритмов машинного обучения
#и Нормализуем отгрузки, чтобы у всех было одинаково
ship_test_low <- sapply(1:nrow(test), function(i)  {as.vector(lowess(scale(ship_test[i,]))$y[c((test[i,]$last_activity-11):(test[i,]$last_activity))])})
#То же самое без нормализации
ship_test_scale <- sapply(1:nrow(test), function(i)  {as.vector(scale(ship_test[i,])[c((test[i,]$last_activity-11):(test[i,]$last_activity))])})

#запоминаем инфу по последним четырем кварталам, с нормализацией и усреднением
test["q1"] <- sapply(1:nrow(test), function(i) {sum(ship_test_low[c(1:3),i])})
test["q2"] <- sapply(1:nrow(test), function(i) {sum(ship_test_low[c(4:6),i])})
test["q3"] <- sapply(1:nrow(test), function(i) {sum(ship_test_low[c(7:9),i])})
#Отгрузки за последний квартал, без усреднения
#Чтобы было видно насколько отличается от средних закупок
test["q4"] <- sapply(1:nrow(test), function(i) {sum(ship_test_scale[c(10:12),i])})

test[is.na(test)] <- 0
test_value <- as.data.frame(test[,c("age_in_month","no_activity","q1","q2","q3","q4")])

################################
##  Обучение                  ##
################################
library(randomForest)

rf <- randomForest(train, train_label, xtest = test_value, ntree=3000,importance=TRUE,proximity=TRUE)
predictions <- levels(train_label)[rf$test$predicted]

result <- arrange(cbind(test,predictions), desc(predictions), desc(all_sum))
churn_customer <- filter(result, predictions == TRUE)

library(neuralnet)
train_net <- as.data.frame(info_12[,c("churn","age_in_month","no_activity","q1","q2","q3","q4")])
net <- neuralnet(churn~age_in_month+no_activity+q1+q2+q3+q4, train_net, hidden=10, threshold=0.01)
#ix_random <- sample(1:nrow(info_12))
#split_one <- as.integer(length(ix_random) * 0.6)
#split_two <- as.integer(length(ix_random) * 0.8)
#train <- info_12[ix_random[c(1:split_one)],c("age_in_month","no_activity","q1","q2","q3","q4")]
#train_label <- as.factor(info_12[ix_random[c(1:split_one)],c("churn")])
#cross <- info_12[ix_random[c((split_one+1):split_two)],c("age_in_month","no_activity","q1","q2","q3","q4")]
#cross_label <- as.factor(info_12[ix_random[c((split_one+1):split_two)],c("churn")])
#test <- info_12[ix_random[c((split_two+1):length(ix_random))],c("age_in_month","no_activity","q1","q2","q3","q4")]
#test_label <- as.factor(info_12[ix_random[c((split_two+1):length(ix_random))],c("churn")])

#train <- read.csv("train.csv", header=TRUE)
#test <- read.csv("test.csv", header=TRUE)

#labels <- as.factor(train$churn)
#train <- train[train[]]

#write(predictions, file="rf_benchmark.csv", ncolumns=1) 
