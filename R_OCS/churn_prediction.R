setwd("D:/GitHub/Repo_R/R_OCS")

library(RODBC)
library(sqldf)
library(lubridate)
library(zoo)

#Библиотеки для построения дерева решений
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Еще один способ построения деревьев
library(randomForest)

#И еще одна библиотека
library(party)

################################
##  Функции                   ##
################################
back_12 <- function(back, dates, info, activity) {
  back_activity <- t(sapply(1:nrow(activity), function(i){
    ix_start <- which(names(activity)==info[i,"start"])
    ix_end <- which(names(activity)==info[i,"end"])
    cut_sales <- as.numeric(activity[i,c(ix_start:ix_end)])
    scaled_and_cut <- cut_sales / sd(cut_sales)
    dates_cut <- dates[c(ix_start: ix_end)]
    series <- zoo(scaled_and_cut, dates_cut)
    if(length(tail(series, -back)) >= 12 & back > 0) {
      return(as.numeric(tail(tail(series, -back), 12)))
    } else if (length(series) >= 12) {
      return(as.numeric(tail(series, 12)))
    } else {
      return(rep(0,12))
    }
  }))
  back_activity <- as.data.frame(back_activity)
  #print(nrow(back_activity))
  names(back_activity) <- paste("m", as.character(c(1:12)), sep="")
  return(back_activity)
}

################################
## Получаем и готовим данные  ##
################################
#Получаем данные из базы по отгрузкам и заказам
con <- odbcDriverConnect("driver={SQL Server};server=plumbum.ural.ocs.ru;database=cisco;uid=sa;pwd=107109")
res_ship <- sqlQuery(con, 'SELECT * FROM olap_shipped')
res_order <- sqlQuery(con, 'SELECT * FROM olap_ordered')
res_cisco <- sqlQuery(con, 'SELECT accountnum, rbu FROM customers')
odbcCloseAll()

l <- ncol(res_ship)
b <- 4
ix_date <- names(res_ship)[c(b:l)]
dates <- as.Date(paste(substr(ix_date,1,4),substr(ix_date,5,6),"01", sep="-"))
dates <- dates + months(1) - days(1)

#Обрезаем неполный месяц в конце
if (Sys.Date() < tail(dates, 1)) {
  dates <- head(dates,-1)
  l <- ncol(res_ship) - 1
}

#Убираем неизвестные данные и создаем три ДФ, с информацией по клиентам, отгрузкам и заказам
data <- sqldf("SELECT * FROM res_ship WHERE rbu<>'Unknown'")
info <- data[,c(1:3)]
shipped <- data[,c(b:l)]

data <- sqldf("SELECT * FROM res_order WHERE rbu<>'Unknown'")
ordered <- data[,c(b:l)]

#Создаем корректные даты в названии колонок
names(shipped) <- as.character(dates)
names(ordered) <- as.character(dates)

#Все NA значения заменяем на 0
shipped[is.na(shipped)] <- 0
ordered[is.na(ordered)] <- 0
len <- ncol(shipped)

#Уберем все строки с 0ыми суммами, т.к. по ним не было никаких продаж
ix_rows <- which(rowSums(shipped) > 0)
info <- info[ix_rows,]
shipped <- shipped[ix_rows,]
ordered <- ordered[ix_rows,]

#Ушедшими будем считать тех кто не отгружал ничего за последние 3и месяца
info$churn <- rowSums(shipped[,c((len-2):len)])==0
info$churn <- as.factor(info$churn)

#Данные за последний год продаж
info$sum <- rowSums(shipped[,c((len-11):len)])

#info$start <- sapply(1:nrow(shipped), function(i){
info[,c("start","end")] <- t(sapply(1:nrow(shipped), function(i){
  start <- names(shipped)[min(which(shipped[i,] > 0))]
  end <- names(shipped)[max(which(shipped[i,] > 0))]
  return(c(start,end))
}))

back_sales <- back_12(6, dates, info, shipped)
ix_non_empty <- which(rowSums(back_sales) > 0)
train_info <- info[ix_non_empty,]
train_sales <- back_sales[ix_non_empty,]
train <- cbind(train_info,train_sales)

test_sales <- back_12(3, dates, info, shipped)
ix_non_empty <- which(rowSums(test_sales) > 0)
test_info <- info[ix_non_empty,]
test <- test_sales[ix_non_empty,]

################################
## Обучаем и тестируем модели ##
################################
#Recursive Partitioning and Regression Trees
#Параметры дерева настраиваются автоматически
fit_rp <- rpart(churn ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12, 
             data=train, method="class")
fancyRpartPlot(fit_rp)

pred_rp <- predict(fit, test, type = "class")

pred_info_rp <- cbind(test_info, pred_rp)

#confusion matrix
cm_rp <- table(pred_info_rp[,c("churn","pred_rp")])
#Смотрим насколько модель получилась точная
f1_rp <- 2*cm_rp[2,2]/(2*cm_rp[2,2] + cm_rp[1,2] + cm_rp[2,1])

#Classification and Regression with Random Forest
#Т.к. в данном случае у нас будет получаться разные варианты, сделаем их воспроизводимыми
#set.seed(415)
fit_rf <- randomForest(churn ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12, 
                    data=train, importance=TRUE, ntree=2000)
varImpPlot(fit_rf)
pred_rf <- predict(fit_rf, test)
pred_info_rf <- cbind(test_info, pred_rf)

#confusion matrix
cm_rf <- table(pred_info_rf[,c("churn","pred_rf")])
#Смотрим насколько модель получилась точная
f1_rf <- 2*cm_rf[2,2]/(2*cm_rf[2,2] + cm_rf[1,2] + cm_rf[2,1])

#Random Forest
fit_cf <- cforest(churn ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
pred_cf <- predict(fit_cf, test, OOB=TRUE, type = "response")
pred_info_cf <- cbind(test_info, pred_cf)

#confusion matrix
cm_cf <- table(pred_info_cf[,c("churn","pred_cf")])
#Смотрим насколько модель получилась точная
f1_cf <- 2*cm_cf[2,2]/(2*cm_cf[2,2] + cm_cf[1,2] + cm_cf[2,1])

#Теперь когда мы выбрали модель можно и проверить текущие данные
final <- back_12(0, dates, info, shipped)
ix_non_empty <- which(rowSums(final) > 0)
final_info <- info[ix_non_empty,]
final_test <- test_sales[ix_non_empty,]
final_train <- cbind(final_info,final_test)

pred_rf <- predict(fit_rf, final_train)

result <- cbind(final_info, pred_rf)
ix_churn <- which(result$end == "2015-03-31" & result$pred_rf == TRUE)
result$rbu <- toupper(result$rbu)
result$daxid <- toupper(result$daxid)

tocsv <- result[ix_churn,-4]

#Еще проверим по цисковским клиентам
res_cisco$rbu <- toupper(res_cisco$rbu)
names(res_cisco)[1] <- "daxid"
res_cisco$flag <- "cisco"

result_m <- merge(tocsv, res_cisco, by=c("daxid","rbu"))

write.table(tocsv, file = "churn.csv", sep=";", dec=",", row.names = FALSE)

