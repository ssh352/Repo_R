setwd("D:/GitHub/R_project/DNS")

library(zoo)

#Загружаем данные
data <- read.csv("sales2.csv", dec=",", header=TRUE, sep = ";", stringsAsFactors=FALSE, encoding="UTF-8")
#Все будем делать через списки
shops <- list()

#Обработаем сразу информацию по всем магазинам
for (i in seq(1,ncol(data),2)) {
  #Выберем все не пустые строки и запомним соответствующие колонки во временную переменную
  tmp_shop <- data[is.na(data[,i+1]) == FALSE,c(i,i+1)]
  #разобьем информацию по датам на две переменные месяц и день
  tmp_shop["month"] <- sapply(1:nrow(tmp_shop), function(i) {
    row <- strsplit(tmp_shop[i,1],".", fixed = TRUE)
    row[[1]][2]
  })
  tmp_shop["day"] <- sapply(1:nrow(tmp_shop), function(i) {
    row <- strsplit(tmp_shop[i,1],".", fixed = TRUE)
    row[[1]][1]
  })
  #информацию по месяцам сразу превратим в фактор для удобства
  tmp_shop["month"] <- factor(tmp_shop$month, 
                              levels=c("янв", "февр", "марта", "апр", "мая", "июня", "июля", "авг", "сент", "окт", "нояб", "дек"))
  #Информацию о датам в текстовом виде удалим
  tmp_shop[,1] <- NULL

  shops[[names(data)[i]]] <- tmp_shop
}

#Добавим реальные даты исходя из того что сейчас 2015 год
#Сразу проставляем номера недель
for(j in 1:length(shops)) {
  year <- 2015
  month <-  as.numeric(tail(shops[[j]][2],1))
  #for(i in nrow(msk_klin):1) {
  for(i in nrow(shops[[j]]):1) {
    cur_month <-  as.numeric(shops[[j]][i,"month"])
    cur_day <-  as.numeric(shops[[j]][i,"day"])
    if(cur_month < month) {
      month <- cur_month
    } else if (cur_month > month) {
      month <- cur_month
      year <- year - 1
    }
    shops[[j]][i, "date"] <- paste(year,month,cur_day, sep="-")
    week <- format(as.Date(shops[[j]][i, "date"]),"%W")
    if (week == "00") {
      week <- 52
      shops[[j]][i, "week"] <- paste((year-1),week, sep="")
    } else {
      shops[[j]][i, "week"] <- paste(year,week, sep="")
    }
    shops[[j]][i, "numofweek"] <- format(as.Date(shops[[j]][i, "date"]), "%w")
  }
}

#Собираем все данные вместе: ежедневные, еженедельные, ежемесячные
sales_daily <- list()
sales_weekly <- list()
sales_monthly <- list()

#Агрегируем данные понедельно и обрезаем по нужной дате
#Будем смещаться по датам на начало следующей недели и конец текущей соответственно
weekly_windows <- function(sales_zoo, group_index, start_date = NULL, end_date = NULL) {
  print(paste("INPUT: Start date = '", start_date,"', end date = '", end_date,"', length = '", length(sales_zoo), "'", sep = ""))
  #Если дата начала пропущена, то вычисляем ее, смещаться будем внизу
  if (is.null(start_date) == TRUE) {
    start_date <- as.Date(start(sales_zoo))
  } else {
    start_date <- as.Date(start_date)
  }
  #Корректируем дату, чтобы период начинался с начала новой недели
  num_of_week <- as.numeric(format(start_date, "%u"))
  shift <- (1 %% num_of_week) * (8 - num_of_week)
  start_date <- start_date + shift
  
  #А если пропущена дата окончания, то вычислим ее и поднимемсяц вверх до конца предыдущей недели
  if (is.null(end_date)) {
    end_date <- as.Date(end(sales_zoo))
  } else {
    end_date <- as.Date(end_date)
  }
  #Корректируем дату, чтобы период заканчивался в конце недели
  num_of_week <- as.numeric(format(end_date, "%u"))
  shift <- ((num_of_week %% 7) / num_of_week) * num_of_week
  end_date <- end_date - shift

  sales <- window(sales_zoo, start=as.Date(start_date), end=as.Date(end_date))
  ix_start <- min(which(index(sales_zoo) >= start_date))
  ix_end <- max(which(index(sales_zoo) <= end_date))
  ix_weekly <- group_index[c(ix_start:ix_end)]

  a <- as.numeric(sales)
  agg_weekly <- aggregate(a, list(ix_weekly), sum)

  start_year <- as.numeric(substr(ix_weekly[1],1,4))
  start_week <- as.numeric(substr(ix_weekly[1],5,6))
  result <- ts(data = agg_weekly$x, start=c(start_year,start_week), frequency = 52)
  print(paste("CORRECTED: Start date = '", start_date,"', end date = '", end_date,"', length = '", length(result), "'", sep = ""))
  return(result)
}

#weekly_windows(sales_daily[[1]], shops[[1]]$week, start_date = "2015-02-01")

#weekly_windows(sales_daily[[1]], shops[[1]]$week, start_date = "2015-01-01", end_date = "2015-03-27")

for(i in 1:length(shops)) {
  #Ежедневные данные
  sales_daily[[names(shops)[i]]] <- zoo(as.Date(shops[[i]]$X), as.Date(shops[[i]]$date))

  a <- as.numeric(sales_daily[[i]])

  #еженедельные
  sales_weekly[[names(shops)[i]]] <- weekly_windows(sales_daily[[i]], shops[[i]]$week)
  
  #ежемесячные
  ix_monthly <- format(index(sales_daily[[i]]), "%Y%m")
  agg_monthly <- aggregate(a, list(ix_monthly), sum)
  sales_monthly[[names(shops)[i]]] <- ts(data = agg_monthly$x, as.numeric(unlist(strsplit(as.character(index(sales_daily[[i]])[1]),"-"))), frequency = 12)
}

################################
##  Time series               ##
################################
library(tseries)
library(forecast)

#Строим общие графики
pdf("observed.pdf")
for(i in 1:length(shops)) {
  plot(sales_daily[[i]], main = names(shops)[i], xlab="days", ylab="Price, RUR", type="l", col="gray")
  lines(zoo(lowess(sales_daily[[i]])$y, as.Date(shops[[i]]$date)), col="red")
  if (length(sales_weekly[[i]]) >= 104) {plot(decompose(sales_weekly[[i]]))}
}
dev.off()

#start_date <- index(head(sales_daily[[4]],1))
#colors <- topo.colors(6)

#Lm <- BoxCox.lambda(sales_monthly[[1]], method="loglik")

#Тестируем модель предсказания
#Предсказывать будем на последние 56 дней или 8мь недель
#days_shift <- 32
pdf("testing.pdf")
for(i in 1:length(shops)) {
#for(i in 6:6) {
  ifelse(i < 4, days_shift <- 49, days_shift <- 35)
  #Тестируем модель предсказания
  #Берем группировку по неделям
  cut_weekly <- weekly_windows(sales_daily[[i]], shops[[i]]$week, end_date = as.Date(tail(index(sales_daily[[i]]),1)) - days_shift)
  full_weekly <- weekly_windows(sales_daily[[i]], shops[[i]]$week)
  #на сколько недель будем предсказывать
  h <- length(full_weekly) - length(cut_weekly)
  sum_cur <- sum(tail(full_weekly, h))

  #однопараметрическое преобразование Бокса-Кокса (пока не очень понимаю что это)
  Lw <- BoxCox.lambda(cut_weekly, method="loglik")
  #Lw <- -0.1

  #TBATS  
  fit.tbats <-tbats(cut_weekly, lambda=Lw)
  fcast.tbats <- forecast(fit.tbats, h, lambda=Lw)
  sum_tbats <- sum(fcast.tbats$mean)

  cut_num <- as.numeric(cut_weekly)
  mean_num <- floor(as.numeric(fcast.tbats$mean))
  all <- c(cut_num, mean_num)
  start_date <- as.Date(paste(c(start(cut_weekly),1), sep="", collapse=" "), "%Y %U %u")
  end_date <- as.Date(paste(c(end(cut_weekly),1), sep="", collapse=" "), "%Y %U %u")
  ix_date <- seq(start_date, end_date + days_shift - 7, "week")
  plot(ix_date,all, type="l", col="black", xlim=as.numeric(as.Date(c("2014-01-06","2015-03-16"))))
  l_train <- (length(ix_date) - h)
  l <- length(ix_date)
  lines(ix_date[c(l_train:l)],all[c(l_train:l)], col="green", lwd=3)
  abline(v = ix_date[min(which(ix_date > "2014-01-01"))], lty="dashed", col = "gray")
  abline(v = ix_date[min(which(ix_date > "2014-04-01"))], lty="dashed", col = "gray")
  abline(v = ix_date[min(which(ix_date > "2014-07-01"))], lty="dashed", col = "gray")
  abline(v = ix_date[min(which(ix_date > "2014-10-01"))], lty="dashed", col = "gray")
  abline(v = ix_date[min(which(ix_date > "2015-01-01"))], lty="dashed", col = "gray")
  lines(ix_date,lowess(all)$y, col="red", lty = "dashed")
  
  #ARIMA
  fit.arima <- auto.arima(cut_weekly, lambda=Lw)
  fcast.arima <- forecast(fit.arima, h, lambda=Lw)
  sum_arima <- floor(sum(fcast.arima$mean))
  
  if (i < 4) {
    plot(fcast.tbats, main = paste("TBATS:", names(shops)[i]), xlim=c(2014.0,2015.213),ylim=c(0,18000))
    lines(full_weekly, col="green")
    lines(cut_weekly, col="black")
    lines(lowess(full_weekly), col="red", lty = "dashed")
    legend("topleft", c("Тренировочные", "Реальные", "Предсказанные","Общая"), 
           col=c("black","green","blue","red"),
           lty=c("solid","solid","solid","dashed"),
           cex=1)
    grid()
    mtext(paste("Разница (Р - П):",sum_cur, "-",floor(sum_tbats),"=",sum_cur-floor(sum_tbats)), side=4,adj=0)
    mtext(paste("Дней для предсказание:",days_shift,", Lambda = ",Lw), adj=0)
  } else {
    plot(fcast.arima, main = paste("ARIMA:", names(shops)[i]), xlim=c(2014.0,2015.213),ylim=c(0,18000))
    lines(full_weekly, col="green")
    lines(cut_weekly, col="black")
    lines(lowess(full_weekly), col="red", lty = "dashed")
    legend("topleft", c("Тренировочные", "Реальные", "Предсказанные","Общая"), 
           col=c("black","green","blue","red"),
           lty=c("solid","solid","solid","dashed"),
           cex=1)
    grid()
    mtext(paste("Разница (Р - П):",sum_cur, "-",floor(sum_arima),"=",sum_cur-floor(sum_arima)), side=4,adj=0)
    mtext(paste("Дней для предсказание:",days_shift,", Lambda = ",Lw), adj=0)
  }
}
dev.off()

#Теперь переходим непосредственно к прогрнозированию
pdf("forecast.pdf")
h <- 8
for(i in 1:length(shops)) {
  #на сколько недель будем предсказывать
  #ifelse(i < 4, h <- 8, h <- 4)
  #Берем группировку по неделям
  full_weekly <- weekly_windows(sales_daily[[i]], shops[[i]]$week)
  
  #однопараметрическое преобразование Бокса-Кокса (пока не очень понимаю что это)
  Lw <- BoxCox.lambda(full_weekly, method="loglik")
  
  #TBATS  
  fit.tbats <-tbats(full_weekly, lambda=Lw)
  fcast.tbats <- forecast(fit.tbats, h, lambda=Lw)
  sum_tbats <- sum(fcast.tbats$mean)
  
  #ARIMA
  fit.arima <- auto.arima(full_weekly, lambda=Lw)
  fcast.arima <- forecast(fit.arima, h, lambda=Lw)
  sum_arima <- floor(sum(fcast.arima$mean))
  
  #if (i < 4) {
    plot(fcast.tbats, main = paste("TBATS:", names(shops)[i]), xlim=c(2014.0,2015.346),ylim=c(0,18000))
    lines(lowess(full_weekly), col="red", lty = "dashed")
    legend("topleft", c("Реальные", "Предсказанные","Общая"), 
           col=c("black","blue","red"),
           lty=c("solid","solid","dashed"),
           cex=1)
    grid()
    mtext(paste("Недель для предсказание:",h,", Lambda = ",Lw), adj=0)
  #} else {
    plot(fcast.arima, main = paste("ARIMA:", names(shops)[i]), xlim=c(2014.0,2015.346),ylim=c(0,18000))
    lines(lowess(full_weekly), col="red", lty = "dashed")
    legend("topleft", c("Реальные", "Предсказанные","Общая"), 
           col=c("black","blue","red"),
           lty=c("solid","solid","dashed"),
           cex=1)
    grid()
    mtext(paste("Недель для предсказание:",h,", Lambda = ",Lw), adj=0)
  #}
}
dev.off()

sales_month <- aggregate(as.numeric(sales_daily[[1]]), by=list(substr(as.character(index(sales_daily[[1]])),1,7)), FUN=sum)
sales_monthly_ts <- ts(data=sales_month[,2], start=as.numeric(unlist(strsplit(as.character(index(sales_daily[[1]])[1]),"-"))), frequency = 12)

#однопараметрическое преобразование Бокса-Кокса (пока не очень понимаю что это)
Lm <- BoxCox.lambda(sales_monthly_ts, method="loglik")
h <- 5

#TBATS  
fit.tbats <-tbats(sales_monthly_ts, lambda=Lm)
fcast.tbats <- forecast(fit.tbats, h, lambda=Lm)
sum_tbats <- sum(fcast.tbats$mean)
plot(fcast.tbats)

#ARIMA
fit.arima <- auto.arima(sales_monthly_ts, lambda=Lm)
fcast.arima <- forecast(fit.arima, h, lambda=Lm)
sum_arima <- floor(sum(fcast.arima$mean))
plot(fcast.arima)

