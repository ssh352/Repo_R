setwd("D:/GitHub/Repo_R/DNS")

library(zoo)
library(xts)
library(Hmisc)
library(lubridate)

to_monthly_ts <- function(zoo_time_series) {
  start_date <- start(zoo_time_series)
  end_date <- end(zoo_time_series)
  ts <- ts(zoo_time_series, 
                 start = as.numeric(unlist(strsplit(as.character(start_date),"-"))[-3]), 
                 end = as.numeric(unlist(strsplit(as.character(end_date),"-"))[-3]), 
                 frequency = 12)
  return(ts)
}

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
    shops[[j]][i, "numofweek"] <- format(as.Date(shops[[j]][i, "date"]), "%W")
  }
}

#Собираем все данные вместе: ежедневные, еженедельные, ежемесячные
sales_daily <- list()
sales_weekly <- list()
sales_monthly <- list()
sales_monthly_zoo <- list()

#Агрегируем данные понедельно и обрезаем по нужной дате
#Будем смещаться по датам на начало следующей недели и конец текущей соответственно
#i <- 1
#test <- t(rbind(as.numeric(shops[[i]]$week),as.character(index(sales_daily[[i]]))))
#sales_zoo <- sales_daily[[1]]
#group_index <- shops[[i]]$week
#end_date <- as.Date(tail(index(sales_daily[[i]]),1)) - days_shift
#start_date <- NULL
#end_date = NULL
#as_ts <- TRUE

#sales_zoo < assembly_zoo

weekly_windows <- function(sales_zoo, start_date = NULL, end_date = NULL, as_ts = NULL) {
  #Группируем по неделям с суммой
  weekly_sum <- apply.weekly(sales_zoo, sum)
  print(paste("INPUT: Start date = '", start_date,"', end date = '", end_date,"', length = '", length(weekly_sum), "'", sep = ""))

  #Отфильтруем все неполные недели
  if (length(weekly_sum) < length(sales_zoo)) {
    weekly_length <- apply.weekly(sales_zoo, length)
    weekly_full <- weekly_sum[which(weekly_length == 7)]
  } else {
    weekly_full <- weekly_sum
  }
  
  #Если дата начала пропущена, то вычисляем ее, смещаться будем внизу
  if (is.null(start_date) == TRUE) {
    start_date <- as.Date(start(weekly_full)) - 6
  } else {
    start_date <- as.Date(start_date)
  }
  #Корректируем дату, чтобы период начинался с начала новой недели
  #day_of_week <- as.numeric(format(start_date, "%u"))
  #shift <- (1 %% day_of_week) * (8 - day_of_week)
  #start_date <- start_date + shift
  
  #А если пропущена дата окончания, то вычислим ее и поднимемсяц вверх до конца предыдущей недели
  if (is.null(end_date)) {
    end_date <- as.Date(end(weekly_full))
  } else {
    end_date <- as.Date(end_date)
  }
  #Корректируем дату, чтобы период заканчивался в конце недели
  #day_of_week <- as.numeric(format(end_date, "%u"))
  #shift <- ((day_of_week %% 7) / day_of_week) * day_of_week
  #end_date <- end_date - shift

  sales <- window(weekly_full, start=start_date, end=end_date)
  #ix_start <- max(which(index(sales_zoo) <= start_date))
  #ix_end <- min(which(index(sales_zoo) >= end_date))
  #ix_weekly <- paste(strtrim(index(sales),4),format(index(sales),"%U"), sep="")
  #ix_weekly <- group_index[c(ix_start:ix_end)]
  
  #a <- as.numeric(sales)
  #agg_weekly <- aggregate(a, list(ix_weekly), sum)

  #start_year <- as.numeric(substr(ix_weekly[1],1,4))
  #start_week <- as.numeric(substr(ix_weekly[1],5,6))
  #result <- ts(data = sales, start=c(start_year,start_week), frequency = 52)
  #result <- sales
  if (is.null(as_ts) == FALSE) {
    #Запомним номера недель и скорректируем 0ые недели
    week_number <- as.numeric(format(index(sales),"%W"))
    zero_weeks <- which(week_number == 0)
    week_number[zero_weeks] <- week_number[zero_weeks - 1] + 1

    start_year <- as.numeric(format(start(sales),"%Y"))
    start_week <- week_number[1]
    
    sales <- ts(data = as.numeric(sales), start=c(start_year,start_week), frequency=52)
  }
  print(paste("CORRECTED: Start date = '", start_date,"', end date = '", end_date,"', length = '", length(sales), "'", sep = ""))
  return(sales)
}

#Теперь создадим нормальные данные по продажам
#Т.е. вместо пропущенных дат будет дата с количеством продаж "0"
for(i in 1:length(shops)) {
  #Ежедневные данные
  #И добавляем пустые значения, например первое число каждого года
  sales <- zoo(as.Date(shops[[i]]$X), as.Date(shops[[i]]$date))
  sales <- na.fill(merge(sales, zoo(,seq(start(sales),end(sales),by="day")), all=TRUE),0)
  
  sales_daily[[names(shops)[i]]] <- sales
  a <- as.numeric(sales_daily[[i]])
  
  #еженедельные
  sales_weekly[[names(shops)[i]]] <- weekly_windows(sales_daily[[i]])
  
  #ежемесячные
  start_date <- ceiling_date(start(sales), "month")
  end_date <- floor_date(end(sales), "month") - days()
  cut_sales <- window(sales, start = start_date, end = end_date)
  sales_zoo <- apply.monthly(cut_sales, FUN=sum)
  sales_ts <- to_monthly_ts(sales_zoo)
  sales_monthly[[names(shops)[i]]] <- sales_ts
  sales_monthly_zoo[[names(shops)[i]]] <- sales_zoo
}

################################
##  Time series               ##
################################
library(tseries)
library(forecast)

################################
##  WEEKLY                    ##
################################

################################
##  Обзорные графики          ##
################################

opar <- par(no.readonly=TRUE)
#Строим общие графики
pdf("observed.pdf", family = "NimbusSan", encoding = "CP1251.enc")

#Нормализуем все продажи и выводим на экран
for (i in 1:length(sales_weekly)) {
  s <- scale(sales_weekly[[i]])
  if (i == 1) {
    plot(s, col=palette()[i], main="Нормализованые продажи по всем магазинам", ylab="Продажи", xlab="Время")
  } else {
    lines(s, col=palette()[i])
  }
  s <- tail(s, 46)
  s <- sqrt(s^2)
  print(sum(s))
}
legend("topleft", names(shops), 
       col=palette()[c(1:6)],
       lty="solid",
       cex=1,
       )

#И выводим общие данные
for(i in 1:length(shops)) {
  plot(sales_daily[[i]], main = names(shops)[i], cex=1, xlab=NA, ylab="Продажи (тыс.р)", type="l", col="gray", las=2)
  # Добавляем линию тренда
  model <- lm(as.numeric(sales_daily[[i]]) ~ index(sales_daily[[i]]))
  abline(model, col = "green")
  #Добавляем сглаженную ЛР для данных о продажах
  lines(zoo(lowess(sales_daily[[i]])$y, as.Date(shops[[i]]$date)), col="red")
  #И описание диаграммы
  legend("topleft", c("Продажи", "Тренд", "Сглаженные"), 
         col=c("gray","green","red"),
         lty=c("solid","solid","solid"),
         cex=1)
  #в случае если у нас есть 2а года продаж и больше добавляем дополнительную информацию
  if (length(sales_weekly[[i]]) >= 104) {
    plot(stl(weekly_windows(sales_daily[[i]], as_ts=TRUE), s.window = "periodic"))
  }
}
dev.off()
#embedFonts("observed.pdf")

#start_date <- index(head(sales_daily[[4]],1))
#colors <- topo.colors(6)

#Lm <- BoxCox.lambda(sales_monthly[[1]], method="loglik")

#Проверю идею с моделированными продажами
#Проверка, какой магазин из московских более похож на питерские
#Сумма продаж
for (i in 1:length(sales_weekly)) {
  s <- scale(sales_weekly[[i]])
  s <- tail(s, 46)
  d <- floor(sum(sqrt(s^2)))
  print(paste(names(shops)[i],": ", d, sep=""))
}

################################
##  тестируем на реальных     ##
##  данных                    ##
################################
# Запомним временную серию первого магазина, т.к. они больше всего похожи на продажи СПБ магазинов
#msk <- scale(sales_weekly[[1]])
#msk <- head(msk, length(msk) - 46)
msk_restored <- head(sales_weekly[[1]], length(sales_weekly[[1]]) - 46)

#Тестируем модель предсказания
#Предсказывать будем на последние 56 дней или 8мь недель
#days_shift <- 32
pdf("testing.pdf", family = "NimbusSan", encoding = "CP1251.enc")
for(i in 1:length(shops)) {
  print(paste(names(shops)[i],": Start...", sep=""))
  #for(i in 6:6) {
  #i <- 4
  ifelse(i < 4, days_shift <- 49, days_shift <- 35)
  #days_shift <- 35
  #Тестируем модель предсказания
  #Берем группировку по неделям
  cut_weekly <- weekly_windows(sales_daily[[i]], end_date = as.Date(tail(index(sales_daily[[i]]),1)) - days_shift, as_ts = TRUE)
  cut_weekly_zoo <- weekly_windows(sales_daily[[i]], end_date = as.Date(tail(index(sales_daily[[i]]),1)) - days_shift)
  full_weekly <- weekly_windows(sales_daily[[i]], as_ts = TRUE)
  full_weekly_zoo <- weekly_windows(sales_daily[[i]])
  #на сколько недель будем предсказывать
  h <- length(full_weekly) - length(cut_weekly)
  sum_cur <- sum(tail(full_weekly, h))

  #однопараметрическое преобразование Бокса-Кокса (пока не очень понимаю что это)
  Lw <- BoxCox.lambda(cut_weekly, method="loglik")
  #Lw <- -0.1

  #TBATS  
  fit.tbats <- tbats(cut_weekly, lambda=Lw)
  fcast.tbats <- forecast(fit.tbats, h, lambda=Lw)
  sum_tbats <- sum(fcast.tbats$mean)
  
  #ARIMA
  fit.arima <- auto.arima(cut_weekly, lambda=Lw)
  fcast.arima <- forecast(fit.arima, h, lambda=Lw)
  sum_arima <- floor(sum(fcast.arima$mean))
  
  cut_num <- as.numeric(cut_weekly)
  mean_tbats_num <- floor(as.numeric(fcast.tbats$mean))
  mean_arima_num <- floor(as.numeric(fcast.arima$mean))
  all_tbats <- c(cut_num, mean_tbats_num)
  all_arima <- c(cut_num, mean_arima_num)

  #Дополнительная тестовая проверка
  if (i > 3) {
    #Узнаем коэффиценты скалирования
    #s <- scale(cut_weekly)
    #cut_center <- attr(s, "scaled:center")
    #cut_scale <- attr(s, "scaled:scale")
    #msk_restored <- msk * cut_scale + cut_center
    assembly_zoo <- rbind(msk_restored, cut_weekly_zoo)
    assembly <- weekly_windows(assembly_zoo, as_ts=TRUE)
    
    Lw <- BoxCox.lambda(assembly, method="loglik")

    #ARIMA
    fit.arima <- auto.arima(assembly, lambda=Lw)
    fcast.arima <- forecast(fit.arima, h, lambda=Lw)
    sum_arima_assembly <- floor(sum(fcast.arima$mean))
    
    mean_arima_assembly_num <- floor(as.numeric(fcast.arima$mean))
    all_arima_assembly <- c(cut_num, mean_arima_assembly_num)
  }
  
  #start_date <- as.Date(paste(c(start(cut_weekly),1), sep="", collapse=" "), "%Y %U %w")
  #end_date <- as.Date(paste(c(end(cut_weekly),1), sep="", collapse=" "), "%Y %U %w")
  ix_date <- index(full_weekly_zoo)
  #Рисуем график продаж
  plot(ix_date,full_weekly, 
       main = names(shops)[i], sub = NULL, xlab = NA, ylab = "Продажи (тыс.р)",
       type="l", col="black", xlim=as.numeric(as.Date(c("2014-01-06","2015-03-16"))), axes=FALSE)
  axis(side=2)
  tick_dates <- aggregate(ix_date, by=list(strtrim(as.character(ix_date),7)), FUN=min)$x
  axis(side=1, at = tick_dates, labels = substr(as.character(tick_dates),3,10), las=2)
  #axis(side=1, at = ix_date, labels = substr(as.character(ix_date),3,10), las=2)
  box()
  #Добавляем линии для решетки
  abline(h = axTicks(2),col="gray", lty="dashed")
  abline(v = tick_dates,col="gray", lty="dashed")
  #lines(ix_date,all, type="l", col="black", xlim=as.numeric(as.Date(c("2014-01-06","2015-03-16"))))
  l_train <- (length(ix_date) - h)
  l <- length(ix_date)

  #Добавляем форкасты 
  lines(ix_date[c(l_train:l)],all_tbats[c(l_train:l)], col="green", lwd=3)
  lines(ix_date[c(l_train:l)],all_arima[c(l_train:l)], col="blue", lwd=3)
  #Если считали доп модель, то выводим ее
  if (i > 3) {
    lines(ix_date[c(l_train:l)],all_arima_assembly[c(l_train:l)], col="magenta", lwd=3)
  }
  #Добавляем смазанные данные о продажах
  lines(ix_date,lowess(full_weekly)$y, col="red", lty = "dashed")
  
  #Посчитаем размер ошибки для обоих предсказаний
  tbats_error <- as.integer(sum_tbats / sum_cur * 100 - 100)
  arima_error <- as.integer(sum_arima / sum_cur * 100 - 100)
  arima_error_assembly <- as.integer(sum_arima_assembly / sum_cur * 100 - 100)
  
  if (i > 3) {
    legend("topleft", 
           c("Реальные", 
             paste("TBATS (",tbats_error,"%)", sep=""),
             paste("ARIMA (",arima_error,"%)", sep=""),
             paste("ARIMA модель (",arima_error_assembly,"%)", sep=""),
             "Lowess"), 
           col=c("black","green","blue", "magenta","red"),
           lty=c("solid","solid","solid","solid","dashed"),
           lwd=2,
           cex=1)
  } else {
    legend("topleft", 
           c("Реальные", 
             paste("TBATS (",tbats_error,"%)", sep=""),
             paste("ARIMA (",arima_error,"%)", sep=""),
             "Lowess"), 
           col=c("black","green","blue", "red"),
           lty=c("solid","solid","solid","dashed"),
           lwd=2,
           cex=1)
  }
  mtext("Расхождения с реальными данными указаны в процентах", adj=0)
  print(paste(names(shops)[i],": Done!", sep=""))
}
dev.off()

################################
##  Строим прогнозы           ##
################################
# Запомним временную серию первого магазина, т.к. они больше всего похожи на продажи СПБ магазинов
#msk <- scale(sales_weekly[[1]])
#msk <- head(msk, length(msk) - 46)
msk_restored <- head(sales_weekly[[1]], length(sales_weekly[[1]]) - 46)

#Теперь переходим непосредственно к прогрнозированию
pdf("forecast.pdf", family = "NimbusSan", encoding = "CP1251.enc")
forecasts <- list()
h <- 5
#i <- 1

for(i in 1:length(shops)) {
  print(paste(names(shops)[i],": Start...", sep=""))
  #на сколько недель будем предсказывать
  #ifelse(i < 4, h <- 8, h <- 4)
  #Берем группировку по неделям
  full_weekly <- weekly_windows(sales_daily[[i]], as_ts=TRUE)
  full_weekly_zoo <- weekly_windows(sales_daily[[i]])
  
  #однопараметрическое преобразование Бокса-Кокса (пока не очень понимаю что это)
  Lw <- BoxCox.lambda(full_weekly, method="loglik")
  #Lw <- 0
  
  #TBATS  
  fit.tbats <-tbats(full_weekly, lambda=Lw)
  fcast.tbats <- forecast(fit.tbats, h, lambda=Lw)
  sum_tbats <- sum(fcast.tbats$mean)
  
  #ARIMA
  fit.arima <- auto.arima(full_weekly, lambda=Lw)
  fcast.arima <- forecast(fit.arima, h, lambda=Lw)
  sum_arima <- floor(sum(fcast.arima$mean))
  #acc_arima <- accuracy(fit.arima)
  
  full_num <- as.numeric(full_weekly)
  mean_tbats_num <- floor(as.numeric(fcast.tbats$mean))
  mean_arima_num <- floor(as.numeric(fcast.arima$mean))
  all_tbats <- c(full_num, mean_tbats_num)
  all_arima <- c(full_num, mean_arima_num)

  #В случае с питерскими магазинами апгрейдим модель, добавляя данные о продажах
  if (i > 3) {
    assembly_zoo <- rbind(msk_restored, full_weekly_zoo)
    assembly <- weekly_windows(assembly_zoo, as_ts=TRUE)
    Lw <- BoxCox.lambda(assembly, method="loglik")

    fit.arima <- auto.arima(assembly, lambda=Lw)
    fcast.arima <- forecast(fit.arima, h, lambda=Lw)
    sum_arima_A <- floor(sum(fcast.arima$mean))
    
    mean_arima_num_A <- floor(as.numeric(fcast.arima$mean))
    all_arima_A <- c(full_num, mean_arima_num_A)
    forecasts[[i]] <- all_arima_A
  } else {
    forecasts[[i]] <- all_arima
  }
  
  #Создаем индексы с датами для предсказания
  start_date <- as.Date("2014-01-06")
  end_date <- end(full_weekly_zoo) + h * 7
  ix_date_forecast <- index(zoo(,seq(start(full_weekly_zoo),end_date,by="week")))
  ix_date <- index(full_weekly_zoo)
  #Рисуем график продаж
  plot(ix_date,full_weekly, 
       main = names(shops)[i], sub = NULL, xlab = NA, ylab = "Продажи (тыс.р)",
       type="l", col="black", xlim=c(start_date,end_date), axes=FALSE)
  axis(side=2)
  tick_dates <- aggregate(ix_date_forecast, by=list(strtrim(as.character(ix_date_forecast),7)), FUN=min)$x
  axis(side=1, at = tick_dates, labels = substr(as.character(tick_dates),3,10), las=2)
  #axis(side=1, at = ix_date, labels = substr(as.character(ix_date),3,10), las=2)
  box()
  #Добавляем линии для решетки
  abline(h = axTicks(2),col="gray", lty="dashed")
  abline(v = tick_dates,col="gray", lty="dashed")
  #lines(ix_date,all, type="l", col="black", xlim=as.numeric(as.Date(c("2014-01-06","2015-03-16"))))
  l_train <- (length(ix_date_forecast) - h)
  l <- length(ix_date_forecast)
  
  #Добавляем форкасты 
  #lines(ix_date_forecast,all_tbats, col="green", lwd=3)
  lines(ix_date_forecast[c(l_train:l)],all_tbats[c(l_train:l)], col="green", lwd=3)
  lines(ix_date_forecast[c(l_train:l)],all_arima[c(l_train:l)], col="blue", lwd=3)
  if (i > 3) {
    lines(ix_date_forecast[c(l_train:l)],all_arima_A[c(l_train:l)], col="magenta", lwd=3)
  }
  
  #Добавляем смазанные данные о продажах
  #lines(ix_date,lowess(full_weekly)$y, col="red", lty = "dashed")
  
  if (i > 3) {
    legend("topleft", 
           c("Реальные", 
             "TBATS",
             "ARIMA",
             "ARIMA модель",
             "Lowess"), 
           col=c("black","green","blue", "magenta","red"),
           lty=c("solid","solid","solid","solid","dashed"),
           lwd=2,
           cex=1)
  } else {
    legend("topleft", 
           c("Реальные", 
             "TBATS",
             "ARIMA",
             "Lowess"), 
           col=c("black","green","blue", "red"),
           lty=c("solid","solid","solid","dashed"),
           lwd=2,
           cex=1)
  }
  print(paste(names(shops)[i],": Done!", sep=""))
}
dev.off()

################################
##  CSV                       ##
################################
#Создаем табличку с прогнозами
df <- data.frame(replicate(46 + h,numeric(0), simplify = F))
#names(df) <- as.character(tail(index(sales_weekly[[1]]),46 + h))
for (i in 1:length(shops)) {
  df[i,] <- tail(forecasts[[i]],46 + h)
}

end_date <- end(sales_weekly[[1]]) + h * 7
ix_date_forecast <- index(zoo(,seq(start(sales_weekly[[1]]),end_date,by="week")))

dimnames(df)[[1]] <- names(shops)
dimnames(df)[[2]] <- as.character(tail(ix_date_forecast,46 + h))
dimnames(df)[[2]][c(47:51)] <- paste("P ",tail(dimnames(df)[[2]],h),sep="")

write.csv(df, file="forecasts.csv")

fcast <- fcast_monthly(zoo_series = apply.monthly(sales_daily[[1]], FUN=sum), fcast.period = 6, test.period = 6,
                       main_text = "клин", ylab_text = "Продажи (Тыс.р.)", xlab_text = "Время")

sales <- rbind(head(sales_daily[[1]],-length(sales_daily[[4]])),sales_daily[[4]])

fcast <- fcast_monthly(zoo_series = apply.monthly(sales, FUN=sum), fcast.period = 3, test.period = 3,
                       main_text = "SPB", ylab_text = "Продажи (Тыс.р.)", xlab_text = "Время")

################################
##  MONTHLY                    ##
################################

################################
##  Обзорные графики          ##
################################

opar <- par(no.readonly=TRUE)
#Строим общие графики
pdf("observed_monthly.pdf", family="NimbusSan", encoding = "CP1251.enc")

#Нормализуем все продажи и выводим на экран
for (i in 1:length(sales_monthly_zoo)) {
  s <- scale(sales_monthly_zoo[[i]])
  if (i == 1) {
    plot(s, col=palette()[i], main="Нормализованые продажи по всем магазинам", ylab="Продажи", xlab="Время")
  } else {
    lines(s, col=palette()[i])
  }
  s <- tail(s, 12)
  s <- sqrt(s^2)
  print(sum(s))
}
legend("topleft", names(shops), 
       col=palette()[c(1:6)],
       lty="solid",
       cex=0.75)

#И выводим общие данные
i <- 1
for(i in 1:length(shops)) {
  plot(sales_monthly_zoo[[i]], main = names(shops)[i], cex=1, xlab=NA, ylab="Продажи (тыс.р)", type="l", col="gray", las=2)
  # Добавляем линию тренда
  model <- lm(as.numeric(sales_monthly_zoo[[i]]) ~ index(sales_monthly_zoo[[i]]))
  abline(model, col = "green")
  #Добавляем сглаженную ЛР для данных о продажах
  lines(zoo(lowess(sales_monthly_zoo[[i]])$y, index(sales_monthly_zoo[[i]])), col="red")
  #И описание диаграммы
  legend("topleft", c("Продажи", "Тренд", "Сглаженные"), 
         col=c("gray","green","red"),
         lty=c("solid","solid","solid"),
         cex=1)
  #в случае если у нас есть 2а года продаж и больше добавляем дополнительную информацию
  if (length(sales_monthly_zoo[[i]]) >= 24) {
    plot(stl(sales_monthly[[i]], s.window = "periodic"))
  }
}
dev.off()
embedFonts("observed_monthly.pdf") 

################################
##  тестируем на реальных     ##
##  данных                    ##
################################
# Запомним временную серию первого магазина, т.к. они больше всего похожи на продажи СПБ магазинов
msk_restored <- head(sales_monthly_zoo[[1]], -9)

#Тестируем модель предсказания
pdf("testing.pdf", family = "NimbusSan", encoding = "CP1251.enc")
#Для тестирования будем использовать данные за последние 12 месяцев
back <- 12
forward <- 6
for(i in 1:length(shops)) {
  print(paste(names(shops)[i],": Start...", sep=""))
  #Тестируем модель предсказания
  #Берем группировку по неделям
  #cut_weekly <- weekly_windows(sales_daily[[i]], end_date = as.Date(tail(index(sales_daily[[i]]),1)) - days_shift, as_ts = TRUE)
  cut_zoo <- head(sales_monthly_zoo[[i]], -back)
  start_ts <- as.numeric(unlist(strsplit(strtrim(start(cut_zoo),7),"-")))
  cut_ts <- ts(as.numeric(cut_zoo),start=start_ts, frequency = 12)
  #full_weekly <- weekly_windows(sales_daily[[i]], as_ts = TRUE)
  full_zoo <- sales_monthly_zoo[[i]]
  #на сколько недель будем предсказывать
  sum_cur <- sum(tail(full_zoo, forward))
  
  #однопараметрическое преобразование Бокса-Кокса (пока не очень понимаю что это)
  Lw <- BoxCox.lambda(cut_ts, method="loglik")
  #Lw <- -0.1
  
  #Custom ARIMA
  sales.monthly <- ts(sales_monthly_zoo[[i]], start=as.numeric(unlist(strsplit(strtrim(start(sales_monthly_zoo[[i]]),7),"-"))), frequency = 12)
  tsdisplay(sales.monthly)
  m.arima <- arima(sales.monthly, order=c(2,1,2), seasonal = c(1,1,1))
  summary(m.arima)
  plot(sales.monthly)
  lines(fitted(m.arima), col="green")
  plot(forecast(m.arima, h=6))
  
  t.value <- data.frame(ar=0, i=1, ma=0, s1 = 0, s2 = 0, s3 = 0, aic = 0, v12 = 0)
  
  library(dplyr)
  r <- 0
  for (i in 0:3) {
    for (j in 0:3) {
      for (k in 0:1) {
        for (s1 in 0:2) {
          for (s2 in 0:2) {
            for (s3 in 0:2) {
              r <- r + 1
              print(c(r, i, k, j, s1, s2, s3))
              aic <- NA
              v12 <- NA
              try({
                m.arima <- arima(sales.monthly, order=c(i,k,j), seasonal = c(s1,s2,s3))
                aic <- AIC(m.arima)
                v12 <- fitted(m.arima)[34]
              })
              t.value[r, ] <- c(i, k, j, s1, s2, s3, aic, v12)
            }
          }
        }
      }
    }
  }
  library(sqldf)
  res <- na.omit(t.value)
  sqldf("SELECT * FROM res ORDER BY aic DESC")
  sqldf("SELECT * FROM res ORDER BY v12")
  #filter(na.omit(t.value), aic < (min(aic) + 5) &  v12 == max(v12))
  m.arima <- arima(sales.monthly, order=c(1,0,3), seasonal = c(1,1,2))
  #plot(sales.monthly)
  plot(forecast(m.arima, h=6))
  lines(fitted(m.arima), col="green")
  
  #TBATS  
  fit.tbats <- tbats(cut_zoo, lambda=Lw)
  fcast.tbats <- forecast(fit.tbats, h, lambda=Lw)
  sum_tbats <- sum(fcast.tbats$mean)
  
  #ARIMA
  fit.arima <- auto.arima(cut_ts, lambda=Lw)
  tsdiag(fit.arima)
  fcast.arima <- forecast(fit.arima, forward, lambda=Lw)
  sum_arima <- floor(sum(fcast.arima$mean))
  
  cut_num <- as.numeric(cut_weekly)
  mean_tbats_num <- floor(as.numeric(fcast.tbats$mean))
  mean_arima_num <- floor(as.numeric(fcast.arima$mean))
  all_tbats <- c(cut_num, mean_tbats_num)
  all_arima <- c(cut_num, mean_arima_num)
  
  #Дополнительная тестовая проверка
  if (i > 3) {
    #Узнаем коэффиценты скалирования
    #s <- scale(cut_weekly)
    #cut_center <- attr(s, "scaled:center")
    #cut_scale <- attr(s, "scaled:scale")
    #msk_restored <- msk * cut_scale + cut_center
    assembly_zoo <- rbind(msk_restored, cut_weekly_zoo)
    assembly <- weekly_windows(assembly_zoo, as_ts=TRUE)
    
    Lw <- BoxCox.lambda(assembly, method="loglik")
    
    #ARIMA
    fit.arima <- auto.arima(assembly, lambda=Lw)
    fcast.arima <- forecast(fit.arima, h, lambda=Lw)
    sum_arima_assembly <- floor(sum(fcast.arima$mean))
    
    mean_arima_assembly_num <- floor(as.numeric(fcast.arima$mean))
    all_arima_assembly <- c(cut_num, mean_arima_assembly_num)
  }
  
  #start_date <- as.Date(paste(c(start(cut_weekly),1), sep="", collapse=" "), "%Y %U %w")
  #end_date <- as.Date(paste(c(end(cut_weekly),1), sep="", collapse=" "), "%Y %U %w")
  ix_date <- index(full_weekly_zoo)
  #Рисуем график продаж
  plot(ix_date,full_weekly, 
       main = names(shops)[i], sub = NULL, xlab = NA, ylab = "Продажи (тыс.р)",
       type="l", col="black", xlim=as.numeric(as.Date(c("2014-01-06","2015-03-16"))), axes=FALSE)
  axis(side=2)
  tick_dates <- aggregate(ix_date, by=list(strtrim(as.character(ix_date),7)), FUN=min)$x
  axis(side=1, at = tick_dates, labels = substr(as.character(tick_dates),3,10), las=2)
  #axis(side=1, at = ix_date, labels = substr(as.character(ix_date),3,10), las=2)
  box()
  #Добавляем линии для решетки
  abline(h = axTicks(2),col="gray", lty="dashed")
  abline(v = tick_dates,col="gray", lty="dashed")
  #lines(ix_date,all, type="l", col="black", xlim=as.numeric(as.Date(c("2014-01-06","2015-03-16"))))
  l_train <- (length(ix_date) - h)
  l <- length(ix_date)
  
  #Добавляем форкасты 
  lines(ix_date[c(l_train:l)],all_tbats[c(l_train:l)], col="green", lwd=3)
  lines(ix_date[c(l_train:l)],all_arima[c(l_train:l)], col="blue", lwd=3)
  #Если считали доп модель, то выводим ее
  if (i > 3) {
    lines(ix_date[c(l_train:l)],all_arima_assembly[c(l_train:l)], col="magenta", lwd=3)
  }
  #Добавляем смазанные данные о продажах
  lines(ix_date,lowess(full_weekly)$y, col="red", lty = "dashed")
  
  #Посчитаем размер ошибки для обоих предсказаний
  tbats_error <- as.integer(sum_tbats / sum_cur * 100 - 100)
  arima_error <- as.integer(sum_arima / sum_cur * 100 - 100)
  arima_error_assembly <- as.integer(sum_arima_assembly / sum_cur * 100 - 100)
  
  if (i > 3) {
    legend("topleft", 
           c("Реальные", 
             paste("TBATS (",tbats_error,"%)", sep=""),
             paste("ARIMA (",arima_error,"%)", sep=""),
             paste("ARIMA модель (",arima_error_assembly,"%)", sep=""),
             "Lowess"), 
           col=c("black","green","blue", "magenta","red"),
           lty=c("solid","solid","solid","solid","dashed"),
           lwd=2,
           cex=1)
  } else {
    legend("topleft", 
           c("Реальные", 
             paste("TBATS (",tbats_error,"%)", sep=""),
             paste("ARIMA (",arima_error,"%)", sep=""),
             "Lowess"), 
           col=c("black","green","blue", "red"),
           lty=c("solid","solid","solid","dashed"),
           lwd=2,
           cex=1)
  }
  mtext("Расхождения с реальными данными указаны в процентах", adj=0)
  print(paste(names(shops)[i],": Done!", sep=""))
}
dev.off()
