setwd("D:/GitHub/Repo_R/R_OCS")

library(RODBC)
library(dplyr)
library(plyr)
library(zoo)
library(lubridate)
#library(spatstat)
library(afex)

library(tseries)
library(forecast)

last_day <- function(date) {
  ceiling_date(date, "month") + months(1) - days(1)
}

zoo_series <- total_zoo
h <- 12
b <- 12
#zoo_series = apply.monthly(sales_daily[[1]]
#h <- 3
#b <- 3
#main_text = "Klin"
#xlab_text = "time"
#ylab_text = "sales"
                           
fcast_monthly <- function(zoo_series, fcast.period = 3, test.period = 3,
                          main_text = NULL, xlab_text = NULL, ylab_text = NULL) {
  h <- fcast.period
  b <- test.period
  
  #Создаем векторы с датами, для тестирования и для прогнозирования
  dates <- index(zoo_series)
  
  #Обрезаем неполный месяц в конце
  if (Sys.Date() < tail(dates, 1)) {
    dates <- head(dates,-1)
    zoo_series <- head(zoo_series, -1)
  }
  
  fdates <- c(dates, last_day(floor_date(tail(dates,1), "month")+ months(1:(h))))
  
  #Запоминаем начальные даты в формате вектора и в обычном
  date_start_ts <- as.double(unlist(strsplit(as.character(start(zoo_series)), "-")))
  date_start <- start(zoo_series)
  date_end <- end(zoo_series)
  
  #Создаем TS из zoo серии
  total <- ts(zoo_series, start=date_start_ts, frequency = 12)
  
  ################################
  ##  Обзорные графики          ##
  ################################
  
  #общая картина по продажам циски
  plot(dates, total, 
       main = main_text, sub = NULL, xlab = xlab_text, ylab = ylab_text,
       type="l", col=palette()[1])
  lines(dates, lowess(total)$y, type="l", lty="dashed", col=palette()[2])

  #Добавляем линии для решетки
  abline(h = axTicks(2),col="gray", lty="dashed")
  abline(v = dates[seq(3, length(dates), 3)],col="gray", lty="dashed")
  
  legend("topleft", 
         c("Реальные", "lowess"), 
         col=c("black","red"),
         lty=c("solid","dashed"),
         lwd=2,
         cex=1)
  
  #Сначала рисуем общие данные по продажам
  if (length(zoo_series)>=24) {
    plot(stl(total, s.window = "periodic"))
    mtext(paste(main_text, ": ", ylab_text, sep=""),side=3,outer=F, cex=1, line=2, adj=0.5)
  }
  
  ################################
  ##  Проверяем модель          ##
  ################################
  print("Строим тестовую модель")
  #Учим модель и делаем тестовый прогноз
  train <- ts(head(total, -b), start=date_start_ts, frequency = 12)
  
  #однопараметрическое преобразование Бокса-Кокса (пока не очень понимаю что это)
  Lw <- BoxCox.lambda(train, method="loglik")
  
  fc <- list()
  fc$tbats <- list()
  fc$arima <- list()
  fc$sum <- sum(head(tail(total,b),h))
  
  #Предсказываем на тестовом наборе
  #Посчитаем размер ошибки для обоих предсказаний
  #TBATS  
  fit.tbats <- tbats(train, lambda=Lw)
  fcast.tbats <- forecast(fit.tbats, h, lambda=Lw, level=c(seq(5,95,5)))
  #Соберем вместе все прогнозы и посмотрим какой больше всего подходит
  fcasts.mean <- as.vector(fcast.tbats$mean)
  dim(fcasts.mean) <- c(length(fcasts.mean),1)
  fcasts.all <- data.frame(cbind(fcast.tbats$lower, fcasts.mean, fcast.tbats$upper))
  fcasts.all <- apply(fcasts.all, 2, FUN=as.double)
  #Смотрим насколько сходится по сумме прогноз
  err <- abs(as.double(colSums(fcasts.all) / fc$sum * 100 - 100))
  #Считаем корреляцию между настоящими данными и прогнозируемыми
  test.vector <- as.double(head(tail(total,b),h))
  test.cor <- 100 - as.double(sapply(1:ncol(fcasts.all), function(i){
    cor(test.vector, as.double(fcasts.all[,i])) * 100
  }))
  #Теперь сводим сумму по предсказанию и корреляцию  
  err <- colSums(rbind(err,test.cor))
  names(err) <- c(seq(-95,-5,5), 0, seq(5,95,5))
  ix <- which(err==min(err))
  #Ищем уровень с наименьшим расхождением
  fc$tbats$level <- as.numeric(names(err)[ix])
  #Запоминаем все эти данные
  fc$tbats$sum <- sum(fcasts.all[,ix])
  fc$tbats$mean <- c(tail(train,1),fcasts.all[,ix])
  fc$tbats$error <- round(err[ix],1)
  
  #ARIMA
  fit.arima <- auto.arima(train, lambda=Lw)
  fcast.arima <- forecast(fit.arima, h, lambda=Lw, level=c(seq(5,95,5)))
  #Соберем вместе все прогнозы и посмотрим какой больше всего подходит
  fcasts.mean <- as.vector(fcast.arima$mean)
  dim(fcasts.mean) <- c(length(fcasts.mean),1)
  fcasts.all <- data.frame(cbind(fcast.arima$lower, fcasts.mean, fcast.arima$upper))
  fcasts.all <- apply(fcasts.all, 2, FUN=as.double)
  #Смотрим насколько сходится по сумме прогноз
  err <- abs(as.double(colSums(fcasts.all) / fc$sum * 100 - 100))
  #Считаем корреляцию между настоящими данными и прогнозируемыми
  #test.vector <- as.double(head(tail(total,b),h))
  #test.cor <- 100 - as.double(sapply(1:ncol(fcasts.all), function(i){
    #cor(test.vector, as.double(fcasts.all[,i])) * 100
  #}))
  #Теперь сводим сумму по предсказанию и корреляцию  
  #err <- colSums(rbind(err,test.cor))
  names(err) <- c(seq(-95,-5,5), 0, seq(5,95,5))
  ix <- which(err==min(err))
  #Ищем уровень с наименьшим расхождением
  fc$arima$level <- as.numeric(names(err)[ix])
  #Запоминаем все эти данные
  fc$arima$sum <- sum(fcasts.all[,ix])
  fc$arima$mean <- c(tail(train,1),fcasts.all[,ix])
  fc$arima$error <- round(err[ix],1)
  
  lim_start <- floor_date(date_end, "month") - months(b + 3)
  if (h < 12) {
    lim_end <- lim_start + months(12)
  } else {
    lim_end <- lim_start + months(h + 6)
  }
  plot(dates, total, 
       main = paste("Тестирование модели : ", main_text, sep=""), sub = NULL, xlab = xlab_text, ylab = ylab_text,
       xlim=c(lim_start,lim_end), type="l", col=palette()[1])
  lines(head(tail(dates,b+1), h+1),fc$arima$mean, type="l", col=palette()[2], lwd=3)
  lines(head(tail(dates,b+1), h+1),fc$tbats$mean, type="l", col=palette()[3], lwd=3)
  lines(dates, lowess(total)$y, type="l", lty="dashed", col=palette()[4])
  #Добавляем линии для решетки
  abline(h = axTicks(2),col="gray", lty="dashed")
  abline(v = dates,col="gray", lty="dashed")
  
  legend("topleft", 
         c("Реальные", 
           paste("ARIMA (",fc$arima$error,"%)", sep=""),
           paste("TBATS (",fc$tbats$error,"%)", sep=""),
           "Lowess"), 
         col=palette()[c(1:4)],
         lty=c("solid","solid","solid","dashed"),
         lwd=2,
         cex=1)
  
  ################################
  ##  Прогноз                   ##
  ################################
  print("Считаем прогноз")
  Lw <- BoxCox.lambda(total, method="loglik")
  
  #Предсказываем на тестовом наборе
  #Посчитаем размер ошибки для обоих предсказаний
  #TBATS  
  fit.tbats <-tbats(total, lambda=Lw)
  fcast.tbats <- forecast(fit.tbats, h, lambda=Lw, level=abs(fc$tbats$level))
  if (fc$tbats$level > 0) {
    fc$tbats$mean <- c(tail(total,1),as.numeric(fcast.tbats$upper))
  } else {
    fc$tbats$mean <- c(tail(total,1),as.numeric(fcast.tbats$lower))
  }
  
  #ARIMA
  fit.arima <- auto.arima(total, lambda=Lw)
  fcast.arima <- forecast(fit.arima, h, lambda=Lw, level=abs(fc$arima$level))
  if (fc$arima$level > 0) {
    fc$arima$mean <- c(tail(total,1),as.numeric(fcast.arima$upper))
  } else {
    fc$arima$mean <- c(tail(total,1),as.numeric(fcast.arima$lower))
  }
  
  lim_start <- date_end - years(1)
  lim_end <- last_day(floor_date(date_end,"month") + months(h+1))
  plot(dates, total, 
       main = paste("Прогноз : ", main_text, sep=""), sub = NULL, xlab = xlab_text, ylab = ylab_text,
       axes=FALSE,
       xlim=c(lim_start,lim_end), type="l", col=palette()[1])
  axis(side=2)
  axis(side=1, at = fdates, labels = substr(as.character(fdates),3,7), las=2)
  box()
  #Добавляем линии для решетки
  abline(h = axTicks(2),col="gray", lty="dashed")
  abline(v = fdates,col="gray", lty="dashed")
  
  lines(tail(fdates,h+1),fc$arima$mean, type="l", col=palette()[2], lwd=3)
  lines(tail(fdates,h+1),fc$tbats$mean, type="l", col=palette()[3], lwd=3)
  lines(dates, lowess(total)$y, type="l", lty="dashed", col=palette()[4])
  
  legend("topleft", 
         c("Реальные", 
           paste("ARIMA (",fc$arima$error,"%)", sep=""),
           paste("TBATS (",fc$tbats$error,"%)", sep=""),
           "Lowess"), 
         col=palette()[c(1:4)],
         lty=c("solid","solid","solid","dashed"),
         lwd=2,
         cex=1)
  if (fc$arima$error <= fc$tbats$error) {
    print("Процент ошибки меньшу у ARIMA")
  } else {
    print("Процент ошибки меньшу у TBATS")
  }
  return(
    list(
      arima=tail(fc$arima$mean, -1), 
      arima_err=fc$arima$error, 
      tbats=tail(fc$tbats$mean, -1), 
      tbats_error=fc$tbats$error)
    )
}

#Достаем данные о продажам по линейкам из базы
con <- odbcDriverConnect("driver={SQL Server};server=plumbum.ural.ocs.ru;database=cisco;uid=sa;pwd=107109")
res <- sqlQuery(con, 'SELECT * FROM olap_pline')
odbcCloseAll()

res[is.na(res)] <- 0

#Создаем вектор дат
index <- tail(names(res), length(names(res)) - 2)
dates <- last_day(as.Date(paste(substr(index,1,4),substr(index,5,6),"01", sep="-")))

#Обрезаем неполный месяц в конце
if (Sys.Date() < tail(dates, 1)) {
  dates <- head(dates,-1)
  res_cut <- res[,c(1:(length(res)-1))]
}

#Группируем данные по филиалам
cisco <- filter(res_cut, pline == "CDU" | pline == "CDL" |  pline == "CSD" |  pline == "CTD" | pline == "CNU" | pline == "CNL" |  pline == "CTN" |  pline == "CIP" |  pline == "CIS")
cisco_rbu <- select(cisco, -pline)
cisco_rbu_sum <- ddply(cisco_rbu, "rbu", numcolwise(sum))

#Собираем все данные по продажам
all_rbu <- zoo(,dates)
for (i in 2:nrow(cisco_rbu_sum)) {
  rbu <- zoo(as.integer(select(cisco_rbu_sum[i,],-rbu)), dates)
  all_rbu <- cbind(all_rbu, rbu)
  names(all_rbu)[length(names(all_rbu))] <- as.character(cisco_rbu_sum[i,1])
}

#Делаем прогноз по Cisco департаменту
total_zoo <- zoo(rowSums(all_rbu), dates)
fcast <- fcast_monthly(zoo_series = total_zoo, fcast.period = 12, test.period = 12,
                       main_text = "Департамент Cisco", ylab_text = "Продажи (Тыс.дол.)", xlab_text = "Время")

f04_zoo <- zoo(all_rbu[,4], dates) / 1000
f04_zoo <- f04_zoo[c(min(which(f04_zoo > 0)):length(f04_zoo))]
fcast <- fcast_monthly(zoo_series = f04_zoo, fcast.period = 6, test.period = 6,
                       main_text = "Ф04 Cisco", ylab_text = "Продажи (Тыс.дол.)", xlab_text = "Время")

rbu_fcast <- list()

for (i in 1:ncol(all_rbu)) {
  rbu_zoo <- zoo(all_rbu[,i], dates) / 1000
  if (tail(rbu_zoo,1) > 0) {
    print(paste("Обрабатываем:", names(all_rbu)[i]))
    rbu_zoo <- rbu_zoo[c(min(which(rbu_zoo > 0)):length(rbu_zoo))]
    rbu_fcast[[names(all_rbu)[i]]] <- fcast_monthly(zoo_series = rbu_zoo, fcast.period = 6, test.period = 6,
                           main_text = paste(names(all_rbu)[i], "Cisco"), ylab_text = "Продажи (Тыс.дол.)", xlab_text = "Время")
  } else {
    print(paste("РБЮ:", names(all_rbu)[i]), "пропущено.")
  }
}

#Теперь по всей компании в целом
ocs <- filter(res_cut, rbu != "Unknown")
ocs <- select(ocs, -pline, -rbu)
ocs_sum <- colSums(ocs) / 1000000
ocs_zoo <- zoo(ocs_sum, dates)

fcast_ocs <- fcast_monthly(zoo_series = ocs_zoo, fcast.period = 12, test.period = 12,
                       main_text = "OCS", ylab_text = "Продажи (млн.дол.)", xlab_text = "Время")

################################
##  CSV                       ##
################################
#Создаем табличку с прогнозами
df <- data.frame(replicate(12 + h,numeric(0), simplify = F))
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
