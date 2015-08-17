setwd("D:/GitHub/Repo_R/titanik_cinema")

options(scipen=5)

library(RODBC)
library(sjPlot)
library(psych)
library(PerformanceAnalytics)
library(zoo)
library(xts)
library(forecast)
library(sqldf)

#install.packages("PerformanceAnalytics")

server <- "cisrepl-v.ocs.ru"
db.name <- "titanik"
login <- passwords(server)$login
pass <- passwords(server)$password
connectionString <- paste("driver={SQL Server};server=", server,";database=", db.name ,";uid=", login,";pwd=", pass, sep="")

con <- odbcDriverConnect(connectionString)
sales.dates <- format(seq(from=as.Date("2010-01-01"), to=as.Date("2015-06-30"), by="day"), "%d.%m.%Y")
sales.dates.month <- seq(from=as.Date("2010-01-01"), to=as.Date("2015-06-30"), by="month")
sales <- data.frame(total=numeric(0))
#for(i to 1:(length(sales.dates)-1)) {
#for(i in 1:4) {
t(sapply(1:(length(sales.dates)-1), function(i) {
#t(sapply(1:4, (i) {
  sql.qry <- paste("
    set datefirst 1 
    select * from (
      select data.* ,
        Query0.Duration FilmDuration,
        Query0.Country FilmCountry,
        Query0.GenreName FilmGenreName,
        Query0.AgeLimitationName AgeLimitationName,
        Query0.Name FilmName,
        FilmCopy.Name FilmCopyName,
        FilmCopy.LeaseStartTime FilmCopyLeaseStartTime,
        FilmCopy.LeaseEndTime FilmCopyLeaseEndTime,
        FilmCopy.LeaseCertificateNumber FilmCopyLeaseCertificateNumber
      from (
        select
          p.FilmID FilmID ,
          p.FilmCopyID FilmCopyID ,
          p.StartDate PerformanceDateDim ,
          sum(case when (od.OperationTypeID =  1 and f.IsOK = 1)  then 1 else 0 end) - sum(case od.OperationTypeID when 2 then 1 else 0 end) TicketCount ,
          sum(case when (od.OperationTypeID =  1 and f.IsOK = 1)  then od.Amount else 0 end) - sum(case od.OperationTypeID when 2 then od.Amount else 0 end) Total 
        from 
          v_rOperation o with (nolock) 
          join OperationDetails od with (nolock) on od.OperationID = o.ID
          left join OperationDetails odr with (nolock) on odr.PreviousOperationDetailsID = od.ID
          left join OperationDetails ods with (nolock) on ods.ID = od.PreviousOperationDetailsID and od.OperationTypeID = 2 
          left join v_rOperation os with (nolock) on ods.OperationID = os.ID 
          left join Form f with (nolock) on f.OperationDetailsID = coalesce(ods.ID, od.ID)
          left join ForeignTransaction ft with (nolock) on od.ID = ft.OperationDetailsID and od.OperationTypeID =  1
          left join ForeignTransaction ftr with (nolock) on od.PreviousOperationDetailsID = ftr.OperationDetailsID and od.OperationTypeID =  2
          left join v_Repertoire prep on od.PerformanceID = prep.PerformanceID
          left join (select sum(Points) Points, OperationDetailsID from CardAccountOperation cao where CardAccountOperationTypeID = 5 group by OperationDetailsID) pts on od.ID = pts.OperationDetailsID
          left join (select sum(Points) Points, OperationDetailsID from CardAccountOperation cao where CardAccountOperationTypeID = 7 group by OperationDetailsID) ptsr on od.ID = ptsr.OperationDetailsID 
          left join v_rPaymentDetails vpd on vpd.OperationDetailsID = od.ID and o.PaymentTypeID = 9
          right join v_Repertoire p on od.PerformanceID = p.PerformanceID
        where
          p.StartTime  between convert(DateTime, '", sales.dates[i]," 07:00:00', 104) and convert(DateTime, '",sales.dates[i+1] ," 06:59:59', 104) 
        group by
          p.FilmID ,
          p.FilmCopyID ,
          p.StartDate 
        ) data 
          left join (select f.ID FilmID, f.*, g.Name GenreName, al.Name AgeLimitationName
      from Film f 
        left join Genre g on f.GenreID = g.ID
        left join AgeLimitation al on f.AgeLimitationID=al.ID) Query0 on Query0.FilmID = data.FilmID
        left join FilmCopy on FilmCopy.ID = data.FilmCopyID
    ) res", sep = "")
  
  res <- sqlQuery(con, sql.qry)
  res$Total <- as.numeric(res$Total)
  date <- as.character(as.Date(sales.dates[i], format = "%d.%m.%Y"))
  sales[date,] <<- sum(res$Total)
  return(c(date,sales[date,]))
}))

sales <- read.csv("sales.csv")
head(sales, 155)
ix.min <- min(which(sales$total != 0))
ix.max <- max(which(sales$total != 0))

sales.dates.total <- as.Date(sales.dates[ix.min:ix.max], "%d.%m.%Y")
sales.dates.total <- as.Date(sales$X[ix.min:ix.max])
sales.total <- sales$total[ix.min:ix.max]
tsdisplay(sales.total)


head(sales.total)
head(sales.dates.total)
head(sales[c(ix.min, ix.max), ])
tail(head(sales, 400))

start.date <- sales.dates.total[1]
start.date.ts <- as.numeric(unlist(strsplit(as.character(format(as.Date(start.date),"%Y-%m-%d")),"-")))

ts.sales <- ts(sales.total, frequency = 365, start = start.date.ts)
#Первый раз взглянем 
plot(stl(ts.sales, s.window = "periodic"))

zoo.sales <- zoo(sales.total, sales.dates.total)
zoo.sales.month <- aggregate(zoo.sales, as.yearmon, sum)


#Попробуем построить прогнозы
start.date <- format(start(zoo.sales.month), "%Y-%m-%d")
start.date.ts <- as.numeric(unlist(strsplit(as.character(format(as.Date(start.date),"%Y-%m-%d")),"-")))
ts.sales.month <- ts(zoo.sales.month, start = start.date.ts, frequency = 12)
ts.sales.month.window <- window(ts.sales.month, start = c(2013,1), end = c(2015,5))
plot(stl(ts.sales.month.window, s.window = "periodic"))

auto.arima.fit <- auto.arima(ts.sales.month.window, trace=TRUE,
                             parallel=TRUE, num.cores=8,
                             max.order=50000,
                             ic="aic",
                             stationary=FALSE, seasonal=TRUE,
                             stepwise=FALSE, 
                             max.d=3, max.D=3,
                             max.P=3, max.Q=3,
                             max.p=5, max.q=5)
auto.arima.forecast <- forecast(auto.arima.fit, 12)
plot(auto.arima.forecast, type = "l")
lines(fitted(auto.arima.forecast), col = "green")


arima.fit <- arima(ts.sales.month.window, order=c(1,0,1), seasonal = c(2,0,0))
summary(arima.fit)
arima.forecast <- forecast(arima.fit, 12)
plot(arima.forecast, type = "l")
grid()
lines(fitted(arima.forecast), col = "green")

zoo.sales.clean <- Return.clean(zoo.sales.month, method = "boudt", alpha = 0.02)
plot(stl(zoo.sales.clean, s.window = "periodic"))
plot(decompose(zoo.sales.month))
lines(zoo.sales.clean, col = "green")
plot(sales.total, type="l")
describe(sales.total)

#Выводим всякую красоту
#Выводим данные помесячно в милионах рублей
dates.month <- index(zoo.sales.month)
sales.month <- as.numeric(zoo.sales.month)

pdf("forecast.pdf", encoding = "CP1251.enc")

main_text = "Кинотеатр Титаник"
ylab_text = "Продажи в рублях"
xlab_text = "Время"
plot(dates.month, sales.month, 
     main = main_text, sub = NULL, xlab = xlab_text, ylab = ylab_text,
     type="l", col=palette()[1])
lines(dates.month, lowess(zoo.sales.month)$y, type="l", lty="dashed", col=palette()[2])

#Добавляем линии для решетки
abline(h = axTicks(2),col="gray", lty="dashed")
abline(v = dates.month[seq(3, length(dates.month), 3)],col="gray", lty="dashed")

model <- lm(as.numeric(zoo.sales.month) ~ as.numeric(dates.month))
abline(model, col = "green")

legend("topleft", 
       c("Реальные", "Тренд", "Сглаженные"), 
       col=c("black","green", "red"),
       lty=c("solid","solid","dashed"),
       lwd=2,
       cex=1)

plot(stl(zoo.sales.month, s.window = "periodic"))

arima.fit <- arima(ts.sales.month.window, order=c(1,0,1), seasonal = c(2,0,0))
arima.forecast <- forecast(arima.fit, 12)
plot(arima.forecast, type = "l")
abline(v = axTicks(1),col="gray", lty="dashed")
abline(h = axTicks(2),col="gray", lty="dashed")

dev.off()

tsdisplay(ts.sales.month)
tsdisplay(diff(ts.sales.month))
