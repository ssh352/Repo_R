setwd("D:/GitHub/Repo_R/titanik_cinema")

options(scipen=5)

library(RODBC)
library(sqldf)
library(GGally)
library(corrgram)
library(httr)
library(memisc)
library(car)
library(XML)
library(randomForest)
library(rpart)
library(MASS)
library(leaps)
library(openxlsx)

server <- "cisrepl-v.ocs.ru"
db.name <- "site"
user <- passwords(server)$login
pass <- passwords(server)$password
connectionString <- paste("driver={SQL Server};server=", server,";database=", db.name ,";uid=", user,";pwd=", pass, sep="")

con <- odbcDriverConnect(connectionString)

#Получаем список фильмов
sql.qry <- "SELECT f.ID, f.Name, f.Duration, f.KinopoiskFilmID, f.Year, f.MainGenre FROM [pc_Film] AS f WHERE LEN(KinopoiskFilmID) > 0"
films <- sqlQuery(con, sql.qry)
#Дополнительные жанры по фильму
sql.qry <- "SELECT * FROM [pc_FilmGenre]"
films.genre <- sqlQuery(con, sql.qry)
#Расшифровка жанров, для удоства
sql.qry <- "SELECT * FROM [pc_Genre]"
genre <- sqlQuery(con, sql.qry)
#Сопоставление между сайтом и базой кинотеатра
sql.qry <- "SELECT DISTINCT [FilmID]
            ,[tsFilmID]
            FROM [site].[dbo].[pc_FilmTS]"
films.compare <- sqlQuery(con, sql.qry)

i <- 45
for(i in 678:nrow(films)) {
  print(i)
  kinoposikID <- films$KinopoiskFilmID[i]
  s.url <- paste("http://www.kinopoisk.ru/film/", kinoposikID ,"/", sep = "")
  response <- GET(s.url, authenticate(user, pass))
  
  #Преобразовываем ответ в обычный текст
  c <- content(response, as = "raw")
  text <- as.character(rawToChar(c))
  text.clear <- gsub("[\r\t\n]+", "", text, perl = TRUE)

  #Ищем в полученном ответе тип валюты и бюджет
  pattern <- "бюджет.*?class=.*?(\\w+).*?film.*?box.*?title.*?>(.*?)<"
  regex.res <- gregexpr(pattern, text.clear, perl = TRUE, ignore.case = TRUE)
  regex.start <- attr(regex.res[[1]], "capture.start")
  regex.length <- attr(regex.res[[1]], "capture.length")
  if (nrow(regex.start) < 2) {
    pattern <- "сборы в России.*?class=.*?(\\w+).*?film.*?box.*?title.*?>(.*?)<"
    regex.res <- gregexpr(pattern, text.clear, perl = TRUE, ignore.case = TRUE)
    regex.start <- attr(regex.res[[1]], "capture.start")
    regex.length <- attr(regex.res[[1]], "capture.length")
  }
  
  if (nrow(regex.start) > 1) {
    #Тип валюты
    currency <- substr(text.clear, regex.start[2,1], (regex.start[2,1] + regex.length[2,1] - 1))
    #Бюджет
    budget <- as.numeric(gsub("\\D", "", substr(text.clear, regex.start[2,2], (regex.start[2,2] + regex.length[2,2] - 1))))
  
    #Запоминаем полученные данные
    films[i, "budget"] <- budget
    films[i, "currency"] <- currency
  }
}

#Убираем разные кривые строки
films.test <- sqldf("SELECT * FROM films WHERE currency = 'dollar'")
films.test <- na.omit(films.test)

sql.qry <- "set datefirst 1 select * from (
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
t.FilmID FilmID ,
t.FilmCopyID FilmCopyID ,
t.NewStartDate PerformanceDateDim ,
sum(t.Visits) TicketCount ,
count(t.PerformanceID) PerformancesCount ,
sum(t.Revenue) Total 
from 
(select st.*, 
sc.*,
case when datepart(hour, st.StartTime) > 7 and datepart(hour, st.StartTime) < 23 then 1 else 2 end ClubID,
st.CinemaDate NewStartDate,
mm.MaxStartDate, mm.MinStartDate
from v_rLeaseTotals st
left join (select StructureElementID, 
count(*) SeatCount
from Seat
group by StructureElementID) sc on sc.StructureElementID = st.HallVarianceID
left join (select min(CinemaDate) MinStartDate, max(CinemaDate) MaxStartDate, FilmID from v_Repertoire where StartTime between convert(DateTime, '26.08.2015 07:00:00', 104) and convert(DateTime, '27.08.2015 06:59:59', 104) group by FilmID) mm on mm.FilmID = st.FilmID    
) t

where  t.StartTime  between convert(DateTime, '01.01.2010 07:00:00', 104) and convert(DateTime, '27.08.2015 06:59:59', 104) 
group by
t.FilmID ,
t.FilmCopyID ,
t.NewStartDate 
) data 
left join (select f.ID FilmID, f.*, g.Name GenreName, al.Name AgeLimitationName
from Film f 
left join Genre g on f.GenreID = g.ID
left join AgeLimitation al on f.AgeLimitationID=al.ID) Query0 on Query0.FilmID = data.FilmID
left join FilmCopy on FilmCopy.ID = data.FilmCopyID

) res"

server <- "cisrepl-v.ocs.ru"
db.name <- "titanik"
user <- passwords(server)$login
pass <- passwords(server)$password
connectionString <- paste("driver={SQL Server};server=", server,";database=", db.name ,";uid=", user,";pwd=", pass, sep="")

con <- odbcDriverConnect(connectionString)
filmsRent <- sqlQuery(con, sql.qry)

films.rent <- sqldf("SELECT FilmID, AgeLimitationName, PerformanceDateDim, Total, FilmName, TicketCount, PerformancesCount FROM filmsRent ORDER BY FilmID, PerformanceDateDim")

head(films.compare)
head(films.test)
head(films.rent)
#head(films.genre)
head(genre)

#Объединяем по жанрам
filmsInfo <- merge(films.test, films.compare,  by.x = "ID", by.y = "FilmID", stringsAsFactor = FALSE)
filmsInfo <- merge(filmsInfo, genre,  by.x = "MainGenre", by.y = "ID")

#Группируем по айдишнику фильма использую сумму
filmsSum <- aggregate(Total ~ FilmID, data = films.rent, FUN = sum)
names(filmsSum) <- c("FilmID", "sum")

#Группируем по айдишнику фильма использую Count
filmsCount <- aggregate(Total ~ FilmID, data = films.rent, FUN = length)
names(filmsCount) <- c("FilmID", "count")

#Группируем по айдишнику фильма использую Count
filmsTickets <- aggregate(TicketCount ~ FilmID, data = films.rent, FUN = sum)
names(filmsTickets) <- c("FilmID", "tickets")

#Группируем по айдишнику фильма использую Count
filmsPerfomance <- aggregate(PerformancesCount ~ FilmID, data = films.rent, FUN = sum)
names(filmsPerfomance) <- c("FilmID", "perfomances")

filmsInfo <- merge(filmsInfo, filmsSum,  by.x = "tsFilmID", by.y = "FilmID")
filmsInfo <- merge(filmsInfo, filmsCount,  by.x = "tsFilmID", by.y = "FilmID")
filmsInfo <- merge(filmsInfo, filmsTickets,  by.x = "tsFilmID", by.y = "FilmID")
filmsInfo <- merge(filmsInfo, filmsPerfomance,  by.x = "tsFilmID", by.y = "FilmID")

names(filmsInfo)[4] <- "Name"
names(filmsInfo)[10] <- "Genre"
head(filmsInfo)

#Добавляем ограничение по возрасту
filmsInfo <- merge(filmsInfo, unique(films.rent[, c("FilmID", "AgeLimitationName")]),  by.x = "tsFilmID", by.y = "FilmID", all.x = TRUE, all.y = FALSE)

#Убираем лишние колонки и строки
films_for_predict <- sqldf("SELECT KinopoiskFilmID, Name, Duration AS duration, Genre, AgeLimitationName AS age, 
                           budget, sum, count, tickets, perfomances FROM filmsInfo WHERE sum > 0")
films_for_predict$age <- factor(films_for_predict$age)
str(films_for_predict)

#Делаем запрос на рейтинг
i <- 1
for(i in 1:nrow(films_for_predict)) {
  print(i)
  kinoposikID <- films_for_predict$KinopoiskFilmID[i]
  s.url <- paste("http://rating.kinopoisk.ru/", kinoposikID ,".xml", sep = "")
  xmlfile <- xmlParse(s.url)
  if (length(getNodeSet(xmlfile, "/rating/kp_rating")) > 0) {
    films_for_predict$kp[i] <- as.numeric(xmlValue(getNodeSet(xmlfile, "/rating/kp_rating")[[1]]))
  } else {
    films_for_predict$kp[i] <- NA
  }
  if (length(getNodeSet(xmlfile, "/rating/imdb_rating")) > 0) {
    films_for_predict$imdb[i] <- as.numeric(xmlValue(getNodeSet(xmlfile, "/rating/imdb_rating")[[1]]))
  } else {
    films_for_predict$imdb[i] <- NA
  }
}


#Отбираем уровние которые будем использовать 
#Комедия - 7, Мюзикл - 11, Фантастика - 18, Фэнтези - 19, 12+ - 2
genre_lvl <- levels(films_for_predict_imdb$Genre)
age_lvl <- levels(films_for_predict_imdb$age)

#Добавляем категорированные бюджеты, чтобы избежать выборосов
films_for_predict$budget.cat <- cut(films_for_predict$budget/1000000, breaks = 5)
levels(films_for_predict$budget.cat)

films_for_predict$sum.cat <- cut(films_for_predict$sum/1000000, breaks = 10)
levels(films_for_predict$sum.cat)

films_for_predict$sum.mean <- round(films_for_predict$sum/films_for_predict$perfomances)

mean_ticket_price <- round(sum(films_for_predict$sum) / sum(films_for_predict$tickets))


films_for_predict_imdb <- sqldf("SELECT * FROM films_for_predict WHERE imdb IS NOT NULL AND count >= 7 ORDER BY count")
head(films_for_predict_imdb)
tail(films_for_predict_imdb)

#sqldf("SELECT * FROM films_for_predict WHERE imdb < 1")
#save(films_for_predict, file = "films.dat")

#m1 <- lm(data = films_for_predict, log(sum) ~ log(duration) + log(count) + log(budget)          + I(log(duration)^2) + I(log(count)^2) + I(log(budget)^2))
#m2 <- lm(data = films_for_predict, log(sum) ~ log(duration) * log(count) * log(budget))

#m2.2 <- lm(data = films_for_predict, log(sum) ~ log(duration) * log(budget) + factor(Genre))

#m2.3 <- lm(data = films_for_predict, log(sum) ~ log(budget) + factor(Genre))

#m2.4 <- lm(data = films_for_predict, log(sum) ~ log(budget) + factor(Genre) + factor(age))

#m2.5 <- lm(data = films_for_predict, log(sum) ~ factor(budget.cat) + factor(Genre) + factor(age))

#m2.6 <- lm(data = films_for_predict, log(sum) ~ log(kp) + log(budget) + factor(Genre) + factor(age))

#m1 <- lm(data = films_for_predict, log(sum.mean) ~ log(kp) + log(budget) + factor(Genre) + factor(age))

#m2 <- lm(data = films_for_predict, log(sum.mean) ~ log(budget) + factor(Genre) + factor(age))


#m4 <- lm(data = films_for_predict, log(sum.mean) ~ factor(Genre))

#m5 <- lm(data = films_for_predict, log(sum.mean) ~ log(budget))


contrasts(films_for_predict_imdb$Genre)
contrasts(films_for_predict_imdb$Genre) <- contr.treatment(20)
m1 <- lm(data = films_for_predict, log(sum.mean) ~ factor(Genre))
summary(m1)
AIC(m1)

dummies_genre <- predict(dummyVars(~ Genre, data = films_for_predict_imdb), newdata = films_for_predict_imdb)
names_dummies_genre <- c("none", "animation", "action", "western", "docum", "drama", "comedy", "crime", "melodrama",
                          "сartoon", "musical", "adventure", "action_adventure", "family", "sport",
                          "thriller", "horror", "fantastic", "fantasy", "erotica")
dimnames(dummies_genre)[[2]] <- names_dummies_genre

dummies_age <- predict(dummyVars(~ age, data = films_for_predict_imdb), newdata = films_for_predict_imdb)
dimnames(dummies_age)[[2]] <- c("age0", "age12", "age16", "age18", "age6")

films_for_predict_imdb_dummy <- cbind(films_for_predict_imdb[, c("sum.mean", "budget","kp","imdb")], dummies_genre, dummies_age)
head(films_for_predict_imdb_dummy)

m2 <- lm(data = films_for_predict_imdb, 
         sum.mean ~ factor(Genre))
summary(m2)
BIC(m2)
head(films_for_predict_imdb_dummy)

m3 <- lm(data = films_for_predict_imdb_dummy, 
         log(sum.mean) ~ log(imdb) + log(kp) + log(budget))
summary(m3)

m4 <- lm(data = films_for_predict_imdb_dummy, 
         log(sum.mean) ~ log(imdb) + log(kp) + log(budget) + action + comedy + сartoon + fantastic + fantasy)
summary(m4)

m5 <- lm(data = films_for_predict_imdb_dummy, 
         log(sum.mean) ~ log(imdb) * log(kp) * log(budget) * action * comedy * сartoon * fantastic * fantasy)
summary(m5)

m6 <- lm(data = films_for_predict_imdb, 
         log(sum.mean) ~ log(imdb) + log(kp) + log(budget) + factor(budget.cat) + factor(Genre))
summary(m6)

m7 <- lm(data = films_for_predict_imdb, 
         log(sum.mean) ~ log(imdb) * log(kp) * log(budget) * factor(Genre) * factor(budget.cat))
summary(m7)
result <- films_for_predict_imdb[ , c("Name", "Genre", "budget", "sum")]

result$fitted <- round(exp(fitted(m7))) * films_for_predict_imdb$perfomances

write.xlsx(result, file = "fitted.xlsx")

leaps <- regsubsets(log(sum.mean) ~ log(duration) + log(imdb)  + log(kp) + log(budget) + factor(Genre) + factor(age), 
                   data=films_for_predict_imdb, nbest=4)
plot(leaps)

leaps <- regsubsets(log(sum.mean) ~ log(imdb) + log(kp) + log(budget) + muzikl + fantastic + fantasy + age12, 
                    data=films_for_predict_imdb_dummy, nbest=4)
plot(leaps)

#m3 <- lm(data = films_for_predict, log(sum) ~ log(duration) * log(count) * log(budget))

#m4 <- lm(data = films_for_predict, log(sum) ~ log(duration) * log(count) * log(budget) * factor(Genre))

#m6 <- lm(data = films_for_predict, log(sum) ~ log(count) * log(budget) * factor(Genre))
#m6 <- lm(data = films_for_predict, log(sum.mean) ~ log(budget) * factor(Genre))

mtable(m3, m4, m5)

stepAIC(m4, direction = "backward")

summary(m6)
films_for_predict <- na.omit(films_for_predict)
films_for_predict <- droplevels(films_for_predict)
rf.fit <- randomForest(sum.cat ~ budget.cat + Genre + age, 
                       data=films_for_predict, importance=TRUE, ntree=2000)
varImpPlot(rf.fit)
Prediction <- predict(rf.fit, films_for_predict[,c("budget.cat", "Genre", "age")])

rp.fit <- rpart(as.numeric(sum.cat) ~ budget.cat + Genre + age, data=films_for_predict, method="class")
plot(rp.fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(rp.fit)
Prediction <- predict(rp.fit, films_for_predict[,c("budget.cat", "Genre", "age")])

table(real = as.numeric(films_for_predict$sum.cat), predict = as.numeric(Prediction))

net <- neuralnet(sum.cat ~ budget.cat + Genre + age, data = films_for_predict, hidden=10, threshold=0.01)
str(films_for_predict)

#m3 <- glm(data = films_for_predict, log(sum) ~ log(duration) + log(count) + log(budget)          + I(log(duration)^2) + I(log(count)^2) + I(log(budget)^2))
#m4 <- glm(data = films_for_predict, log(sum) ~ log(duration) * log(count) * log(budget))


nn

cbind(as.character(films_for_predict$Name), as.numeric(films_for_predict$sum), as.numeric(exp(fitted(m1)), exp(fitted(m2)), exp(fitted(m3)), exp(fitted(m4)))
mtable(m1, m2, m3, m4)
scatterplotMatrix(~ duration + count + budget, data = films_for_predict, spread = FALSE)
ggpairs(data = films_for_predict, columns = 2:5)

