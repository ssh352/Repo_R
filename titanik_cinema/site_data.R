setwd("D:/GitHub/Repo_R/titanik_cinema")

options(scipen=5)

library(RODBC)
library(sqldf)

server <- "cisrepl-v.ocs.ru"
db.name <- "site"
login <- passwords(server)$login
pass <- passwords(server)$password
connectionString <- paste("driver={SQL Server};server=", server,";database=", db.name ,";uid=", login,";pwd=", pass, sep="")

con <- odbcDriverConnect(connectionString)
sql.qry <- "SELECT * FROM [pc_Film] WHERE LEN(KinopoiskFilmID) > 0"
res <- sqlQuery(con, sql.qry)
