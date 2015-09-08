setwd("D:/GitHub/Repo_R/titanik_cinema")

s.url <- "http://www.kinopoisk.ru/film/722860/"

#####
library(httr)
library(XML)

user <- passwords("kinopoisk")$login
pass <- passwords("kinopoisk")$pass

handle <- handle("http://www.kinopoisk.ru") 
path   <- "login/"
# fields found in the login form.
login <- list(
  amember_login = user
  ,amember_pass  = pass
  ,amember_redirect_url = s.url
)

#response <- POST(handle = handle, path = path, body = login)
response <- GET(s.url, authenticate(user, pass))

c <- content(response, as = "raw")
text <- as.character(rawToChar(c))
nchar(text)
text.clear <- gsub("[\r\t\n]+", "", text, perl = TRUE)
nchar(text.clear)

pattern <- "year.*?title.*?(\\d+)"
c <- gregexpr(pattern, text, perl = TRUE)
a <- attr(c[[1]], "capture.start")[1]
b <- a + attr(c[[1]], "capture.length")[1] - 1
substr(text, a, b)

pattern <- "бюджет.*?film.*?box.*?title.*?>(.*?)<"
c <- gregexpr(pattern, text.clear, perl = TRUE, ignore.case = TRUE)
a <- attr(c[[1]], "capture.start")[2]
b <- a + attr(c[[1]], "capture.length")[2] - 1
substr(text.clear, a, b)
trimws
pattern <- "(бюджет)"
c <- gregexpr(pattern, text, perl = TRUE)
a <- attr(c[[1]], "capture.start")[1]
b <- a + attr(c[[1]], "capture.length")[1] - 1
substr(text, a, b)
