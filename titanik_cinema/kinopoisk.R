setwd("D:/GitHub/Repo_R/titanik_cinema")

s.url <- "http://www.kinopoisk.ru/film/722860/"

#####
library(httr)
library(XML)

login <- passwords("kinopoisk")$login
pass <- passwords("kinopoisk")$pass

handle <- handle("http://www.kinopoisk.ru") 
path   <- "login/"
# fields found in the login form.
login <- list(
  amember_login = login
  ,amember_pass  = pass
  ,amember_redirect_url = s.url
)

#response <- POST(handle = handle, path = path, body = login)
response <- GET(s.url, authenticate(login, pass))

c <- content(response, as = "raw")
text <- as.character(rawToChar(c))
nchar(text)
text.clear <- sub("\n", "", text)
nchar(text.clear)

pattern <- "year.*?title.*?(\\d+)"
c <- gregexpr(pattern, text, perl = TRUE)
a <- attr(c[[1]], "capture.start")[1]
b <- a + attr(c[[1]], "capture.length")[1] - 1
substr(text, a, b)

pattern <- "film/722860/box.*?title.*?>(.*?)<"
c <- gregexpr(pattern, text, perl = TRUE)
a <- attr(c[[1]], "capture.start")[1]
b <- a + attr(c[[1]], "capture.length")[1] - 1
substr(text, a, b)
trimws
pattern <- "(бюджет)"
c <- gregexpr(pattern, text, perl = TRUE)
a <- attr(c[[1]], "capture.start")[1]
b <- a + attr(c[[1]], "capture.length")[1] - 1
substr(text, a, b)
