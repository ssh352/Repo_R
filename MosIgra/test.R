library(twitteR)
#http://habrahabr.ru/post/140093/

#reqURL <- "https://api.twitter.com/oauth/request_token"
#accessURL <- "http://api.twitter.com/oauth/access_token"
#authURL <- "http://api.twitter.com/oauth/authorize"
consumerKey <- "g5okHbYbV9EiaPRYixK48HvIi"
consumerSecret <- "g5okHbYbV9EiaPRYixK48HvIibcglcw5nuvxlB6uGvGWzAASh2uWlLGBQjZM2tEfRreMyEOfvm2"
#twitCred <- OAuthFactory$new(consumerKey=consumerKey,
#                             consumerSecret=consumerSecret,
#                             requestURL=reqURL,
#                             accessURL=accessURL,
#                             authURL=authURL)
#twitCred$handshake()
#registerTwitterOAuth(twitCred)
oauth <- getTwitterOAuth(consumerKey, consumerSecret)

api_key <- "g5okHbYbV9EiaPRYixK48HvIi"

api_secret <- "bcglcw5nuvxlB6uGvGWzAASh2uWlLGBQjZM2tEfRreMyEOfvm2"

access_token <- "2809579978-fLwhuwP0znOeN7s7ZcTFVMjjPcZqeLWe4P67e1n"

access_token_secret <- "BntJMAUBJK9A6wqa5ce473dwAg5OVwaluOG7giB5lnXVR"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets = searchTwitter("R.I.P. Rowan Atkinson", n=1500)
data = twListToDF(tweets)

library(ggplot2)
c <- ggplot(data, aes(created))
c + geom_bar() 

library(ggplot2)
data$month=sapply(data$created, function(x) {p=as.POSIXlt(x);p$mon}) 
data$hour=sapply(data$created, function(x) {p=as.POSIXlt(x);p$hour}) 
data$wday=sapply(data$created, function(x) {p=as.POSIXlt(x);p$wday}) 
ggplot(data)+geom_jitter(aes(x=wday,y=hour))