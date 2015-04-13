setwd("D:/GitHub/R_project/DNS")
library(RODBC)
library(plyr)

#library(ggplot2)
#library(reshape)
con <- odbcDriverConnect("driver={SQL Server};server=plumbum.ural.ocs.ru;database=twitter;uid=sa;pwd=107109")
#con <- odbcDriverConnect("driver={SQL Server};server=cisrepl-v.ocs.ru;database=cisco;uid=reader;pwd=xbnfntkm")
#con <- odbcConnect("CISREPL", uid='reader', pwd='xbnfntkm')
res <- sqlQuery(con, 'SELECT [date]
    ,[score]
      ,[joy]
      ,[surprise]
      ,[sadness]
      ,[disgust]
      ,[fear]
      ,[anger]
  FROM [twitter].[dbo].[tweets_score] AS sc
  LEFT JOIN [twitter].[dbo].[tweets] AS tw ON sc.tweetid = tw.tweetid')
score <- aggregate(x = res[c("score","joy","surprise","sadness","disgust","fear","anger")], FUN = sum, by = list(Group.date = substr(res$date,1,10)))

#Molten <- melt(score, id.vars = "Group.date")
#ggplot(Molten, aes(x = Group.date, y = value, colour = variable)) + geom_line()
#plot(score$score,ylim=c(-100,100),type="l",col="2")
plot(as.Date(score$Group.date),score[,2],xlim=c(min(as.Date(score$Group.date)),max(as.Date(score$Group.date))),
     ylim=c(-100,100),type="l",col="2", xlab="Date", ylab="Score")
for (f in c(2,3,4)) {
  lines(as.Date(score$Group.date),score[,f],type="l", col=f)
}
legend("topleft",names(score)[c(2,3,4)],col=c(2,3,4),lty=c(1,1))
grid()

plot(as.Date(score$Group.date),score[,2],xlim=c(min(as.Date(score$Group.date)),max(as.Date(score$Group.date))),
     ylim=c(-100,100),type="l",col="2", xlab="Date", ylab="Score")
for (f in c(2,5:8)) {
  lines(as.Date(score$Group.date),score[,f],type="l", col=f)
}
legend("topleft",names(score)[c(2,5:8)],col=c(2,5:8),lty=c(1,1))
grid()
