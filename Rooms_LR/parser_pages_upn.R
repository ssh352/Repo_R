setwd("D:/GitHub/R_project/Rooms_LR")
library(stringr)
library(httpRequest)
library(tau)

pattern <- "<table id=\"printObjA\".*?>(.*?)</table>"
links <- read.csv("test_upn.csv", sep=";", stringsAsFactors = FALSE, header = FALSE)
all_opt <- list()
for (i in 1:nrow(links)) {
  cat(sprintf("Parse page = %d\n", i))
  html <- getToHost("upn.ru",links[i,2])

  table <- fixEncoding(str_extract(html, pattern))
  table <- gsub("\\s{1,}"," ",gsub("[\\s\t\n\r]+", "", table))
  rows <- unlist(str_extract_all(table,"<tr>(.*?)</tr>"))
  opt <- str_match_all(rows,">([^<>]{2,})<")
  all_opt[[i]] <- opt
  Sys.sleep(10)
}

df <- data.frame()
for (i in 1:length(all_opt)) {
  cat(sprintf("Extract values = %d\n", i))
  if (length(all_opt[[i]]) > 0) {
    for (j in 1:length(all_opt[[i]])){
      if (nrow(all_opt[[i]][[j]]) > 1) df[i,all_opt[[i]][[j]][1,2]] <- all_opt[[i]][[j]][2,2]
    }
  }
}

dfc <- df[which(df[,"Обьект:"] == "Квартира "),]
dfc <- dfc[which(is.na(dfc[,"Цена:"]) != TRUE),]
