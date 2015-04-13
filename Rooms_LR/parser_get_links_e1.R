setwd("D:/GitHub/R_project/Rooms_LR")
library(stringr)

pattern <- "<.*?href=\"(.*?)\".*re-search-result-table__body-cell__title.*?>"
URL <- "http://homes.e1.ru/kupit/rooms-1,2,3,4,5,6/?gorod=ekaterinburg&form=5&zhilaya_novostrojki=1&period=2014-12-15&on_page=100&page="

all_url <- numeric()
for (j in 1:92) {
  cat(sprintf("Parse page = %d", j))
  txt <- readLines(url(paste(URL,j,sep="")))
  for (i in 1:length(txt)) {
    result <- str_match(txt[i],pattern)
    if (is.na(result[1,1])==FALSE) {
      all_url <- c(all_url,result[1,2])
    }
  }
}
write.table(all_url, file="test.csv", sep = ";", col.names = FALSE)