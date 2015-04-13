setwd("D:/GitHub/R_project/Rooms_LR")
library(stringr)

pattern <- "href=\"(/realty_eburg_flat_sale_info.*?)\""
URL <- "http://upn.ru/realty_eburg_flat_sale.htm?ofdays=7&ag=0&vm=1&scn=6&id=%d"

all_url <- numeric()
for (j in 1:18) {
  cat(sprintf("Parse page = %d\n", j))
  txt <- readLines(url(sprintf(URL,j)))
  for (i in 1:length(txt)) {
    result <- str_match(txt[i],pattern)
    if (is.na(result[1,1])==FALSE) {
      all_url <- c(all_url,result[1,2])
    }
  }
}
write.table(all_url, file="test_upn.csv", sep = ";", col.names = FALSE)
