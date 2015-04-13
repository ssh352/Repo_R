#library(XLConnect)
#options(java.parameters = "-Xmx4g")
#wk = loadWorkbook("services-price-change-report.xlsx")
# 8 - desc
# 10 - code
data <- read.csv("serv_glrus_scs.csv", dec=",", header=TRUE, sep = ";", stringsAsFactors=FALSE)
uniq_str <- numeric()
#uniq_str <- as.vector(uniq_str)
#for (i in c(1:nrow(data))) {
for (i in c(1:nrow(data))) {
  str <- data[i,8]
  if (nchar(str) > 0) {
    str <- gsub(",|\\(\\)|\\s+"," ",str)
    vector <- unlist(strsplit(str, " "))
    uniq_str <- unlist(unique(c(uniq_str, vector)))
  }
}

