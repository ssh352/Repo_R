setwd("D:/Data Mining/_data/Crime")

data <- read.csv("train.csv", header = TRUE, stringsAsFactor = FALSE)
head(data)
str(data)
levels(data$Resolution)
table(data$Resolution)

data2 <- data
data2$Dates <- as.POSIXlt(data2$Dates)
table(data2$DayOfWeek)
plot(data2$DayOfWeek)
