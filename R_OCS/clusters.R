setwd("D:/GitHub/Repo_R/R_OCS")

#install.packages("car")
install.packages("pastecs")
library(sqldf)
library(Hmisc)
library(pastecs)
library(xlsx)

data <- read.csv("clusters.csv", header=TRUE, stringsAsFactors=FALSE, dec=",", sep=";")
full_names <- names(data)
names(data) <- c("RBU", "DaxID", "ShortName", "Margin", "Shipment", "Cost", "MarginRebate", "Size", "Orders", "CorrCO", "CorrSales", "Counts","Rows","Convert")
data[,1] <- as.factor(data[,1])
head(data)
levels(data[,1])

f04 <- sqldf("SELECT * FROM data WHERE rbu = 'РБЮ Екатеринбург'")

#Смотрим распределение значений
for (i in 4:14) {
  x <- f04[,i]
  name <- full_names[i]
  h <- hist(x, freq=TRUE, breaks=36, col="red", xlab=name, axes=FALSE)
  axis(1, axTicks(1), labels = axTicks(1) / 1000)
  axis(2, axTicks(2), labels = axTicks(2))
  rug(jitter(x))
  #lines(density(x, col="blue", lwd=2))
  
  xfit <- seq(min(x), max(x), length=40)
  yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="green", lwd=2)

  summary(x)
  
  cat ("Press [enter] to continue")
  line <- readline()
}


describe(f04[,-c(1:3)])
stat.desc(f04[,-c(1:3)])

boxplot(f04$Shipment, f04$Cost)
boxplot(f04$Margin, f04$MarginRebate)
x <- f04$Shipment + abs(min(f04$Shipment))

shipment <- log(f04$Shipment)
shipment[which(is.nan(shipment) == TRUE)] <- 2
hist(shipment, breaks = 24)

size <- log(f04$Size)
size[which(is.nan(size) == TRUE)] <- -10
size[which(is.infinite(size) == TRUE)] <- -10
size_mod <- size + min(size)
size_mod <- abs(size_mod - max(size_mod))
hist(size_mod, breaks = 24)


margin <- log(f04$Margin)
margin[which(is.nan(margin) == TRUE)] <- -3
hist(margin, breaks = 24)

orders <- log(f04$Orders)
#orders[which(is.nan(orders) == TRUE)] <- -3
hist(orders, breaks = 24)
orders_mod <- abs(orders - max(orders))

#f04_cut <- sqldf("SELECT Margin, Shipment,Size, Orders, CorrCO, CorrSales, Counts, Rows, Convert FROM f04")

analyze <- cbind(margin, shipment, size_mod, orders_mod)

c <- kmeans(analyze, 6)

write.xlsx(cbind(c$cluster, f04), file="clusters_f04.xlsx")


f01 <- sqldf("SELECT * FROM data WHERE rbu = 'OCS Центральный офис'")

shipment <- log(f01$Shipment)
shipment[which(is.nan(shipment) == TRUE)] <- 4
shipment[which(is.infinite(shipment) == TRUE)] <- -4
hist(shipment, breaks = 24)

size <- log(f01$Size)
size[which(is.nan(size) == TRUE)] <- -14
size[which(is.infinite(size) == TRUE)] <- -14
hist(size, breaks = 24)
size_mod <- size + min(size)
size_mod <- abs(size_mod - max(size_mod))

margin <- log(f01$Margin)
margin[which(is.nan(margin) == TRUE)] <- -2
margin[which(is.na(margin) == TRUE)] <- -2
hist(margin, breaks = 24)

orders <- log(f01$Orders)
hist(orders, breaks = 24)
orders_mod <- abs(orders - max(orders))

analyze <- cbind(margin, shipment, size_mod, orders_mod)

describe(analyze)

c <- kmeans(analyze, 6)

write.xlsx(cbind(c$cluster, f01), file="clusters_f01.xlsx")

cut <- sqldf("SELECT RBU, Margin, Shipment,Size, Orders, CorrCO, CorrSales, Counts, Rows, Convert FROM data")

for(rbu in levels(cut[,1])) {
  print(rbu)
}
