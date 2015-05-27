setwd("D:/GitHub/Repo_R/R_OCS")

library("GGally")
library(dplyr)
library(psych)
library(ggplot2)
library(grid)

PCbiplot <- function(PC, xy=c(1, 2), colors=c('black', 'black', 'red', 'red'), classic=TRUE) {
  x <- paste("PC", xy[1], sep="")
  y <- paste("PC", xy[2], sep="")
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames), color=colors[1])
  plot <- plot + theme_bw()
  plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2, color=colors[2])
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  if (classic == TRUE) {
    plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
    plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])
  } else {
    #Выбираем позитивные и негативные значения для X
    x.pca <- data.frame(value=datapc[, x], rownames=dimnames(datapc)[[1]])
    x.pca.p <- subset(x.pca, value > 0)
    x.v.p <- sort(x.pca.p[,1]) * mult * 0.4
    x.pca.n <- subset(x.pca, value < 0)
    x.v.n <- sort(x.pca.n[,1], decreasing = TRUE) * mult * 0.4
    #Выбираем позитивные и негативные значения для Y
    y.pca <- data.frame(value=datapc[, y], rownames=dimnames(datapc)[[1]])
    y.pca.p <- subset(y.pca, value > 0)
    y.v.p <- sort(y.pca.p[,1]) * mult * 0.4
    y.pca.n <- subset(y.pca, value < 0)
    y.v.n <- sort(y.pca.n[,1], decreasing = TRUE) * mult * 0.4
    
    data.line <- rbind(
      cbind(x.pca.p, start=head(cumsum(c(0, x.v.p)), -1), end=cumsum(x.v.p) ),
      cbind(x.pca.n, start=head(cumsum(c(0, x.v.n)), -1), end=cumsum(x.v.n) ),
      cbind(y.pca.p, start=head(cumsum(c(0, y.v.p)), -1), end=cumsum(y.v.p) ),
      cbind(y.pca.n, start=head(cumsum(c(0, y.v.n)), -1), end=cumsum(y.v.n) )
    )
    
    n <- nrow(data.line)
    x.coord <- rep(1, n / 2)
    y.coord <- rep(0, n / 2)
    
    data.line <- data.frame(
      data.line,
      xy=c(x.coord, y.coord),
      colors=rep(c("red","green"), round(n / 4), length.out = n / 2),
      adj=ifelse(data.line$value > 0, 1, 0)
    )
    
    plot <- plot + geom_segment(data=data.line, 
                                x=data.line$start * data.line$xy, y=data.line$start * abs(data.line$xy - 1), 
                                xend=data.line$end * data.line$xy, yend=data.line$end * abs(data.line$xy - 1), 
                                color=data.line$color, size=0.75, alpha=0.75, 
                                arrow=arrow(length=unit(0.2,"cm")))
    
    plot <- plot + coord_equal() + geom_text(data=data.line, 
                                             x=(data.line$start + data.line$end) / 2 * data.line$xy, y=(data.line$start + data.line$end) / 2 * abs(data.line$xy - 1), 
                                             color=data.line$colors, label=data.line$rownames, size = 4, angle=90 * data.line$xy, 
                                             hjust=abs(data.line$adj - abs(data.line$xy - 1)) )
  }
  return(plot)
}

#Считываем данные
f <- read.csv("rbu04_partners.csv", header = TRUE, sep=";", dec=",", stringsAsFactors = FALSE)
#Убираем дубликаты
f <- f[-which(duplicated(f$КлиентКрНаим, fromLast = FALSE)), ]

#И убираем ненужные колонки
partners <- as.data.frame(f, row.names = f$КлиентКрНаим)
p.names <- cbind(partners$КлиентКрНаим, partners$КлиентНомер)
partners$КлиентКрНаим <- NULL
partners$КлиентНомер <- NULL
#nrow(p.names)

#Запоминаем оригинальные названия колонок
colnames <- names(partners)
names(partners) <- c("margin", "sum", "cost", "m.rebate", "size", "orders", "corrCO", "corrS", "count", "rows", "m.conv")
ggpairs(partners)

#Используем метод главных компонент
#Смотрим сильно коррелированные данные и убираем из их выборки
#ggpairs(p)
# 2, 4, 7, 8, 9
#p.select <- dplyr::select(partners, -m.rebate, -cost, -count, -corrCO, -corrS)
p.select <- partners[, -c(3, 4, 7, 8, 9)]
pca.partners <- prcomp(p.select, scale=TRUE)
#Можно поспотреть как распределены теперь новые компоненты
#ggpairs(p.select)

l <- ncol(pca.partners$x)
pdf("rbu04_PCA.pdf", family = "NimbusSan", encoding = "CP1251.enc", width = 10, height = 10)
options(warn=-1)
for (i in 1:(l-1)) {
  for (j in seq(i+1,l,length = max(0,l-i)) ) {
    print(paste(i,j))
    pca.partners.ij <- pca.partners
    
    sd.pci <- 2 * sd(pca.partners$x[, i])
    sd.pcj <- 2 * sd(pca.partners$x[, j])
    
    ix.sd <- which(between(pca.partners$x[, i], -sd.pci, sd.pci) & between(pca.partners$x[, j], -sd.pcj, sd.pcj))
    dimnames(pca.partners.ij$x)[[1]][ix.sd] <- "."
    
    print(PCbiplot(pca.partners.ij, xy=c(i, j), classic = FALSE))
    #print("Press [enter] to continue")
    #line <- readline()
  }
}
dev.off()
options(warn=0)
embedFonts("rbu04_PCA.pdf") 

#names(partners) <- c("margin", "sum", "cost", "m.rebate", "size", "orders", "corrCO", "corrS", "count", "rows", "m.conv")
p.select <- partners[, -c(3, 4, 5, 7, 8, 9, 10)]
pca.partners <- prcomp(p.select, scale=TRUE)
#Можно поспотреть как распределены теперь новые компоненты
#ggpairs(p.select)
#summary(pca.partners)

l <- ncol(pca.partners$x)
pdf("rbu04_PCA_cut.pdf", family = "NimbusSan", encoding = "CP1251.enc", width = 10, height = 10)
options(warn=-1)
for (i in 1:(l-1)) {
  for (j in seq(i+1,l,length = max(0,l-i)) ) {
    print(paste(i,j))
    pca.partners.ij <- pca.partners
    
    sd.pci <- 2 * sd(pca.partners$x[, i])
    sd.pcj <- 2 * sd(pca.partners$x[, j])
    
    ix.sd <- which(between(pca.partners$x[, i], -sd.pci, sd.pci) & between(pca.partners$x[, j], -sd.pcj, sd.pcj))
    dimnames(pca.partners.ij$x)[[1]][ix.sd] <- "."
    
    print(PCbiplot(pca.partners.ij, xy=c(i, j)))
    #print("Press [enter] to continue")
    #line <- readline()
  }
}
dev.off()
options(warn=0)
embedFonts("rbu04_PCA_cut.pdf") 

#######################
PC <- pca.partners
xy <- c(2, 3)
colors=c('black', 'black', 'red', 'red')

x <- paste("PC", xy[1], sep="")
y <- paste("PC", xy[2], sep="")
# PC being a prcomp object
data <- data.frame(obsnames=row.names(PC$x), PC$x)
plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames), color=colors[1])
plot <- plot + theme_bw()
plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2, color=colors[2])
datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
mult <- min(
  (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
  (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
)
datapc <- transform(datapc,
                    v1 = .7 * mult * (get(x)),
                    v2 = .7 * mult * (get(y))
)
#Добавляем текст
#plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
#Добавялем стрелочки измерений
#plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])
#plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])

#plot

#newplot <- plot
#Выбираем позитивные и негативные значения для X
x.pca <- data.frame(value=datapc[, x], rownames=dimnames(datapc)[[1]])
x.pca.p <- subset(x.pca, value > 0)
x.v.p <- sort(x.pca.p[,1]) * mult * 0.4
x.pca.n <- subset(x.pca, value < 0)
x.v.n <- sort(x.pca.n[,1], decreasing = TRUE) * mult * 0.4
#Выбираем позитивные и негативные значения для Y
y.pca <- data.frame(value=datapc[, y], rownames=dimnames(datapc)[[1]])
y.pca.p <- subset(y.pca, value > 0)
y.v.p <- sort(y.pca.p[,1]) * mult * 0.4
y.pca.n <- subset(y.pca, value < 0)
y.v.n <- sort(y.pca.n[,1], decreasing = TRUE) * mult * 0.4

data.line <- rbind(
  cbind(x.pca.p, start=head(cumsum(c(0, x.v.p)), -1), end=cumsum(x.v.p) ),
  cbind(x.pca.n, start=head(cumsum(c(0, x.v.n)), -1), end=cumsum(x.v.n) ),
  cbind(y.pca.p, start=head(cumsum(c(0, y.v.p)), -1), end=cumsum(y.v.p) ),
  cbind(y.pca.n, start=head(cumsum(c(0, y.v.n)), -1), end=cumsum(y.v.n) )
)

n <- nrow(data.line)
x.coord <- rep(1, n / 2)
y.coord <- rep(0, n / 2)

data.line <- data.frame(
  data.line,
  xy=c(x.coord, y.coord),
  colors=rep(c("red","green"), round(n / 4), length.out = n / 2),
  adj=ifelse(data.line$value > 0, 1, 0)
)

plot <- plot + geom_segment(data=data.line, 
                       x=data.line$start * data.line$xy, y=data.line$start * abs(data.line$xy - 1), 
                       xend=data.line$end * data.line$xy, yend=data.line$end * abs(data.line$xy - 1), 
                       color=data.line$color, size=0.75, alpha=0.75, 
                       arrow=arrow(length=unit(0.2,"cm")))

plot <- plot + coord_equal() + geom_text(data=data.line, 
  x=(data.line$start + data.line$end) / 2 * data.line$xy, y=(data.line$start + data.line$end) / 2 * abs(data.line$xy - 1), 
  color=data.line$colors, label=data.line$rownames, size = 4, angle=90 * data.line$xy, 
  hjust=abs(data.line$adj - abs(data.line$xy - 1)) )


ggplot(df, aes(Date, CPU)) + geom_point() + 
  annotate("text", x = as.POSIXct(c("2013-03-11 23:00:00")), 
           y = 90, label = "problem 1", angle=90, size=5, 
           colour='black', face="bold")
+    geom_segment(aes(x = as.POSIXct(c("2013-03-11 09:00:00")), y = -30, xend = as.POSIXct(c("2013-03-11 09:00:00")), yend = 0), colour='#CC00FF', size=1,arrow = arrow(length = unit(0.5, "cm")))