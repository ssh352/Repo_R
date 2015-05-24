setwd("D:/GitHub/Repo_R/R_OCS")

library("GGally")
library(dplyr)
library(psych)
library(ggplot2)
library(grid)

PCbiplot <- function(PC, xy=c(1, 2), colors=c('black', 'black', 'red', 'red')) {
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
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])
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
    
    print(PCbiplot(pca.partners.ij, xy=c(i, j)))
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
