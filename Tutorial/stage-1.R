library(car) 
Моллюски <-
  read.table("http://figshare.com/media/download/98923/97987",
             header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
cor.test(Моллюски$CAnumber, Моллюски$ZMlength,
         alternative="two.sided", method="pearson")
scatterplot(CAnumber ~ ZMlength | Lake, reg.line=lm, smooth=TRUE,
            spread=TRUE, boxplots='xy', span=0.5, ylab="Численность инфузорий",
            xlab="Длина раковины", by.groups=FALSE, data=Моллюски)
            
