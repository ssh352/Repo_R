#
# Projecting XIV risk
#
library(quantmod)

# ------------------------------------------------------------------
SAVE_IMAGES = FALSE
source("openImage.R")
# ------------------------------------------------------------------

# For the presentation, make this determinate
set.seed(667)

XIV.ohlc = getSymbols("XIV", auto.assign=FALSE)
### price = last(Cl(XIV.ohlc), "3 months")
price = Cl(XIV.ohlc["2013-10-24::2014-01-24"])

diffs = diff(as.numeric(price))

openImage("RecentPriceHistory")
plot(price, main="Recent Price History")
closeImage()

openImage("RecentPriceDifferences")
plot(xts(diffs, index(price)[-1]), main="Recent Price Differences")
abline(h=0.0, col="grey")
closeImage()

openImage("PriceDifferences-ACF")
acf(diffs, main="Price Differences - ACF")
closeImage()

# BEGIN presentation code

HOR = 21
N = 999
reps = replicate(N, sample(diffs, HOR, replace=TRUE), simplify=TRUE)

reps = rbind(matrix(0, 1, N), reps)
reps = apply(reps, 2, cumsum)
reps = xts(reps, index(last(price)) + (0:HOR))

colors = c("black", "grey")[sample(c(1,2), 50, replace=TRUE, prob=c(.1, .9))]

openImage("NaiveBootstrapReplicates")
plot(as.zoo(reps[,1:50]), screens=1,
     col=colors,
     main="Naive Bootstrap Replicates (50 of 999)",
     xlab="Time", ylab="Price Change" )
abline(h=0.0, col="black", lty="solid")
closeImage()

outcomes = as.numeric(last(reps))

openImage("BootstrapReplicateOutcomes-Histogram")
hist(outcomes, 20, main="Bootstrap Replicate Outcomes")
closeImage()

cat("*** Summary of naive outcomes:\n")
print(summary(outcomes))
print(
  quantile(outcomes, prob=c(0.025, 0.975), na.rm=TRUE) )

stop("After naive bootstrap")

###
### SAME, BUT USING tsbootstrap
###
library(tseries)

tsamps = tsbootstrap(diffs, nb=N)

firstRow = matrix(as.numeric(first(price)), 1, N)
tsamps = rbind(matrix(0, 1, N), tsamps)
tsamps = apply(tsamps, 2, cumsum)
tsamps = xts(tsamps, index(price))

openImage("tsboostrapReplicates")
plot(as.zoo(tsamps[,1:50]), screens=1,
     col=colors,
     main="tsbootstrap Replicates (50 of 999)",
     xlab="Time", ylab="Price Change" )
abline(h=0.0, col="black", lty="solid")
closeImage()

tsout = as.numeric((last(tsamps)))

cat("*** Summary of tsbootstrap outcomes:\n")
print(summary(tsout))
print(
  quantile(tsout, prob=c(0.025, 0.975), na.rm=TRUE) )

openImage("tsbootstrapOutcomes-Histogram")
hist(tsout, 20, main="tsbootstrap Outcomes")
closeImage()

# ----------------------------------------------------------------------

#
# Parametric variation
#
# NOT USED IN FINAL PRESENTATION
#
if (FALSE) {
  oneReplicate = function() rnorm(HOR, mean=mean(diffs), sd=sd(diffs))
  reps = replicate(N, oneReplicate(), simplify=TRUE)
  
  reps = rbind(matrix(0, 1, N), reps)
  reps = apply(reps, 2, cumsum)
  
  reps = xts(reps, index(last(price)) + (0:HOR))
  colors = c("black", "grey")[sample(c(1,2), 50, replace=TRUE, prob=c(.1, .9))]
  plot(as.zoo(reps[,1:50]), screens=1,
       col=colors,
       main="Parametric Bootstrap Replicates (50 of 999)",
       xlab="Time", ylab="Price Change" )
  abline(h=0.0, col="black", lty="solid")
  
  outcomes = as.numeric(last(reps))
  hist(outcomes, 20, main="Parametric Replicate Outcomes")
  
  cat("*** Summary of parametric outcomes:\n")
  print(summary(outcomes))
  print(
    quantile(outcomes, prob=c(0.025, 0.975), na.rm=TRUE) )
}

# END OF UNUSED CODE
