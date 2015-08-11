#
# Backing code for ASA CSP talk: Bootstrapping Time Series Data
#
library(boot)
library(meboot)
library(xts)
set.seed(666)

# ---------------------------------------------

#
# Bootstrapped statistic #1: maximum drawdown
#
maxDD = function(z) {
  hwm = cummax(z)     # High water mark
  dd = hwm - z        # Drawdown
  max(dd)             # Maximum drawdown
}

#
# Bootstrapped statistic #2: Lower 5% tail
#
lowerTail = function(y) as.vector(quantile(y, probs=0.05))

# Choose one:
theStatistic = maxDD

# ------------------------------------------------------------------------------

#
#   Synthesize on AR(1) time series, as defined in this presentation
#
synthAR1 = function(n, b0, b1, gamma, noise) {
    z = numeric(n)
    time = 0:(n-1)

    z[1] = b0 + b1*0.0 + gamma*time[1]     # Removed: ... + noise[1]
    for (t in 2:n) {
        z[t] = b0 + b1*z[t-1] + gamma*time[t] + noise[t]
    }

    return(z)
}

#
#   OR: Load market data
#
loadData = function(n, symbol="^TNX") {
    require(quantmod)

    ohlc = getSymbols(symbol, auto.assign=FALSE)
    as.vector(tail(Cl(ohlc), n))
}

# ---------------------------------------------

#
#   Support code: Save an image
#
SAVE_IMAGES = FALSE

source("openImage.R")

# ---------------------------------------------

#
# Generate one, simulated observation using AR(1) with trend
#
N = 100
Beta0 = 0.00     # Keep at zero
Beta1 = -0.20    # -0.50
Sigma = 1.5
Gamma = 0.05

### X0 = rnorm(1, mean=Beta0, sd=Sigma)

time = 0:(N-1)

N_DF = 4
noise = rt(N, df=N_DF) / sqrt(N_DF / (N_DF-2))   # Force SD = 1.0
noise = Sigma * noise

xa = synthAR1(N, Beta0, Beta1, Gamma, noise)

AR_NAME = "AR(1) Series"

if (!exists("xm")) {
    xm = loadData(N)
    ### xm = xm - xm[1] + Beta0        # Conform to initial Beta0 assumption
    stopifnot(any(xm != 0.0))
}

MKT_NAME = "Market Data"

openImage("MarketData")
plot(xm, typ='l', main=MKT_NAME, xlab="Time")
abline(h=xm[1], col="gray")
closeImage()


# Not useful:
## qqnorm(diff(xm), main="Market Data: QQ Plot of Deltas")
## qqline(diff(xm))

# ----------------------------------------------------------

#
# Toy example
#

cat("\n*** Toy example: One replicate sample and replicate statistic\n\n")
y = round(cumsum(c(10, rnorm(9,mean=0.25))), 2)

cat("Given time series:\n")
print(y)

cat("Statistic of interest for given data:\n")
print(theStatistic(y))

cat("Differences:\n")
print(diff(y))

cat("Resampled differences:\n")
resamp_diffs = sample(diff(y), 9, replace=TRUE)
print(resamp_diffs)

cat("Constructed replicate sample:\n")
resamp = cumsum(c(y[1], resamp_diffs))
print(resamp)

cat("Replicate statistic:\n")
print(theStatistic(resamp))

cat("\n*** End toy example\n")
rm(y)

# ----------------------------------------------------------

reportEns = function(x, ens, dsname, label, nplot=5, ngrey=50) {
  openImage(paste("TypicalReplicates", dsname, label, sep="-"))
  matplot(ens[,1:nplot], typ='l',
            main=paste0("Typical Replicates: ", dsname, ", ", label),
            xlab="Time", ylab="Replicates")
  abline(h=x[1], col="grey")
  closeImage()
  
  openImage(paste("SampleAndReplicates", dsname, label, sep="-"))
  matplot(ens[,1:ngrey], typ='l', col="grey",
            main=paste0("Obs'ed Sample and Replicates: ", dsname, ", ", label),
            xlab="Time", ylab="Observed and Replicates")
  abline(h=x[1], col="grey")
  lines(x, col="black")         # Add original data
  closeImage()
}

reportBoot = function(bout, dsname, label) {
    stats = as.vector(bout$t)

    openImage(paste("ReplicateStatistics", dsname, label, sep="-"))
    hist(stats, 20, main=paste0("Replicate Statistics: ", dsname, ", ", label))
    abline(v=bout$t0[1], lty="dashed")
    closeImage()
    
    # Unused output:
    ### plot(bout)
    
    cat("\n")
    cat("*** Summary of Replicate Statistics:", dsname, ",", label, "\n")
    print(summary(stats))
    ### cat("SD =", sd(stats), "\n")

    cat("\n")
    cat("*** Confidence Intervals:", dsname, ",", label, "\n")
    print(boot.ci(bout, type=c("norm","basic","perc")))
}

# ----------------------------------------------------------

#
#   Random walk - Innovations from empirical distribution
#

#
# Generate one bootstrap replicate: non-parametric, random walk
#
nonpRandWalkRepl = function(y, rand.args) {
  N = length(y)
  innov = diff(y)
  resamp = c(y[1], sample(innov, N-1, replace=TRUE))
  cumsum(resamp)
}

#
# Histogram of deltas (useful for discussion)
#
openImage(paste(MKT_NAME, "Deltas", "Histogram", sep="."))
hist(diff(xm), 20, main=paste(MKT_NAME, "Deltas"))
closeImage()

#
# Check deltas for autocorrelation
#
openImage(paste(MKT_NAME, "Deltas", "ACF", sep="-"))
acf(diff(xm), main=paste(MKT_NAME, "Deltas"))
closeImage()

#
# Generate demonstration of replicates
#
repls = replicate(50, nonpRandWalkRepl(xm, NULL))
reportEns(xm, repls, MKT_NAME, "Random Walk")

#
# Perform full bootstrap
#
R = 999
bootOut = boot(xm, theStatistic, R=R, sim="parametric", ran.gen=nonpRandWalkRepl)
reportBoot(bootOut, MKT_NAME, "Random Walk")

# ------------------------------------------------------------------------------

#
#   Random walk - Gaussian innovations
#

#
# Generate one replicate: Gausian innovations, random walk
#
gaussRandWalkRepl = function(y, rand.args) {
  N = length(y)
  innov = diff(y)
  m = mean(innov)
  s = sd(innov)
  resamp = c(y[1], rnorm(N-1, mean=m, sd=s))
  cumsum(resamp)
}

#
# Generate demo ensemble
#
repls = replicate(50, gaussRandWalkRepl(xm,NULL))
reportEns(xm, repls, MKT_NAME, "Gaussian Random Walk")

#
# Perform full bootstrap
#
bootOut = boot(xm, theStatistic, R=R, sim="parametric", ran.gen=gaussRandWalkRepl)
reportBoot(bootOut, MKT_NAME, "Gaussian Random Walk")

# ---------------------------------------------------------

#
#   What happens if the data show autocorrelation?
#

openImage(AR_NAME)
plot(xa, typ='l', main=AR_NAME, xlab="Time")
abline(h=Beta0, col="gray")
closeImage()

openImage(paste(AR_NAME, "Deltas", "Histogram", sep="-"))
hist(diff(xa), 20, main=paste(AR_NAME, "Deltas"))
closeImage()

openImage(paste(AR_NAME, "Deltas", "ACF", sep="-"))
acf(diff(xa), main=paste(AR_NAME, "Deltas"))
closeImage()

#
# Generate demonstration of bad replicates
#
repls = replicate(50, nonpRandWalkRepl(xa, NULL))
reportEns(xa, repls, AR_NAME, "Random Walk Model")

bootOut = boot(xa, theStatistic, R=R, sim="parametric", ran.gen=nonpRandWalkRepl)
reportBoot(bootOut, AR_NAME, "Random Walk Model")

# ---------------------------------------------------------

#
# Fixed-block bootstrap using the tsboot function.
#
# With a block size of 3 or more (5?), this should capture
# some of the autocorrelation
#

# Missing: Display typical replicate samples (don't have replicate generator)

BLOCK_SIZE = 5    # Uh, just a guess...

blockBoot = tsboot(ts(xa), theStatistic, R=R, l=BLOCK_SIZE, sim="fixed")   # or "geom"
reportBoot(blockBoot, AR_NAME, "Block Bootstrap")

# ---------------------------------------------------------

#
# Fit data to AR(1) model with trend
#
m = arima(as.ts(xa), order=c(1,0,0), xreg=time, include.mean=FALSE)

cat("\n")
cat("*** Fitted AR(1) model:\n")
print(m)

openImage(paste("AR1Model", "Residuals", "Histogram", sep="-"))
hist(resid(m), 20, main="AR(1) Model: Residuals")
closeImage()

openImage(paste("AR1Model", "Residuals", "ACF", sep="-"))
acf(resid(m), main="AR(1) Model: Residuals")
closeImage()

coefs = coef(m)
stderrs = sqrt(diag(m$var.coef))

beta1_hat = coefs["ar1"]
beta1_se  = stderrs["ar1"]
gamma_hat = coefs["time"]
gamma_se  = stderrs["time"]
sigma_hat = sqrt(m$sigma2)

# ---------------------------------------------------------

#
#   AR(1) Model - Bootstrap the residuals
#
ar1ResidRepl = function(y, rand.args) {
    n = length(y)
    noise = sample(resid(m), n, replace=TRUE)

    # NOTE: Could use arima.sim here, instead, with explicit innovations
    synthAR1(n, Beta0, beta1_hat, gamma_hat, noise)
}

openImage(paste("Residuals", AR_NAME))
plot(ts(resid(m)), main="Residuals: AR(1) Model")
grid()
closeImage()

#
# Show some typical replicate samples
#
repls = replicate(50, ar1ResidRepl(xa, NULL))
reportEns(xa, repls, AR_NAME, "Bootstrap of Residuals")

#
# Perform the full bootstrap
#
bootOut = boot(xa, theStatistic, R=R, sim="parametric", ran.gen=ar1ResidRepl)
reportBoot(bootOut, AR_NAME, "Bootstrap of Residuals")

# ---------------------------------------------------------

#
# AR(1) model with trend - parametric bootstrap
#

#
# Generate one AR(1) replicate, based on estimated parameters
#
ar1ParamRepl = function(y, rand.args) {
  n = length(y)
  
  b0 = Beta0       # Punt
  b1 = rnorm(1, mean=beta1_hat, sd=beta1_se)
  gamma = rnorm(1, mean=gamma_hat, sd=gamma_se)
  
  N_DF = 4
  noise = rt(n, df=N_DF) / sqrt(N_DF / (N_DF-2))   # Force SD = 1.0
  noise = sigma_hat * noise

  # QUESTION: Should this really use stochastic noise? Or just no noise at all?

  synthAR1(n, b0, b1, gamma, noise)
}

#
# Generate example ensemble
#
repls = replicate(50, ar1ParamRepl(xa,NULL))
reportEns(xa, repls, AR_NAME, "Bootstrap of Parameters")

#
# Perform the full bootstrap
#
bootOut = boot(xa, theStatistic, R=R, sim="parametric", ran.gen=ar1ParamRepl)
reportBoot(bootOut, AR_NAME, "Bootstrap of Parameters")

# ------------------------------------------------------------------------------

#
# Examples of ME boot replicates: Differences
#
mebOut = meboot(ts(diff(xm)), reps=R)
mebens = mebOut$ensemble
mebens = rbind(xm[1], mebens)
meseries = apply(mebens, 2, cumsum)
reportEns(xm, meseries, MKT_NAME, "Max. Ent. Boot.")

#
# Example of ME boot: undifferenced data
#
# NOT NEEDED
if (FALSE) {
  mebOut = meboot(ts(xm), reps=R)
  mebens = mebOut$ensemble
  reportEns(xm, mebens, MKT_NAME, "ME Boot (undiff)")
}

# ------------------------------------------------------------------------------

#
# Wrap up with original data
#
### plot(xm, typ='l', main=MKT_NAME, xlab="Time")
### abline(h=xm[1], col="gray")

### plot(xa, typ='l', main=AR_NAME, xlab="Time")
### abline(h=Beta0, col="gray")
