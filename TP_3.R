remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ####
source("Data.R")
options(scipen=999)

# yi   <- EMAE
# pc   <- inflación
# tasa <- BADLAR

#############
# PUNTO UNO #
#############
yi <- ts(yi, start = c(2004,1), end = c(2019,12), frequency = 12)
d.yi <-   diff(yi)
d.pc <-   diff(pc)
d.tasa <- diff(tasa)

r <- (tasa - pc) # tasa real 

# EMAE
library(jpeg)
jpeg("Output/PTO_1/EMAE_pto1.jpg")
par(mfrow=c(2,1))
plot(yi, ylab="EMAE", xlab="Tiempo")
plot(d.yi, ylab="Diff(EMAE)", xlab="Tiempo")
dev.off()

# INFLA
jpeg("Output/PTO_1/INFLA_pto1.jpg")
par(mfrow=c(2,1))
plot(pc, ylab="Inflación", xlab="Tiempo")
plot(d.pc, ylab="Diff(Inflación)", xlab="Tiempo")
dev.off()

# TASA
jpeg("Output/PTO_1/TASA_pto1.jpg")
par(mfrow=c(2,1))
plot(tasa, ylab="Tasa", xlab="Tiempo")
plot(d.tasa, ylab="Diff(Tasa)", xlab="Tiempo")
dev.off()

# TASA REAL
jpeg("Output/PTO_1/REAL_pto1.jpg")
par(mfrow=c(2,1))
plot(r, ylab="Tasa Real", xlab="Tiempo")
plot(diff(r), ylab="Diff(Tasa Real)", xlab="Tiempo")
dev.off()

# test raiz unitaria
library(urca)
#ur.df(VARIABLE, type = c("none", "drift", "trend"), lags = 1,
#      selectlags = c("Fixed", "AIC", "BIC")) 
# les puse a todos trend pero nose si ta bien

summary(ur.df(yi, type = c("trend"), lags = 12))
summary(ur.df(yi, type = c("drift"), lags = 12))

summary(ur.df(pc, type = c("trend"), lags = 12))
summary(ur.df(pc, type = c("drift"), lags = 12))

summary(ur.df(tasa, type = c("trend"), lags = 12))
summary(ur.df(tasa, type = c("drift"), lags = 12))

summary(ur.df(r, type = c("trend"), lags = 12))
summary(ur.df(r, type = c("drift"), lags = 12))


summary(ur.df(diff(yi), type = c("trend"), lags = 12))
summary(ur.df(diff(yi), type = c("drift"), lags = 12))

summary(ur.df(diff(pc), type = c("trend"), lags = 12))
summary(ur.df(diff(pc), type = c("drift"), lags = 12))

summary(ur.df(diff(tasa), type = c("trend"), lags = 12))
summary(ur.df(diff(tasa), type = c("drift"), lags = 12))

summary(ur.df(diff(r), type = c("trend"), lags = 12))
summary(ur.df(diff(r), type = c("drift"), lags = 12))


#############
# PUNTO DOS #
#############

Y <- cbind(tasa, pc, yi) # <- luiggi dijo que asi podia ir
dY <- 100*diff(Y) # no puedo log porqe hay negativos
vars <- colnames(Y)
#remove(vars, pcom, er, pc, yi, cd)
Y <- ts(Y, start = c(2004,2), frequency = 12)
plot(Y)
plot(dY)

library(vars)

# Lag order selection
pmax <- 12 # Maximum lag length

popt <- VARselect(dY, lag.max = pmax, type = "const")
popt
p <- popt$selection[2];p # HQIC

Yd0 <- dY[1:pmax, ] # Initial values
Ydt <- dY[(pmax - p + 1):nrow(dY), ] # Starting in Jan-04

# Estimation
VAR <- VAR(dY, p = p, type = "const") 
summary(VAR)

# Model checking (Autovalores en modulo menor a 1 => Variable Estacionaria)
roots(VAR, modulus = TRUE)
serial.test(VAR, lags.bg = 12, type = "PT.asymptotic")

m <- VAR$K # Number of variables in the VAR
N <- VAR$obs # Number of effective sample observations, excluding "p" starting values

q <- min(10, trunc(N / 5)) # Rule of thumb for Portmanteau tests (Rob Hyndman) # https://robjhyndman.com/hyndsight/ljung-box-test/
r <- q # Lag order for BG test auxiliary regression
s <- q # Lag order for ARCH test auxiliary regression

# Portmanteau test
VAR.test.serial.pt <- serial.test(VAR, lags.pt = q, type = "PT.asymptotic")
VAR.test.serial.pt

# Adjusted Portmanteau test
VAR.test.serial.pt.ss <- serial.test(VAR, lags.pt = q, type = "PT.adjusted") # Small sample correc.
VAR.test.serial.pt.ss

# Breusch-Godfrey test
VAR.test.serial.bg <- serial.test(VAR, lags.bg = r, type = "BG")
VAR.test.serial.bg

# Adjusted Breusch-Godfrey test (Edgerton & Shukur)
VAR.test.serial.bg.ss <- serial.test(VAR, lags.bg = r, type = "ES") # Small sample correc.
VAR.test.serial.bg.ss


# Ad hoc function
matC <- function(m, p, vx) {
  vy <- setdiff(1:m, vx)
  Cm <- matrix(1, m, m * p + 1)
  for (i in vx) {
    for (l in 1:p) {
      for (j in vy) {
        Cm[i, m * (l - 1) + j] <- 0
      }
    }
  }
  return(Cm)
}

# Re-estimate VAR (no feedback from local vars. to pcom)
#VAR <- restrict(VAR, method = "man", resmat = matC(m, p, 1))
summary(VAR)

# Model checking (Autovalores en modulo menor a 1 => Variable Estacionaria)
roots(VAR, modulus = TRUE)
serial.test(VAR, lags.bg = 12, type = "PT.asymptotic")

# SVAR estimation ####

# A Matrix
Amat <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat[i, j] <- NA
  }
}

# B Matrix
Bmat <- matrix(0, m, m)
for (i in 1:m) {
  Bmat[i, i] <- NA
}

# SVAR estimation (AB model configuration)
SVAR <- SVAR(VAR, Amat = Amat, Bmat = Bmat, lrtest = FALSE)
SVAR

# SVAR t0 impact matrix (Cholesky decomposition)
S <- t(resid(VAR)) %*% resid(VAR) / N
P.chol <- t(chol(S)) # Cholesky decomposition
S

# SVAR t0 impact matrix (implied by AB model)
P <- solve(SVAR$A, SVAR$B) # inv(A) %% B
S.SVAR <- P %*% t(P)
S.SVAR

# Other SVAR parameters
pars.R <- Bcoef(VAR) # VAR
pars.S <- solve(P, pars.R) # SVAR
pars.R
pars.S

# SVAR analysis ####

source("PS2_SVAR_Analysis.R")
source("PS2_SVAR_Bootstrap.R")
source("PS2_SVAR_Plots.R")

H <- 18 # Horizon
H_ERPT <- 120 # Horizon for ERPT

# IRF
SVAR.SIRF <- SVAR.sirf(SVAR, H)
jpeg("Output/IRF_pto2.jpg")
plot.sirf(SVAR.SIRF, m, H)
dev.off()

# Cumulative IRF
SVAR.SIRF.c <- SVAR.sirf(SVAR, H, cumulative = TRUE)
plot.sirf(SVAR.SIRF.c, m, H)

# FEVD
SVAR.FEVD <- SVAR.fevd(SVAR, H)
jpeg("Output/FEDV_pto2.jpg")
plot.fevd(SVAR.FEVD, m, H)
dev.off()

# Bootstrap inference ####

a <- 0.95 # Confidence level
R <- 500 # No. of bootstrap replications

# Bootstrap replications
Yb <- boot.rb.replicate(VAR, Yd0, pmax, R)

# IRF (bootstrap)
SVAR.SIRF.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, Yb, pmax, H, a, R)
#jpeg("Output/PTO_2/BIRF_pto2.jpg")
plot.sirf.boot(SVAR.SIRF.boot, m, H)
#dev.off()

# Cumulative IRF (bootstrap)
SVAR.SIRF.c.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, Yb, pmax, H, a, R, cumulative = TRUE)
plot.sirf.boot(SVAR.SIRF.c.boot, m, H)

# FEVD (bootstrap)
SVAR.FEVD.boot <- SVAR.fevd.boot(SVAR, Amat, Bmat, Yb, pmax, H, a, R)
#jpeg("Output/BFEVD_pto2.jpg")
plot.fevd.boot(SVAR.FEVD.boot, m, H)
#dev.off()


##############
# PUNTO TRES #
##############
#y, r, i
r <- (tasa - pc) # tasa real 

Y <- cbind(yi, r, tasa)
dY <- 100*diff(Y) # no puedo log porqe hay negativos
vars <- colnames(Y)
#remove(vars, pcom, er, pc, yi, cd)
Y <- ts(Y, start = c(2004,2), frequency = 12)
Yd <- ts(dY, start = c(2004,2), frequency = 12)

plot(Y)
plot(dY)

library(vars)

# Lag order selection
pmax <- 12

popt <- VARselect(Yd, lag.max = pmax, type = "const")
popt
p <- popt$selection[2];p # SC

Yd0 <- Yd[1:pmax, ] # Initial values
Ydt <- Yd[(pmax - p + 1):nrow(Yd), ] # Starting in Jan-04

# Estimation
VAR <- VAR(Ydt, p = p, type = "const")

m <- VAR$K # No. of variables in the VAR
N <- VAR$obs # No. of effective sample observations, excluding "p" starting values

# Model checking
roots(VAR, modulus = TRUE)
serial.test(VAR, lags.bg = 12, type = "ES")

# SVAR estimation (AB model configuration)
SVAR <- BQ(VAR)
SVAR

# SVAR t0 impact matrix
#S.ML <- t(resid(VAR)) %*% resid(VAR) / N
S.OLS <- t(resid(VAR)) %*% resid(VAR) / (N - m * p - 1)
S.OLS

# SVAR t0 impact matrix (implied by LR restrictions)
P <- SVAR$B
S.SVAR <- P %*% t(P)
S.SVAR

# Other SVAR parameters
pars.R <- Bcoef(VAR) # VAR
pars.S <- solve(P, pars.R) # SVAR
pars.R
pars.S

# SVAR analysis ####

source("PS3_SVAR_Analysis_LR.R")
source("PS3_SVAR_Bootstrap_LR.R")
source("PS3_SVAR_Plots_LR.R")

H <- 12 # Horizon
H_ERPT <- 12 # Horizon for ERPT

# IRF
SVAR.SIRF <- SVAR.sirf(SVAR, H)
plot.sirf(SVAR.SIRF, m, H)

# FEVD
SVAR.FEVD <- SVAR.fevd(SVAR, H)
plot.fevd(SVAR.FEVD, m, H)

# Bootstrap inference ####

source("SVAR_SIRF_m.R")
source("SIRF_transform.R")

a <- 0.95 # Confidence level
R <- 500 # No. of bootstrap replications

# Bootstrap replications
Yb <- boot.rb.replicate(VAR, Yd0, pmax, R)

# IRF (bootstrap)
SVAR.SIRF.boot.LR <- SVAR.sirf.boot.LR.modified(SVAR, Yb, pmax, H, a, R)
plot.sirf.boot(SVAR.SIRF.boot.LR, m, H)

# FEVD (bootstrap)
#jpeg("Output/BFEVD_SVAR_pto3.jpeg")
SVAR.FEVD.boot.LR <- SVAR.fevd.boot.LR(SVAR, Yb, pmax, H, a, R)
plot.fevd.boot(SVAR.FEVD.boot.LR, m, H)
#dev.off()

# operator: arithmetic operators        valid options are: "+", "-", "*", "/", ...
# I:        m x m x (H + 1) array       point estimate SIRFs (output from SVAR.sirf... function)
# vi:       integer                     response variable position, first
# vj:       integer                     response variable position, second
# sk:       integer                     shock position
# boot:     m x m x (H + 1) x R array   bootstrap SIRFs (output from SVAR.sirf.boot... function)
# a:        float                       confidence level

operator <- "-"
I  <- SVAR.SIRF.boot.LR$pe
vi <- 3
vj <- 2
sk <- 3
boot <- SVAR.SIRF.boot.LR$boot
#a <- 0.95 # esta seteado mas arriba

trans.pw(operator, I, vi, vj, sk)

T.SIRF <- trans.pw.boot(operator, I, vi, vj, sk, boot, a)

T.SIRF$lb
T.SIRF$ub
T.SIRF$pe

# r = i - infla
# infla = i - r
# por esto le pongo el negativo en los plots

jpeg("Output/PTO_3/PTO_3_BIRF_INFLA.jpeg")
plot(T.SIRF$pe, ylim=c(-40,25), type="l", col="red", ylab="",xlab="Horizon", main="Response of Inflación to S. shock 3")
par(new=TRUE)
plot.ts(T.SIRF$lb, ylim=c(-40,25), ylab="", xlab="")
par(new=TRUE)
plot.ts(T.SIRF$ub, ylim=c(-40,25), ylab="", xlab="")
dev.off()


