# Unlike the originals,
# these functions also store the bootstrap distribution,

# Modified SR IRF (boot)
SVAR.sirf.boot.modified <- function(SVAR, Amat, Bmat, Yb, pmax, H, a, R, cumulative = FALSE) {
  Ib <- array(NA, c(SVAR$var$K, SVAR$var$K, H + 1, R))
  nvars <- colnames(SVAR$var$y)
  for (r in 1:R) {
    VAR.rep.R <- boot.rb.VAR(nvars, Yb[, , r], SVAR$var$K, SVAR$var$p, pmax)
    VAR.rep.S <- SVAR(VAR.rep.R, Amat = Amat, Bmat = Bmat, lrtest = FALSE)
    if (cumulative == FALSE) {
      Ib[, , , r] <- SVAR.sirf(VAR.rep.S, H)
    } else {
      Ib[, , , r] <- SVAR.sirf(VAR.rep.S, H, cumulative = TRUE)
    }
  }
  Il <- array(NA, c(SVAR$var$K, SVAR$var$K, H + 1))
  Iu <- array(NA, c(SVAR$var$K, SVAR$var$K, H + 1))
  if (cumulative == FALSE) {
    Ic <- SVAR.sirf(SVAR, H)
  } else {
    Ic <- SVAR.sirf(SVAR, H, cumulative = TRUE)
  }
  for (h in 1:(H + 1)) {
    for (j in 1:m) {
      for (i in 1:m) {
        ci <- boot.ci.efron(Ib[i, j, h, ], a)
        Il[i, j, h] <- ci[1]
        Iu[i, j, h] <- ci[2]
      }
    }
  }
  dimnames(Il) <- dimnames(Ic)
  dimnames(Iu) <- dimnames(Ic)
  SIRF <- list(lb = Il, pe = Ic, ub = Iu, boot = Ib)
  return(SIRF)
}

# Modified LR IRF (boot)
SVAR.sirf.boot.LR.modified <- function(SVAR, Yb, pmax, H, a, R, cumulative = FALSE) {
  Ib <- array(NA, c(SVAR$var$K, SVAR$var$K, H + 1, R))
  nvars <- colnames(SVAR$var$y)
  for (r in 1:R) {
    VAR.rep.R <- boot.rb.VAR(nvars, Yb[, , r], SVAR$var$K, SVAR$var$p, pmax)
    VAR.rep.S <- BQ(VAR.rep.R)
    if (cumulative == FALSE) {
      Ib[, , , r] <- SVAR.sirf(VAR.rep.S, H)
    } else {
      Ib[, , , r] <- SVAR.sirf(VAR.rep.S, H, cumulative = TRUE)
    }
  }
  Il <- array(NA, c(SVAR$var$K, SVAR$var$K, H + 1))
  Iu <- array(NA, c(SVAR$var$K, SVAR$var$K, H + 1))
  if (cumulative == FALSE) {
    Ic <- SVAR.sirf(SVAR, H)
  } else {
    Ic <- SVAR.sirf(SVAR, H, cumulative = TRUE)
  }
  for (h in 1:(H + 1)) {
    for (j in 1:m) {
      for (i in 1:m) {
        ci <- boot.ci.efron(Ib[i, j, h, ], a)
        Il[i, j, h] <- ci[1]
        Iu[i, j, h] <- ci[2]
      }
    }
  }
  dimnames(Il) <- dimnames(Ic)
  dimnames(Iu) <- dimnames(Ic)
  SIRF <- list(lb = Il, pe = Ic, ub = Iu, boot = Ib)
  return(SIRF)
}