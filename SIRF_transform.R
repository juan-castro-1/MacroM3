# operator: arithmetic operators        valid options are: "+", "-", "*", "/", ...
# I:        m x m x (H + 1) array       point estimate SIRFs (output from SVAR.sirf... function)
# vi:       integer                     response variable position, first
# vj:       integer                     response variable position, second
# sk:       integer                     shock position
# boot:     m x m x (H + 1) x R array   bootstrap SIRFs (output from SVAR.sirf.boot... function)
# a:        float                       confidence level

# Transformation (point estimate)
trans.pw <- function(operator, I, vi, vj, sk) {
  do.call(operator, list(I[vi, sk, ], I[vj, sk, ]))
}

# Transformation (boot)
trans.pw.boot <- function(operator, I, vi, vj, sk, boot, a) {
  Tc <- trans.pw(operator, I, vi, vj, sk)
  Tb <- do.call(operator, list(boot[vi, sk, , ], boot[vj, sk, , ]))
  Tl <- array(NA, length(Tc))
  Tu <- array(NA, length(Tc))
  for (h in 1:(H + 1)) {
    ci <- boot.ci.efron(Tb[h, ], a)
    Tl[h] <- ci[1]
    Tu[h] <- ci[2]
  }
  list(lb = Tl, pe = Tc, ub = Tu, boot = Tb)
}