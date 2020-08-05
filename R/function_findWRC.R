cat("\nfindWRC(w, h): automatically find the best fit WRC using maximum likelihood\n")
findWRC = function(w, h) {
   y = w; x = h
   tS = max(y)
   tR = min(y)

   # Eqs
   models = c("van Genuchten", "Brooks & Corey", "Groenevelt & Grant", "Dexter")
   npar = c(2, 2, 3, 4)
   eqVG = expression( tR + (tS - tR) * (1 + (a * x)^n)^(-(1 - 1/n)) )
   eqBC = expression( ifelse(x < hb, tS, tR + (tS - tR) * (x/hb)^-lambda) )
   eqGG = expression( k1 * (exp(-k0/6.653^n) - exp(-k0/x^n)) )
   eqDE = expression( tR + a1 * exp(-x/p1) + a2 * exp(-x/p2) )

   # -logliks
   nllVG = function(par) {
      e = y - eval(eqVG, list(a = par[1], n = par[2]))
      -sum(dnorm(e, 0, sd(e), log = T))
   }
   nllBC = function(par) {
      e = y - eval(eqBC, list(hb = par[1], lambda = par[2]))
      -sum(dnorm(e, 0, sd(e), log = T))
   }
   nllGG = function(par) {
      e = y - eval(eqGG, list(k0 = par[1], k1 = par[2], n = par[3]))
      -sum(dnorm(e, 0, sd(e), log = T))
   }
   nllDE = function(par) {
      e = y - eval(eqDE, list(a1 = par[1], p1 = par[2], 
         a2 = par[3], p2 = par[4]))
      -sum(dnorm(e, 0, sd(e), log = T))
   }

   # starting values
   grVG = expand.grid(a = seq(0.002, 0.2, len = 30), 
      n = seq(1, 4, len = 30))
   grBC = expand.grid(hb = seq(0, 100, len = 50), 
      lambda = seq(0.1, 3, len = 30))
   grGG = expand.grid(k0 = seq(1, 20, len = 30), 
      k1 = seq(0, 1, len = 30),
      n = seq(1, 10, len = 20))
   grDE = expand.grid(a1 = seq(0.01, 0.4, len = 12), 
      p1 = seq(1000, 10000, len = 20),
      a2 = seq(0.01, 0.5, len = 12), 
      p2 = seq(10, 2000, len = 12))
   criVG = apply(grVG, 1, nllVG)
   criBC = apply(grBC, 1, nllBC)
   criGG = apply(grGG, 1, nllGG)
   criDE = apply(grDE, 1, nllDE)
   oVG = which.min(criVG)
   oBC = which.min(criBC)
   oGG = which.min(criGG)
   oDE = which.min(criDE)

   # MLEs
   mleVG = optim(par = grVG[oVG, ], nllVG, hessian = TRUE)
   mleBC = optim(par = grBC[oBC, ], nllBC, hessian = TRUE)
   mleGG = optim(par = grGG[oGG, ], nllGG, hessian = TRUE)
   mleDE = optim(par = grDE[oDE, ], nllDE, hessian = TRUE)
   pars = list(VG = mleVG$par, BC = mleBC$par, GG = mleGG$par, DE = mleDE$par)

   # Fitting criteria
   aic = c(VG = 2 * mleVG$value + 2*2,
      BC = 2 * mleBC$value + 2*2,
      GG = 2 * mleGG$value + 2*3,
      DE = 2 * mleDE$value + 2*4
   )
   std = list(VG = sqrt(diag(solve(mleVG$hessian))),
      BC = sqrt(diag(solve(mleBC$hessian))),
      GG = sqrt(diag(solve(mleGG$hessian))),
      DE = sqrt(diag(solve(mleDE$hessian)))
   )
   eVG = y - eval(eqVG, as.list(mleVG$par))
   eBC = y - eval(eqBC, as.list(mleBC$par))
   eGG = y - eval(eqGG, as.list(mleGG$par))
   eDE = y - eval(eqDE, as.list(mleDE$par))
   r2 = c(VG = 1 - sum(eVG^2)/sum((y - mean(y))^2),
      BC = 1 - sum(eBC^2)/sum((y - mean(y))^2),
      GG = 1 - sum(eGG^2)/sum((y - mean(y))^2),
      DE = 1 - sum(eDE^2)/sum((y - mean(y))^2)
   )
   mape = c(VG = 100 * mean(abs(eVG)/y),
      BC = 100 * mean(abs(eBC)/y),
      GG = 100 * mean(abs(eGG)/y),
      DE = 100 * mean(abs(eDE)/y)
   )

   # out
   dtf = data.frame(Rsq = r2, MAPE = mape, AIC = aic, npar)
   best_id = which.min(dtf[, "AIC"])
   best = paste("Best fit (AIC):", models[best_id])
   out = list(fitting_criteria = dtf, 
      best_fit = models[best_id], 
      par = pars[[best_id]], std = std[[best_id]])
   return(out)
}
