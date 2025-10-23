# --- Black-76 pricing ---
pnorm_safe <- function(x) stats::pnorm(x)

black76_price <- function(F, K, r, T, sigma, type = c("call","put")) {
  type <- match.arg(type)
  if (T <= 0 || sigma <= 0) return(NA_real_)
  d1 <- (log(F/K) + 0.5 * sigma^2 * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  df <- exp(-r * T)
  if (type == "call") {
    return(df * (F * pnorm_safe(d1) - K * pnorm_safe(d2)))
  } else {
    return(df * (K * pnorm_safe(-d2) - F * pnorm_safe(-d1)))
  }
}

# --- Implied vol via uniroot (Black-76) ---
impvol_black76 <- function(price, F, K, r, T, type = c("call","put"),
                           lower = 1e-6, upper = 5, tol = 1e-7, maxit = 100) {
  type <- match.arg(type)
  # Bornes de non-arbitrage (prix doit être entre valeur intrinsèque actualisée et borne sup)
  df <- exp(-r*T)
  intrinsic <- if (type=="call") df * pmax(F - K, 0) else df * pmax(K - F, 0)
  upper_bound <- if (type=="call") df * F else df * K
  if (is.na(price) || price < intrinsic - 1e-10 || price > upper_bound + 1e-10) return(NA_real_)
  # Si prix très proche de l'intrinsèque -> sigma ~ 0
  if (abs(price - intrinsic) < 1e-10) return(0)
  # Fonction d'écart
  f <- function(s) black76_price(F,K,r,T,s,type) - price
  # Sécuriser le bracket : élargir si besoin
  lo <- lower; hi <- upper; flo <- f(lo); fhi <- f(hi)
  tries <- 0
  while (flo * fhi > 0 && tries < 10) {
    lo <- lo / 2
    hi <- hi * 2
    flo <- f(lo); fhi <- f(hi)
    tries <- tries + 1
  }
  if (flo * fhi > 0) return(NA_real_)  # pas de racine trouvée
  uniroot(f, lower = lo, upper = hi, tol = tol, maxiter = maxit)$root
}

# Exemple fictif
F  <- 78.25                 # OPT_UNDL_PX
K  <- 80
r  <- 0.02                  # 2% annuel ; utilisez votre courbe OIS si dispo
T  <- as.numeric(45)/365    # 45 jours jusqu'à l'échéance
bid <- 1.35; ask <- 1.55
price_mid <- (bid + ask)/2
type <- "call"

sigma_iv <- impvol_black76(price = price_mid, F = F, K = K, r = r, T = T, type = type)
sigma_iv


