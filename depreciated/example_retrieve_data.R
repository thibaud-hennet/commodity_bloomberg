library(Rblpapi)
library(dplyr)
library(tidyr)
# Connect to Bloomberg API

blpConnect()

bdp(c("ESA Index", "SPY US Equity"), c("PX_LAST", "VOLUME"))
bdh("SPY US Equity", c("PX_LAST", "VOLUME"), start.date=Sys.Date()-31)

# Get option chain for Brent Nov-2025
chain <- bds("COX5 Comdty", "OPT_CHAIN")

# Extract tickers and strikes
chain$Security <- chain$`Security Description`

# Regex to pull strike as numeric
chain$strike <- as.numeric(sub(".* ([0-9.]+) Comdty", "\\1", chain$Security))

sampled <- chain %>% sample_n(5)
ticker <- sampled$Security[1]


results <- lapply(sampled $Security, function(ticker) {
  df <- tryCatch(
    bdh(ticker,
        fields = c("PX_BID", "PX_MID", "PX_ASK"),
        start.date = Sys.Date() - 365),
    error = function(e) NULL
  )
  df <- df %>% mutate(spread_rel = (PX_ASK - PX_BID)/PX_MID)
  # Filter illiquid options (spread_rel > 0.2)
  df <- df %>% filter(spread_rel <= 0.2)
  #Filters out illiquid options (spread_rel > threshold, e.g., 20%)
  #filter for liquid strikes that is that which contains 
  K <- as.numeric(sub(".* ([0-9.]+) Comdty", "\\1", ticker))
  df$IV <- mapply(impvol_black76, price=df$PX_MID,
                  MoreArgs=list(F=F,K=K,r=r,T=T,type="put"))
  if (!is.null(df) && nrow(df) > 0) {
    df$ticker <- ticker
    return(df)
  } else {
    return(NULL)  # skip tickers with no data
  }
  
  
  
})

# Combine all non-empty results
all_data <- do.call(rbind, results)
head(all_data)


