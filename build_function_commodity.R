
#CO1C 100 Comdty


underlying <- "CO1 Comdty"
underlying_bis <- "COX5 Comdty"

historical_chain <- bds(underlying, "OPT_CHAIN")
test_chain <- bds(underlying, "OPT_CHAIN", start.date = Sys.Date() -60)


# Get option chain for Brent Nov-2025

##'   bds("GOOG US Equity", "TOP_20_HOLDERS_PUBLIC_FILINGS")
##'   ## example of using overrides
#overrd <- c("START_DT"="20150101", "END_DT"="20160101")
chain <- bds(underlying, "OPT_CHAIN")
##'   bds("CPI YOY Index","ECO_RELEASE_DT_LIST", overrides = overrd)
head(chain,5)
# Extract tickers and strikes
chain$Security <- chain$`Security Description`

# Regex to pull strike as numeric
chain$strike <- as.numeric(sub(".* ([0-9.]+) Comdty", "\\1", chain$Security))
strikes <- chain$strike
sampled <- chain %>% sample_n(5)
ticker <- sampled$Security[1]

targets = c( 0.8, 0.9, 1, 1.1, 1.2)

# 3️⃣ Get current underlying futures price
under_px <- bdp(underlying, "PX_LAST")$PX_LAST

chain <- chain %>%
  mutate(MONEYNESS = strike / under_px)
closest <- map_dfr(targets, function(t) {
  chain %>%
    slice_min(order_by = abs(MONEYNESS - t), n = 1) %>%
    mutate(target = t)
})



underlying <- futures_table$ticker[[1]]

get_strikes_by_moneyness <- function(strikes, spot_price,
                                     targets = c(0.8, 0.9, 1, 1.1, 1.2)) {
  moneyness <- strikes / spot_price
  
  sapply(targets, function(t) {
    strikes[which.min(abs(moneyness - t))]
  })
}

make_option_ticker <- function(underlying, type = "C", strike) {
  # ensure uppercase and remove trailing spaces
  underlying <- trimws(underlying)
  
  # Format strike as character (integer or with decimals)
  strike_str <- formatC(strike, format = "f", digits = ifelse(strike %% 1 == 0, 0, 2))
  
  # Bloomberg typically expects total width of ~10 chars between type and 'Comdty'
  # We'll pad with spaces so the total alignment looks like Bloomberg's
  ticker <- sprintf("%s%s%6s Comdty", underlying, type, strike_str)
  
  return(ticker)
}
# Helper to build Bloomberg-style option ticker
# make_option_ticker <- function(underlying, type = "P", strike) {
#   underlying <- trimws(underlying)
#   strike_str <- formatC(strike, format = "f", digits = ifelse(strike %% 1 == 0, 0, 2))
#   sprintf("%s%s%6s Comdty", underlying, type, strike_str)
# }

make_option_ticker <- function(underlying, strike, type = "P") {
  underlying <- trimws(underlying)
  
  map_chr(strike, function(s) {
    strike_str <- formatC(s, format = "f", digits = ifelse(s %% 1 == 0, 0, 2))
    sprintf("%s%s%6s Comdty", underlying, type, strike_str)
  })
}


# Main function
get_ticker <- function(underlying, spot_price, strikes, type = "C",
                       targets = c(0.8, 0.9, 1, 1.1, 1.2)) {
  
  filtered_strikes <- get_strikes_by_moneyness(strikes, spot_price, targets)
  
  tickers <- make_option_ticker(gsub(" Comdty", "", underlying), filtered_strikes)
  
  tibble(
    target_moneyness = targets,
    strike = filtered_strikes,
    ticker = tickers
  )
}

#pour une date donnée
#je ne demande que les fields entre 80 et 120 in the Moneyness

#attention ce spot ne permet que de remonter à 2008
library(purrr)
opt_price <- c(periodicitySelection="DAILY")#, returnRelativeDate=TRUE)

price_future_spot_data <- bdh("EUCRBRDT Index", "PX_LAST",
                         options=opt_price ,
                         start.date = as.Date("1988-01-01")
)
price_future_spot_data

#ici jusqu'en 1988
price_future_1M_data <- bdh(underlying, "PX_LAST",
                              options=opt_price ,
                              start.date = as.Date("1988-01-01")
)
price_future_1M_data




dates <- seq(as.Date("2025-07-01"), as.Date("2025-10-01"), by = "1 day")

futures_table <- map_dfr(dates, get_co_futures)
colnames(futures_table)
colnames(price_future_1M_data)

futures_table_merge <- merge(futures_table,
                             price_future_1M_data %>% 
                               rename(front_month_pxlast = PX_LAST)
                               ,
                             by.x ="ref_date", by.y ="date"
                             )







