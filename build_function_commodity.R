
#CO1C 100 Comdty
ticker


underlying <- "CO1 Comdty"
underlying_bis <- "COX5 Comdty"

historical_chain <- bds(underlying, "OPT_CHAIN")
test_chain <- bds(underlying, "OPT_CHAIN", start.date = Sys.Date() -60)

bds("CO1 Comdty", "FUT_CHAIN")
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








sampled <- chain %>% sample_n(5)
ticker <- sampled$Security[1]


#pour une date donnée
#je ne demande que les fields entre 80 et 120 in the Moneyness
library(purrr)
opt_price <- c(periodicitySelection="DAILY")#, returnRelativeDate=TRUE)

price_future_data <- bdh(underlying, "PX_LAST",
                         options=opt_price ,
                         start.date = as.Date("1988-01-01")
)
price_future_data


# 4️⃣ Extract strikes from option tickers
chain <- chain %>%
  mutate(
    TYPE = ifelse(str_detect(Security, "C "), "Call", "Put")
  )

# 5️⃣ Compute moneyness
chain <- chain %>%
  mutate(MONEYNESS = strike / under_px)



df <- bdh("COZ5P    60 Comdty",
      fields = c("PX_BID", "PX_MID", "PX_ASK"),
      start.date = as.Date("2023-01-01"))

bdh(ticker,
    fields = c("PX_BID", "PX_MID", "PX_ASK"),
    start.date = Sys.Date()-365,
    end.date   = Sys.Date()-300)

fieldSearch("OPT_CHAIN")






# 3️⃣ Get current underlying futures price
under_px <- bdp(underlying, "PX_LAST")$PX_LAST
#bdp(underlying_bis, "PX_LAST")$PX_LAST






# Target moneyness points
target_mny <- c(0.8, 0.9, 1.0, 1.1, 1.2)

# For each target, find the option with moneyness closest to it
closest_points <- map_dfr(target_mny, function(tgt) {
  chain %>%
    mutate(diff = abs(MONEYNESS - tgt)) %>%
    slice_min(order_by = diff, n = 1) %>%
    mutate(target = tgt)
})

# View results
closest_points %>%
  select(target, Security, strike, MONEYNESS, TYPE)


















