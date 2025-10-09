
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
strikes_vector <- chain$strike
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

#I can take instead spot price when the contract was first priced

get_ticker_cste_strike <- function(underlying, spot_price, strikes, type = "C",
                       targets = c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3)) {
  

  tickers <- make_option_ticker(gsub(" Comdty", "", underlying), filtered_strikes)
  
  tibble(
    target_moneyness = targets,
    strike = filtered_strikes,
    ticker = tickers
  )
}

# Helper to format Bloomberg option tickers
make_option_ticker_cst_strike <- function(underlying, type = "C", strike) {
  underlying <- trimws(underlying)
  
  strike_str <- vapply(
    strike,
    function(s) formatC(s, format = "f", digits = ifelse(s %% 1 == 0, 0, 2)),
    FUN.VALUE = character(1)
  )
  
  paste0(underlying, type, " ", strike_str, " Comdty")
}

# --- Create fixed strikes based on first spot and select closest available strikes
get_ticker_cste_strike <- function(underlying, first_spot,
                                   type = "C",
                                   targets = seq(0.7, 1.3, by = 0.1)) {
  
  # Theoretical target strikes (ideal values based on moneyness)
  target_strikes <- targets * first_spot
  
  # For each target, pick the closest actual strike from strikes_vector
  selected_strikes <- map_dbl(target_strikes, function(tgt) {
    strikes_vector[which.min(abs(strikes_vector - tgt))]
  })
  
  # Build Bloomberg tickers
  tickers <- make_option_ticker_cst_strike(gsub(" Comdty", "", underlying), type, selected_strikes)
  
  # Return tibble
  tibble(
    target_moneyness = targets,
    strike = selected_strikes,
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



dates <- seq(as.Date("2025-07-01"), as.Date("2025-09-01"), by = "1 monthly")

dates <- seq(as.Date("2025-07-01"), as.Date("2025-09-01"), by = "1 day")


futures_table <- map_dfr(dates, get_co_futures)

futures_table_merge <- merge(futures_table %>%
                               mutate(join_date = delivery_date %m-% months(12)),
                             price_future_1M_data %>% 
                               rename(front_month_pxlast_1Yold = PX_LAST),
                             by.x ="join_date", by.y ="date"
                             )

futures_table_merge <- merge(futures_table_merge,
                             price_future_1M_data %>% 
                               rename(front_month_pxlast = PX_LAST) %>% 
                               select(front_month_pxlast, date),
                             by.y ="date", by.x ="ref_date"
)

colnames(futures_table_merge)

# get_ticker(underlying, under_px, 
#            strikes, type = "C",
#            targets = c(0.8, 0.9, 1, 1.1, 1.2))

#idea but with dedoublement des lignes for each different strike
# futures_table_merge$ticker <- get_ticker(futures_table_merge$underlying, futures_table_merge$front_month_pxlast, 
#                                          strikes, type = "C",
#                                          targets = c(0.8, 0.9, 1, 1.1, 1.2))


# Generate tickers using the *initial spot* for each underlying
futures_table_expanded <- futures_table_merge %>%
  mutate(
    tickers = pmap(
      list(underlying, front_month_pxlast_1Yold),
      ~ get_ticker_cste_strike(..1, ..2, type = "C", targets = seq(0.7, 1.3, by = 0.1))
    )
  ) %>%
  unnest_longer(tickers, values_to = "ticker")

#futures_table_expanded$ticker

futures_table_final <- futures_table_expanded %>%
  unnest_wider(ticker) %>%          # éclate la liste en colonnes
  rename(
    target_moneyness = target_moneyness,
    strike = strike,
    ticker = ticker
  )


# ticker <- "COZ5P    112 Comdty"
# bdh(ticker,
#     fields = c("PX_LAST","PX_BID", "PX_MID", "PX_ASK"),
#     start.date = Sys.Date()-2)

# my_ticker <- futures_table_final$ticker[1]
# my_date <- futures_table_final$ref_date[1]



futures_prices_fast <- futures_table_final %>%
  group_split(ref_date) %>%   # une liste par date
  map_dfr(~ {
    date <- unique(.x$ref_date)
    tickers <- unique(.x$ticker)
    message("Fetching ", length(tickers), " tickers for date ", date)
    
    res <- bdh(
      tickers,
      fields = c("PX_LAST","PX_BID","PX_MID","PX_ASK"),
      start.date = date,
      end.date = date
    )
    
    # 🧩 Si plusieurs tickers, bdh renvoie une liste → les fusionner
    res_df <- if (is.list(res) && !is.data.frame(res)) {
      bind_rows(res, .id = "ticker")  # 'ticker' devient la clé
    } else {
      res %>% mutate(ticker = tickers)
    }
    
    res_df %>%
      mutate(ref_date = date)
})


futures_options_data <- futures_table_final %>%
  left_join(futures_prices_fast %>% select(-date),
            by = c("ticker", "ref_date"))

setwd("C:/Users/B00310412/OneDrive - Association Groupe ESSEC")
write_parquet(futures_options_data,
              "initial_test.parquet")
