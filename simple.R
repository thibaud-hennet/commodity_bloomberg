
#01/01/1987
#initial_date
dates <- seq(as.Date("2025-07-01"), as.Date("2025-10-01"), by = "1 month")


futures_table <- map_dfr(dates, get_co_futures)


futures_table_merge <- merge(futures_table,
                             price_future_1M_data %>% 
                               rename(front_month_pxlast = PX_LAST),
                             by.x ="ref_date", by.y ="date"
)
colnames(futures_table_merge)

# get_ticker(underlying, under_px, 
#            strikes, type = "C",
#            targets = c(0.8, 0.9, 1, 1.1, 1.2))

#idea but with dedoublement des lignes for each different strike
# futures_table_merge$ticker <- get_ticker(futures_table_merge$underlying, futures_table_merge$front_month_pxlast, 
#                                          strikes, type = "C",
#                                          targets = c(0.8, 0.9, 1, 1.1, 1.2))

futures_table_expanded <- futures_table_merge %>%
  mutate(
    tickers = pmap(
      list(underlying, front_month_pxlast),
      ~ get_ticker(..1, ..2, strikes, type = "C", targets = c(0.8, 0.9, 1, 1.1, 1.2))
    )
  ) %>%
  unnest_longer(tickers, values_to = "ticker")

futures_table_final <- futures_table_expanded %>%
  unnest_wider(ticker) %>%          # éclate la liste en colonnes
  rename(
    target_moneyness = target_moneyness,
    strike = strike,
    ticker = ticker
  )


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

colnames(futures_table_final)
colnames(futures_prices_fast)

futures_prices_fast%>% select(-date) %>%  rename(underlying = ticker) %>% pull(underlying)
futures_table_final%>% pull(underlying)
futures_table_final%>% pull(ticker)
futures_prices_fast$ticker
futures_prices_fast$ticker

futures_options_data <- futures_table_final %>%
  full_join(futures_prices_fast %>% select(-date),
            by = c("ticker", "ref_date")) %>% 
  arrange(ref_date)

class(futures_table_final$ref_date)
class(futures_options_data$ref_date)
