# Mesurer la version lente
time_slow <- system.time({
  futures_prices <- map2_dfr(
    futures_table_merge$underlying,
    futures_table_merge$ref_date,
    ~ {
      bdh(.x,
          fields = c("PX_LAST","PX_BID","PX_MID","PX_ASK"),
          start.date = .y,
          end.date   = .y) %>%
        mutate(ticker = .x, ref_date = .y)
    }
  )
})

# Mesurer la version optimisée
time_fast <- system.time({
  library(dplyr)
  library(purrr)
  
  time_fast <- system.time({
    futures_prices_fast <- futures_table_merge %>%
      group_split(ref_date) %>%   # une liste par date
      map_dfr(~ {
        date <- unique(.x$ref_date)
        tickers <- unique(.x$underlying)
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
  })
  
})

# Afficher les deux temps
time_slow
time_fast
#10 ten times faster by sending the same reference date within the same bdh request

setwd("C:/Users/B00310412/OneDrive - Association Groupe ESSEC")
write_parquet(futures_table_merge_final,
              "initial_test.parquet")

