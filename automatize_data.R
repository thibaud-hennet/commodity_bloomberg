library(dplyr)
library(purrr)
library(tidyr)
library(arrow)
library(lubridate)
library(Rblpapi)


#next lead
#bdp("CO SEP25 100C VOL BVOL Comdty", "PX_LAST")

# 🔧 Fonction helper avec retry et backoff exponentiel
safe_bdh <- function(tickers, fields, start.date, end.date,
                     max_retries = 5, base_delay = 0.5) {
  attempt <- 1
  repeat {
    tryCatch({
      res <- bdh(tickers, fields = fields,
                 start.date = start.date, end.date = end.date)
      return(res) # ✅ réussite : on sort
    },
    error = function(e) {
      msg <- conditionMessage(e)
      # 🧠 Si c’est une erreur de rate limit → on attend exponentiellement
      if (grepl("Too many requests|limit", msg, ignore.case = TRUE)) {
        wait_time <- base_delay * 2^(attempt - 1)
        message("⚠️ Rate limit hit — retrying in ", round(wait_time, 1), "s (attempt ", attempt, ")")
        Sys.sleep(wait_time)
        attempt <- attempt + 1
        if (attempt > max_retries) stop("❌ Too many rate-limit retries, aborting.")
      } else {
        # autre type d’erreur → on arrête
        stop(e)
      }
    })
  }
}

# 1️⃣ Définir les mois
months_seq <- seq(from = as.Date("2019-08-01"),
                  to   = as.Date("2019-09-01"),
                  by   = "month")

month_start = months_seq[1]
# 2️⃣ Boucle mensuelle principale
for (month_start in months_seq) {
  message("\n=== Processing month: ", format(month_start, "%Y-%m"), " ===")
  
  month_end <- ceiling_date(month_start, "month") - 1
  dates <- seq.Date(month_start, month_end, by = "1 day")
  
  # --- 🛢 1. Récupération futures ---
  futures_table <- map_dfr(dates, get_co_futures) 
  # --- 💰 2. Merge avec prix du front month ---

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
  
  # --- 📈 5. Merge final ---
  futures_options_data <- futures_table_final %>%
    left_join(futures_prices_fast %>% select(-date),
              by = c("ticker", "ref_date"))
  
  # --- 💾 6. Sauvegarde ---
  
  # Safely extract a scalar Date
  month_start_date <- as.Date(
    if (is.list(month_start)) month_start[[1]] else month_start
  )
  
  # Build filename
  #file_name <- sprintf("initial_test_%s.parquet", format(month_start_date, "%Y_%m"))
  file_name <- sprintf(
    "initial_test_%s.parquet",
    format(as.Date(month_start[1]), "%Y_%m")
  )

  write_parquet(futures_options_data,
                paste0("C:/Users/B00310412/OneDrive - Association Groupe ESSEC/Data", file_name))
  
  message("✅ Saved: ", file_name)
}
