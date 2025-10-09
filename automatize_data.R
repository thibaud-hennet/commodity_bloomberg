library(dplyr)
library(purrr)
library(tidyr)
library(arrow)
library(lubridate)
library(Rblpapi)

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
months_seq <- seq(from = as.Date("1988-01-01"),
                  to   = Sys.Date(),
                  by   = "month")

# 2️⃣ Boucle mensuelle principale
for (month_start in months_seq) {
  message("\n=== Processing month: ", format(month_start, "%Y-%m"), " ===")
  
  month_end <- ceiling_date(month_start, "month") - 1
  dates <- seq.Date(month_start, month_end, by = "1 day")
  
  # --- 🛢 1. Récupération futures ---
  futures_table <- map_dfr(dates, safely(get_co_futures)) %>%
    map_dfr("result")
  
  # --- 💰 2. Merge avec prix du front month ---
  futures_table_merge <- merge(
    futures_table,
    price_future_1M_data %>% rename(front_month_pxlast = PX_LAST),
    by.x = "ref_date", by.y = "date"
  )
  
  # --- 🧩 3. Génération tickers options ---
  futures_table_expanded <- futures_table_merge %>%
    mutate(
      tickers = pmap(
        list(underlying, front_month_pxlast),
        ~ get_ticker(..1, ..2, strikes, type = "C",
                     targets = c(0.8, 0.9, 1, 1.1, 1.2))
      )
    ) %>%
    unnest_longer(tickers, values_to = "ticker")
  
  futures_table_final <- futures_table_expanded %>%
    unnest_wider(ticker)
  
  # --- ⛽ 4. Récupération des prix Bloomberg avec backoff ---
  futures_prices_fast <- futures_table_merge %>%
    group_split(ref_date) %>%
    map_dfr(~ {
      date <- unique(.x$ref_date)
      tickers <- unique(.x$underlying)
      message("Fetching ", length(tickers), " tickers for date ", date)
      
      res <- safe_bdh(
        tickers = tickers,
        fields = c("PX_LAST", "PX_BID", "PX_MID", "PX_ASK"),
        start.date = date,
        end.date = date
      )
      
      res_df <- if (is.list(res) && !is.data.frame(res)) {
        bind_rows(res, .id = "ticker")
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
  file_name <- sprintf("initial_test_%s.parquet", format(month_start, "%Y_%m"))
  write_parquet(futures_options_data,
                paste0("C:/Users/B00310412/OneDrive - Association Groupe ESSEC/", file_name))
  
  message("✅ Saved: ", file_name)
}
