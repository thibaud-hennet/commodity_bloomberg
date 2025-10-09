

# 
# bvol_fields <- fieldSearch("BVOL")
# #bvol_fields$Mnemonic
# ticker <- "CO1 Comdty"
# sp_vols <- c("SP_VOL_SURF_MID" ,
#              "SP_VOL_SURF_BID"  ,              "SP_VOL_SURF_ASK", "SP_VOL_SURF_SPREAD" )
# bvols <- c( "BVOL_OIS_SWAPTION" ,"BVOL_CAP_VOL"   , "BVOL_ATM_SWAPTION_VOL"  ,        "BVOL_OTM_SWAPTION_VOL"  ,        "BVOL_RFR_CAP"    ,
#             "BVOL_RFR_SWAPTION_ATM"  ,        "BVOL_RFR_SWAPTION"  ,     "BVOL_OIS_CAP",                   "BVOL_IBOR_CAP" ,
#             "BVOL_IBOR_SWAPTION" ,            "BVOL_DATA_TIME_STAMP" )
# bdh(ticker,
#     fields = c("PX_LAST","PX_BID", "PX_MID", "PX_ASK","IVOL_SURFACE",
#                sp_vols, bvols ),
#     start.date = Sys.Date()-2)


dates <- seq(as.Date("2018-06-01"), as.Date("2018-09-01"), by = "1 month")

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




