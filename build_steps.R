library(lubridate)
library(dplyr)
library(purrr)

get_co_futures <- function(date, n_months = 12, include_front = FALSE) {
  # Ensure date is Date object
  if (is.character(date)) date <- as.Date(date)
  
  # Month codes (Bloomberg convention)
  month_codes <- c("F","G","H","J","K","M","N","Q","U","V","X","Z")
  
  # Define front month logic
  # if (day(date) > 15) {
  #   front_date <- date %m+% months(1)
  # } else {
  #   front_date <- date
  # }
  
  front_date <- date %m+% months(2)
  
  # Sequence of months for front + n_months
  months_seq <- seq.Date(
    from = floor_date(front_date, "month"),
    by = "1 month",
    length.out = n_months + ifelse(include_front, 1, 0)
  )
  
  # Generate tickers + horizon info
  tickers <- sapply(months_seq, function(d) {
    m_code <- month_codes[month(d)]
    y_code <- substr(year(d), 4, 4)
    paste0("CO", m_code, y_code, " Comdty")
  })
  
  horizons <- if (include_front) {
    c("Front", paste0(1:n_months, "M"))
  } else {
    paste0(1:n_months, "M")
  }
  
  tibble(
    ref_date = date,
    delivery_date = months_seq,
    horizon = horizons,
    ticker = tickers
  )
}

dates <- seq(as.Date("2025-07-01"), as.Date("2025-10-01"), by = "1 month")

futures_table <- map_dfr(dates, get_co_futures)



#do a check for current date
futures_table %>% 
  filter(ref_date == "2025-10-01")
bds("CO1 Comdty", "FUT_CHAIN")


#Jan 2025 signifie livraison en Jan 2025
#1 mois de délai de livraison (décembre)
# donc dernière quotation fin nnovembre ici 29 Nov 2024


# futures_table <- futures_table %>%
#   mutate(PX_LAST = map_dbl(ticker, ~ bdp(.x, "PX_LAST")[[1]]))


