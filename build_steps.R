library(lubridate)

# Fonction pour générer les tickers CO futures (front month + 12 mois)
get_co_futures <- function(date, n_months = 12, include_front = TRUE) {
  # Tableau des codes mois
  month_codes <- c("F","G","H","J","K","M","N","Q","U","V","X","Z")
  
  # Front month = mois du contrat actif pour cette date
  # (on suppose que le contrat du mois courant expire vers le 15, sinon suivant)
  if (day(date) > 15) {
    front_date <- date %m+% months(1)
  } else {
    front_date <- date
  }
  
  # Générer une séquence de mois pour front + n_months
  months_seq <- seq.Date(from = floor_date(front_date, "month"),
                         by = "1 month",
                         length.out = n_months + ifelse(include_front, 1, 0))
  
  # Génération des tickers CO futures
  tickers <- sapply(months_seq, function(d) {
    m_code <- month_codes[month(d)]
    y_code <- substr(year(d), 4, 4)  # dernière chiffre de l’année
    paste0("CO", m_code, y_code, " Comdty")
  })
  
  return(tickers)
}

get_co_futures(date = as.Date("2025-10-09"))

dates <- seq(as.Date("2024-10-01"), as.Date("2025-10-01"), by = "3 months")

futures_by_date <- lapply(dates, get_co_futures)

names(futures_by_date) <- as.character(dates)
futures_by_date


futures_table <- purrr::map_dfr(dates, function(d) {
  data.frame(
    date_ref = d,
    ticker = get_co_futures(d)
  )
})

head(futures_table)


