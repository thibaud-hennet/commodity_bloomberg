

vol_hist <- bdh(
  "CO1 Comdty",
  c("PX_LAST","30DAY_IMPVOL_90.0%MNY_DF"),
  start.date = as.Date("2005-01-01")
)



# horizons temporels
# Niveaux de moneyness
moneyness_levels <- c(90, 95, 97.5, 100, 102.5, 105, 110)

# Standardiser les moneyness à une décimale
moneyness_levels_standardized <- sprintf("%.1f", moneyness_levels)

# Horizons de volatilité
horizons <- c("30DAY", "60DAY", "3MTH", "6MTH", "12MTH", "18MTH", "24MTH")

# Générer les codes des champs à partir des horizons et des moneyness
fields <- expand.grid(
  horizon = horizons,
  moneyness = moneyness_levels_standardized
) %>%
  mutate(
    field = paste0(horizon, "_IMPVOL_", moneyness, "%MNY_DF")
  ) %>%
  pull(field)

# Afficher les champs générés
head(fields)


# Initialiser un dataframe vide pour collecter les données
vol_hist_df <- data.frame()

# Récupérer les données un champ à la fois et les ajouter au dataframe
for (field in fields) {
  print(field)
  # Télécharger les données pour un seul champ
  temp_data <- bdh(
    "CO1 Comdty", 
    field, 
    start.date = as.Date("2005-01-01")
  )
  
  # Si le dataframe est vide, ajouter directement les données
  if (nrow(vol_hist_df) == 0) {
    vol_hist_df <- temp_data
  } else {
    # Sinon, effectuer un merge sur la colonne 'date' pour ajouter les nouvelles données
    vol_hist_df <- merge(vol_hist_df, temp_data, by = "date", all = TRUE)
  }
  
  # Optionnel : Afficher les premières lignes après chaque ajout
  cat("Ajout des données pour le champ:", field, "\n")
  print(head(vol_hist_df))
}

setwd("D:/DATA_bloomberg")
write_parquet(vol_hist_df,
              "implied_vol_hist.parquet")


# # Télécharger les données pour chaque champ
# vol_hist <- bdh(
#   "CO1 Comdty",                # Ticker de l'option
#   fields,                       # Liste des champs générés
#   start.date = as.Date("2005-01-01")  # Date de début
# )


# Remplacer '30DAY' par '1MTH' et '60DAY' par '2MTH' dans les noms de colonnes
colnames(vol_hist_df) <- gsub("30DAY", "1MTH", colnames(vol_hist_df))  # Remplacer 30DAY par 1MTH
colnames(vol_hist_df) <- gsub("60DAY", "2MTH", colnames(vol_hist_df))  # Remplacer 60DAY par 2MTH

# Afficher les nouveaux noms de colonnes
colnames(vol_hist_df)

# Utiliser pivot_longer() pour transformer les colonnes en lignes
vol_hist_long <- vol_hist_df %>%
  pivot_longer(
    cols = -date,  # Garder la colonne 'date' intacte
    names_to = c("horizon", "moneyness"),  # Séparer le nom de la colonne en deux nouvelles colonnes
    names_pattern = "(\\d+MTH)_IMPVOL_(\\d+\\.\\d+)%MNY_DF",  # Nouvelle expression régulière
    values_to = "volatility"  # Nom de la colonne où les valeurs seront stockées
  )

# Reorder 'moneyness' as numeric (to control the levels)
vol_hist_long$moneyness <- factor(as.numeric(vol_hist_long$moneyness), 
                                  levels = sort(unique(as.numeric(vol_hist_long$moneyness))))

# Reorder 'horizon' based on the numeric value (1MTH, 2MTH, etc.)
vol_hist_long$horizon_plot <- factor(as.numeric(sub("MTH", "", vol_hist_long$horizon)), 
                                levels = sort(unique(as.numeric(sub("MTH", "", vol_hist_long$horizon)))))


vol_hist_long$horizon
colnames(vol_hist_long)
unique(vol_hist_long$horizon)
unique(vol_hist_long$moneyness)
# Sélectionner les colonnes qui contiennent "30DAY" dans leur nom
vol_hist_30day <- vol_hist_long %>%
  filter(horizon == "1MTH")
# Sélectionner les colonnes qui contiennent "30DAY" dans leur nom
vol_hist_100strike <- vol_hist_long %>%
  filter(moneyness == "100")


#Vol(in % on annual basis)
plot_30day <- ggplot()+
  geom_line(data = vol_hist_30day, aes(x = date, y = volatility* 1/sqrt(12), color = moneyness))+
  labs(x="Time",
       y = "Vol(in % on monthly basis)")+
  theme_minimal() +
  theme(
    legend.title = element_text(size = 14),  # Adjust legend title size
    legend.text = element_text(size = 12)    # Adjust legend text size
  )
plot_30day

setwd("D:/commodity_bloomberg/outputs")
ggsave(
  "vol_by_moneyness.png", plot_30day
)


plot_100strike <- ggplot()+
  geom_line(data = vol_hist_100strike, aes(x = date, y = volatility, color = horizon_plot))+
  labs(x="Time",
       y = "Vol(in % on annual basis)",
       color = "Horizon (in months)"  # Adjust the legend title for color
       )+
  theme_minimal() +
  theme(
    legend.title = element_text(size = 14),  # Adjust legend title size
    legend.text = element_text(size = 12)    # Adjust legend text size
  )
plot_100strike


#compare normalized levels
#Is the 3month horizon more or less uncertain than 12 month

plot_100strike_standardized <- ggplot()+
  geom_line(data = vol_hist_100strike, aes(x = date, y = volatility / sqrt(as.numeric(horizon_plot) * 12), color = horizon_plot))+
  labs(x="Time",
       y = "Vol(in % on horizon basis)",
       color = "Horizon (in months)"  # Adjust the legend title for color
  )+
  theme_minimal() +
  theme(
    legend.title = element_text(size = 14),  # Adjust legend title size
    legend.text = element_text(size = 12)    # Adjust legend text size
  )

plot_100strike_standardized


setwd("D:/commodity_bloomberg/outputs")
ggsave(
  "vol_by_horizon.png", plot_100strike_standardized
)

