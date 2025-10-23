
#01/01/1987
#initial_date
dates <- seq(as.Date("2025-07-01"), as.Date("2025-09-01"), by = "1 day")


futures_options_data %>%
  group_by(underlying) %>% 
  summarise( n = n()) %>% 
  arrange(desc(n))

futures_options_data %>%
  group_by(target_moneyness, underlying) %>% 
  summarise( n = n())

library(ggplot2)
colnames(futures_options_data )

plot_data <- futures_options_data %>%
  filter(underlying == "COJ6 Comdty") %>% 
  arrange(ref_date, strike)

plot_data_1M <- futures_options_data %>%
  filter(horizon == "1M") %>% 
  arrange(ref_date, strike)

plot_data_6M <- futures_options_data %>%
  filter(horizon == "6M") %>% 
  arrange(ref_date, strike)

ggplot() +
  geom_line(
    data = plot_data_1M, aes(x = ref_date, y = PX_LAST, color = factor(target_moneyness), linetype = "1M")) +
  geom_point(data = plot_data_1M,aes(x = ref_date, y = PX_LAST, color = factor(target_moneyness))) +
  geom_line(data = plot_data_6M, aes(x = ref_date, y = PX_LAST, color = factor(target_moneyness), linetype = "6M")) +
  geom_point(data = plot_data_6M,aes(x = ref_date, y = PX_LAST, color = factor(target_moneyness))) +
  # geom_line(aes(x = ref_date, y = strike, color = factor(target_moneyness), linetype = "Strike")) +
  # geom_point(aes(x = ref_date, y = strike, color = factor(target_moneyness), linetype = "Strike")) +
  labs(
    title = "Option Prices vs Strike for COF6",
    x = "Time",
    y = "Option Price (PX_LAST)",
    color = "Moneyness"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#ici j'ajuste pour la baisse en prenant en même temps la baisse conjointe du spot

#il faut que je suive les mêmes strikes
#en décidant en début de ref_date quels sont les strikes que je vais suivre
#en prenant des moneyness de début de période
#car ici ne sont pas comparables

