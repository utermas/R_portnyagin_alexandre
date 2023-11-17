library(dplyr)
library(ggplot2)

transaction_data <- read.csv('transaction_data.csv')

transaction_data$hour <- transaction_data$TRANS_TIME %/% 100

hourly_transactions <- transaction_data %>%
  group_by(hour) %>%
  summarise(count = n())

transactions_before_noon <- sum(hourly_transactions$count[hourly_transactions$hour < 12])
transactions_after_noon <- sum(hourly_transactions$count[hourly_transactions$hour >= 12])

print(paste("Transactions before noon:", transactions_before_noon))
print(paste("Transactions after noon:", transactions_after_noon))

ggplot(hourly_transactions, aes(x = hour, y = count)) +
  geom_line() +
  labs(title = "Customer Activity by Hour", x = "Hour of Day", y = "Number of Transactions")
