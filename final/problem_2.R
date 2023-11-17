
install.packages("dplyr")
library(dplyr)


transaction_data <- read.csv("transaction_data.csv")
product_data <- read.csv("product.csv")


merged_data <- inner_join(transaction_data, product_data, by = "PRODUCT_ID")

merged_data$QUANTITY <- as.numeric(merged_data$QUANTITY)
merged_data$SALES_VALUE <- as.numeric(merged_data$SALES_VALUE)

top_products_quantity <- merged_data %>%
  group_by(PRODUCT_ID, COMMODITY_DESC) %>%
  summarise(total_quantity = sum(QUANTITY), total_sales = sum(SALES_VALUE)) %>%
  arrange(desc(total_quantity)) %>%
  head(10)

stores_with_low_sales <- merged_data %>%
  group_by(STORE_ID) %>%
  filter(all(!(PRODUCT_ID %in% top_products_quantity$PRODUCT_ID))) %>%
  summarise(total_sales = sum(SALES_VALUE)) %>%
  arrange(desc(total_sales)) %>%
  head(10)

print("Top 10 products by quantity sold with total sales value:")
print(top_products_quantity)

print("Top 10 stores with low or no sales of top 10 products:")
print(stores_with_low_sales)
