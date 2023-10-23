df <- read.csv("c:\\zd6.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(plotly)
library(leaflet)
library(rpivotTable)
library(DT)
print(head(df))
df <- df %>%
  filter(Income > 30000)

print(nrow(df))
df <- df %>%
select(Id, Year_Birth, Education, Marital_Status, Income, Response)
current_year <- 2023
df <- df %>%
  mutate(Age = current_year - Year_Birth)

df <- df %>%
  mutate(Rich_flag = Income > 80000)
print(head(df))
  summary_df <- df %>%
  group_by(Education) %>%  
  summarize(Average_Income = mean(Income)) 
print(summary_df)
df <- df %>%
  left_join(summary_df, by = "Education")
print(head(df))


scatter_plot <- plot_ly(data = df, x = ~Age, y = ~Income, mode = "markers", type = "scatter")
scatter_plot <- layout(scatter_plot, xaxis = list(title = "Age"), yaxis = list(title = "Income"), title = "Точечный график Age vs. Income")
scatter_plot

labels <- c("cat 1", " cat2", "cat 3")
values <- c(30, 40, 50)
tree_map_data <- data.frame(
  labels = labels,
  parent = "",
  values = values
)
tree_map <- plot_ly(data = tree_map_data, ids = ~labels, labels = ~labels, parents = ~parent, values = ~values, type = "treemap")
tree_map <- layout(tree_map, title = "Карта-дерево")
tree_map

Moscow_lat <- 55.754079
Moscow_lon <- 37.620037
Moscow_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lat = Moscow_lat, lng = Moscow_lon, popup = "Москва")
Moscow_map


print(datatable(df))


rpivotTable(df, rows = "Education", cols = "Marital_Status")
print(rpivotTable)