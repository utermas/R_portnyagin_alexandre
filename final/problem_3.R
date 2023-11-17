library(dplyr)
library(lubridate)
library(shiny)
library(shinydashboard)

campaign_data <- read.csv('campaign_desc.csv')
transaction_data <- read.csv('transaction_data.csv')
customer_data <- read.csv('hh_demographic.csv')

current_day <- max(transaction_data$DAY)

active_campaigns <- campaign_data %>%
  filter(START_DAY <= current_day, END_DAY >= current_day) %>%
  nrow()

unique_customers <- transaction_data %>%
  distinct(household_key) %>%
  nrow()

average_purchase <- transaction_data %>%
  group_by(household_key) %>%
  summarise(Total_Spent = sum(SALES_VALUE)) %>%
  summarise(Average_Spent = mean(Total_Spent)) %>%
  pull(Average_Spent)

total_products_sold <- sum(transaction_data$QUANTITY)

ui <- dashboardPage(
  dashboardHeader(title = "Business Key Metrics Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(title = "Active Marketing Campaigns", status = "primary", solidHeader = TRUE,
          valueBoxOutput("activeCampaigns")),
      box(title = "Unique Customers", status = "warning", solidHeader = TRUE,
          valueBoxOutput("uniqueCustomers")),
      box(title = "Average Purchase per Family", status = "info", solidHeader = TRUE,
          valueBoxOutput("averagePurchase")),
      box(title = "Total Products Sold", status = "success", solidHeader = TRUE,
          valueBoxOutput("totalProductsSold"))
    )
  )
)

server <- function(input, output) {
  output$activeCampaigns <- renderValueBox({
    valueBox(active_campaigns, subtitle = "Active Campaigns", icon = icon("calendar"))
  })

  output$uniqueCustomers <- renderValueBox({
    valueBox(unique_customers, subtitle = "Unique Customers", icon = icon("users"))
  })

  output$averagePurchase <- renderValueBox({
    valueBox(formatC(average_purchase, format = "f", digits = 2), 
             subtitle = "Average Purchase (per family)", icon = icon("shopping-cart"))
  })

  output$totalProductsSold <- renderValueBox({
    valueBox(total_products_sold, subtitle = "Total Products Sold", icon = icon("tags"))
  })
}

shinyApp(ui = ui, server = server)
