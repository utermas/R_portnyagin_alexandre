library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

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
      box(width = 6, solidHeader = TRUE, status = "primary",
          title = "Active Marketing Campaigns", 
          plotOutput("plotActiveCampaigns"), 
          downloadButton("downloadActiveCampaigns", "Download CSV")),
      box(width = 6, solidHeader = TRUE, status = "warning",
          title = "Unique Customers", 
          plotOutput("plotUniqueCustomers"), 
          downloadButton("downloadUniqueCustomers", "Download CSV")),
      box(width = 6, solidHeader = TRUE, status = "info",
          title = "Average Purchase per Family", 
          plotOutput("plotAveragePurchase"), 
          downloadButton("downloadAveragePurchase", "Download CSV")),
      box(width = 6, solidHeader = TRUE, status = "success",
          title = "Total Products Sold", 
          plotOutput("plotTotalProductsSold"), 
          downloadButton("downloadTotalProducts", "Download CSV"))
    )
  )
)


server <- function(input, output) {
  
  output$plotActiveCampaigns <- renderPlot({
    ggplot(campaign_data, aes(x = CAMPAIGN, y = END_DAY - START_DAY)) +
      geom_bar(stat = "identity") +
      labs(x = "Campaign", y = "Duration", title = "Active Marketing Campaigns")
  })
  
  output$plotUniqueCustomers <- renderPlot({
    
    if(nrow(transaction_data) == 0) {
      print("No data available to plot.")
      return(NULL)
    }
    
   
    ggplot(transaction_data, aes(x = household_key)) +
      geom_histogram(stat = "count") +
      labs(x = "Unique Customers", y = "Frequency", title = "Unique Customers Distribution")
  })
  
  output$plotAveragePurchase <- renderPlot({
    
    avg_purchase_data <- transaction_data %>%
      group_by(household_key) %>%
      summarise(Average_Purchase = mean(SALES_VALUE))
    ggplot(avg_purchase_data, aes(x = household_key, y = Average_Purchase)) +
      geom_bar(stat = "identity") +
      labs(x = "Household Key", y = "Average Purchase Value", title = "Average Purchase per Family")
  })
  
  output$plotTotalProductsSold <- renderPlot({
    
    total_sales_data <- transaction_data %>%
      group_by(PRODUCT_ID) %>%
      summarise(Total_Sold = sum(QUANTITY)) %>%
      arrange(desc(Total_Sold)) 
    
    
    if(nrow(total_sales_data) == 0) {
      print("No data available to plot.")
      return(NULL)
    }
    
    
    ggplot(total_sales_data, aes(x = reorder(PRODUCT_ID, Total_Sold), y = Total_Sold)) +
      geom_bar(stat = "identity") +
      labs(x = "Product ID", y = "Total Quantity Sold", title = "Total Products Sold") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x labels if needed
  })
  
  
  output$downloadActiveCampaigns <- downloadHandler(
    filename = function() {
      paste("active_marketing_campaigns-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      active_campaigns_data <- campaign_data %>%
        filter(START_DAY <= max(transaction_data$DAY), END_DAY >= max(transaction_data$DAY))
      write.csv(active_campaigns_data, file, row.names = FALSE)
    }
  )
  
  
  output$downloadUniqueCustomers <- downloadHandler(
    filename = function() {
      paste("unique_customers-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      unique_customers_data <- transaction_data %>%
        distinct(household_key)
      write.csv(unique_customers_data, file, row.names = FALSE)
    }
  )
  
  
  output$downloadAveragePurchase <- downloadHandler(
    filename = function() {
      paste("average_purchase_per_family-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      average_purchase_data <- transaction_data %>%
        group_by(household_key) %>%
        summarise(Total_Spent = sum(SALES_VALUE)) %>%
        mutate(Average_Spent = Total_Spent / n()) %>%
        select(household_key, Average_Spent)
      write.csv(average_purchase_data, file, row.names = FALSE)
    }
  )
  
  
  output$downloadTotalProducts <- downloadHandler(
    filename = function() {
      paste("total_products_sold-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      total_products_data <- transaction_data %>%
        summarise(Total_Quantity = sum(SALES_VALUE))
      write.csv(total_products_data, file, row.names = FALSE)
    }
  )
}



shinyApp(ui = ui, server = server)
