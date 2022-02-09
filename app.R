# libraries
library(lubridate)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

# Read data
df <- read.table(file = "station_data.tsv", sep = "\t", header=TRUE, quote = "\"")
df <- df[, -1]

# Set date column in Date format
df$date <- as.Date(df$date, "%Y-%m-%d")
str(df)

uic_station <- subset(df, df$stationname == "UIC-Halsted")
racine_station <- subset(df, df$stationname == "Racine")
ohare_station <- subset(df, df$stationname == "O'Hare Airport")

# Create Shiny app
ui <- dashboardPage(
  dashboardHeader(title = "Chicago 'L' Visualization"),
  dashboardSidebar(collapsed = FALSE, disable = FALSE,
                   sidebarMenu(
                     id = "menu_tabs",
                     menuItem("About", tabName = "About")
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "About",
              h1('About'),
              h2('Created by Aditya Ranganathan on 02/07/2022'),
              h2("The dashboard display data reagrding CTA rides in a clear and intuitive manner.
                 Users can check ride data of 3 different CTA stations: O'Hare Airport, UIC-Halsted and Racine.
                 The data can be viewed from a yearly, monthly, weekly or daily basis. Users can see the data either as plots or as a tabular form"))
    )
    
  )
)

server <- function(input, output){
  showLog()
  logjs("App started")
  observe({
    logjs(paste("Length of text:", nchar(input$text)))
  })
}
# 
# observeEvent(ip$menu_tabs, {if (input$tabs == "About") {
#   NULL
# }})

shinyApp(ui = ui, server = server)
