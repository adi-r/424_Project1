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
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Chicago 'L' Visualization"),
  dashboardSidebar(collapsed = FALSE, disable = FALSE,
                   sidebarMenu(
                     id = "menu_tabs",
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("Station Data", tabName = "station_data", selected = TRUE, icon = icon("dashboard")),
                     menuItem("Station Comparison", tabName = "station_compare", icon = icon("signal", lib = "glyphicon")),
                     menuItem("Dates of Interest", tabName = "doi", icon = icon("calendar")),
                     menuItem("About", tabName = "about", icon = icon("sunglasses", lib = "glyphicon"))
                     )
                   
                   
                   ),
  dashboardBody(
    tags$head(tags$style(".sidebar-menu li { margin-bottom: 10px; }")),
    tabItems(
      tabItem(tabName = "station_data",
              sidebarLayout(position = "left",
                            sidebarPanel(
                              style = "margin-top:100%",
                              div(selectInput("select_station_data", "Station",
                                              choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                              selected = c("UIC-Halsted"))),
                              fluidRow(
                                column(8,
                                       div(selectInput("select_year_data", "Year",
                                                       choices = c("All", 2021:2001), selected = c(2021)
                                                       )
                                           )
                                       ),
                                column(8,
                                       div(checkboxGroupInput("chrono_data", "Period",
                                                              choices = c("Week", "Month", "Year"),
                                                              selected = c("Week", "Month", "Year")
                                                              )
                                           )
                                       )
                              ),
                              width = 2
                            ),
                            mainPanel(uiOutput("data_table"), width = 12)
                            )
              ),
      tabItem(tabName = "station_compare", 
              sidebarLayout(position = "left",
                            sidebarPanel(style = "margin-top: 50%",
                                         h4("Left"),
                                         div(selectInput("station1_compare", "Station",
                                                         choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                         selected = c("UIC-Halsted")
                                                         )
                                             ),
                                         fluidRow(column(8,
                                                  div(selectInput("year1_compare", "Year",
                                                                  choices = c("All", 2021:2001),
                                                                  selected = c(2021)
                                                                  )
                                                      )
                                                  ),
                                                  column(8, 
                                                         div(checkboxGroupInput("chrono1_compare", "Period",
                                                                                choices = c("Week", "Month", "Year"),
                                                                                selected = c("Week", "Month", "Year")
                                                         )
                                                         )
                                         )
                                         ),
                                         h4("Right"),
                                         div(selectInput("station2_compare", "Station",
                                                         choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                         selected = c("UIC-Halsted")
                                         )
                                         ),
                                         fluidRow(column(8,
                                                         div(selectInput("year2_compare", "Year",
                                                                         choices = c("All", 2021:2001),
                                                                         selected = c(2021)
                                                         )
                                                         )
                                         ),
                                         column(8, 
                                                div(checkboxGroupInput("chrono2_compare", "Period",
                                                                       choices = c("Week", "Month", "Year"),
                                                                       selected = c("Week", "Month", "Year")
                                                )
                                                )
                                         )
                                         ),
                                         width = 2
                                         ),
                            mainPanel(uiOutput("comare_plots1"),
                                      uiOutput("compare_plots2"),
                                      width = 12)
        
      )
      ),
      tabItem(tabName = "doi",
              sidebarLayout(position = "left",
                            sidebarPanel(style = "margin-top: 100%",
                                         div(selectInput("select_date", "Interesting Dates",
                                                         choices = c("Date 1", "Date 2", "Date 3", "Date 4", "Date 5", "Date 6",
                                                                     "Date 7", "Date 8", "Date 9", "Date 10"),
                                                         selected = c("Date 1")
                                                         )),
                                         width = 2),
                            mainPanel(uiOutput("date_1"),
                                      uiOutput("date_2"),
                                      uiOutput("date_3"),
                                      uiOutput("date_4"),
                                      uiOutput("date_5"),
                                      uiOutput("date_6"),
                                      uiOutput("date_7"),
                                      uiOutput("date_8"),
                                      uiOutput("date_9"),
                                      uiOutput("date_10"),)
                            )
              ),
      tabItem(tabName = "about",
              h1('About'),
              h4('Created by Aditya Ranganathan on 02/07/2022'),
              h4("The dashboard display data reagrding CTA rides in a clear and intuitive manner.
                 Users can check ride data of 3 different CTA stations: O'Hare Airport, UIC-Halsted and Racine.
                 The data can be viewed from a yearly, monthly, weekly or daily basis. Users can see the data either as plots or as a tabular form")
              )
      
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
