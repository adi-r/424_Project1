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


uic_df <- subset(df, df$stationname == "UIC-Halsted")
racine_df <- subset(df, df$stationname == "Racine")
ohare_df <- subset(df, df$stationname == "O'Hare Airport")

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
                                       )
                                ),
                              width = 2
                            ),
                            mainPanel(uiOutput("data_table"), width = 10)
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
                                                                                choices = c("Weekly", "Monthly", "Yearly"),
                                                                                selected = c("Weekly", "Monthly", "Yearly")
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
                                                                       choices = c("Weekly", "Monthly", "Yearly"),
                                                                       selected = c("Weekly", "Monthly", "Yearly")
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
  # Bar COlor
  bar_color <- function(station){
    # color <- "black"
    
    if (station == "All"){
      color <- "#dd1c77"
    } else if (station == "UIC-Halsted"){
      color <- "#756bb1"
    } else if (station == "O'Hare Airport") {
      color <- "#de2d26"
    } else {
      color <- "#2c7fb8"
    }
  }
  
  # Sum Functions
  year_sum <- function(year){
    if(input$select_station_data == "All"){
      sum(df[df$year == year,]$rides)
    }
    else{
      if(input$select_station_data == "UIC-Halsted"){
        sum(uic_df[uic_df$year == year,]$rides)
      } else if(input$select_station_data == "O'Hare Airport"){
        sum(ohare_df[ohare_df$year == year,]$rides)
      } else{
        sum(racine_df[racine_df$year == year,]$rides)
      }
    }
  }
  
  month_sum <- function(month){
    if(input$select_year_data == "All"){
      if(input$select_station_data == "All"){
        sum(df[df$month_name == month,]$rides)
      } else if(input$select_station_data == "UIC-Halsted"){
        sum(uic_df[uic_df$month_name == month,]$rides)
      } else if(input$select_station_data == "O'Hare Airport"){
        sum(ohare_df[ohare_df$month_name == month,]$ rides)
      } else{
        sum(racine_df[racine_df$month == month,]$rides)
      }
    }
    else{
      if(input$select_station_data == "All"){
        sum(df[df$year == input$select_year_data & df$month_name == month,]$rides)
      } else if(input$select_station_data == "UIC-Halsted"){
        sum(uic_df[uic_df$year == input$select_year_data & uic_df$month_name == month,]$rides)
      } else if(input$select_station_data == "O'Hare Airport"){
        sum(ohare_df[ohare_df$year == input$select_year_data & ohare_df$month_name == month,]$ rides)
      } else{
        sum(racine_df[racine_df$year == input$select_year_data & racine_df$month_name == month,]$rides)
      }
    }
  }
  
  daily_sum <- function(day){
    if(input$select_year_data == "All"){
      if(input$select_station_data == "All"){
        sum(df[df$week_day == day,]$rides)
      } else if(input$select_station_data == "UIC-Halsted"){
        sum(uic_df[uic_df$week_day == day,]$rides)
      } else if(input$select_station_data == "O'Hare Airport"){
        sum(ohare_df[ohare_df$week_day == day,]$ rides)
      } else{
        sum(racine_df[racine_df$week_day == day,]$rides)
      }
    }
    else{
      if(input$select_station_data == "All"){
        sum(df[df$year == input$select_year_data & df$week_day == day,]$rides)
      } else if(input$select_station_data == "UIC-Halsted"){
        sum(uic_df[uic_df$year == input$select_year_data & uic_df$week_day == day,]$rides)
      } else if(input$select_station_data == "O'Hare Airport"){
        sum(ohare_df[ohare_df$year == input$select_year_data & ohare_df$week_day == day,]$ rides)
      } else{
        sum(racine_df[racine_df$year == input$select_year_data & racine_df$week_day == day,]$rides)
      }
    }
  }
  
  # Create dataframes from specific years
  # year_frame <- reactive({
  #   if(input$select_station_data == "All"){
  #     if(input$select_year_data == "All"){
  #       df
  #     } else{
  #       subset(df, df$year == input$select_year_data)
  #     }
  #   }
  #   else{
  #     if(input$select_year_data == "All"){
  #       if(input$select_station_data == "UIC-Halsted"){
  #         uic_df
  #       } else if(input$select_station_data == "O'Hare Airport"){
  #         ohare_df
  #       } else{
  #         racine_df
  #       }
  #     } else{
  #       if(input$select_station_data == "UIC-Halsted"){
  #         subset(uic_df, uic_df$year == input$select_year_data)
  #       } else if(input$select_station_data == "O'Hare Airport"){
  #         subset(ohare_df, ohare_df$year == input$select_year_data)
  #       } else{
  #         subset(racine_df, racine_df$year == input$select_year_data)
  #       }
  #     }
  #   }
  # })
  
  year_frame <- function(station, object="graph"){
    if(object != "table"){
      if(station == "All"){df}
      else if(station == "UIC-Halsted"){uic_df}
      else if(station == "Racine"){racine_df}
      else{ohare_df}
    } 
    else{
      if(station == "All"){
            if(station == "All"){
              df
            } else{
              subset(df, df$year == input$select_year_data)
            }
          }
          else{
            if(input$select_year_data == "All"){
              if(station == "UIC-Halsted"){
                uic_df
              } else if(station == "O'Hare Airport"){
                ohare_df
              } else{
                racine_df
              }
            } else{
              if(station == "UIC-Halsted"){
                subset(uic_df, uic_df$year == input$select_year_data)
              } else if(station == "O'Hare Airport"){
                subset(ohare_df, ohare_df$year == input$select_year_data)
              } else{
                subset(racine_df, racine_df$year == input$select_year_data)
              }
            }
          }
    }
    
  }
  
  # Create sum dataframes
  
  month_df <- reactive({
    month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    rides <- array(unlist(
      lapply(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), month_sum)
    ))
    retrieve_package <- data.frame(month, rides)
    return(retrieve_package)
  })
  
  daily_df <- reactive({
    days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    rides <- array(unlist(
      lapply(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), daily_sum)
    ))
    retrieve_package <- data.frame(days, rides)
    return(retrieve_package)
  })
  
  year_df <- reactive({
    year <- 2001:2021
    rides <- array(unlist(lapply(2001:2021, year_sum)))
    data.frame(year, rides)
  })
  
  # Render graph to show data
  output$rides_year <- renderPlot({
    ggplot(data = year_frame(input$select_station_data), aes(x = year, y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$select_station_data)) +
      labs(x = "Year", y ="Rides", title = "Station Rides Across the Years")
  })
  
  output$rides_month <- renderPlot({
    ggplot(data = month_df(), aes(x = factor(month, level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$select_station_data)) +
      labs(x = "Month", y ="Rides", title = "Monthly entries")
  })
  
  output$rides_week <- renderPlot({
    ggplot(data = daily_df(), aes(x = factor(days, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$select_station_data)) +
      labs(x = "Week Day", y ="Rides", title = "Weekly entries")
  })
  
  # Create dataframe to show data
  year_table <- reactive({
    retrieve_package <- year_frame(input$select_station_data, "table")
    cols <- c("stationname", "date", "rides")
    retrieve_package <- retrieve_package[cols]
    
    names(retrieve_package)[1] <- "Station"
    names(retrieve_package)[2] <- "Date"
    names(retrieve_package)[3] <- "Rides"
    
    return(retrieve_package)
  })
  
  month_table <- reactive({
    retrieve_package <- month_df()
    # cols <- c("month", "rides")
    # retrieve_package <- retrieve_package[cols]
    
    names(retrieve_package)[1] <- "Month"
    names(retrieve_package)[2] <- "Rides"
    return(retrieve_package)
  })
  
  daily_table <- reactive({
    retrieve_package <- daily_df()
    # cols <- c("days", "rides")
    # retrieve_package <- retrieve_package[cols]
    names(retrieve_package)[1] <- "Day"
    names(retrieve_package)[2] <- "Rides"
    return(retrieve_package)
  })
  
  
  # Create table to show tabular data
  output$year_table <- renderUI({
    # format the table layout
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        year_table(),
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  output$month_table <- renderUI({
    # format the table layout
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        month_table(),
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  output$daily_table <- renderUI({
    # format the table layout
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        daily_table(),
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  # render plot and table
  output$data_table <- renderUI({
    
    # put three plots in a row
    fluidRow(
        column(4, 
               div(plotOutput("rides_year")),
               uiOutput("year_table")
        ),
        column(4, 
               div(plotOutput("rides_month")),
               uiOutput("month_table")
        ),
        column(4, 
               div(plotOutput("rides_week")),
               uiOutput("daily_table")
        )
      
    )
  })
  
  
}


shinyApp(ui = ui, server = server)
