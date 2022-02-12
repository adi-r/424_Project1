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
                     tags$div(style = "margin-top: 400px;"),
                     menuItem("Station Comparison", tabName = "station_compare", selected = TRUE, icon = icon("signal", lib = "glyphicon")),
                     menuItem("Table Data", tabName = "table_data",icon = icon("dashboard")),
                     menuItem("Dates of Interest", tabName = "doi", icon = icon("calendar")),
                     menuItem("About", tabName = "about", icon = icon("sunglasses", lib = "glyphicon"))
                     )
                   ),
  dashboardBody(
    tags$head(tags$style(".sidebar-menu li { margin-bottom: 20px; }")),
    tabItems(
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
                                         )
                                         ),
                                         h4("Right"),
                                         div(selectInput("station2_compare", "Station",
                                                         choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                         selected = c("O'Hare Airport")
                                         )
                                         ),
                                         fluidRow(column(8,
                                                         div(selectInput("year2_compare", "Year",
                                                                         choices = c("All", 2021:2001),
                                                                         selected = c(2021)
                                                         )
                                                         )
                                         )
                                         ),
                                         width = 2
                            ),
                            mainPanel(
                              fluidRow(
                                splitLayout(cellWidths = c("75%", "75%"), uiOutput("compare_plots1"), uiOutput("compare_plots2"))), width = 8))
      ),
      tabItem(tabName = "table_data",
              sidebarLayout(position = "left",
                            sidebarPanel(style = "margin-top: 50%",
                                         h4("Left"),
                                         div(selectInput("station1_table_data", "Station",
                                                         choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                         selected = c("UIC-Halsted")
                                         )
                                         ),
                                         fluidRow(column(8,
                                                         div(selectInput("year1_table_data", "Year",
                                                                         choices = c("All", 2021:2001),
                                                                         selected = c(2021)
                                                         )
                                                         )
                                         )
                                         ),
                                         fluidRow(column(8,
                                                         div(selectInput("time1_table_data", "Time Frame",
                                                                         choices = c("Daily", "Weekly", "Monthly"),
                                                                         selected = c("Daily")
                                                         )
                                                         )
                                         )
                                         ),
                                         h4("Right"),
                                         div(selectInput("station2_table_data", "Station",
                                                         choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                         selected = c("O'Hare Airport")
                                         )
                                         ),
                                         fluidRow(column(8,
                                                         div(selectInput("time2_table_data", "Time Frame",
                                                                         choices = c("Daily", "Weekly", "Monthly"),
                                                                         selected = c("Daily")
                                                         )
                                                         )
                                         )
                                         ),
                                         fluidRow(column(8,
                                                         div(selectInput("year2_table_data", "Year",
                                                                         choices = c("All", 2021:2001),
                                                                         selected = c(2021)
                                                         )
                                                         )
                                         )
                                         ),
                                         width = 2
                            ),
                            mainPanel(
                              fluidRow(
                                splitLayout(cellWidths = c("75%", "75%"), uiOutput("table_plots1"), uiOutput("table_plots2"))), width = 8)
                            )
              ),
      tabItem(tabName = "doi",
              fluidRow(
                tags$div(style = "margin-top: 100px;"),
                tabBox(
                  title = "select_station_tab",
                  id = "tabset1", height = "250px", selected = "Monthly",
                  tabPanel("Through the Years", plotOutput("plot1")),
                  tabPanel("Monthly", h1("MKCCCCCCCCCCCC"), plotOutput("plot2")),
                  tabPanel("Weekly", plotOutput("plot3"))
                )
              )),
      
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
##############################################################################################################################################

server <- function(input, output){
   
  # Sum Functions
  month_sigma <- function(month, year, station){
    if(year == "All"){
      if(station == "All"){
        sum(df[df$month_name == month,]$rides)
      } else if(station == "UIC-Halsted"){
        sum(uic_df[uic_df$month_name == month,]$rides)
      } else if(station == "O'Hare Airport"){
        sum(ohare_df[ohare_df$month_name == month,]$ rides)
      } else{
        sum(racine_df[racine_df$month_name == month,]$rides)
      }
    }
    else{
      if(station == "All"){
        sum(df[df$year == year & df$month_name == month,]$rides)
      } else if(station == "UIC-Halsted"){
        sum(uic_df[uic_df$year == year & uic_df$month_name == month,]$rides)
      } else if(station == "O'Hare Airport"){
        sum(ohare_df[ohare_df$year == year & ohare_df$month_name == month,]$ rides)
      } else{
        sum(racine_df[racine_df$year == year & racine_df$month_name == month,]$rides)
      }
    }
  }
  
  week_sigma <- function(day, year, station){
    if(year == "All"){
      if(station == "All"){
        sum(df[df$week_day == day,]$rides)
      } else if(station == "UIC-Halsted"){
        sum(uic_df[uic_df$week_day == day,]$rides)
      } else if(station == "O'Hare Airport"){
        sum(ohare_df[ohare_df$week_day == day,]$ rides)
      } else{
        sum(racine_df[racine_df$week_day == day,]$rides)
      }
    }
    else{
      if(station == "All"){
        sum(df[df$year == year & df$week_day == day,]$rides)
      } else if(station == "UIC-Halsted"){
        sum(uic_df[uic_df$year == year & uic_df$week_day == day,]$rides)
      } else if(station == "O'Hare Airport"){
        sum(ohare_df[ohare_df$year == year & ohare_df$week_day == day,]$ rides)
      } else{
        sum(racine_df[racine_df$year == year & racine_df$week_day == day,]$rides)
      }
    }
  }
  
  
  
  year_frame <- function(year, station, object){
    if(object == "graph"){
      if(station == "All"){df}
      else if(station == "UIC-Halsted"){uic_df}
      else if(station == "Racine"){racine_df}
      else{ohare_df}
    } 
    else{
      if(station == "All"){
            if(year == "All"){
              date_frame <- df
              date_frame <- date_frame[c("stationname", "date", "year", "rides")]
              return(date_frame)
            } else{
              date_frame <- df[df$year == year,]
              date_frame <- date_frame[c("stationname", "date", "year", "rides")]
              return(date_frame)
            }
          }
          else{
            if(year == "All"){
              if(station == "UIC-Halsted"){
                date_frame <- uic_df
                date_frame <- date_frame[c("stationname", "date", "year", "rides")]
                return(date_frame)
              } else if(station == "O'Hare Airport"){
                date_frame <- ohare_df
                date_frame <- date_frame[c("stationname", "date", "year", "rides")]
                return(date_frame)
              } else{
                date_frame <- racine_df
                date_frame <- date_frame[c("stationname", "date", "year", "rides")]
                return(date_frame)
              }
            } else{
              if(station == "UIC-Halsted"){
                date_frame <- uic_df[uic_df$year == year,]
                date_frame <- date_frame[c("stationname", "date", "year", "rides")]
                return(date_frame)
              } else if(station == "O'Hare Airport"){
                date_frame <- ohare_df[ohare_df$year == year,]
                date_frame <- date_frame[c("stationname", "date", "year", "rides")]
                return(date_frame)
              } else{
                date_frame <- racine_df[racine_df$year == year,]
                date_frame <- date_frame[c("stationname", "date", "year", "rides")]
                return(date_frame)
              }
            }
          }
    }
    
  }
  
  # Create sum dataframes
  date_df <- function(year, station){
    if(year == "All"){
      if(station == "All"){
        date_frame <- df[c("date", "rides")]
        return(date_frame)
      }
      else{
        if(station == "UIC-Halsted"){
          date_frame <- uic_df[c("date", "rides")]
          return(date_frame)
        }
        else if(station == "Racine"){
          date_frame <- racine_df[c("date", "rides")]
          return(date_frame)
        }
        else{
          date_frame <- ohare_df[c("date", "rides")]
        }
      }
    }
    else{
      if(station == "All"){
        date_frame <- df[df$year == year,]
        date_frame <- date_frame[c("date", "rides")]
        return(date_frame)
      }
      else{
        if(station == "UIC-Halsted"){
          date_frame <- uic_df[uic_df$year == year,]
          date_frame <- date_frame[c("date", "rides")]
          return(date_frame)
        }
        else if(station == "Racine"){
          date_frame <- racine_df[racine_df$year == year,]
          date_frame <- date_frame[c("date", "rides")]
          return(date_frame)
        }
        else{
          date_frame <- ohare_df[ohare_df$year == year,]
          date_frame <- date_frame[c("date", "rides")]
          return(date_frame)
        }
      }
    }
  }
  month_df <- function(year, station){
    month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    rides <- sapply(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), function(month) month_sigma(month, year, station))
    month_frame <- data.frame(month, rides)
    return(month_frame)
  }
  
  week_df <- function(year, station){
    days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    rides <- sapply(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), function(day) week_sigma(day, year, station))
    week_frame <- data.frame(days, rides)
    return(week_frame)
  }
  
###############################################################################################################################################
  # Render graph to show data for Table Station 1
  output$rides_dates_table1 <- renderPlot({
    ggplot(data = date_df(input$year1_table_data, input$station1_table_data), aes(x = date, y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station1_table_data))+
      labs(x = "Date", y ="Rides", title = "Daily Entries")
  })
  
  output$rides_week_table1 <- renderPlot({
    ggplot(data = week_df(input$year1_table_data, input$station1_table_data), aes(x = factor(days, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station1_table_data)) +
      labs(x = "Week Day", y ="Rides", title = "Weekly entries")
  })
  
  output$rides_month_table1 <- renderPlot({
    ggplot(data = month_df(input$year1_table_data, input$station1_table_data), aes(x = factor(month, level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station1_table_data)) +
      labs(x = "Month", y ="Rides", title = "Monthly entries")
  })
  
  # Render graph for Table Station 2
  output$rides_dates_table2 <- renderPlot({
    ggplot(data = date_df(input$year2_table_data, input$station2_table_data), aes(x = date, y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station2_table_data))+
      labs(x = "Date", y ="Rides", title = "Daily Entries")
  })
  
  output$rides_week_table2 <- renderPlot({
    ggplot(data = week_df(input$year2_table_data, input$station2_table_data), aes(x = factor(days, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station2_table_data)) +
      labs(x = "Week Day", y ="Rides", title = "Weekly entries")
  })
  
  output$rides_month_table2 <- renderPlot({
    ggplot(data = month_df(input$year2_table_data, input$station2_table_data), aes(x = factor(month, level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station2_compare)) +
      labs(x = "Month", y ="Rides", title = "Monthly entries")
  })
  
#####################################################################################################################################################  
  # Render graph to show data for Compare Station 1
  output$rides_dates_compare1 <- renderPlot({
    ggplot(data = date_df(input$year1_compare, input$station1_compare), aes(x = date, y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station1_compare))+
      labs(x = "Date", y ="Rides", title = "Daily Entries")
  })
  
  output$rides_year_compare1 <- renderPlot({
    ggplot(data = year_frame(input$year1_compare, input$station1_compare, "graph"), aes(x = year, y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station1_compare)) +
      labs(x = "Year", y ="Rides", title = "Station Rides Across the Years")
  })
  
  output$rides_month_compare1 <- renderPlot({
    ggplot(data = month_df(input$year1_compare, input$station1_compare), aes(x = factor(month, level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station1_compare)) +
      labs(x = "Month", y ="Rides", title = "Monthly entries")
  })
  
  output$rides_week_compare1 <- renderPlot({
    ggplot(data = week_df(input$year1_compare, input$station1_compare), aes(x = factor(days, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station1_compare)) +
      labs(x = "Week Day", y ="Rides", title = "Weekly entries")
  })
  
  # Render graph to show data for Compare Station 2
  output$rides_dates_compare2 <- renderPlot({
    ggplot(data = date_df(input$year2_compare, input$station2_compare), aes(x = date, y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station2_compare))+
      labs(x = "Date", y ="Rides", title = "Daily Entries")
  })
  
  output$rides_year_compare2 <- renderPlot({
    ggplot(data = year_frame(input$year2_compare, input$station2_compare, "graph"), aes(x = year, y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station2_compare)) +
      labs(x = "Year", y ="Rides", title = "Station Rides Across the Years")
  })
  
  output$rides_month_compare2 <- renderPlot({
    ggplot(data = month_df(input$year2_compare, input$station2_compare), aes(x = factor(month, level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station2_compare)) +
      labs(x = "Month", y ="Rides", title = "Monthly entries")
  })
  
  output$rides_week_compare2 <- renderPlot({
    ggplot(data = week_df(input$year2_compare, input$station2_compare), aes(x = factor(days, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color(input$station2_compare)) +
      labs(x = "Week Day", y ="Rides", title = "Weekly entries")
  })

  #########################################################################################################################################################
  # DATA FOR TABLE UI  
  # Create table frames to show data for Section 1
  year1_table <- function(){
    table_frame <- year_frame(input$year1_table_data, input$station1_table_data, "table")
    table_frame <- table_frame[c("date", "rides")]
    table_frame <- table_frame %>%
      rename(Date = date, Rides = rides)
    return(table_frame)
  }
  
  year2_table <- function(){
    table_frame <- year_frame(input$year2_table_data, input$station2_table_data, "table")
    table_frame <- table_frame[c("date", "rides")]
    table_frame <- table_frame %>%
      rename(Date = date, Rides = rides)
    return(table_frame)
  }
  
  month1_table <- function(){
    table_frame <- month_df(input$year1_table_data, input$station1_table_data)
    table_frame <- table_frame %>%
      rename(Month = month, Rides = rides)
    return(table_frame)
  }
  
  month2_table <- function(){
    table_frame <- month_df(input$year2_table_data, input$station2_table_data)
    table_frame <- table_frame %>%
      rename(Month = month, Rides = rides)
    return(table_frame)
  }
  
  week1_table <- function(){
    table_frame <- week_df(input$year1_table_data, input$station1_table_data)
    table_frame <- table_frame %>%
      rename(Week_Day = days, Rides = rides)
    return(table_frame)
  }
  
  week2_table <- function(){
    table_frame <- week_df(input$year2_table_data, input$station2_table_data)
    table_frame <- table_frame %>%
      rename(Week_Day = days, Rides = rides)
    return(table_frame)
  }

####################################################################################################################################################  
  # Bar Graph color
  bar_color <- function(station){
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

 ################################################################################################################################### 
  # FORMAT TABLE UI
  # Create table to show tabular data
  # Dates UI
  output$year1_table <- renderUI({
    div(
      tags$style(
        HTML('.datatables {width: inherit !important;}')
      ),
      datatable(
        year1_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
    })
  
  output$year2_table <- renderUI({
    div(
      tags$style(
        HTML('.datatables {width: inherit !important;}')
      ),
      datatable(
        year2_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  # Months UI
  output$month1_table <- renderUI({
    div(
      tags$style(
        HTML('.datatables {width: inherit !important;}')
      ),
      datatable(
        month1_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  output$month2_table <- renderUI({
    div(
      tags$style(
        HTML('
          .datatables {width: inherit !important;}')
        ),
      datatable(
        month2_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  # Week UI
  output$week1_table <- renderUI({
    div(
      tags$style(
        HTML('.datatables {width: inherit !important;}')
      ),
      datatable(
        week1_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
  })
  
  output$week2_table <- renderUI({
    div(
      tags$style(
        HTML('.datatables {width: inherit !important;}')
      ),
      datatable(
        week2_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
  })
 ############################################################################################################################################# 
  # Render UI
  # Table station 1
  output$table_plots1 <- renderUI({
    if(input$time1_table_data == "Daily"){
      fluidPage(
        fluidRow(column(8, div(plotOutput("rides_dates_table1")))),
        fluidRow(column(8, uiOutput("year1_table"))))
      }
    else if(input$time1_table_data == "Weekly"){
      fluidPage(
        fluidRow(column(8, div(plotOutput("rides_week_table1")))),
        fluidRow(column(8, uiOutput("week1_table"))))
      }
    else{
      fluidPage(
        fluidRow(column(8, div(plotOutput("rides_month_table1")))),
        fluidRow(column(8, uiOutput("month1_table"))))
      }
    })
  
  # Table station 2
  output$table_plots2 <- renderUI({
    if(input$time2_table_data == "Daily"){
      fluidPage(
        fluidRow(column(8, div(plotOutput("rides_dates_table2")))),
        fluidRow(column(8, uiOutput("year2_table"))))
    }
    else if(input$time2_table_data == "Weekly"){
      fluidPage(
        fluidRow(column(8, div(plotOutput("rides_week_table2")))),
        fluidRow(column(8, uiOutput("week2_table"))))
    }
    else{
      fluidPage(
        fluidRow(column(8, div(plotOutput("rides_month_table2")))),
        fluidRow(column(8, uiOutput("month2_table"))))
    }
  })
###################################################################################################################################################  
  # Render Compare station 1
  output$compare_plots1 <- renderUI({
    fluidPage(
      fluidRow(column(8, div(plotOutput("rides_year_compare1")))),
      fluidRow(column(8, div(plotOutput("rides_dates_compare1")))),
      fluidRow(column(8, div(plotOutput("rides_month_compare1")))),
      fluidRow(column(8, div(plotOutput("rides_week_compare1")))))
  })
  
  # Render Compare station 2
  output$compare_plots2 <- renderUI({
    fluidPage(
      fluidRow(column(8, div(plotOutput("rides_year_compare2")))),
      fluidRow(column(8, div(plotOutput("rides_dates_compare2")))),
      fluidRow(column(8, div(plotOutput("rides_month_compare2")))),
      fluidRow(column(8, div(plotOutput("rides_week_compare2")))))
  })
  
###############################################################################################################################################################  
  # Render Dates of Interest
  output$plot1 <- renderPlot({
    ggplot(data = year_frame("Racine"), aes(x = year, y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color("Racine")) +
      labs(x = "Year", y ="Rides", title = "Station Rides Across the Years")
  })
  output$plot2 <- renderPlot({
    ggplot(data = month_df(2021, "O'Hare Airport"), aes(x = factor(month, level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), y = rides)) +
      geom_bar(stat = "identity", aes(fill = rides), fill = bar_color("O'Hare Airport")) +
      labs(x = "Month", y ="Rides", title = "Monthly entries")
  })
  
}

shinyApp(ui = ui, server = server)
