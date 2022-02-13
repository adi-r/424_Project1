# 424_Project1
CS 424 Analysis and Dashboard of Chicago CTA Data

This project is a hands on experience developing dashboards in Shiny using R.

Prerequisites
-------------
1. Requires installation of R and RStudio
2. Installation of libraries: "shiny", "shinydashboard", "scales", "tidyr", "ggplot2", "dplyr", "lubridate", "DT". 
   These libraries can be installed via the command install.packages("library name").
   
Running the Application
-----------------------
1. project1.R (Optional)
      
      This file opens in RStudio. Clicking on This file prepares the data for the app.R file. The data is extracted to a table. The columns are processed accordingly. The date column is converted to its proper format. The day, month and year are separately extracted and added to new columns. A subset of the dataframe is made based on certain stations. Afterwhich, the final data is written to the 'station_data.tsv' file. The data file has already been generated, hence the running of this file is optional.
      
2. app.R
      
      This file runs on RStudio. Ensure the data source: 'station_data.tsv' is in the same directory as the app.R file. Click on 'Run App' in the top right corner of the Code Editor section. 