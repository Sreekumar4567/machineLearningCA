#the code
library(ggplot2)
library(dplyr)
library(DT)
library(shiny)
library(tidyr)
library(ggvis)

shinyUI(fluidPage(
  
  
  # Application title
  titlePanel("HR analytics"),
  
  
  sidebarLayout(
    sidebarPanel(fileInput("file","Upload the survey results"), 
                 width =2),
    mainPanel(
      width = 10,
      dataTableOutput("table"),
      downloadButton("downloadtable", "Download the table"),
      tags$br(),
      tags$hr("Team FT03")
      
      
    )
  )
))


