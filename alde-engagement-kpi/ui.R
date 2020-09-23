library(tidyverse)
library(lubridate)
library(RPostgres)
library(readxl)
library(scales)
library(bbplot)
library(config)
library(shiny)





# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("ALDE Engagement KPIs"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            radioButtons(inputId = "kpi",
                         label = "Select Engagement KPI:",
                         choices = c("Total Contacts", "Emailable Contacts", "Emails Sent", "Open Rate", "Click Rate", "Donations"),
                         selected = "Open Rate")

        ),

        mainPanel(
            
            plotOutput("kpi_graph", height = 600, width = 1100)
            
        )
    )
))
