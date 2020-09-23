library(tidyverse)
library(lubridate)
library(RPostgres)
library(readxl)
library(scales)
library(bbplot)
library(config)
library(shiny)

suppressWarnings(key <- config::get("NationBuilder"))


source("helper_functions.R")



goals <- read_excel("ALDE Engagement Quantitative Goals.xlsx") %>%
  mutate(year_month = str_c(year,"-",str_pad(month,2,pad = "0")),
         date = str_c(year,"-",str_pad(month,2,pad = "0"),"-01"),
         date = as_date(ceiling_date(ymd(date), "month")-1))


dat <- goals %>% 
  
  left_join(data_contacts(server = key$server,
                          uid = key$uid,
                          pwd = key$pwd,
                          port = key$port,
                          database = key$database)) %>%
  
  left_join(data_email(server = key$server,
                       uid = key$uid,
                       pwd = key$pwd,
                       port = key$port,
                       database = key$database))


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  
  kpi_graph <- reactive({

    
    if(input$kpi == "Total Contacts"){
      
      kpi_graph <- ggplot(dat, aes(x = date)) +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        geom_vline(xintercept = ymd("2019-01-01"), size = 1, colour = "#333333") +
        
        geom_vline(xintercept = ymd("2020-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2021-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2022-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2023-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2024-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") + 
        
        geom_point(mapping = aes(y = total_signups_long), colour = "#ec008c", size = 3) +
        geom_line(mapping = aes(y = signups_goal), colour = "#284ba0", size = 1, linetype = "dashed") +
        
        scale_y_continuous(labels = comma_format()) +
        scale_x_date(date_breaks = "4 months", date_labels =  "%b %Y", limits = c(ymd("2019-01-01"),ymd("2024-09-01"))) +
        
        bbc_style()+
        theme(panel.grid.major.x = element_line(colour = "grey"),
              axis.text.x = element_text(hjust = 1, vjust = 1, angle = 55, size = 20))
      
    } else if(input$kpi == "Open Rate") {
      
      kpi_graph <- ggplot(dat, aes(x = date)) +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        geom_vline(xintercept = ymd("2019-01-01"), size = 1, colour = "#333333") +
        
        geom_vline(xintercept = ymd("2020-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2021-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2022-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2023-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2024-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") + 
        
        geom_point(mapping = aes(y = open_rate_long), colour = "#ec008c", size = 3) +
        geom_line(mapping = aes(y = open_rate_goal), colour = "#284ba0", size = 1, linetype = "dashed") +
        
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,0.3)) +
        scale_x_date(date_breaks = "4 months", date_labels =  "%b %Y", limits = c(ymd("2019-01-01"),ymd("2024-09-01"))) +
        
        bbc_style()+
        theme(panel.grid.major.x = element_line(colour = "grey"),
              axis.text.x = element_text(hjust = 1, vjust = 1, angle = 55, size = 20))
      
      
      
    } else if (input$kpi == "Click Rate") {
      
      kpi_graph <- ggplot(dat, aes(x = date)) +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        geom_vline(xintercept = ymd("2019-01-01"), size = 1, colour = "#333333") +
        
        geom_vline(xintercept = ymd("2020-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2021-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2022-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2023-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
        geom_vline(xintercept = ymd("2024-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") + 
        
        geom_point(mapping = aes(y = click_rate_long), colour = "#ec008c", size = 3) +
        geom_line(mapping = aes(y = click_rate_goal), colour = "#284ba0", size = 1, linetype = "dashed") +
        
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,0.06)) +
        scale_x_date(date_breaks = "4 months", date_labels =  "%b %Y", limits = c(ymd("2019-01-01"),ymd("2024-09-01"))) +
        
        bbc_style()+
        theme(panel.grid.major.x = element_line(colour = "grey"),
              axis.text.x = element_text(hjust = 1, vjust = 1, angle = 55, size = 20))
      
      
    }
    
    kpi_graph
    
  })
  
  output$kpi_graph <- renderPlot(kpi_graph())
  

})
