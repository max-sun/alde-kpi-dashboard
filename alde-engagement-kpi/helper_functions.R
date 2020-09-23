library(tidyverse)
library(lubridate)
library(RPostgres)
library(readxl)
library(scales)
library(bbplot)
library(config)


data_contacts <- function(server, uid, pwd, port, database){
  
  suppressWarnings(key <- config::get("NationBuilder"))
  
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = key$database,
                   host = key$server,
                   port = key$port,
                   user = key$uid,
                   password = key$pwd)
  
  
  contacts <- tbl(con, sql("SELECT created_at from nbuild_aldeparty.signups")) %>% collect() %>%
    mutate(created_at = as.character(as_date(created_at)),
           created_at = ifelse(created_at < "2019-01-01","2019-01-01",created_at),
           created_at = ymd(created_at),
           month = month(created_at),
           year = year(created_at)) %>%
    group_by(year, month) %>%
    summarise(total_signups = n()) %>%
    ungroup() %>%
    mutate(total_signups_long = cumsum(total_signups))
  
  dbDisconnect(con)

  contacts
  
}




data_email <- function(server, uid, pwd, port, database){
  
  
  
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = database,
                   host = server,
                   port =  port,
                   user = uid,
                   password = pwd)
  
  
  
  
  emails <- tbl(con, dplyr::sql("SELECT published_at, sent, opened, clicked
                             
                             FROM nbuild_aldeparty.mailings

                             LEFT JOIN (SELECT mailing_id, COUNT(*) AS sent 
                                        FROM nbuild_aldeparty.mailing_events_sent 
                                        GROUP BY mailing_id) AS mails_sent 
                                        ON nbuild_aldeparty.mailings.id = mails_sent.mailing_id
                                        
                             LEFT JOIN (SELECT mailing_id, COUNT(*) AS opened
                                        FROM nbuild_aldeparty.mailing_events_opened 
                                        GROUP BY mailing_id) AS mails_opened 
                                        ON nbuild_aldeparty.mailings.id = mails_opened.mailing_id
                                        
                             LEFT JOIN (SELECT mailing_id, COUNT(*) AS clicked
                                        FROM nbuild_aldeparty.mailing_events_clicked
                                        GROUP BY mailing_id) AS mails_clicked 
                                        ON nbuild_aldeparty.mailings.id = mails_clicked.mailing_id
                              
                             WHERE published_at is not null
                             AND published_at > '2018-01-01'
                             AND broadcaster_id = 1007
                             ORDER BY published_at")) %>% collect()
  
  
  
  dbDisconnect(con)
  
  emails_final <- emails %>%
    mutate(published_at = ifelse(published_at < '2019-01-01', '2019-01-01', as.character(as_date(published_at))),
           year = year(ymd(published_at)),
           month = month(ymd(published_at))) %>%
    group_by(year,month) %>%
    summarise(emails = n(),
              sent = sum(sent),
              opened = sum(opened, na.rm = T),
              clicked = sum(clicked, na.rm = T)) %>%
    ungroup() %>%
    mutate(open_rate = opened/sent,
           click_rate = clicked/sent,
           click_through_rate = clicked/opened) %>%
    mutate(sent_long = cumsum(sent),
           open_long = cumsum(opened),
           click_long = cumsum(clicked),
           open_rate_long = open_long/sent_long,
           click_rate_long = click_long/sent_long)
  
  emails_final
  
}





