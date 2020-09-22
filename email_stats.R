library(tidyverse)
library(lubridate)
library(RPostgres)
library(readxl)
library(scales)
library(bbplot)
library(config)


suppressWarnings(key <- config::get("NationBuilder"))

goals <- read_excel("ALDE Engagement Quantitative Goals.xlsx") %>%
  mutate(year_month = str_c(year,"-",str_pad(month,2,pad = "0")),
         date = str_c(year,"-",str_pad(month,2,pad = "0"),"-01"),
         date = as_date(ceiling_date(ymd(date), "month")-1))

con <- dbConnect(RPostgres::Postgres(),
                 dbname = key$database,
                 host = key$server,
                 port = key$port,
                 user = key$uid,
                 password = key$pwd)




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

final <- goals %>% left_join(emails_final, by = c("year","month"))



ggplot(final, aes(x = date)) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(xintercept = ymd("2019-01-01"), size = 1, colour = "#333333") +
  
  geom_vline(xintercept = ymd("2020-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2021-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2022-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2023-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2024-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") + 
  
  geom_point(mapping = aes(y = open_rate_long), colour = "#ec008c", size = 2) +
  geom_line(mapping = aes(y = goal_open_rate), colour = "#284ba0", size = 1, linetype = "dashed") +
  
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,0.3)) +
  scale_x_date(date_breaks = "4 months", date_labels =  "%b %Y", limits = c(ymd("2019-01-01"),ymd("2024-09-01"))) +
  
  bbc_style()+
  theme(panel.grid.major.x = element_line(colour = "grey"),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 55, size = 20))




ggplot(final, aes(x = date)) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(xintercept = ymd("2019-01-01"), size = 1, colour = "#333333") +
  
  geom_vline(xintercept = ymd("2020-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2021-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2022-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2023-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2024-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") + 
  
  geom_point(mapping = aes(y = click_rate_long), colour = "#ec008c", size = 2) +
  geom_line(mapping = aes(y = goal_click_rate), colour = "#284ba0", size = 1, linetype = "dashed") +
  
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,0.06)) +
  scale_x_date(date_breaks = "4 months", date_labels =  "%b %Y", limits = c(ymd("2019-01-01"),ymd("2024-09-01"))) +
  
  bbc_style()+
  theme(panel.grid.major.x = element_line(colour = "grey"),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 55, size = 20))

