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
         date = ymd(date))

con <- dbConnect(RPostgres::Postgres(),
                 dbname = key$database,
                 host = key$server,
                 port = key$port,
                 user = key$uid,
                 password = key$pwd)


signup_new <- tbl(con, sql("SELECT created_at,
                                   unsubscribed_at, 
                                   bad_at
                            FROM nbuild_aldeparty.signups
                            LEFT JOIN (SELECT signup_id, created_at as bad_at FROM nbuild_aldeparty.mailing_events_bad) AS bad 
                            ON bad.signup_id = signups.id
                            WHERE email1 is not null")) %>% collect() %>%
  mutate(emailable = ifelse(is.na(unsubscribed_at) & is.na(bad_at), T, F))


dbDisconnect(con)




final <- data.frame(date = character(), count = integer())

for (i in as_date(master$date)[master$date<Sys.Date()]) {
  
  print(i)
  
  temp <- signup_new %>% 
    mutate(date = as.character(as_date(i)),
           emailable = ifelse(created_at < as_date(i) & 
                                (is.na(unsubscribed_at) | unsubscribed_at > as_date(i)) & 
                                (is.na(bad_at) | bad_at > as_date(i)), T, F)) %>%
    filter(emailable) %>%
    group_by(date) %>%
    summarise(count = n())
  
  final <- bind_rows(final,temp)
  
}

final <- final %>% mutate(date = ymd(date))

master <- goals %>% left_join(final, by = "date")

ggplot(master, aes(x = date)) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(xintercept = ymd("2019-01-01"), size = 1, colour = "#333333") +
  geom_vline(xintercept = ymd("2020-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  
  geom_vline(xintercept = ymd("2021-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  
  geom_vline(xintercept = ymd("2022-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2023-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  
  geom_vline(xintercept = ymd("2024-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  
  
  geom_line(mapping = aes(y = emailable), colour = "#284ba0", size = 1, linetype = "dashed") +
  geom_point(mapping = aes(y = count), colour = "#ec008c", size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "4 months", date_labels =  "%b %Y") +
  bbc_style()+
  theme(panel.grid.major.x = element_line(colour = "grey"),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 55, size = 20))



unsubs <- tbl(con, sql("SELECT * FROM nbuild_aldeparty.unsubscribes")) %>% collect() 

bad <- tbl(con, sql("SELECT * FROM nbuild_aldeparty.mailing_events_bad")) %>% collect()
