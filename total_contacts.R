library(tidyverse)
library(lubridate)
library(RPostgres)
library(readxl)
library(scales)
library(bbplot)
library(config)

goals <- read_excel("ALDE Engagement Quantitative Goals.xlsx") %>%
  mutate(year_month = str_c(year,"-",str_pad(month,2,pad = "0")),
         date = str_c(year,"-",str_pad(month,2,pad = "0"),"-01"),
         date = ymd(date))


suppressWarnings(key <- config::get("NationBuilder"))

con <- dbConnect(RPostgres::Postgres(),
                 dbname = key$database,
                 host = key$server,
                 port = key$port,
                 user = key$uid,
                 password = key$pwd)


signup_total <- tbl(con, sql("SELECT created_at from nbuild_aldeparty.signups")) %>% collect() 



signup_perday <- signup_total %>%
  mutate(created_at = as.character(as_date(created_at)),
         created_at = ifelse(created_at < "2019-01-01","2019-01-01",created_at),
         created_at = ymd(created_at),
         month = month(created_at),
         year = year(created_at)) %>%
  group_by(year, month) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(cum_count = cumsum(count))


master <- goals %>% left_join(signup_perday, by = c("year","month"))


ggplot(master, aes(x = date)) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(xintercept = ymd("2019-01-01"), size = 1, colour = "#333333") +
  geom_vline(xintercept = ymd("2020-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  
  geom_vline(xintercept = ymd("2021-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  
  geom_vline(xintercept = ymd("2022-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  geom_vline(xintercept = ymd("2023-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  
  geom_vline(xintercept = ymd("2024-01-01"), size = 0.5, colour = "#333333", linetype = "dashed") +
  
  
  geom_line(mapping = aes(y = signups), colour = "#284ba0", size = 1, linetype = "dashed") +
  geom_point(mapping = aes(y = cum_count), colour = "#ec008c", size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "4 months", date_labels =  "%b %Y") +
  bbc_style()+
  theme(panel.grid.major.x = element_line(colour = "grey"),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 55, size = 20))


dbDisconnect(con)