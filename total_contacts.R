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


contacts <- tbl(con, sql("SELECT created_at from nbuild_aldeparty.signups")) %>% collect() %>%
  mutate(created_at = as.character(as_date(created_at)),
         created_at = ifelse(created_at < "2019-01-01","2019-01-01",created_at),
         created_at = ymd(created_at),
         month = month(created_at),
         year = year(created_at)) %>%
  group_by(year, month) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(cum_count = cumsum(count))

dbDisconnect(con)

final <- goals %>% left_join(contacts, by = c("year","month"))

