library(tidyverse)
library(RPostgres)
library(config)


suppressWarnings(key <- config::get("NationBuilder"))

con <- dbConnect(RPostgres::Postgres(),
                 dbname = key$database,
                 host = key$server,
                 port = key$port,
                 user = key$uid,
                 password = key$pwd)


polling <- tbl(con, sql("")) %>% collect()


results <- tbl(con, sql("SELECT * FROM results WHERE election_id = 'DE-EU-20190526' AND party_id = 'DE-FDP'")) %>%
  filter(pollingarea_id %in% !!polling$pollingarea_id) %>%
  collect()

dbDisconnect(con)