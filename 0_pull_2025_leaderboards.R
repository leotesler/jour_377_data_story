# Pull Split Positional Data from Fangraphs

# load libraries ----
library(tidyverse)
library(here)
library(baseballr)
library(httr)
library(rvest)
library(jsonlite)

# load positional data ----
# catchers
url_c <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=35&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_c <- GET(url_c)
raw_c <- content(response_c, as = "text", encoding = "UTF-8")
json_c <- fromJSON(raw_c)

catchers <- tibble(json_c[[1]]) |> 
  mutate(position = "C")

# first basemen
url_1b <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=36&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_1b <- GET(url_1b)
raw_1b <- content(response_1b, as = "text", encoding = "UTF-8")
json_1b <- fromJSON(raw_1b)

first_basemen <- tibble(json_1b[[1]]) |> 
  mutate(position = "1B")

# second basemen
url_2b <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=37&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_2b <- GET(url_2b)
raw_2b <- content(response_2b, as = "text", encoding = "UTF-8")
json_2b <- fromJSON(raw_2b)

second_basemen <- tibble(json_2b[[1]]) |> 
  mutate(position = "2B")

# shortstops
url_ss <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=38&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_ss <- GET(url_ss)
raw_ss <- content(response_ss, as = "text", encoding = "UTF-8")
json_ss <- fromJSON(raw_ss)

shortstops <- tibble(json_ss[[1]]) |> 
  mutate(position = "SS")

# third basemen
url_3b <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=39&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_3b <- GET(url_3b)
raw_3b <- content(response_3b, as = "text", encoding = "UTF-8")
json_3b <- fromJSON(raw_3b)

third_basemen <- tibble(json_3b[[1]]) |> 
  mutate(position = "3B")

# left fielders
url_lf <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=42&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_lf <- GET(url_lf)
raw_lf <- content(response_lf, as = "text", encoding = "UTF-8")
json_lf <- fromJSON(raw_lf)

left_fielders <- tibble(json_lf[[1]]) |> 
  mutate(position = "LF")

# center fielders
url_cf <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=41&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_cf <- GET(url_cf)
raw_cf <- content(response_cf, as = "text", encoding = "UTF-8")
json_cf <- fromJSON(raw_cf)

center_fielders <- tibble(json_cf[[1]]) |> 
  mutate(position = "CF")

# right fielders
url_rf <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=40&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_rf <- GET(url_rf)
raw_rf <- content(response_rf, as = "text", encoding = "UTF-8")
json_rf <- fromJSON(raw_rf)

right_fielders <- tibble(json_rf[[1]]) |> 
  mutate(position = "RF")

# designated hitters
url_dh <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=40&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_dh <- GET(url_dh)
raw_dh <- content(response_dh, as = "text", encoding = "UTF-8")
json_dh <- fromJSON(raw_dh)

designated_hitters <- tibble(json_dh[[1]]) |> 
  mutate(position = "DH")

# starting pitchers
url_sp <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=sta&lg=all&qual=y&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_sp <- GET(url_sp)
raw_sp <- content(response_sp, as = "text", encoding = "UTF-8")
json_sp <- fromJSON(raw_sp)

starting_pitchers <- tibble(json_sp[[1]]) |> 
  mutate(position = "SP")

# relief pitchers
url_rp <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=rel&lg=all&qual=y&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0%2Cto&pageitems=10000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_rp <- GET(url_rp)
raw_rp <- content(response_rp, as = "text", encoding = "UTF-8")
json_rp <- fromJSON(raw_rp)

relief_pitchers <- tibble(json_rp[[1]]) |> 
  mutate(position = "RP")

# combine datasets ----
batters_25 <- bind_rows(
  catchers, first_basemen, second_basemen, shortstops, third_basemen,
  left_fielders, center_fielders, right_fielders, designated_hitters
)

pitchers_25 <- bind_rows(
  starting_pitchers, relief_pitchers
)

# save raw data ----
dir.create("raw_data")

save(batters_25, file = "raw_data/batters_25.rds")
save(pitchers_25, file = "raw_data/pitchers_25.rds")
