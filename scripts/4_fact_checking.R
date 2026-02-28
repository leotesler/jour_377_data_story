# Fact Checking

# load libraries ----
library(tidyverse)
library(here)
library(rvest)
library(httr)
library(jsonlite)

# load data ----
load(here("raw_data/batter_proj_26.rds"))
load(here("raw_data/pitcher_proj_26.rds"))
load(here("raw_data/batters_25.rds"))
load(here("raw_data/pitchers_25.rds"))
load(here("clean_data/players_full.rds"))

# compare aggregated total fWAR values with Fangraphs' calculated values ----
batter_totals <- batter_proj_26 |> 
  janitor::clean_names() |> 
  select(name, position, team, war) |> 
  filter(name == "Total") # keeps only Fangraphs' totals rows

pitcher_totals <- pitcher_proj_26 |> 
  janitor::clean_names() |> 
  select(name, position, team, war) |> 
  filter(name == "Total") # keeps only Fangraphs' totals rows

fangraphs_totals <- bind_rows(
  batter_totals, pitcher_totals
)

players_full |> 
  select(!war_25) |> 
  # joins the manually aggregated WAR totals to the Fangraphs-calculated totals
  left_join(
    fangraphs_totals, 
    by = join_by(team_name == team, position)
  ) |> 
  rename(war_total = war) |> 
  mutate(error = abs(war_total - war_26)) |> 
  filter(error != 0) |> 
  # There are some slight discrepancies based on Fangraphs' rounding of WAR in the HTML tables
  # Still, the worst error is within 3/10 of Fangraphs' totals
  # To be safe, I went back and changed my aggregations to include Fangraphs' totals instead of mine.
  # I changed all of my plots and tables as well.
  arrange(desc(error)) |> 
  print(n = Inf)

# compare aggregated 2025 fWAR to Fangraphs' totals ----
batters_25_clean <- batters_25 |> 
  janitor::clean_names() |> 
  group_by(team_name, position) |> 
  summarize(war_agg = sum(war))

pitchers_25_clean <- pitchers_25 |> 
  janitor::clean_names() |> 
  group_by(team_name, position) |> 
  summarize(war_agg = sum(war))

groups_agg_25 <- bind_rows(
  batters_25_clean, pitchers_25_clean
)

# Pull team-level aggregated data from Fangraphs
url_c <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=35"
url_1b <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=36"
url_2b <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=37"
url_3b <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=39"
url_ss <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=38"
url_lf <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=42"
url_cf <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=41"
url_rf <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=40"
url_dh <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=44"
url_sp <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=0&pos=all&stats=sta"
url_rp <- "https://www.fangraphs.com/leaders/major-league?lg=all&qual=y&season=2025&season1=2025&ind=0&team=0%2Cts&type=8&month=0&pos=all&stats=rel"

table_c <- read_html(url_c) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |> 
  mutate(position = "C")

table_1b <- read_html(url_1b) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "1B")

table_2b <- read_html(url_2b) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "2B")

table_3b <- read_html(url_3b) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "3B")

table_ss <- read_html(url_ss) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "SS")

table_lf <- read_html(url_lf) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "LF")

table_cf <- read_html(url_cf) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "CF")

table_rf <- read_html(url_rf) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "RF")

table_dh <- read_html(url_dh) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "DH")

table_sp <- read_html(url_sp) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "SP")

table_rp <- read_html(url_rp) |> 
  html_nodes("table") |> 
  _[[10]] |> 
  html_table(fill = TRUE) |> 
  select(!contains("Line Break")) |>
  mutate(position = "RP")

groups_fg_25 <- bind_rows(
  table_c, table_1b, table_2b, table_3b,
  table_ss, table_lf, table_cf, table_rf,
  table_dh, table_sp, table_rp
) |> 
  janitor::clean_names() |> 
  select(team, position, war_fg = warwar_wins_above_replacement)

# join tables and compare
groups_agg_25 |> 
  left_join(
    groups_fg_25,
    by = join_by(team_name == team, position)
  ) |> 
  mutate(war_agg = round(war_agg, digits = 1),
         diff = war_agg - war_fg) |> 
  # All values are accurate here since the WAR values from the API are in full, un-rounded form.
  # Thus, my rounding during aggregation is the same as what Fangraphs does behind the scenes for their HTML tables.
  # No changes needed.
  filter(diff != 0)
