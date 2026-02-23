# Pull Split Positional Projections from Fangraphs

# load libraries ----
library(tidyverse)
library(rvest)
library(httr)

# map team names to abbreviations ----
team_abb_map <- tibble(
  name = c(
    "yankees", "red-sox", "orioles", "blue-jays", "rays",
    "white-sox", "guardians", "twins", "royals", "tigers",
    "angels", "athletics", "astros", "mariners", "rangers",
    "mets", "phillies", "braves", "marlins", "nationals",
    "cubs", "reds", "pirates", "cardinals", "brewers",
    "dodgers", "giants", "padres", "rockies", "diamondbacks"
  ),
  abb = c(
    "NYY", "BOS", "BAL", "TOR", "TBR",
    "CHW", "CLE", "MIN", "KCR", "DET",
    "LAA", "ATH", "HOU", "SEA", "TEX",
    "NYM", "PHI", "ATL", "MIA", "WSN",
    "CHC", "CIN", "PIT", "STL", "MIL",
    "LAD", "SFG", "SDP", "COL", "ARI"
  )
)

# load data from fangraphs ----
batters_list <- list()
pitchers_list <- list()

for (team_name in team_abb_map$name) {
  url <- paste0("https://www.fangraphs.com/teams/", team_name, "/depth-chart")
  page <- read_html(url)
  
  tables <- page |> 
    html_nodes("table") |> 
    html_table(fill = TRUE)
  
  catchers <- tables[[9]] |> 
    mutate(position = "C")
  
  first_basemen <- tables[[10]] |> 
    mutate(position = "1B")
  
  second_basemen <- tables[[11]] |> 
    mutate(position = "2B")
  
  shortstops <- tables[[12]] |> 
    mutate(position = "SS")
  
  third_basemen <- tables[[13]] |> 
    mutate(position = "3B")
  
  left_fielders <- tables[[14]] |> 
    mutate(position = "LF")
  
  center_fielders <- tables[[16]] |> 
    mutate(position = "CF")
  
  right_fielders <- tables[[15]] |> 
    mutate(position = "RF")
  
  designated_hitters <- tables[[17]] |> 
    mutate(position = "DH")
  
  starting_pitchers <- tables[[19]] |> 
    mutate(position = "SP")
  
  relief_pitchers <- tables[[20]] |> 
    mutate(position = "RP")
  
  batters <- bind_rows(
    catchers, first_basemen, second_basemen, shortstops, third_basemen,
    left_fielders, center_fielders, right_fielders, designated_hitters
  ) |> 
    mutate(team = team_abb_map$abb[team_abb_map$name == team_name])
  
  pitchers <- bind_rows(
    starting_pitchers, relief_pitchers
  ) |> 
    mutate(team = team_abb_map$abb[team_abb_map$name == team_name])
  
  batters_list[[team_name]] <- batters
  pitchers_list[[team_name]] <- pitchers
}

batter_proj_26 <- bind_rows(batters_list)
pitcher_proj_26 <- bind_rows(pitchers_list)

# save raw data ----
save(batter_proj_26, file = "raw_data/batter_proj_26.rds")
save(pitcher_proj_26, file = "raw_data/pitcher_proj_26.rds")
