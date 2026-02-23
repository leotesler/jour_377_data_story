# Analyze joined dataset

# load libraries ----
library(tidyverse)
library(here)
library(ggrepel)

# load data ----
load(here("clean_data/players_full.rds"))
load(here("raw_data/batters_25.rds"))
load(here("raw_data/batter_proj_26.rds"))

# calculate WAR difference ----
group_diff <- players_full |> 
  mutate(diff = war_26 - war_25)

# plot differences ----
group_diff |> 
  ggplot(aes(x = diff)) +
  geom_histogram()

# sort differences ----
group_diff |> 
  arrange(desc(diff))

# rank by WAR ----
group_diff |> 
  group_by(position) |> 
  mutate(war_25_rank = 31 - rank(war_25),
         war_26_rank = 31 - rank(war_26),
         rank_diff = war_25_rank - war_26_rank) |> 
  arrange(rank_diff)

# inspect raw data to understand differences ----
batters_25 |> 
  janitor::clean_names() |> 
  select(player_name, team = team_name, position, pa, war) |> 
  filter(team == "CIN", position == "CF") |> 
  arrange(desc(war))

batter_proj_26 |> 
  janitor::clean_names() |>
  filter(name != "Total") |> 
  select(name, team, position, pa, war) |> 
  filter(team == "CIN", position == "CF") |> 
  arrange(desc(war))
