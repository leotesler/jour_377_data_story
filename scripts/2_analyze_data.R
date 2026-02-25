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
  mutate(diff = war_26 - war_25) # Calculates difference in WAR

# plot differences ----
group_diff |> # Checks distribution of differences
  ggplot(aes(x = diff)) +
  geom_histogram()

# sort differences ----
group_diff |> # Inspecting the biggest differences
  arrange(desc(diff))

# rank by WAR ----
group_diff |> # Sorting differences with ranks in mind
  group_by(position) |> 
  mutate(war_25_rank = 31 - rank(war_25),
         war_26_rank = 31 - rank(war_26),
         rank_diff = war_25_rank - war_26_rank) |> 
  ungroup() |> 
  arrange(war_25)

group_diff |> 
  group_by(team_name) |> 
  summarize(war_25 = sum(war_25),
            war_26 = sum(war_26),
            diff = sum(diff)) |> 
  arrange(war_26)

# Normalize WAR values ----
group_diff |> # Inspecting effects of normalizing the differences
  group_by(position) |> 
  mutate(
    war_25_norm = (war_25 - mean(war_25))/sd(war_25),
    war_26_norm = (war_26 - mean(war_26))/sd(war_26),
    diff_norm = war_26_norm - war_25_norm
  ) |> 
  arrange(desc(diff_norm))

# inspect raw data to understand differences ----
batters_25 |> # Inspecting player-level data to under stand team/position level differences
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
