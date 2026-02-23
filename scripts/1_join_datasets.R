# Join 2025 results with 2026 projections

# load libraries ----
library(tidyverse)
library(here)

# load raw data ----
load(here("raw_data/batters_25.rds"))
load(here("raw_data/pitchers_25.rds"))
load(here("raw_data/batter_proj_26.rds"))
load(here("raw_data/pitcher_proj_26.rds"))

# aggregate raw data ----
batters_25_agg <- batters_25 |> 
  janitor::clean_names() |> 
  group_by(team_name, position) |> 
  summarize(war = sum(war, na.rm = TRUE)) |> 
  arrange(desc(war)) |> 
  ungroup()

pitchers_25_agg <- pitchers_25 |> 
  janitor::clean_names() |> 
  group_by(team_name, position) |> 
  summarize(war = sum(war, na.rm = TRUE)) |> 
  arrange(desc(war)) |> 
  ungroup()

batters_26_agg <- batter_proj_26 |> 
  janitor::clean_names() |> 
  mutate(team_name = team,
         .keep = "unused") |> 
  filter(name != "Total") |> 
  group_by(team_name, position) |> 
  summarize(war = sum(war, na.rm = TRUE)) |> 
  arrange(desc(war)) |> 
  ungroup()

pitchers_26_agg <- pitcher_proj_26 |> 
  janitor::clean_names() |> 
  mutate(team_name = team,
         .keep = "unused") |> 
  filter(name != "Total") |> 
  group_by(team_name, position) |> 
  summarize(war = sum(war, na.rm = TRUE)) |> 
  arrange(desc(war)) |> 
  ungroup()

players_25 <- bind_rows(
  batters_25_agg, pitchers_25_agg
)

players_26 <- bind_rows(
  batters_26_agg, pitchers_26_agg
)

# join datasets ----
players_full <- players_25 |> 
  left_join(
    players_26,
    by = join_by(team_name, position),
    suffix = c("_25", "_26")
  )

# save results ----
dir.create("clean_data")

save(players_full, file = "clean_data/players_full.rds")
