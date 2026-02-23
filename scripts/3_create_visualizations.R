# Create Visualizations for Story

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

# create color map ----
color_map <- tibble(
  team = c(
    "ARI", "ATH", "ATL", "BAL", "BOS", 
    "CHC", "CHW", "CIN", "CLE", "COL", 
    "DET", "HOU", "KCR", "LAA", "LAD", 
    "MIA", "MIL", "MIN", "NYM", "NYY", 
    "PHI", "PIT", "SDP", "SEA", "SFG", 
    "STL", "TBR", "TEX", "TOR", "WSN"
  ),
  hex_code = c(
    "#A71930", "#003831", "#CE1141", "#DF4601", "#BD3039",
    "#0E3386", "#27251F", "#C6011F", "#E50022", "#333366",
    "#0C2340", "#EB6E1F", "#004687", "#BA0021", "#005A9C",
    "#00A3E0", "#FFC52F", "#D31145", "#FF5910", "#0C2340",
    "#E81828", "#FDB827", "#2F241D", "#005C5C", "#FD5A1E",
    "#C41E3A", "#0FBCE6", "#003278", "#134A8E", "#AB0003"
  )
)

# plot team WAR rank difference ----
team_diff <- group_diff |> 
  group_by(team_name) |> 
  summarize(war_25 = sum(war_25),
            war_26 = sum(war_26),
            diff = sum(diff),
            .groups = "drop") |> 
  mutate(war_25_rank = 31 - rank(war_25),
         war_26_rank = 31 - rank(war_26),
         rank_diff = war_25_rank - war_26_rank)
  
team_diff |>
  left_join(color_map, by = join_by(team_name == team)) |> 
  mutate(team_name = factor(team_name)) |> 
  ggplot(aes(x = reorder(team_name, -diff), y = diff, fill = hex_code)) +
  geom_col(width = 0.8) +
  geom_text(
    aes(
      label = team_name,
      y = if_else(diff > 0, diff - 0.5, diff + 0.5)
    ),
    size = 2,
    color = if_else(abs(team_diff$diff) > 0, "white", "black"),
    fontface = "bold"
  ) +
  scale_fill_identity()

# plot WAR rank difference by position ----
for (p in unique(group_diff$position)) {
  plot <- group_diff |> 
    filter(position == p) |> 
    mutate(war_25_rank = 31 - rank(war_25),
           war_26_rank = 31 - rank(war_26),
           rank_diff = war_25_rank - war_26_rank) |> 
    arrange(rank_diff) |> 
    left_join(color_map, by = join_by(team_name == team)) |> 
    ggplot(aes(x = war_25_rank, y = war_26_rank, colour = hex_code, label = team_name)) +
    geom_vline(xintercept = 15, linetype = "dotted", linewidth = 0.8) +
    geom_hline(yintercept = 15, linetype = "dotted", linewidth = 0.8) +
    geom_point(size = 3) +
    geom_text_repel(size = 3, max.overlaps = Inf) +
    scale_color_identity() +
    scale_x_reverse() +
    scale_y_reverse() +
    labs(
      title = paste0("WAR rankings for MLB ", p, ", 2025 vs 2026 (projected)"),
      subtitle = "Quadrants separate differences in change in production",
      x = paste0("2025 ", p, " WAR Rank"),
      y = paste0("2026 Projected ", p, " WAR Rank")
    ) +
    theme_minimal()
  
  print(plot)
}
