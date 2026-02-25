# Create Visualizations for Story

# load libraries ----
library(tidyverse)
library(here)
library(ggrepel)
library(knitr)
library(kableExtra)
library(webshot2)

# load data ----
load(here("clean_data/players_full.rds"))
load(here("raw_data/batters_25.rds"))
load(here("raw_data/batter_proj_26.rds"))

# calculate WAR difference ----
group_diff <- players_full |> 
  mutate(diff = war_26 - war_25)

# create color map ----
color_map <- tibble(
  # Maps teams to hex color codes for scatterplots
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
team_diff <- group_diff |> # Re-aggregates data from team/position level to just team level
  group_by(team_name) |> 
  summarize(war_25 = sum(war_25),
            war_26 = sum(war_26),
            diff = sum(diff),
            .groups = "drop") |> 
  mutate(war_25_rank = 31 - rank(war_25),
         war_26_rank = 31 - rank(war_26),
         rank_diff = war_25_rank - war_26_rank)
  
team_diff |> # Team WAR differential bar chart
  left_join(color_map, by = join_by(team_name == team)) |> 
  mutate(team_name = factor(team_name)) |> 
  ggplot(aes(
    x = reorder(team_name, -diff), # specifies order of bars from largest to smallest positive difference
    y = diff, 
    fill = hex_code # colors by hex code
  )) +
  geom_col(width = 0.8) +
  geom_text(
    aes(
      label = team_name,
      y = if_else(diff > 0, diff - 0.5, diff + 0.5) # Places team names in bars
    ),
    size = 2,
    color = if_else(abs(team_diff$diff) > 0, "white", "black"), # Ensures team names are visible
    fontface = "bold"
  ) +
  scale_fill_identity() + # Recognizes hex codes as hex codes instead of text
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(), # Takes team names off of x-axis
    axis.text.x = element_blank()
  ) +
  labs(
    title = "Team Difference in fWAR, 2025 to 2026 (Projected)",
    x = "Team",
    y = "fWAR differential"
  )

dir.create("plots")

ggsave(filename = "plots/team_war_diff.png") # Saves plot image

# plot WAR rank difference by position ----
for (p in unique(group_diff$position)) {
  # Iterates through every position to create a scatterplot
  plot <- group_diff |> 
    filter(position == p) |> 
    mutate(
      # Calculating normalized WAR and difference
      war_25_norm = round((war_25 - mean(war_25))/sd(war_25), digits = 1),
      war_26_norm = round((war_26 - mean(war_26))/sd(war_26), digits = 1),
      diff_norm = war_26_norm - war_25_norm
    ) |> 
    arrange(diff_norm) |> 
    left_join(color_map, by = join_by(team_name == team)) |> # adds color map
    ggplot(aes(x = war_25_norm, y = war_26_norm, colour = hex_code, label = team_name)) +
    geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.8) + # adds dotted vertical line
    geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.8) + # adds dotted horizontal line
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + # adds dotted diagonal
    geom_point(size = 3) +
    geom_text_repel(size = 3, max.overlaps = Inf) +
    scale_color_identity() + # Recognizes hex codes as hex codes instead of text
    labs(
      title = paste0("Normalized WAR scatterplot for MLB ", p, ", 2025 vs 2026 (projected)"),
      subtitle = "Quadrants separate differences in change in production",
      x = paste0("2025 ", p, " Normalized WAR"),
      y = paste0("2026 Projected ", p, " Normalized WAR")
    ) +
    theme_minimal()
  
  ggsave(filename = paste0("plots/war_norm_scatterplot_", p, ".png")) # saves plot as image
  
  print(plot) # Prints plot in R Studio viewer
}

# 10 biggest fWAR decreases table ----
biggest_fwar_decrease <- group_diff |> 
  select(team_name, position, war_25, war_26, diff) |>
  mutate(war_25 = round(war_25, digits = 1),
         diff = round(diff, digits = 1)) |> 
  arrange(diff) |> # Sorts by raw WAR decrease
  slice_head(n = 10) |> # Keeps top 10 items
  kable( # Formats into an HTML table
    format = "html",
    col.names = c("Team", "Position", "2025 fWAR", "2026 fWAR", "Difference"),
    caption = "Largest Decreases in fWAR from 2025 to 2026"
  ) |> 
  kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE)

# Saves as image
save_kable(biggest_fwar_decrease, file = "plots/biggest_fwar_decrease.png", zoom = 2)

# 10 biggest normalized fWAR decreases table ----
biggest_norm_fwar_decrease <- group_diff |> 
  group_by(position) |> 
  mutate(
    war_25_norm = round((war_25 - mean(war_25))/sd(war_25), digits = 1),
    war_26_norm = round((war_26 - mean(war_26))/sd(war_26), digits = 1),
    diff_norm = war_26_norm - war_25_norm
  ) |> 
  ungroup() |> 
  select(team_name, position, war_25_norm, war_26_norm, diff_norm) |>
  arrange(diff_norm) |> # Sorts by normalized WAR decrease
  slice_head(n = 10) |> # Keeps top 10 items
  kable( # Formats as HTML table
    format = "html",
    col.names = c("Team", "Position", "2025 Norm. fWAR", "2026 Norm. fWAR", "Difference"),
    caption = "Largest Decreases in Normalized fWAR from 2025 to 2026"
  ) |> 
  kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE)

# saves as image
save_kable(biggest_norm_fwar_decrease, file = "plots/biggest_norm_fwar_decrease.png", zoom = 2)

# 10 biggest normalized fWAR increases table ----
biggest_norm_fwar_increase <- group_diff |> 
  group_by(position) |> 
  mutate(
    war_25_norm = round((war_25 - mean(war_25))/sd(war_25), digits = 1),
    war_26_norm = round((war_26 - mean(war_26))/sd(war_26), digits = 1),
    diff_norm = war_26_norm - war_25_norm
  ) |> 
  ungroup() |> 
  select(team_name, position, war_25_norm, war_26_norm, diff_norm) |>
  arrange(desc(diff_norm)) |> 
  slice_head(n = 10) |> 
  kable(
    format = "html",
    col.names = c("Team", "Position", "2025 Norm. fWAR", "2026 Norm. fWAR", "Difference"),
    caption = "Largest Increases in Normalized fWAR from 2025 to 2026"
  ) |> 
  kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE)

save_kable(biggest_norm_fwar_increase, file = "plots/biggest_norm_fwar_increase.png", zoom = 2)
