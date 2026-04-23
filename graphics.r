#########################################################################
# This script loads the detailed simulation results for the "variable"
# benefit scenario and creates a single, combined figure comparing the
# performance of all three models. The figure includes:
#   1. Density plot
#   2. Horizontal box plots for direct comparison of spread and median
#
#########################################################################

# --- 1. SETUP ---
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggokabeito)  # Color-blind safe palette


# --- 2. LOAD AND PREPARE DATA ---
raw_variable <- read.csv("raw_simulation_results_variable.csv")

data_to_plot <- raw_variable %>%
  mutate(
    Model = case_when(
      Model == "optimal"   ~ "Non-stationary MDP",
      Model == "greedy"    ~ "Greedy Heuristic",
      Model == "fl_myopic" ~ "Myopic MDP"
    ),
    Model = factor(Model, levels = c("Non-stationary MDP", "Myopic MDP", "Greedy Heuristic"))
  )


# --- 3. CREATE THE VISUALIZATIONS ---

# --- Graphic A: Overlapping Density Plot (Smoothed Histogram) ---
plot_density <- ggplot(data_to_plot, aes(x = TerminalReward, fill = Model)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = palette_okabe_ito(order = c(2, 3, 6))) +
  labs(
    title = "Overall Distribution of Outcomes",
    x = "Final Cumulative Reward",
    y = "Density"
  ) +
  theme_minimal(base_size = 14, base_family = "serif")


# --- Graphic B: Horizontal Box Plots on a Shared Axis ---
plot_boxplot_horizontal <- ggplot(data_to_plot, aes(x = Model, y = TerminalReward, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  scale_fill_manual(values = palette_okabe_ito(order = c(2, 3, 6))) +
  labs(
    title = "Summary of Reward Distributions",
    x = "",
    y = "Final Cumulative Reward"
  ) +
  theme_minimal(base_size = 14, base_family = "serif")


# --- 4. COMBINE INTO A SINGLE FIGURE ---
final_figure <- (plot_density / plot_boxplot_horizontal) +
  plot_layout(guides = 'collect')

print(final_figure)




####### RE-DO USING TERMINAL REWARDS WITHOUT OUTLIERS ########
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(ggokabeito)  # Color-blind safe palette

# --- 1. LOAD AND PREPARE DATA ---
raw_variable <- read.csv("raw_simulation_results_variable_no_outliers.csv")

data_to_plot <- raw_variable %>%
  mutate(
    Model = case_when(
      Model == "optimal"   ~ "Non-stationary MDP",
      Model == "greedy"    ~ "Greedy Heuristic",
      Model == "fl_myopic" ~ "Myopic MDP"
    ),
    Model = factor(Model, levels = c("Non-stationary MDP", "Myopic MDP", "Greedy Heuristic"))
  )

# --- 2. SIGNIFICANCE LABELS ---
p_myopic <- .0001
p_greedy <- .0001

sig_label <- function(p) {
  if (is.na(p)) {
    "ns"
  } else if (p < 0.001) {
    "***"
  } else if (p < 0.01) {
    "**"
  } else if (p < 0.05) {
    "*"
  } else {
    "ns"
  }
}

# --- 3. COMPUTE Y-POSITIONS FROM THE PLOTTED DATA ---
y_max <- max(data_to_plot$TerminalReward, na.rm = TRUE)
y_min <- min(data_to_plot$TerminalReward, na.rm = TRUE)
y_range <- y_max - y_min

anno_df <- data.frame(
  xmin = c("Non-stationary MDP", "Non-stationary MDP"),
  xmax = c("Myopic MDP", "Greedy Heuristic"),
  y.position = c(y_max + 0.06 * y_range, y_max + 0.12 * y_range),
  label = c(sig_label(p_myopic), sig_label(p_greedy))
)

# --- 4. CREATE THE VISUALIZATIONS ---

plot_density <- ggplot(data_to_plot, aes(x = TerminalReward, fill = Model)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = palette_okabe_ito(order = c(2, 3, 6))) +
  labs(
    title = "Overall Distribution of Outcomes",
    x = "Final Cumulative Reward",
    y = "Density"
  ) +
  theme_minimal(base_size = 14, base_family = "serif")

plot_boxplot_horizontal <- ggplot(data_to_plot, aes(x = Model, y = TerminalReward, fill = Model)) +
  geom_boxplot(alpha = 0.7, width = 0.65) +
  scale_fill_manual(values = palette_okabe_ito(order = c(2, 3, 6))) +
  geom_bracket(
    data = anno_df,
    aes(
      xmin = xmin,
      xmax = xmax,
      y.position = y.position,
      label = label
    ),
    inherit.aes = FALSE,
    tip.length = 0.02,
    size = 0.6,
    coord.flip = TRUE
  ) +
  labs(
    title = "Summary of Reward Distributions",
    x = "",
    y = "Final Cumulative Reward"
  ) +
  coord_flip(clip = "off") +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.margin = margin(10, 35, 10, 10)
  )

# --- 5. COMBINE INTO A SINGLE FIGURE ---
final_figure <- (plot_density / plot_boxplot_horizontal) +
  plot_layout(guides = "collect")

print(final_figure)