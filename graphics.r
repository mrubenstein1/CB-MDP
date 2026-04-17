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

# --- 2. LOAD AND PREPARE DATA ---
# Load the raw data from the variable scenario ONLY
raw_variable <- read.csv("raw_simulation_results_variable.csv")

# Prepare the data for plotting
data_to_plot <- raw_variable %>%
  mutate(
    # Create cleaner, more descriptive names for the plot legend
    Model = case_when(
      Model == "optimal"   ~ "Non-stationary MDP",
      Model == "greedy"    ~ "Greedy Heuristic",
      Model == "fl_myopic" ~ "Myopic MDP"
    ),
    # Convert 'Model' to a factor to control the order in the plots.
    Model = factor(Model, levels = c("Non-stationary MDP", "Myopic MDP", "Greedy Heuristic"))
  )


# --- 3. CREATE THE VISUALIZATIONS ---

# --- Graphic A: Overlapping Density Plot (Smoothed Histogram) ---
plot_density <- ggplot(data_to_plot, aes(x = TerminalReward, fill = Model)) +
  geom_density(alpha = 0.8) +
  
  # UPDATE: Added 'begin' and 'end' to lighten the colors
  # 'begin = 0.3' skips the darkest blues, ensuring better visibility
  scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.95) +
  
  labs(
    title = "Overall Distribution of Outcomes",
    x = "Final Cumulative Reward",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)


# --- Graphic B: Horizontal Box Plots on a Shared Axis ---
plot_boxplot_horizontal <- ggplot(data_to_plot, aes(x = Model, y = TerminalReward, fill = Model)) +
  
  # UPDATE: Added alpha = 0.7 to make the fill semi-transparent
  # This makes the black median line stand out clearly against the color
  geom_boxplot(alpha = 0.7) +
  
  coord_flip() +
  
  # UPDATE: Matches the density plot colors exactly
  scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.95) +
  
  labs(
    title = "Summary of Reward Distributions",
    x = "", 
    y = "Final Cumulative Reward"
  ) +
  theme_minimal(base_size = 14)


# --- 4. COMBINE INTO A SINGLE FIGURE ---
final_figure <- (plot_density / plot_boxplot_horizontal) +
  plot_layout(guides = 'collect')

# Print the final, combined figure
print(final_figure)




####### RE-DO USING TERMINAL REWARDS WITHOUT OUTLIERS ########
library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)
library(ggpubr)

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
  scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.95) +
  labs(
    title = "Overall Distribution of Outcomes",
    x = "Final Cumulative Reward",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

plot_boxplot_horizontal <- ggplot(data_to_plot, aes(x = Model, y = TerminalReward, fill = Model)) +
  geom_boxplot(alpha = 0.7, width = 0.65) +
  scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.95) +
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
  theme_minimal(base_size = 14) +
  theme(
    plot.margin = margin(10, 35, 10, 10)
  )

# --- 5. COMBINE INTO A SINGLE FIGURE ---
final_figure <- (plot_density / plot_boxplot_horizontal) +
  plot_layout(guides = "collect")

print(final_figure)
