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
  scale_fill_viridis_d(option = "cividis") +
  labs(
    title = "Overall Distribution of Outcomes",
    x = "Final Cumulative Reward",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)


# --- Graphic B: Horizontal Box Plots on a Shared Axis ---
plot_boxplot_horizontal <- ggplot(data_to_plot, aes(x = Model, y = TerminalReward, fill = Model)) +
  geom_boxplot() +
  coord_flip() +
  
  # Use the same professional color palette to ensure consistency
  scale_fill_viridis_d(option = "cividis") +
  
  labs(
    title = "Summary of Reward Distributions",
    x = "", # The vertical axis is categorical, so its label is not needed
    y = "Final Cumulative Reward"
  ) +
  theme_minimal(base_size = 14)


# --- 4. COMBINE INTO A SINGLE FIGURE ---
final_figure <- (plot_density / plot_boxplot_horizontal) +
  plot_layout(guides = 'collect')

# Print the final, combined figure
print(final_figure)