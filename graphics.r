#########################################################################
# SCRIPT: Create Publication-Ready Figure for the Variable Scenario
#
# DESCRIPTION:
# This script loads the detailed simulation results for the "variable"
# benefit scenario and creates a single, combined figure comparing the
# performance of all three models. The figure includes:
#   1. A smoothed histogram (density plot) for distribution shape.
#   2. Horizontal box plots for direct comparison of spread and median.
# The color palette is selected for clarity and accessibility, suitable
# for journals like Conservation Biology.
#
#########################################################################

# --- 1. SETUP ---
# Load necessary libraries. You may need to install patchwork: install.packages("patchwork")
library(ggplot2)
library(dplyr)
library(patchwork) # For combining plots into a single figure

# --- 2. LOAD AND PREPARE DATA ---
# Load the raw data from the variable scenario ONLY
raw_variable <- read.csv("raw_simulation_results_variable.csv")

# Prepare the data for plotting
data_to_plot <- raw_variable %>%
  mutate(
    # Create cleaner, more descriptive names for the plot legend
    Model = case_when(
      Model == "optimal"   ~ "Optimal MDP",
      Model == "greedy"    ~ "Greedy Solver",
      Model == "fl_myopic" ~ "Forward-Looking Myopic"
    ),
    # Convert 'Model' to a factor to control the order in the plots.
    Model = factor(Model, levels = c("Optimal MDP", "Forward-Looking Myopic", "Greedy Solver"))
  )


# --- 3. CREATE THE VISUALIZATIONS ---

# --- Graphic A: Overlapping Density Plot (Smoothed Histogram) ---
plot_density <- ggplot(data_to_plot, aes(x = TerminalReward, fill = Model)) +
  geom_density(alpha = 0.8) +
  # Use a colorblind-safe and professional palette. The 'viridis' family is an excellent choice.
  # The "_d" stands for "discrete", which is correct for this aesthetic.
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
# The 'patchwork' library allows you to combine plots using intuitive operators.
# - '/' stacks plots vertically.
# - plot_layout(guides = 'collect') creates a single, shared legend for the entire figure.
# - plot_annotation(tag_levels = 'A') automatically labels the panels.

final_figure <- (plot_density / plot_boxplot_horizontal) +
  plot_layout(guides = 'collect')

# Print the final, combined figure
print(final_figure)