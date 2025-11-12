#########################################################################
# ITERATIVE EXPERIMENT: Non-Stationarity vs. Model Performance
# -- UPDATED --
#########################################################################

# --- 1. SETUP ---

rm(list=ls()) 

# Load necessary libraries
library(MDPtoolbox)
library(dplyr)
library(ggplot2)
library(patchwork) 
library(tidyr) # <<< FIX 1: Added library(tidyr) to load pivot_longer()

# Source all the required model and helper functions
source('mdp_finite_horizon_nonStationary.r')
source('mdp_myopic_forward_look_policy.R')
source('mdp_example_PPR_non_stationary.r')
source('explore_solution_PPR.r')
source('dec2binvec.r')
source('getSite.r')
source('binvec2dec.r')
source('getState.r')


# --- 2. EXPERIMENT PARAMETERS ---

# Define the levels of non-stationarity we want to test.
volatility_levels <- c(0, 0.5, 1, 2, 4, 8) 

# Number of simulations to run for each volatility level.
n_sims <- 200 

# Initialize a dataframe to store the results from all iterations
experiment_results <- data.frame()

# --- 3. RUN THE ITERATIVE EXPERIMENT ---

for (v in volatility_levels) {
  
  cat(paste("\n--- Running Experiment for Volatility Level:", v, "---\n"))
  
  # --- 3a. Generate Dynamic Problem Parameters based on Volatility ---
  init_site <- 6
  time_step <- 5
  set.seed(42) # Ensure reproducibility
  
  p1 <- c(15 + 5*v, 10, 5, 2, 1) 
  p2 <- c(1, 2, 5 + 3*v, 10 + 6*v, 15 + 9*v) 
  p3 <- rep(8, 5)
  p4 <- c(10, 9, 8, 7, 6)
  p5 <- c(5, 6, 7, 6, 5)
  p6 <- rep(3, 5)
  
  dataR <- rbind(p1, p2, p3, p4, p5, p6)
  M <- matrix(nrow=init_site, ncol=time_step, data=dataR)
  
  term <- matrix(M[, time_step], nrow = init_site, ncol = 1)
  Pj <- array(rep(0.1, init_site * time_step), c(init_site, time_step))
  
  # --- 3b. Build and Solve MDPs ---
  PR <- mdp_example_PPR_non_stationary(M, term, Pj)
  P <- PR$P; R <- PR$R; h <- PR$RT
  
  policy_optimal <- mdp_finite_horizon_nonStationary(P, R, 1, time_step, h)$policy
  policy_fl_myopic <- mdp_myopic_forward_look_policy(P, R, 1, time_step, h)$policy
  
  # --- 3c. Run Simulations ---
  sim_runs_optimal <- lapply(1:n_sims, function(i) {
    explore_solution_PPR(numeric(init_site), policy_optimal, M, P, R, h)$Treward
  })
  sim_runs_fl_myopic <- lapply(1:n_sims, function(i) {
    explore_solution_PPR(numeric(init_site), policy_fl_myopic, M, P, R, h)$Treward
  })
  
  # --- 3d. Calculate and Store Results ---
  terminal_rewards_optimal <- do.call(rbind, sim_runs_optimal)[, time_step + 1]
  terminal_rewards_fl_myopic <- do.call(rbind, sim_runs_fl_myopic)[, time_step + 1]
  
  mean_optimal <- mean(terminal_rewards_optimal)
  mean_fl_myopic <- mean(terminal_rewards_fl_myopic)
  
  current_results <- data.frame(
    Volatility = v,
    MeanReward_Optimal = mean_optimal,
    MeanReward_FLMyopic = mean_fl_myopic,
    PerformanceGap = mean_optimal - mean_fl_myopic
  )
  
  experiment_results <- rbind(experiment_results, current_results)
}

cat("\n--- Experiment Complete ---\n")
print(experiment_results)


# --- 4. VISUALIZE THE RESULTS ---

# Plot 1: Performance Gap vs. Volatility
p_gap <- ggplot(experiment_results, aes(x = Volatility, y = PerformanceGap)) +
  geom_line(color = "#0072B2", linewidth = 1.5) + # <<< FIX 2: Changed size to linewidth
  geom_point(color = "#0072B2", size = 4) +
  labs(
    title = "The 'Value of Information' Increases with Non-Stationarity",
    subtitle = "The performance advantage of the optimal MDP grows in more volatile environments",
    x = "Volatility Factor (Degree of Non-Stationarity)",
    y = "Performance Gap (Optimal - Myopic)"
  ) +
  theme_minimal(base_size = 14) +
  expand_limits(y = 0)

# Prepare data for the second plot by converting it to a "long" format
results_long <- experiment_results %>%
  select(Volatility, MeanReward_Optimal, MeanReward_FLMyopic) %>%
  pivot_longer(
    cols = -Volatility,
    names_to = "Model",
    values_to = "MeanReward",
    names_prefix = "MeanReward_"
  )

# Plot 2: Absolute Performance of Both Models
p_performance <- ggplot(results_long, aes(x = Volatility, y = MeanReward, color = Model, group = Model)) +
  geom_line(linewidth = 1.5) + # <<< FIX 2: Changed size to linewidth
  geom_point(size = 4, aes(shape = Model)) +
  labs(
    title = "Model Performance Under Increasing Volatility",
    subtitle = "The Optimal MDP adapts while the Myopic model makes increasingly poor choices",
    x = "Volatility Factor (Degree of Non-Stationarity)",
    y = "Mean Terminal Reward"
  ) +
  scale_color_manual(values = c("Optimal" = "#0072B2", "FLMyopic" = "#D55E00")) +
  theme_minimal(base_size = 14)

# Combine and display the plots
final_plot <- p_gap / p_performance

print(final_plot)