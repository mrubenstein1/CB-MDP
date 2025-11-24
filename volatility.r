#########################################################################
# ITERATIVE EXPERIMENT: Non-Stationarity vs. Model Performance
# -- UPDATED: SIGNIFICANCE TESTS & PERCENT IMPROVEMENT --
#########################################################################

# --- 1. SETUP ---

rm(list=ls()) 

# Load necessary libraries
library(MDPtoolbox)
library(dplyr)
library(ggplot2)
library(patchwork) 
library(tidyr) 

# Source all the required model and helper functions
source('mdp_finite_horizon_nonStationary.r')
source('mdp_myopic_forward_look_policy.R')
source('greedy_solver.r') 
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
n_sims <- 1000 

# Initialize a dataframe to store the results from all iterations
experiment_results <- data.frame()

# Initialize a list to store the dataR matrices for inspection later
dataR_storage <- list() 

# --- 3. RUN THE ITERATIVE EXPERIMENT ---

for (v in volatility_levels) {
  
  cat(paste("\n--- Running Experiment for Volatility Level:", v, "---\n"))
  
  # --- 3a. Generate Dynamic Problem Parameters based on Volatility ---
  init_site <- 6
  time_step <- 5
  
  set.seed(42) 
  
  t_seq <- 1:5
  calc_trend <- function(val) { pmax(0, round(val)) }
  
  # P1: THE CRASH 
  p1_start <- sample(18:22, 1)
  p1_decay <- runif(1, 3, 4)
  p1 <- calc_trend((p1_start + v * 5) - (p1_decay + v * 4) * (t_seq - 1))
  
  # P2: THE SPIKE 
  p2_start <- sample(1:3, 1)
  p2_growth <- runif(1, 2, 3)
  p2 <- calc_trend(p2_start + (p2_growth + v * 3) * (t_seq - 1))
  
  # P3: THE FLUCTUATOR 
  p3_base <- sample(7:9, 1)
  p3 <- calc_trend(p3_base + (v * 3 * sin(t_seq)))
  
  # P4: THE SLIGHT DECLINE 
  p4_start <- sample(9:11, 1)
  p4_decline <- runif(1, 0.5, 1.0) 
  p4 <- calc_trend(p4_start - (p4_decline + v * 0.5) * (t_seq - 1))
  
  # P5: THE COSINE WAVE
  p5_base <- sample(5:7, 1)
  p5 <- calc_trend(p5_base + (v * 3 * cos(t_seq)))
  
  # P6: THE OFFSET WAVE
  p6_base <- sample(3:5, 1)
  p6 <- calc_trend(p6_base + (v * 3 * sin(t_seq + 2)))
  
  # Combine into Reward Matrix
  dataR <- rbind(p1, p2, p3, p4, p5, p6)
  dataR_storage[[paste0("Volatility_Level_", v)]] <- dataR
  
  M <- matrix(nrow=init_site, ncol=time_step, data=dataR)
  term <- matrix(M[, time_step], nrow = init_site, ncol = 1)
  Pj <- array(rep(0.1, init_site * time_step), c(init_site, time_step))
  
  # --- 3b. Build and Solve MDPs ---
  PR <- mdp_example_PPR_non_stationary(M, term, Pj)
  P <- PR$P; R <- PR$R; h <- PR$RT
  
  policy_optimal <- mdp_finite_horizon_nonStationary(P, R, 1, time_step, h)$policy
  policy_fl_myopic <- mdp_myopic_forward_look_policy(P, R, 1, time_step, h)$policy
  results_greedy <- mdp_greedy_policy_nonStationary(M, P, R, 1, time_step, h, init_site)
  policy_greedy <- results_greedy$policy
  
  # --- 3c. Run Simulations ---
  sim_runs_optimal <- lapply(1:n_sims, function(i) {
    explore_solution_PPR(numeric(init_site), policy_optimal, M, P, R, h)$Treward
  })
  sim_runs_fl_myopic <- lapply(1:n_sims, function(i) {
    explore_solution_PPR(numeric(init_site), policy_fl_myopic, M, P, R, h)$Treward
  })
  sim_runs_greedy <- lapply(1:n_sims, function(i) {
    explore_solution_PPR(numeric(init_site), policy_greedy, M, P, R, h)$Treward
  })
  
  # --- 3d. Store Results & Calculate Significance ---
  
  # Extract the raw vector of rewards for this volatility level
  r_opt <- do.call(rbind, sim_runs_optimal)[, time_step + 1]
  r_myo <- do.call(rbind, sim_runs_fl_myopic)[, time_step + 1]
  r_grd <- do.call(rbind, sim_runs_greedy)[, time_step + 1]
  
  # Calculate Means
  mean_optimal <- mean(r_opt)
  mean_fl_myopic <- mean(r_myo)
  mean_greedy <- mean(r_grd)
  
  # --- STATISTICAL TESTS (t-test) ---
  # Compare Optimal vs Myopic
  test_myopic <- t.test(r_opt, r_myo)
  
  # Compare Optimal vs Greedy
  test_greedy <- t.test(r_opt, r_grd)
  
  current_results <- data.frame(
    Volatility = v,
    MeanReward_Optimal = mean_optimal,
    MeanReward_FLMyopic = mean_fl_myopic,
    MeanReward_Greedy = mean_greedy,
    # Store raw p-values
    P_Val_vs_Myopic = test_myopic$p.value,
    P_Val_vs_Greedy = test_greedy$p.value
  )
  
  experiment_results <- rbind(experiment_results, current_results)
}

cat("\n--- Experiment Complete ---\n")

# --- 4. POST-PROCESSING: PERCENT IMPROVEMENT & FORMATTING ---

# Calculate Percent Improvement: ((Optimal - Baseline) / Baseline) * 100
experiment_results <- experiment_results %>%
  mutate(
    Pct_Imp_vs_Myopic = ((MeanReward_Optimal - MeanReward_FLMyopic) / MeanReward_FLMyopic) * 100,
    Pct_Imp_vs_Greedy = ((MeanReward_Optimal - MeanReward_Greedy) / MeanReward_Greedy) * 100
  )

# Create a clean display table with formatted significance stars
# Significance codes: *** < 0.001, ** < 0.01, * < 0.05, ns >= 0.05
format_sig <- function(p) {
  ifelse(p < 0.001, "*** (<.001)",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "ns")))
}

display_table <- experiment_results %>%
  mutate(
    Sig_vs_Myopic = format_sig(P_Val_vs_Myopic),
    Sig_vs_Greedy = format_sig(P_Val_vs_Greedy)
  ) %>%
  select(
    Volatility,
    Mean_Opt = MeanReward_Optimal,
    Imp_vs_Myo_Pct = Pct_Imp_vs_Myopic,
    Sig_vs_Myo = Sig_vs_Myopic,
    Imp_vs_Grd_Pct = Pct_Imp_vs_Greedy,
    Sig_vs_Grd = Sig_vs_Greedy
  ) %>%
  mutate(across(where(is.numeric), round, 2)) # Round numbers for readability

cat("\n--- FINAL SUMMARY: PERFORMANCE & SIGNIFICANCE ---\n")
print(display_table)


# --- 5. VISUALIZE THE RESULTS ---

# 5a. Prepare Data for Plot 1 (Percent Improvement)
pct_long <- experiment_results %>%
  select(Volatility, Pct_Imp_vs_Myopic, Pct_Imp_vs_Greedy) %>%
  pivot_longer(
    cols = starts_with("Pct"),
    names_to = "Comparison",
    values_to = "PctImprovement"
  ) %>%
  mutate(Comparison = recode(Comparison, 
                             "Pct_Imp_vs_Myopic" = "vs. Myopic",
                             "Pct_Imp_vs_Greedy" = "vs. Greedy"))

# Plot 1: Percent Performance Improvement
p_pct <- ggplot(pct_long, aes(x = Volatility, y = PctImprovement, color = Comparison)) +
  geom_line(linewidth = 1.5) + 
  geom_point(size = 4) +
  labs(
    title = "Optimal MDP Performance Advantage",
    subtitle = "Percent improvement over sub-optimal strategies",
    x = "Volatility Factor",
    y = "Performance Improvement (%)"
  ) +
  scale_color_manual(values = c("vs. Myopic" = "#D55E00", "vs. Greedy" = "#009E73")) +
  theme_minimal(base_size = 14) +
  expand_limits(y = 0)


# 5b. Prepare Data for Plot 2 (Absolute Performance)
results_long <- experiment_results %>%
  select(Volatility, MeanReward_Optimal, MeanReward_FLMyopic, MeanReward_Greedy) %>%
  pivot_longer(
    cols = starts_with("MeanReward"),
    names_to = "Model",
    values_to = "MeanReward",
    names_prefix = "MeanReward_"
  )

# Plot 2: Absolute Performance of All Three Models
p_performance <- ggplot(results_long, aes(x = Volatility, y = MeanReward, color = Model, group = Model)) +
  geom_line(linewidth = 1.5) + 
  geom_point(size = 4, aes(shape = Model)) +
  labs(
    title = "Comparative Model Performance (Raw Value)",
    x = "Volatility Factor",
    y = "Mean Terminal Reward"
  ) +
  scale_color_manual(values = c(
    "Optimal" = "#0072B2",   # Blue
    "FLMyopic" = "#D55E00",  # Vermillion
    "Greedy" = "#009E73"     # Bluish Green
  )) +
  theme_minimal(base_size = 14)

# Combine and display the plots
final_plot <- p_pct / p_performance

print(final_plot)

write.csv(experiment_results,"volatility results.csv")
