#########################################################################
# ITERATIVE EXPERIMENT: Non-Stationarity vs. Model Performance
# -- UPDATED: PARALLELIZED + OPTIMIZED --
#########################################################################

# --- 1. SETUP ---

rm(list=ls())

# Load necessary libraries
library(MDPtoolbox)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(doParallel)
library(foreach)
library(rstatix)
library(ggokabeito)

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

volatility_levels <- c(0, 0.5, 1, 2, 4, 8)
n_sims <- 1000

# Set seed once, outside the loop
set.seed(42)

# Pre-allocate results as a list to avoid rbind copies
experiment_results_list <- vector("list", length(volatility_levels))
dataR_storage <- list()


# --- 3. SET UP PARALLEL BACKEND ---

n_cores <- parallel::detectCores() - 1
cat(paste("Running on", n_cores, "cores.\n"))
cl <- makeCluster(n_cores)
registerDoParallel(cl)


# --- 4. RUN THE ITERATIVE EXPERIMENT (PARALLELIZED) ---

experiment_results_list <- foreach(
  v = volatility_levels,
  .combine  = rbind,
  .packages = c("dplyr", "rstatix", "MDPtoolbox")
) %dopar% {
  
  # --- 4a. Generate Dynamic Problem Parameters based on Volatility ---
  init_site <- 6
  time_step <- 5
  
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
  M     <- matrix(nrow = init_site, ncol = time_step, data = dataR)
  term  <- matrix(M[, time_step], nrow = init_site, ncol = 1)
  Pj    <- array(rep(0.1, init_site * time_step), c(init_site, time_step))
  
  # --- 4b. Build and Solve MDPs ---
  PR <- mdp_example_PPR_non_stationary(M, term, Pj)
  P  <- PR$P; R <- PR$R; h <- PR$RT
  
  policy_optimal   <- mdp_finite_horizon_nonStationary(P, R, 1, time_step, h)$policy
  policy_fl_myopic <- mdp_myopic_forward_look_policy(P, R, 1, time_step, h)$policy
  results_greedy   <- mdp_greedy_policy_nonStationary(M, P, R, 1, time_step, h, init_site)
  policy_greedy    <- results_greedy$policy
  
  # --- 4c. Run Simulations (vectorized with vapply) ---
  r_opt <- vapply(1:n_sims, function(i)
    explore_solution_PPR(numeric(init_site), policy_optimal,  M, P, R, h)$Treward[time_step + 1],
    numeric(1))
  
  r_myo <- vapply(1:n_sims, function(i)
    explore_solution_PPR(numeric(init_site), policy_fl_myopic, M, P, R, h)$Treward[time_step + 1],
    numeric(1))
  
  r_grd <- vapply(1:n_sims, function(i)
    explore_solution_PPR(numeric(init_site), policy_greedy,   M, P, R, h)$Treward[time_step + 1],
    numeric(1))
  
  # --- 4d. Statistics ---
  raw_results <- bind_rows(
    data.frame(TerminalReward = r_opt, Model = "optimal"),
    data.frame(TerminalReward = r_myo, Model = "fl_myopic"),
    data.frame(TerminalReward = r_grd, Model = "greedy")
  )
  
  welch_anova <- oneway.test(TerminalReward ~ Model, data = raw_results, var.equal = FALSE)
  
  posthoc_gh <- raw_results %>%
    games_howell_test(TerminalReward ~ Model, detailed = TRUE)
  
  p_opt_myo <- posthoc_gh %>%
    filter((group1 == "optimal"  & group2 == "fl_myopic") |
             (group1 == "fl_myopic" & group2 == "optimal")) %>%
    pull(p.adj)
  
  p_opt_grd <- posthoc_gh %>%
    filter((group1 == "optimal" & group2 == "greedy") |
             (group1 == "greedy"  & group2 == "optimal")) %>%
    pull(p.adj)
  
  # Cache t.test calls to avoid redundant computation
  tt_myo <- t.test(r_opt, r_myo)
  tt_grd <- t.test(r_opt, r_grd)
  
  # --- 4e. Return results for this volatility level ---
  data.frame(
    Volatility          = v,
    MeanReward_Optimal  = mean(r_opt),
    MeanReward_FLMyopic = mean(r_myo),
    MeanReward_Greedy   = mean(r_grd),
    Welch_F             = welch_anova$statistic["F"],
    Welch_P             = welch_anova$p.value,
    P_Val_vs_Myopic     = p_opt_myo,
    P_Val_vs_Greedy     = p_opt_grd,
    CI_Myo_Lower        = tt_myo$conf.int[1],
    CI_Myo_Upper        = tt_myo$conf.int[2],
    CI_Grd_Lower        = tt_grd$conf.int[1],
    CI_Grd_Upper        = tt_grd$conf.int[2]
  )
}

# Stop the cluster once the experiment is complete
stopCluster(cl)

# Bind list into final dataframe (single allocation)
experiment_results <- bind_rows(experiment_results_list)

# --- 4f. Compute derived columns after parallelized loop ---
experiment_results <- experiment_results %>%
  mutate(
    Diff_Means_Myopic = MeanReward_Optimal - MeanReward_FLMyopic,
    Diff_Means_Greedy = MeanReward_Optimal - MeanReward_Greedy,
    Pct_Imp_vs_Myopic = (Diff_Means_Myopic / abs(MeanReward_FLMyopic)) * 100,
    Pct_Imp_vs_Greedy = (Diff_Means_Greedy / abs(MeanReward_Greedy))   * 100
  )


# --- 5. EXPORT TABLES ---

format_p_val <- function(p) {
  ifelse(p < 0.001, "< 0.001", as.character(round(p, 4)))
}

table_opt_vs_myopic <- experiment_results %>%
  mutate(CI_vs_Myopic = paste0("[", round(CI_Myo_Lower, 3), ", ", round(CI_Myo_Upper, 3), "]")) %>%
  select(Volatility,
         MeanReward_Optimal, MeanReward_FLMyopic,
         Welch_F, Welch_P,
         P_Val_vs_Myopic,
         CI_vs_Myopic,
         Diff_Means_Myopic, Pct_Imp_vs_Myopic)

write.csv(table_opt_vs_myopic, "Table5a_opt_myo.csv", row.names = FALSE)

table_opt_vs_greedy <- experiment_results %>%
  mutate(CI_vs_Greedy = paste0("[", round(CI_Grd_Lower, 3), ", ", round(CI_Grd_Upper, 3), "]")) %>%
  select(Volatility,
         MeanReward_Optimal, MeanReward_Greedy,
         Welch_F, Welch_P,
         P_Val_vs_Greedy,
         CI_vs_Greedy,
         Diff_Means_Greedy, Pct_Imp_vs_Greedy)

write.csv(table_opt_vs_greedy, "Table5b_opt_greedy.csv", row.names = FALSE)


# --- 6. VISUALIZE THE RESULTS ---

# 6a. Prepare Data for Plot 1 (Percent Improvement)
pct_long <- experiment_results %>%
  select(Volatility, Pct_Imp_vs_Myopic, Pct_Imp_vs_Greedy) %>%
  pivot_longer(
    cols      = starts_with("Pct"),
    names_to  = "Comparison",
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
    title    = "Optimal MDP Performance Advantage",
    subtitle = "Percent improvement over sub-optimal strategies",
    x        = "Volatility Factor",
    y        = "Performance Improvement (%)"
  ) +
  scale_color_manual(values = c(
    "vs. Myopic" = palette_okabe_ito(order = 3),
    "vs. Greedy" = palette_okabe_ito(order = 6)
  )) +
  theme_minimal(base_size = 14, base_family = "serif") +
  expand_limits(y = 0)

# 6b. Prepare Data for Plot 2 (Absolute Performance)
results_long <- experiment_results %>%
  select(Volatility, MeanReward_Optimal, MeanReward_FLMyopic, MeanReward_Greedy) %>%
  pivot_longer(
    cols         = starts_with("MeanReward"),
    names_to     = "Model",
    values_to    = "MeanReward",
    names_prefix = "MeanReward_"
  )

# Plot 2: Absolute Performance of All Three Models
p_performance <- ggplot(results_long, aes(x = Volatility, y = MeanReward, color = Model, group = Model)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4, aes(shape = Model)) +
  labs(
    title = "Comparative Model Performance (Raw Value)",
    x     = "Volatility Factor",
    y     = "Mean Cumulative Reward"
  ) +
  scale_color_manual(values = c(
    "Optimal"  = palette_okabe_ito(order = 2),
    "FLMyopic" = palette_okabe_ito(order = 3),
    "Greedy"   = palette_okabe_ito(order = 6)
  )) +
  theme_minimal(base_size = 14, base_family = "serif")

# Combine and display
final_plot <- p_pct / p_performance
print(final_plot)