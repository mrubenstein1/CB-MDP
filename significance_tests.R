#### SIGNIFICANCE TESTS FOR DISTRIBUTION OF TERMINAL REWARDS#####

# This script performs statistical tests to assess whether the terminal rewards, 
  # as calculated by the three different models (optimal, myopic, greedy), 
  # are significantly different from one another. 

# It calls from the script "PPR_nonStationary_example.R", which runs the simulations
  # of the three models and collects the terminal rewards in the following objects:
  # terminal_rewards_optimal 
  # terminal_rewards_greedy 
  # terminal_rewards_fl_myopic

# Remember that the benefit scenario ("constant" vs "variable") is set in the PPR_nonstationary_example script,
  #and re-running it will change the analysis from constant to variable


##### TWO SAMPLE T-TEST ############
# Two-sample t-test

# Greedy vs. non-stationary MDP
ttest_vs_greedy <- t.test(terminal_rewards_optimal, terminal_rewards_greedy)

# Myopic MDP vs Greedy
ttest_vs_fl_myopic <- t.test(terminal_rewards_optimal, terminal_rewards_fl_myopic)

#combine all raw results into a single dataframe.
raw_results_table <- bind_rows(
  data.frame(TerminalReward = terminal_rewards_optimal, Model = "optimal"),
  data.frame(TerminalReward = terminal_rewards_greedy,  Model = "greedy"),
  data.frame(TerminalReward = terminal_rewards_fl_myopic, Model = "fl_myopic")
)

# Save into csv w/ dynamic file name
raw_output_filename <- paste0("raw_simulation_results_", benefit_scenario, ".csv")

# Save the raw data to a new CSV file
write.csv(raw_results_table, raw_output_filename, row.names = FALSE)
cat("\nRaw simulation data saved to:", raw_output_filename, "\n")

summary_stats <- raw_results_table %>%
  group_by(Model) %>%
  summarise(
    mean_r = mean(TerminalReward),
    sd_r = sd(TerminalReward)
  )

# Mean of MDP nonstationary (baseline)
mean_optimal <- summary_stats$mean_r[summary_stats$Model == "optimal"]

# Calculate difference in mean between MDP optimal/greedy/FL Myopic
results_sum_enhanced <- summary_stats %>%
  mutate(
    # Add a column for the difference from the optimal model's mean reward
    DifferenceFromOptimal = round(mean_optimal - mean_r, 2),
    
    # Add a column for the p-value from the t-test
    PValue = case_when(
      Model == "optimal"   ~ NA_real_, # No p-value for comparing to itself
      Model == "greedy"    ~ ttest_vs_greedy$p.value,
      Model == "fl_myopic" ~ ttest_vs_fl_myopic$p.value
    ),
    
    # Format the p-value for clean presentation (e.g., showing "< 0.001")
    Significance = case_when(
      is.na(PValue) ~ "--",
      PValue < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(PValue, 3))
    ),
    
    # Add the scenario information
    data_scenario = benefit_scenario
  ) %>%
  # Select and reorder columns for the final output
  select(
    Model,
    MeanReward = mean_r,
    StDev = sd_r,
    DifferenceFromOptimal,
    Significance,
    Scenario = data_scenario
  )

# Round the numeric columns for a cleaner final table
results_sum_enhanced <- results_sum_enhanced %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Define the dynamic name for the summary object and file
dynamic_name <- paste0("results_sum_enhanced_", benefit_scenario)

# Assign to a dynamically named object
assign(dynamic_name, results_sum_enhanced)

# Save the enhanced summary to a CSV file
write.csv(results_sum_enhanced, paste0(dynamic_name, ".csv"), row.names = FALSE)
cat("Enhanced summary statistics saved to:", paste0(dynamic_name, ".csv"), "\n")

# Print the contents of the newly created enhanced summary object
cat("\n--- Final Enhanced Summary Table (`", dynamic_name, "`) ---\n", sep="")
print(get(dynamic_name))
cat("----------------------------------------------------------\n")


######### T- TEST WITHOUT OUTLIERS ###########
# --- 1. PROCESS SIMULATION RESULTS ---
# Extract 1000 terminal rewards for each model
terminal_rewards_optimal <- do.call(rbind, sim_runs_optimal)[, ncol(M) + 1]
terminal_rewards_greedy <- do.call(rbind, sim_runs_greedy)[, ncol(M) + 1]
terminal_rewards_fl_myopic <- do.call(rbind, sim_runs_fl_myopic)[, ncol(M) + 1]

# --- 1b. REMOVE OUTLIERS USING 1.5 * IQR RULE ---
remove_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  x[x >= lower & x <= upper]
}

terminal_rewards_optimal_clean <- remove_outliers_iqr(terminal_rewards_optimal)
terminal_rewards_greedy_clean <- remove_outliers_iqr(terminal_rewards_greedy)
terminal_rewards_fl_myopic_clean <- remove_outliers_iqr(terminal_rewards_fl_myopic)

# Count removed outliers
n_outliers_optimal <- length(terminal_rewards_optimal) - length(terminal_rewards_optimal_clean)
n_outliers_greedy <- length(terminal_rewards_greedy) - length(terminal_rewards_greedy_clean)
n_outliers_fl_myopic <- length(terminal_rewards_fl_myopic) - length(terminal_rewards_fl_myopic_clean)

# --- 2. PERFORM STATISTICAL TESTS: WITHOUT OUTLIERS ---
ttest_vs_greedy_clean <- t.test(terminal_rewards_optimal_clean, terminal_rewards_greedy_clean)
ttest_vs_fl_myopic_clean <- t.test(terminal_rewards_optimal_clean, terminal_rewards_fl_myopic_clean)

raw_results_table_clean <- bind_rows(
  data.frame(TerminalReward = terminal_rewards_optimal_clean, Model = "optimal"),
  data.frame(TerminalReward = terminal_rewards_greedy_clean, Model = "greedy"),
  data.frame(TerminalReward = terminal_rewards_fl_myopic_clean, Model = "fl_myopic")
)

raw_output_filename_clean <- paste0("raw_simulation_results_", benefit_scenario, "_no_outliers.csv")
write.csv(raw_results_table_clean, raw_output_filename_clean, row.names = FALSE)
cat("\nRaw simulation data saved to:", raw_output_filename_clean, "\n")

summary_stats_clean <- raw_results_table_clean %>%
  group_by(Model) %>%
  summarise(
    mean_r = mean(TerminalReward),
    sd_r = sd(TerminalReward),
    .groups = "drop"
  )

mean_optimal_clean <- summary_stats_clean$mean_r[summary_stats_clean$Model == "optimal"]

results_sum_enhanced_clean <- summary_stats_clean %>%
  mutate(
    DifferenceFromOptimal = round(mean_optimal_clean - mean_r, 2),
    PValue = case_when(
      Model == "optimal"   ~ NA_real_,
      Model == "greedy"    ~ ttest_vs_greedy_clean$p.value,
      Model == "fl_myopic" ~ ttest_vs_fl_myopic_clean$p.value
    ),
    Significance = case_when(
      is.na(PValue) ~ "--",
      PValue < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(PValue, 3))
    ),
    data_scenario = benefit_scenario
  ) %>%
  select(
    Model,
    MeanReward = mean_r,
    StDev = sd_r,
    DifferenceFromOptimal,
    Significance,
    Scenario = data_scenario
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

dynamic_name_clean <- paste0("results_sum_enhanced_", benefit_scenario, "_no_outliers")
assign(dynamic_name_clean, results_sum_enhanced_clean)
write.csv(results_sum_enhanced_clean, paste0(dynamic_name_clean, ".csv"), row.names = FALSE)

cat("\n--- Final Enhanced Summary Table (no outliers) ---\n")
print(get(dynamic_name_clean))
cat("----------------------------------------------------------\n")

# --- 4. OUTLIER COUNT TABLE ---
outlier_counts <- data.frame(
  Model = c("optimal", "greedy", "fl_myopic"),
  TotalN = c(length(terminal_rewards_optimal),
             length(terminal_rewards_greedy),
             length(terminal_rewards_fl_myopic)),
  RemovedOutliers = c(n_outliers_optimal, n_outliers_greedy, n_outliers_fl_myopic),
  RemainingN = c(length(terminal_rewards_optimal_clean),
                 length(terminal_rewards_greedy_clean),
                 length(terminal_rewards_fl_myopic_clean))
)

write.csv(outlier_counts,
          paste0("outlier_counts_", benefit_scenario, ".csv"),
          row.names = FALSE)

# --- 5. COMPARISON TABLE: WITH VS WITHOUT OUTLIERS ---
comparison_table <- data.frame(
  Model = c("myopic", "greedy", "optimal"),
  Mean_With_Outliers = results_sum_enhanced$MeanReward,
  Mean_No_Outliers = results_sum_enhanced_clean$MeanReward,
  SD_With_Outliers = results_sum_enhanced$StDev,
  SD_No_Outliers = results_sum_enhanced_clean$StDev,
  DiffFromOptimal_With_Outliers = results_sum_enhanced$DifferenceFromOptimal,
  DiffFromOptimal_No_Outliers = results_sum_enhanced_clean$DifferenceFromOptimal,
  PValue_With_Outliers = results_sum_enhanced$Significance,
  PValue_No_Outliers = results_sum_enhanced_clean$Significance
)

write.csv(comparison_table,
          paste0("comparison_with_vs_without_outliers_", benefit_scenario, ".csv"),
          row.names = FALSE)

cat("\n--- Outlier Counts ---\n")
print(outlier_counts)

cat("\n--- Comparison Table: With vs Without Outliers ---\n")
print(comparison_table)
cat("----------------------------------------------------------\n")

##### ANOVA #########
## Assess significance of difference using ANOVA
# First assess whether the variance is equal and terminal rewards are normally distributed
# NOTE: this analysis uses only the raw results with outliers removed ("raw_results_table_clean")

####1. Residual Plots ####

# Fit one-way ANOVA model
anova_fit <- aov(TerminalReward ~ Model, data = raw_results_table_clean)

# Create a diagnostics dataframe
anova_diag <- data.frame(
  Fitted = fitted(anova_fit),
  Residuals = residuals(anova_fit),
  Model = raw_results_table_clean$Model
)

# Residuals vs fitted
p1 <- ggplot(anova_diag, aes(x = Fitted, y = Residuals, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, method = "loess", color = "black") +
  theme_minimal() +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted values",
    y = "Residuals"
  )

# Scale-location style plot
p2 <- ggplot(anova_diag, aes(x = Fitted, y = sqrt(abs(Residuals)), color = Model)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess", color = "black") +
  theme_minimal() +
  labs(
    title = "Scale-Location Plot",
    x = "Fitted values",
    y = "Sqrt(|residuals|)"
  )

# Residuals by model
p3 <- ggplot(anova_diag, aes(x = Model, y = Residuals, fill = Model)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 0.8) +
  theme_minimal() +
  labs(
    title = "Residuals by Model",
    x = "Model",
    y = "Residuals"
  )

print(p1)
print(p2)
print(p3)

#### 2. Use Welch's ANOVA #####
#the plots above demonstrate non-uniform variance; therefore, I will use Welch's ANOVA to test for differences in means

welch_anova_clean <- welch.test(TerminalReward ~ Model, data = raw_results_table_clean)
print(welch_anova_clean)
summary(welch_anova_clean) #one of these groups is not like the other!

# Games-Howell post hoc tests (appropriate for unequal variances; otherwise we would use tukey)
posthoc_games_howell <- raw_results_table_clean %>%
  games_howell_test(TerminalReward ~ Model, detailed = TRUE)

print(posthoc_games_howell)

write.csv(posthoc_games_howell, "posthoc_games_howell.csv", row.names = FALSE)

# p-value extraction
p_values <- posthoc_games_howell %>%
  filter(.data$group1 == "optimal" | .data$group2 == "optimal")


# Find greedy vs optimal
greedy_row <- p_values %>% 
  filter(group1 == "optimal" & group2 == "greedy" | 
           group1 == "greedy" & group2 == "optimal")
p_greedy_vs_optimal <- greedy_row$p.adj[1]

# Find fl_myopic vs optimal  
fl_myopic_row <- p_values %>% 
  filter(group1 == "optimal" & group2 == "fl_myopic" | 
           group1 == "fl_myopic" & group2 == "optimal")
p_fl_myopic_vs_optimal <- fl_myopic_row$p.adj[1]

# Debug print to verify extraction worked
cat("Extracted p-values:\n")
cat("greedy vs optimal:", p_greedy_vs_optimal, "\n")
cat("fl_myopic vs optimal:", p_fl_myopic_vs_optimal, "\n")

# Build enhanced results table (your exact structure)
summary_stats_clean <- raw_results_table_clean %>%
  group_by(Model) %>%
  summarise(
    mean_r = mean(TerminalReward),
    sd_r = sd(TerminalReward),
    .groups = "drop"
  )

mean_optimal_clean <- summary_stats_clean$mean_r[summary_stats_clean$Model == "optimal"]

results_sum_enhanced_clean <- summary_stats_clean %>%
  mutate(
    DifferenceFromOptimal = round(mean_optimal_clean - mean_r, 2),
    PValue = case_when(
      Model == "optimal" ~ NA_real_,
      Model == "greedy" ~ as.numeric(p_greedy_vs_optimal),
      Model == "fl_myopic" ~ as.numeric(p_fl_myopic_vs_optimal),
      TRUE ~ NA_real_
    ),
    Significance = case_when(
      is.na(PValue) ~ "--",
      PValue < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(PValue, 3))
    ),
    data_scenario = benefit_scenario,
    OmnibusPValue = welch_anova_clean$p.value
  ) %>%
  select(
    Model,
    MeanReward = mean_r,
    StDev = sd_r,
    DifferenceFromOptimal,
    Significance,
    OmnibusPValue,
    Scenario = data_scenario
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Save with your naming convention
dynamic_name_clean <- paste0("WELCHresults_sum_enhanced_", benefit_scenario, "_no_outliers")
assign(dynamic_name_clean, results_sum_enhanced_clean)
write.csv(results_sum_enhanced_clean, paste0(dynamic_name_clean, ".csv"), row.names = FALSE)

cat("\n--- Welch ANOVA Results (no outliers) ---\n")
print(get(dynamic_name_clean))
cat("----------------------------------------------------------\n")
