########### VISUALIZE COMPARISON RESULTS (Base R) #########

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) # mar sets margins

# --- Plot 1: Histograms ---
hist(terminal_rewards_optimal, 
     main = "Optimal Policy Final Rewards", 
     xlab = "Total Reward", 
     col = "skyblue")
hist(terminal_rewards_fl_myopic, 
     main = "FL Myopic Policy Final Rewards", 
     xlab = "Total Reward", 
     col = "salmon")

# --- Plot 2: Box Plot ---
boxplot(terminal_rewards_optimal, terminal_rewards_fl_myopic,
        names = c("Optimal", "FL Myopic"),
        main = "Final Reward Comparison",
        ylab = "Total Reward",
        col = c("skyblue", "salmon"))

# --- Plot 3: Mean Cumulative Reward Over Time ---
sim_matrix_optimal <- do.call(rbind, sim_runs_optimal)
sim_matrix_fl_myopic <- do.call(rbind, sim_runs_fl_myopic)
mean_optimal <- colMeans(sim_matrix_optimal)
mean_fl_myopic <- colMeans(sim_matrix_fl_myopic)

plot(mean_optimal, 
     type = 'o', # 'o' for overplotted points and lines
     col = "blue", 
     ylim = range(c(mean_optimal, mean_fl_myopic)), # Ensure y-axis fits both lines
     xlab = "Time Step", 
     ylab = "Mean Cumulative Reward",
     main = "Average Performance Over Time",
     lwd=2, pch=16)
lines(mean_fl_myopic, type = 'o', col = "red", lwd=2, pch=16)
legend("topleft", 
       legend = c("Optimal", "FL Myopic"), 
       col = c("blue", "red"), 
       lty = 1, lwd = 2, bty = "n")

# Reset plotting area to default
par(mfrow = c(1, 1))