# ===================================================================================
#
# SCRIPT FOR COMPARING OPTIMAL MDP POLICY vs. FORCED PURCHASE HEURISTIC
#
# ===================================================================================

# -----------------------------------------------------------------------------------
# PART 1: DEFINE THE "FORCED PURCHASE" HEURISTIC SIMULATION FUNCTION
# This function represents an "aggressive acquisition" strategy.
# -----------------------------------------------------------------------------------

#' Simulate a path using a Forced Purchase Greedy Heuristic
#'
#' This function implements an "aggressive acquisition" strategy. At each timestep,
#' the decision-maker MUST purchase one available parcel. It chooses the parcel
#' with the highest immediate reward. It only stops purchasing (and defaults to
#' the "Do Nothing" action) when all parcels have been acquired or converted.
#'
#' @param init_site The initial state of the J sites.
#' @param M, P, R, h Standard MDP model components.
#' @return A list containing the realized rewards and site history.
explore_solution_forced_purchase <- function(init_site, M, P, R, h) {
  
  J <- nrow(M) # number of sites
  H <- ncol(M) # timestep
  S <- 3^J
  
  horizon <- H + 1
  action <- numeric(horizon)
  Tsites <- matrix(0, nrow = J, ncol = horizon)
  Treward <- numeric(horizon)
  
  current_site <- init_site
  
  for (i in 1:(horizon - 1)) {
    state <- getState(current_site) + 1
    Tsites[, i] <- current_site
    
    # --- FORCED PURCHASE LOGIC STARTS HERE ---
    
    # 1. Identify which parcels are available for purchase (state code 0)
    available_parcels <- which(current_site == 0)
    
    # 2. Check if there are any parcels left to purchase.
    if (length(available_parcels) > 0) {
      # --- MANDATORY PURCHASE LOGIC ---
      # If parcels are available, we MUST purchase one.
      
      # Get the immediate rewards for the current state and time
      immediate_rewards <- R[state, , i]
      
      # Filter to see the rewards for only the available purchase actions
      rewards_for_available_purchases <- immediate_rewards[available_parcels]
      
      # Find the best action among the available options.
      # This implicitly selects only one parcel, even in case of a tie.
      best_purchase_action <- available_parcels[which.max(rewards_for_available_purchases)]
      
      action[i] <- best_purchase_action
      
    } else {
      # --- NO PARCELS LEFT ---
      # If no parcels are available, we default to "Do Nothing".
      action[i] <- J + 1 # The "Do Nothing" action
    }
    
    # --- DECISION LOGIC ENDS HERE ---
    
    if (i != 1) {
      Treward[i] <- R[state, action[i], i] + Treward[i - 1]
    } else {
      Treward[i] <- R[state, action[i], i]
    }
    
    p_state_new <- runif(1)
    p <- 0
    state_new <- 0
    while(p < p_state_new & state_new < S) {
      state_new <- state_new + 1
      p <- p + P[state, state_new, action[i], i]
    }
    
    current_site <- getSite(state_new - 1, J)
  }
  # terminal state
  state <- getState(current_site) + 1
  Tsites[, horizon] <- current_site
  Treward[horizon] <- h[state] + Treward[horizon - 1]
  
  # Plotting the results
  par(mfrow=c(2,1), mar = c(4, 4, 2, 2))
  breaks <- 0:3
  colors <- c("white", "grey", "black")
  Tsites_indices <- Tsites+1
  image(t(Tsites_indices), axes = FALSE,xlab="Time horizon", ylab="Sites", breaks = breaks, col=colors)
  box()
  title(main="Site Status (white=Available, grey=Purchased, black=Converted)", font.main=1)
  plot(1:horizon, Treward,type="s", xlab="Time horizon", ylab="Sum of benefits")
  
  return(list("Treward"=Treward, "Tsites"=Tsites))
}


# ===================================================================================
# PART 2: MAIN WORKFLOW FOR COMPARISON
# This section runs both models and compares their final results.
# ASSUMPTION: You have already run the setup part of 'PPR_nonStationary_example.r'
# to generate the problem variables: J, H, S, A, M, P, R, h, discount_factor
# ===================================================================================

# --- Source all necessary helper functions from the repository ---
source("getState.r")
source("getSite.r")
source("mdp_value_iteration.r")
source("explore_solution_PPR.r")


# --- Scenario 1: Solve for the "Smart" Forward-Looking MDP Policy ---
cat("Solving for the optimal 'Smart' MDP policy...\n")
solution_smart <- mdp_value_iteration(S, A, H, P, R, h, discount_factor)
policy_smart <- solution_smart$policy


# --- Define a common starting point for the simulations ---
init_site <- rep(0, J) 


# --- Run Simulation 1: The "Smart" Policy ---
cat("\nRunning simulation for SMART forward-looking policy...\n")
results_smart <- explore_solution_PPR(init_site, policy_smart, M, P, R, h)
mtext("Strategy: SMART (Forward-Looking MDP)", outer = TRUE, cex = 1.5, line = -2)
final_reward_smart <- tail(results_smart$Treward, 1)

# Pause to allow user to see the first set of plots
readline(prompt="Press [enter] to run the simulation for the greedy heuristic")


# --- Run Simulation 2: The "Forced Purchase" Greedy Heuristic ---
cat("\nRunning simulation for FORCED PURCHASE greedy heuristic...\n")
results_greedy <- explore_solution_forced_purchase(init_site, M, P, R, h)
mtext("Strategy: AGGRESSIVE (Forced Purchase Heuristic)", outer = TRUE, cex = 1.5, line = -2)
final_reward_greedy <- tail(results_greedy$Treward, 1)


# --- Final Comparison of Results ---
cat("\n\n===================================================\n")
cat("          POLICY PERFORMANCE COMPARISON          \n")
cat("===================================================\n\n")
cat("Final Cumulative Benefit (Smart Policy):      ", final_reward_smart, "\n")
cat("Final Cumulative Benefit (Greedy Heuristic):  ", final_reward_greedy, "\n\n")

value_gain <- ((final_reward_smart / final_reward_greedy) - 1) * 100
cat(sprintf("The 'Smart' forward-looking policy performed %.2f%% better than the aggressive heuristic.", value_gain), "\n")
cat("\n===================================================\n")