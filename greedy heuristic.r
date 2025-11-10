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
    
    # ===================================================================
    # NEW FORCED PURCHASE LOGIC STARTS HERE
    # ===================================================================
    
    # 1. Identify which parcels are available for purchase (state code 0)
    available_parcels <- which(current_site == 0)
    
    # 2. Check if there are any parcels left to purchase.
    if (length(available_parcels) > 0) {
      # --- MANDATORY PURCHASE LOGIC ---
      # If parcels are available, we MUST purchase one.
      
      # Get the immediate rewards for all possible actions in the current state and time
      immediate_rewards <- R[state, , i]
      
      # Filter to see the rewards for only the available purchase actions
      rewards_for_available_purchases <- immediate_rewards[available_parcels]
      
      # Find the best action among the available options.
      # 'which.max' finds the index of the max reward within the filtered list.
      # We then use that index to find the corresponding parcel number.
      best_purchase_action <- available_parcels[which.max(rewards_for_available_purchases)]
      
      action[i] <- best_purchase_action
      
    } else {
      # --- NO PARCELS LEFT ---
      # If no parcels are available, we can finally do nothing.
      action[i] <- J + 1 # The "Do Nothing" action
    }
    
    # ===================================================================
    # NEW DECISION LOGIC ENDS HERE
    # ===================================================================
    
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
  
  # ... (The plotting code remains the same)
  par(mfrow=c(2,1), mar = c(4, 4, 2, 2))
  breaks <- 0:3
  colors <- c("white", "grey", "black")
  Tsites_indices <- Tsites+1
  image(t(Tsites_indices), axes = FALSE,xlab="Time horizon", ylab="Sites", breaks = breaks, col=colors)
  box()
  title(main="white=Available, grey=Purchased, black=Converted", font.main=1)
  plot(1:horizon, Treward,type="s", xlab="Time horizon", ylab="Sum of benefits")
  
  return(list("Treward"=Treward, "Tsites"=Tsites))
}



# ... (After running the setup and solving for the smart MDP policy) ...

# --- Run simulation with the SMART policy ---
results_smart <- explore_solution_PPR(init_site, policy, M, P, R, h)
final_reward_smart <- tail(results_smart$Treward, 1)

# --- Run simulation with YOUR ADAPTIVE GREEDY heuristic ---
results_adaptive_greedy <- explore_solution_adaptive_greedy(init_site, M, P, R, h)
final_reward_adaptive_greedy <- tail(results_adaptive_greedy$Treward, 1)


# --- Compare final cumulative rewards ---
cat("\n============================================\n")
cat("          Comparison of Results        \n")
cat("============================================\n")
cat("Final Benefit (Smart, Forward-Looking MDP):", final_reward_smart, "\n")
cat("Final Benefit (Adaptive Greedy Heuristic):", final_reward_adaptive_greedy, "\n")