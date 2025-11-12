# --- BENEFITS: PARCEL VALUES OVER TIME ---
# This script reads a variable 'benefit_scenario' from the main script
# to determine which benefit matrix (M) to generate. This version defines
# all time step values at once, according to scenarios, then separates tiem steps 1:5 from the
# terminal value (term). This is done to align with structure of explore_solution_PPR & other scripts.

# Common parameters for both scenarios
init_site <- 6
time_step <- 5 # last step is time_step+1
full_horizon <- time_step + 1 # Total steps including terminal

##################################################
#      DEFINE REWARD & TRANSITION MATRIX         #
#################################################

if (!exists("benefit_scenario")) {
  stop("ERROR: The 'benefit_scenario' variable is not set in the main script.")
}

if (benefit_scenario == "constant") {
  
  # --- Scenario 1: CONSTANT BENEFIT ---
  # Each site has a different, but constant, benefit value over time.
  # Benefits are defined for all 6 time steps.
  p1=rep(10, full_horizon); p2=rep(8, full_horizon); p3=rep(7, full_horizon)
  p4=rep(5, full_horizon); p5=rep(3, full_horizon); p6=rep(1, full_horizon) 
  
} else if (benefit_scenario == "variable") {
  
  # --- Scenario 2: VARIABLE BENEFIT ---
  # Benefits are defined for all 6 time steps, with the 6th step continuing the trend.
  set.seed(42) 
  p1 <- c(20, 5, 2, 1, 0, 0)      # "The Trap" value stays at 0
  p2 <- c(1, 2, 8, 16, 25, 36)    # "The Sleeper" value continues to accelerate
  p3 <- rep(8, full_horizon)     # Stable, average control site
  p4 <- c(5, 6, 7, 6, 5, 4)      # Mid-horizon peak continues its decline
  p5 <- c(10, 9, 8, 7, 6, 5)     # Steady decline continues
  p6 <- rep(3, full_horizon)     # Stable low value
  
} else {
  stop(paste("Invalid 'benefit_scenario' specified:", benefit_scenario,
             '. Please choose "constant" or "variable".'))
}


# --- Common Code to Finalize Matrices (for both scenarios) ---
# --- Common Code to Split and Finalize Matrices ---

# 1. Create a matrix with the full benefit data for all 6 time steps
dataR_full <- rbind(p1,p2,p3,p4,p5,p6)

# 2. The main benefit matrix 'M' uses only the first 5 time steps
M <- dataR_full[, 1:time_step]

# 3. The terminal benefit 'term' is the value from the 6th time step
dataRT <- dataR_full[, full_horizon]
term <- matrix(dataRT, nrow = init_site, ncol = 1)

# Conversion probability matrix Pj (kept constant for this example)
pj1=rep(0.1, 5); pj2=rep(0.1, 5); pj3=rep(0.1, 5) 
pj4=rep(0.1, 5); pj5=rep(0.1, 5); pj6=rep(0.1, 5)
dataC = rbind(pj1,pj2,pj3,pj4,pj5,pj6)
Pj <- array(dataC, c(init_site,time_step))