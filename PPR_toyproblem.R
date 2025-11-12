# --- CHOOSE SCENARIO INPUTS ---
# This script reads a variable 'benefit_scenario' from the main script
# to determine which benefit matrix (M) to generate.

# Common parameters for both scenarios
init_site <- 6
time_step <- 5 # last step is time_step+1

##################################################
#      DEFINE REWARD & TRANSITION MATRIX         #
#################################################

if (!exists("benefit_scenario")) {
  stop("ERROR: The 'benefit_scenario' variable is not set in the main script.")
}

if (benefit_scenario == "constant") {
  
  # --- Scenario 1: CONSTANT BENEFIT ---
  # Each site has a different, but constant, benefit value over time.
  cat("\n--- Generating input data for CONSTANT benefit scenario ---\n")
  p1=rep(10, 5) # Site 1 has a constant benefit of 10
  p2=rep(8, 5)  # Site 2 has a constant benefit of 8
  p3=rep(7, 5)  # Site 3 has a constant benefit of 7
  p4=rep(5, 5)  # Site 4 has a constant benefit of 5
  p5=rep(3, 5)  # Site 5 has a constant benefit of 3
  p6=rep(1, 5)  # Site 6 has a constant benefit of 1
  
} else if (benefit_scenario == "variable") {
  
  # --- Scenario 2: VARIABLE BENEFIT ---
  # Site benefits change over time, creating a challenge for myopic models.
  cat("\n--- Generating input data for VARIABLE benefit scenario ---\n")
  set.seed(42) 
  
  # "The Trap": A very high immediate reward that quickly drops.
  p1 <- c(20, 5, 2, 1, 0)
  # "The Sleeper": Starts low but becomes invaluable later.
  p2 <- c(1, 2, 8, 16, 25)
  # A stable, average control site.
  p3 <- rep(8, 5)
  # Other options to fill out the state space.
  p4 <- c(5, 6, 7, 6, 5)
  p5 <- c(10, 9, 8, 7, 6)
  p6 <- c(3, 3, 3, 3, 3)
  
} else {
  stop(paste("Invalid 'benefit_scenario' specified:", benefit_scenario,
             '. Please choose "constant" or "variable".'))
}


# --- Common Code to Finalize Matrices (for both scenarios) ---

# Build the benefit matrix M
dataR = rbind(p1,p2,p3,p4,p5,p6)
M <- matrix(nrow=init_site, ncol=time_step, data=dataR)

# Terminal benefit is the value at the final time step
dataRT <- M[, time_step]
term <- matrix(dataRT, nrow = init_site, ncol = 1)

# Conversion probability matrix Pj (kept constant for this example)
pj1=rep(0.1, 5); pj2=rep(0.1, 5); pj3=rep(0.1, 5) 
pj4=rep(0.1, 5); pj5=rep(0.1, 5); pj6=rep(0.1, 5)
dataC = rbind(pj1,pj2,pj3,pj4,pj5,pj6)
Pj <- array(dataC, c(init_site,time_step))