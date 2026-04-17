#########################################################################
# RESERVE EXAMPLE####
#########################################################################


### note on naming convention ###
  # Available Parcels = 0; benefits are counted
  # Conserved (Purchased) Parcels = 1 ; benefits are counted
  # Converted (Lost) Parcels = 2 ; benefits are not counted

library(MDPtoolbox)
library(graphics)
library(dplyr)
library(ggplot2)
library(patchwork)
library(rstatix)
library(onewaytests)
source('mdp_finite_horizon_nonStationary.r')
source('greedy_solver.r')
source('mdp_myopic_forward_look_policy.R')
source('mdp_example_PPR_non_stationary.r')
source('explore_solution_PPR.r')
source('dec2binvec.r')
source('getSite.r')
source('binvec2dec.r')
source('getState.r')


##################################################
#      CHOOSE SCENARIO TO RUN        ####
##################################################
#
# Set this variable to control the input data.
# Options: "constant" or "variable"
#
benefit_scenario <- "constant"

toyPB = TRUE
# CAREFULL when using getState(), the id of the state returned should get +1 (starts at 0)
# > x= c(2,1,0)
# > getState(x)
# [1] 5
# > policy[6,2]
# [1] 3
if (toyPB==F){
  ## Specification of the non stationary PPR problem
  # How many sites and time steps
  init_site <- 3
  time_step <- 3 # last step is time_step+1
  
  # M is the time dependent benefit matrix Site x time_step
  # random generation or provide data
  M <- round(matrix(nrow=init_site, ncol=time_step, data=runif(init_site*time_step,1,5)))
  
  # term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
  term <- round(matrix(runif(init_site, 1, 5), nrow = init_site, ncol = 1))
  
  # Pj is the time dependent matrix representing the probability of a site being converted at every time step 
  Pj <- round(array(runif(init_site*time_step, min=0, max=0.4), c(init_site,time_step))*100)/100
} else {source('PPR_toyproblem.r')}

##################################################
#      BUILD & SOLVE MDP    #####
#################################################

## Build the MDP
# Generate the transition and reward matrix
PR <- mdp_example_PPR_non_stationary(M,term,Pj)
P <- PR$P   # Probability transitions P(SxSxAxT)
R <- PR$R   # Reward R(SxAxT)
h <- PR$RT  # terminal Reward R(S)

## Solve the MDP
results_optimal <- mdp_finite_horizon_nonStationary(P, R, 1, time_step, h)
policy_optimal <- results_optimal$policy; head(policy_optimal)


## Explore solution
sim_optimal <- explore_solution_PPR(numeric(init_site), policy_optimal, M, P, R,h)
sim_optimal$Treward
sim_optimal$Tsites

# Run the simulation 1000 times and collect sim$Treward from each run
sim_runs_optimal <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy_optimal, M, P, R, h)
  sim$Treward
})



#########################################################
# BUILD & SOLVE Greedy Algorithm #####
#########################################################

## Solve the greedy algorithm
results_greedy <- mdp_greedy_policy_nonStationary(M, P, R, 1, time_step, h, init_site)
policy_greedy <- results_greedy$policy; head(policy_greedy)


## Explore solution
sim_greedy <- explore_solution_PPR(numeric(init_site), policy_greedy, M, P, R,h)
sim_greedy$Treward
sim_greedy$Tsites

# Run the simulation 1000 times and collect sim$Treward from each run
sim_runs_greedy <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy_greedy, M, P, R, h)
  sim$Treward
})


#########################################################
# BUILD & SOLVE Forward Looking Myopic Model #####
#########################################################

## Solve the forward-looking myopic algorithm
results_fl_myopic <- mdp_myopic_forward_look_policy(P, R, 1, time_step, h)
policy_fl_myopic <- results_fl_myopic$policy; head(policy_fl_myopic)


## Explore solution
sim_fl_myopic <- explore_solution_PPR(numeric(init_site), policy_fl_myopic, M, P, R,h)
sim_fl_myopic$Treward
sim_fl_myopic$Tsites

# Run the simulation 1000 times and collect sim$Treward from each run
sim_runs_fl_myopic <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy_fl_myopic, M, P, R, h)
  sim$Treward
})


########### STORE RESULTS #########

# Extract 1000 terminal rewards for each model
terminal_rewards_optimal <- do.call(rbind, sim_runs_optimal)[, ncol(M) + 1]
terminal_rewards_greedy <- do.call(rbind, sim_runs_greedy)[, ncol(M) + 1]
terminal_rewards_fl_myopic <- do.call(rbind, sim_runs_fl_myopic)[, ncol(M) + 1]

