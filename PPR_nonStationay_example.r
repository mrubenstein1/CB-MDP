#########################################################################
# RESERVE EXAMPLE
#########################################################################

rm(list=ls()) # remove existing variables

### note on naming convention ###
  # Available Parcels = 0; benefits are counted
  # Conserved (Purchased) Parcels = 1 ; benefits are counted
  # Converted (Lost) Parcels = 2 ; benefits are not counted

library(MDPtoolbox)
library(graphics)
library(dplyr)
source('mdp_finite_horizon_nonStationary.r')
source('mdp_myopic_policy_nonStationary.r')
source('mdp_myopic_forward_look_policy.R')
source('mdp_example_PPR_non_stationary.r')
source('explore_solution_PPR.r')
source('dec2binvec.r')
source('getSite.r')
source('binvec2dec.r')
source('getState.r')

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
#      BUILD & SOLVE MDP    #
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
# BUILD & SOLVE Myopic Model #
#########################################################

## Solve the myopic algorithm
results_myopic <- mdp_myopic_policy_nonStationary(P, R, 1, time_step, h)
policy_myopic <- results_myopic$policy; head(policy_myopic)


## Explore solution
sim_myopic <- explore_solution_PPR(numeric(init_site), policy_myopic, M, P, R,h)
sim_myopic$Treward
sim_myopic$Tsites

# Run the simulation 1000 times and collect sim$Treward from each run
sim_runs_myopic <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy_myopic, M, P, R, h)
  sim$Treward
})


#########################################################
# BUILD & SOLVE Forward Looking Myopic Model #
#########################################################

## Solve the forward-looking myopic algorithm
results_fl_myopic <- mdp_myopic_forward_look_policy(P, R, 1, time_step, h)
policy_fl_myopic <- results_fl_myopic$policy; head(policy_fl_myopic)


## Explore solution
sim_runs_fl_myopic <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy_fl_myopic, M, P, R, h)
  sim$Treward
})




########### COMPARE AND STORE RESULTS #########
# First, process the simulation results to get terminal rewards
# Optimal
sim_matrix_optimal <- do.call(rbind, sim_runs_optimal)
terminal_rewards_optimal <- sim_matrix_optimal[, ncol(sim_matrix_optimal)]
#Myopic
sim_matrix_myopic <- do.call(rbind, sim_runs_myopic)
terminal_rewards_myopic <- sim_matrix_myopic[, ncol(sim_matrix_myopic)]
#FL Myopic
sim_matrix_fl_myopic <- do.call(rbind, sim_runs_fl_myopic)
terminal_rewards_fl_myopic <- sim_matrix_fl_myopic[, ncol(sim_matrix_fl_myopic)]


#Print
cat("---- Comparison Results ----\n")
cat("Optimal Mean Reward:", mean(terminal_rewards_optimal), " | SD:", sd(terminal_rewards_optimal), "\n")
cat("Myopic Mean Reward: ", mean(terminal_rewards_myopic), " | SD:", sd(terminal_rewards_myopic), "\n")
cat("FL Myopic Mean Reward: ", mean(terminal_rewards_fl_myopic), " | SD:", sd(terminal_rewards_fl_myopic), "\n")

#histogram
par(mfrow=c(3,1))
hist(terminal_rewards_optimal, main="Optimal Policy Terminal Rewards")
hist(terminal_rewards_myopic, main="Myopic Policy Terminal Rewards")
hist(terminal_rewards_fl_myopic, main="FL Myopic Policy Terminal Rewards")
par(mfrow=c(1,1))

# Save into dataframe
results_sum <- data.frame(scenario=c("optimal", "myopic", "fl myopic"), 
                          mean_r= NA, 
                          sd_r=NA)

results_sum <- results_sum %>% 
  mutate(
    mean_r = case_when(
      scenario == "optimal" ~ round(mean(terminal_rewards_optimal), 2),
      scenario == "myopic" ~ round(mean(terminal_rewards_myopic), 2),
      scenario == "fl myopic"  ~ round(mean(terminal_rewards_fl_myopic), 2),
      TRUE ~ mean_r
    ),
    sd_r = case_when(
      scenario == "optimal" ~ round(sd(terminal_rewards_optimal), 2),
      scenario == "myopic"  ~ round(sd(terminal_rewards_myopic), 2),
      scenario == "fl myopic"  ~ round(sd(terminal_rewards_fl_myopic), 2),
      TRUE ~ sd_r
    )
  )
print(results_sum); write.csv(results_sum, "results_sum.csv")
