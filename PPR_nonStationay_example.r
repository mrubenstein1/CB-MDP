#########################################################################
# RESERVE EXAMPLE
#########################################################################

rm(list=ls()) # remove existing variables

library(MDPtoolbox)
library(graphics)
source('mdp_finite_horizon_nonStationary.r')

source('mdp_example_PPR_non_stationary.r')

source('explore_solution_PPR.r')

source('dec2binvec.r')
source('getSite.r')
source('binvec2dec.r')
source('getState.r')

toyPB = TRUE


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


## Build the MDP
# Generate the transition and reward matrix
PR <- mdp_example_PPR_non_stationary(M,term,Pj)
P <- PR$P   # Probability transitions P(SxSxAxT)
R <- PR$R   # Reward R(SxAxT)
h <- PR$RT  # terminal Reward R(S)

## Solve the MDP
# Solve the PPR problem
results <- mdp_finite_horizon_nonStationary(P, R, 1, time_step, h);
V <- results$V
policy <- results$policy
print(policy)
print(V)

## Explore solution
sim <- explore_solution_PPR(numeric(init_site), policy, M, P, R,h)
sim$Treward
sim$Tsites

