#########################################################################
# RESERVE EXAMPLE
#########################################################################

rm(list=ls()) # remove existing variables

library(MDPtoolbox)
library(graphics)
library(dplyr)
source('mdp_finite_horizon_nonStationary.r')

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

# Run the simulation 1000 times and collect sim$Treward from each run
results <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy, M, P, R, h)
  sim$Treward
})

#Combine all Treward into a matrix
results <- do.call(rbind, results)

#Only keep terminal reward
n <- ncol(results) 
results <- results[,n]
mean(results)
hist(results)
boxplot(results)
sd(results)

