#########################################################################
# RESERVE EXAMPLE
#########################################################################

#rm(list=ls()) # remove existing variables

library(MDPtoolbox)
library(graphics)
library(dplyr)
source('mdp_finite_horizon_nonStationary.r')

source('mdp_example_PPR_non_stationary.r')
source('mdp_myopic_policy_non_Stationary.r')

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
sim <- explore_solution_PPR(numeric(init_site), policy_optimal, M, P, R,h)
sim$Treward
sim$Tsites

# Run the simulation 1000 times and collect sim$Treward from each run
results_optimal <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy_optimal, M, P, R, h)
  sim$Treward
})

    #Combine all Treward into a matrix 
    results_optimal <- do.call(rbind, results_optimal)
        #Only keep terminal reward
        n <- ncol(results_optimal) 
        results_optimal <- results_optimal[,n]
        
        #visualize
        mean(results_optimal)
        hist(results_optimal)
        boxplot(results_optimal)
        sd(results_optimal)

##################################################
#      BUILD & SOLVE Myopic Model    #
#################################################
        

## Solve the MDP with the myopic, greedy solver
results_myopic <- mdp_myopic_policy_nonStationary(P, R, 1, time_step, h) # Note: P and discount are ignored inside
policy_myopic <- results_myopic$policy; head(policy_myopic)


# Run the simulation 1000 times and collect sim$Treward from each run   
results_myopic <- lapply(1:1000, function(i) {
sim <- explore_solution_PPR(numeric(init_site), policy_myopic, M, P, R, h)
sim$Treward
  })
    
  #Combine all Treward into a matrix 
    results_myopic <- do.call(rbind, results_myopic)
    n <- ncol(results_myopic) 
    results_myopic <- results_myopic[,n]  
    
    #visualize
    mean(results_myopic)
    hist(results_myopic)
    boxplot(results_myopic)
    
    
    
    
    
    
    
    
    
########### COMPARE AND STORE RESULTS #########
# Visualize and compare
cat("---- Scenario S1 Results ----\n")
cat("Optimal Mean Reward:", mean(results_optimal), " | SD:", sd(results_optimal), "\n")
cat("Myopic Mean Reward: ", mean(results_myopic), " | SD:", sd(results_myopic), "\n")
par(mfrow=c(1,2))
hist(results_optimal, main="Optimal Policy Rewards")
hist(results_myopic, main="Myopic Policy Rewards")
par(mfrow=c(1,1))

# Save into dataframe
# We'll add new rows for the myopic scenarios
results_sum <- data.frame(scenario=c("optimal", "myopic"), 
                          mean_r= NA, 
                          sd_r=NA)

results_sum <- results_sum %>% 
  mutate(
    mean_r = case_when(
      scenario == "optimal" ~ round(mean(results_optimal), 2),
      scenario == "myopic"  ~ round(mean(results_myopic), 2),
      TRUE ~ mean_r
    ),
    sd_r = case_when(
      scenario == "s1_optimal" ~ round(sd(results_optimal), 2),
      scenario == "s1_myopic"  ~ round(sd(results_myopic), 2),
      TRUE ~ sd_r
    )
  )
print(results_sum)
   
    
    
    
    
    
    
    
    
    
    
  