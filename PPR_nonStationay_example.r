#########################################################################
# RESERVE EXAMPLE
#########################################################################

#rm(list=ls()) # remove existing variables

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

##################################################
#       S1: BUILD & SOLVE MDP    #
#################################################

# ---- S1 ----

## Build the MDP
# Generate the transition and reward matrix
PR <- mdp_example_PPR_non_stationary(M_s1,term_s1,Pj_s1)
P <- PR$P   # Probability transitions P(SxSxAxT)
R <- PR$R   # Reward R(SxAxT)
h <- PR$RT  # terminal Reward R(S)

## Solve the MDP
# Solve the PPR problem
results <- mdp_finite_horizon_nonStationary(P, R, 1, time_step, h);
V <- results$V
policy <- results$policy

## Explore solution
sim <- explore_solution_PPR(numeric(init_site), policy, M_s1, P, R,h)
sim$Treward
sim$Tsites

# Run the simulation 1000 times and collect sim$Treward from each run
results_s1 <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy, M_s1, P, R, h)
  sim$Treward
})

#Combine all Treward into a matrix 
results_s1 <- do.call(rbind, results_s1)
    #Only keep terminal reward
    n <- ncol(results_s1) 
    results_s1 <- results_s1[,n]
    
    #visualize
    mean(results_s1)
    hist(results_s1)
    boxplot(results_s1)
    sd(results_s1)
    
    #save into dataframe
    results_sum <- data.frame(scenario=c("s1","s2","s3"), mean_r= NA, sd_r=NA)
    results_sum <- results_sum %>% 
      mutate(
        mean_r = if_else(scenario == "s1", round(mean(results_s1),2), mean_r),
        sd_r = if_else(scenario == "s1", round(sd(results_s1),2), sd_r)
      )
    print(results_sum)

# ---- S2 ----
    
    ## Build the MDP
    # Generate the transition and reward matrix
    PR <- mdp_example_PPR_non_stationary(M_s2,term_s2,Pj_s2)
    P <- PR$P   # Probability transitions P(SxSxAxT)
    R <- PR$R   # Reward R(SxAxT)
    h <- PR$RT  # terminal Reward R(S)
    
    ## Solve the MDP
    # Solve the PPR problem
    results <- mdp_finite_horizon_nonStationary(P, R, 1, time_step, h);
    V <- results$V
    policy <- results$policy
    
    ## Explore solution
    sim <- explore_solution_PPR(numeric(init_site), policy, M_s2, P, R,h)
    sim$Treward
    sim$Tsites
    
    # Run the simulation 1000 times and collect sim$Treward from each run
    results_s2 <- lapply(1:1000, function(i) {
      sim <- explore_solution_PPR(numeric(init_site), policy, M_s2, P, R, h)
      sim$Treward
    })
    
    #Combine all Treward into a matrix 
    results_s2 <- do.call(rbind, results_s2)
    #Only keep terminal reward
    n <- ncol(results_s2) 
    results_s2 <- results_s2[,n]
    
    #visualize
    mean(results_s2)
    hist(results_s2)
    boxplot(results_s2)
    sd(results_s2)
    
    #save into dataframe
    results_sum <- results_sum %>% 
      mutate(
        mean_r = if_else(scenario == "s2", round(mean(results_s2),2), mean_r),
        sd_r = if_else(scenario == "s2", round(sd(results_s2),2), sd_r)
      )
    print(results_sum)
    
# ---- S3 ----
    
    ## Build the MDP
    # Generate the transition and reward matrix
    PR <- mdp_example_PPR_non_stationary(M_s3,term_s3,Pj_s3)
    P <- PR$P   # Probability transitions P(SxSxAxT)
    R <- PR$R   # Reward R(SxAxT)
    h <- PR$RT  # terminal Reward R(S)
    
    ## Solve the MDP
    # Solve the PPR problem
    results <- mdp_finite_horizon_nonStationary(P, R, 1, time_step, h);
    V <- results$V
    policy <- results$policy
    
    ## Explore solution
    sim <- explore_solution_PPR(numeric(init_site), policy, M_s3, P, R,h)
    sim$Treward
    sim$Tsites
    
    # Run the simulation 1000 times and collect sim$Treward from each run
    results_s3 <- lapply(1:1000, function(i) {
      sim <- explore_solution_PPR(numeric(init_site), policy, M_s3, P, R, h)
      sim$Treward
    })
    
    #Combine all Treward into a matrix 
    results_s3 <- do.call(rbind, results_s3)
    #Only keep terminal reward
    n <- ncol(results_s3) 
    results_s3 <- results_s3[,n]
    
    #visualize
    mean(results_s3)
    hist(results_s3)
    boxplot(results_s3)
    sd(results_s3)
    
    #save into dataframe
    results_sum <- results_sum %>% 
      mutate(
        mean_r = if_else(scenario == "s3", round(mean(results_s3),2), mean_r),
        sd_r = if_else(scenario == "s3", round(sd(results_s3),2), sd_r)
      )
    print(results_sum)
    
    
    
    
    
    
    


