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

# Generate the species richness matrix (e.g. 7 species, 20 sites)

init_site <- 6
time_step <- 7
M <- round(matrix(nrow=init_site, ncol=time_step, data=runif(init_site*time_step,1,5)))
term <- round(matrix(runif(init_site, 1, 5), nrow = init_site, ncol = 1))
Pj <- round(array(runif(init_site*time_step, min=0, max=0.4), c(init_site,time_step))*100)/100

# Generate the transition and reward matrix
PR <- mdp_example_PPR_non_stationary(M,term,Pj)
P <- PR$P
R <- PR$R
h <- PR$RT

# Solve the reserve design problem
results <- mdp_finite_horizon_nonStationary(P, R, 0.96, time_step, h);
V <- results$V
policy <- results$policy
print(policy)
print(V)

# Explore solution - to do
sim <- explore_solution_PPR(numeric(init_site), policy, M, P, R,h)

