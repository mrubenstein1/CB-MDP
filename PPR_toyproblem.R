#MAIN BRANCH: SAME INITIAL INPUTS & PROBLEM STRUCTURE AS IADINE'S ORIGINAL ###

## Specification of the non stationary PPR problem
# How many sites and time steps
# PPR_toyproblem.R

# translation of the xlsx example
init_site <- 3
time_step <- 2 # last step is time_step+1

# M is the time dependent benefit matrix Site x time_step
# random generation or provide data: define benefits for Parcels 1:3
dataR = c(1,1,1,1,3,3)
M <- matrix(nrow=init_site, ncol=time_step, data=dataR); M

# term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
dataRT = c(7,6,1); #note that this is in the order of parcels (runs down row)
term <- matrix(dataRT, nrow = init_site, ncol = 1)

# Pj is the time dependent matrix representing the probability of a site being 
# converted at every time step 
# Pj(Sites x time_step)

dataC = c(0.25,0.5,0.6,0.5,0.6,0.4);
Pj <- array(dataC, c(init_site,time_step))

