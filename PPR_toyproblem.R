## Specification of the non stationary PPR problem
# How many sites and time steps
# PPR_toyproblem.R

# translation of the xlsx example
init_site <- 6
time_step <- 5 # last step is time_step+1

# M is the time dependent benefit matrix Site x time_step
# random generation or provide data
dataR = c(3,4,2,1,5,6,3,4,2,1,5,6,3,4,2,1,5,6,3,4,2,1,5,6,3,4,2,1,5,6) # is transformed into the M benefit matrix by running down all rows, then on to the subsequent column
M <- matrix(nrow=init_site, ncol=time_step, data=dataR); M

# term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
dataRT = c(3,4,2,1,5,6);
term <- matrix(dataRT, nrow = init_site, ncol = 1)

# Pj is the time dependent matrix representing the probability of a site being 
# converted at every time step 
# Pj(Sites x time_step)
dataC = c(0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25);
Pj <- array(dataC, c(init_site,time_step))

