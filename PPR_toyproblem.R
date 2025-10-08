#MAIN BRANCH: SAME INITIAL INPUTS & PROBLEM STRUCTURE AS IADINE'S ORIGINAL ###

## Specification of the non stationary PPR problem
# How many sites and time steps
# PPR_toyproblem.R

# translation of the xlsx example
init_site <- 6
time_step <- 5 # last step is time_step+1

# M is the time dependent benefit matrix Site x time_step
# random generation or provide data: define benefits for Parcels 1:3
p1=c(7,7,7,7,7) 
p2=c(6,6,6,6,6) 
p3=c(5,5,5,5,5)
p4=c(2,2,2,2,2)
p5=c(8,8,8,8,8)
p6=c(1,1,1,1,1)
dataR = rbind(p1,p2,p3,p4,p5,p6) # is transformed into the M benefit matrix by running down all rows, then on to the subsequent column
M <- matrix(nrow=init_site, ncol=time_step, data=dataR); M

# term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
dataRT = c(7,6,5,2,8,1); #note that this is in the order of parcels (runs down row)
term <- matrix(dataRT, nrow = init_site, ncol = 1)

# Pj is the time dependent matrix representing the probability of a site being 
# converted at every time step 
# Pj(Sites x time_step)

pj1=c(.1,.1,.1,.1,.1) 
pj2=c(.1,.1,.1,.1,.1) 
pj3=c(.1,.1,.1,.1,.1) 
pj4=c(.1,.1,.1,.1,.1) 
pj5=c(.1,.1,.1,.1,.1) 
pj6=c(.1,.1,.1,.1,.1) 
dataC = rbind(pj1,pj2,pj3,pj4,pj5,pj6)
Pj <- array(dataC, c(init_site,time_step))

