#MAIN BRANCH: SAME INITIAL INPUTS & PROBLEM STRUCTURE AS IADINE'S ORIGINAL ###

## Specification of the non stationary PPR problem
# How many sites and time steps
# PPR_toyproblem.R

# translation of the xlsx example
init_site <- 6
time_step <- 5 # last step is time_step+1

# M is the time dependent benefit matrix Site x time_step
# random generation or provide data: define benefits for Parcels 1:3
p1=c(7,5,4,3,2) 
p2=c(6,3,2,1,1) 
p3=c(5,6,7,8,9)
p4=c(2,3,4,6,6)
p5=c(8,6,3,3,1)
p6=c(1,2,5,6,7)
dataR = rbind(p1,p2,p3,p4,p5,p6) # is transformed into the M benefit matrix by running down all rows, then on to the subsequent column
M <- matrix(nrow=init_site, ncol=time_step, data=dataR); M

# term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
dataRT = c(1,1,8,4,5,6);
term <- matrix(dataRT, nrow = init_site, ncol = 1)

# Pj is the time dependent matrix representing the probability of a site being 
# converted at every time step 
# Pj(Sites x time_step)

pj1=c(.25,0.5) 
pj2=c(0.5,0.6) 
pj3=c(0.6,0.4)
pj4=c(0.6,0.4)
pj5=c(0.6,0.4)
pj6=c(0.6,0.4)
dataC = rbind(pj1,pj2,pj3,pj4,pj5,pj6)
Pj <- array(dataC, c(init_site,time_step))

