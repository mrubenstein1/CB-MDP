#EVALUATE ALL SCENARIOS ###

# translation of the xlsx example
init_site <- 6
time_step <- 5 # last step is time_step+1


##################################################
#      DEFINE REWARD & TRANSITION MATRIX    #
#################################################

# M is the time dependent benefit matrix Site x time_step
# random generation or provide data: define benefits for Parcels 1:3
p1=c(7,7,7,7,7) 
p2=c(7,7,7,7,7) 
p3=c(7,7,7,7,7)
p4=c(7,7,7,7,7)
p5=c(7,7,7,7,7)
p6=c(7,7,7,7,7)
dataR = rbind(p1,p2,p3,p4,p5,p6) # is transformed into the M benefit matrix by running down all rows, then on to the subsequent column
M <- matrix(nrow=init_site, ncol=time_step, data=dataR); M

# term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
dataRT = c(1,2,7,5,6,7); #note that this is in the order of parcels (runs down row)
term <- matrix(dataRT, nrow = init_site, ncol = 1)

# Pj is the time dependent matrix representing the probability of a site being 
# converted at every time step 
# Pj(Sites x time_step)

pj1=c(.1,.2,.1,.3,.3) 
pj2=c(.1,.2,.3,.4,.5) 
pj3=c(.3,.2,.1,.1,.1) 
pj4=c(.5,.4,.3,.2,.1) 
pj5=c(.1,.2,.2,.2,.3) 
pj6=c(.1,.1,.1,.1,.1) 
dataC = rbind(pj1,pj2,pj3,pj4,pj5,pj6)
Pj <- array(dataC, c(init_site,time_step))


