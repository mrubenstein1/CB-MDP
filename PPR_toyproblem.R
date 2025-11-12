#EVALUATE ALL SCENARIOS ###

# translation of the xlsx example
init_site <- 6
time_step <- 5 # last step is time_step+1


##################################################
#      DEFINE REWARD & TRANSITION MATRIX    #
#################################################

# M is the time dependent benefit matrix Site x time_step
# CONSTANT BENEFIT: define benefits for Parcels 1:6
#p1=rep(10, 5) # Site 1 has a constant benefit of 10
#p2=rep(8, 5)  # Site 2 has a constant benefit of 8
#p3=rep(7, 5)  # Site 3 has a constant benefit of 7
#p4=rep(5, 5)  # Site 4 has a constant benefit of 5
#p5=rep(3, 5)  # Site 5 has a constant benefit of 3
#p6=rep(1, 5)  # Site 6 has a constant benefit of 1

#VARIABLE BENEFIT
set.seed(42) 


# Site 1: "The Trap". A very high immediate reward that quickly drops to zero.
# A myopic model should be strongly attracted to this.
p1 <- c(20, 5, 2, 1, 0)

# Site 2: "The Sleeper". Starts with almost no value but becomes invaluable later.
# An optimal model should be able to recognize this potential.
p2 <- c(1, 2, 8, 16, 25)

# Site 3: A stable, average control site.
p3 <- rep(8, 5)

# Sites 4, 5, 6: Other options to fill out the state space.
p4 <- c(5, 6, 7, 6, 5)
p5 <- c(10, 9, 8, 7, 6)
p6 <- c(3, 3, 3, 3, 3)


dataR = rbind(p1,p2,p3,p4,p5,p6) # is transformed into the M benefit matrix by running down all rows, then on to the subsequent column
M <- matrix(nrow=init_site, ncol=time_step, data=dataR); M
# term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
dataRT <- M[, time_step]; #note that this a placeholder, such that the terminal value is whatever the penultaimte value is
term <- matrix(dataRT, nrow = init_site, ncol = 1)

# Pj is the time dependent matrix representing the probability of a site being 
# converted at every time step 
# Pj(Sites x time_step)

pj1=rep(0.1, 5) 
pj2=rep(0.1, 5) 
pj3=rep(0.1, 5) 
pj4=rep(0.1, 5) 
pj5=rep(0.1, 5) 
pj6=rep(0.1, 5)
dataC = rbind(pj1,pj2,pj3,pj4,pj5,pj6)
Pj <- array(dataC, c(init_site,time_step))


