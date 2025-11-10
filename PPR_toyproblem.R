#EVALUATE ALL SCENARIOS ###

# translation of the xlsx example
init_site <- 6
time_step <- 5 # last step is time_step+1


##################################################
#      DEFINE REWARD & TRANSITION MATRIX    #
#################################################

# M is the time dependent benefit matrix Site x time_step
# random generation or provide data: define benefits for Parcels 1:3
p1=c(7,5,4,3,2) 
p2=c(6,5,3,2,1) 
p3=c(5,6,7,5,6)
p4=c(2,1,2,3,4)
p5=c(8,5,4,6,7)
p6=c(1,2,3,5,7)
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



##################################################
#       S1: DEFINE REWARD & TRANSITION MATRIX    #
#################################################

# ---- S1 ----

# M is the time dependent benefit matrix Site x time_step
# random generation (constant across time periods)
p1_s1=rep(sample(1:10, 1), 5)
p2_s1=rep(sample(1:10, 1), 5)
p3_s1=rep(sample(1:10, 1), 5)
p4_s1=rep(sample(1:10, 1), 5)
p5_s1=rep(sample(1:10, 1), 5)
p6_s1=rep(sample(1:10, 1), 5)
dataR_s1 = rbind(p1_s1,p2_s1,p3_s1,p4_s1,p5_s1,p6_s1) # is transformed into the M benefit matrix by running down all rows, then on to the subsequent column
M_s1 <- matrix(nrow=init_site, ncol=time_step, data=dataR_s1); M_s1

# term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
dataRT_s1 = c(p1_s1[1],p2_s1[1],p3_s1[1],p4_s1[1],p5_s1[1],p6_s1[1]); #note that this is in the order of parcels (runs down row)
term_s1 <- matrix(dataRT_s1, nrow = init_site, ncol = 1)

# Pj is the time dependent matrix representing the probability of a site being 
# converted at every time step 
# Pj(Sites x time_step)

pj1_s1=rep(runif(1, min = 0.1, max = 0.9), 5) 
pj2_s1=rep(runif(1, min = 0.1, max = 0.9), 5) 
pj3_s1=rep(runif(1, min = 0.1, max = 0.9), 5) 
pj4_s1=rep(runif(1, min = 0.1, max = 0.9), 5) 
pj5_s1=rep(runif(1, min = 0.1, max = 0.9), 5) 
pj6_s1=rep(runif(1, min = 0.1, max = 0.9), 5) 
dataC_s1 = rbind(pj1_s1,pj2_s1,pj3_s1,pj4_s1,pj5_s1,pj6_s1)
Pj_s1 <- array(dataC_s1, c(init_site,time_step)); Pj_s1


# ---- S2 ----

# M is the time dependent benefit matrix Site x time_step
# random generation or provide data:
p1_s2=c(sample(1:10, 5)) 
p2_s2=c(sample(1:10, 5)) 
p3_s2=c(sample(1:10, 5))
p4_s2=c(sample(1:10, 5))
p5_s2=c(sample(1:10, 5))
p6_s2=c(sample(1:10, 5))
dataR_s2 = rbind(p1_s2,p2_s2,p3_s2,p4_s2,p5_s2,p6_s2) # is transformed into the M benefit matrix by running down all rows, then on to the subsequent column
M_s2 <- matrix(nrow=init_site, ncol=time_step, data=dataR_s2); M_s2

# term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
dataRT_s2 = c(sample(1:10, 6)); #note that this is in the order of parcels (runs down row)
term_s2 <- matrix(dataRT_s2, nrow = init_site, ncol = 1)

# Pj is the time dependent matrix representing the probability of a site being 
# converted at every time step 
# Pj(Sites x time_step)

pj1_s2=runif(6, min = 0.1, max = 0.9)
pj2_s2=runif(6, min = 0.1, max = 0.9)
pj3_s2=runif(6, min = 0.1, max = 0.9)
pj4_s2=runif(6, min = 0.1, max = 0.9) 
pj5_s2=runif(6, min = 0.1, max = 0.9)
pj6_s2=runif(6, min = 0.1, max = 0.9)
dataC_s2 = rbind(pj1_s2,pj2_s2,pj3_s2,pj4_s2,pj5_s2,pj6_s2)
Pj_s2 <- array(dataC_s2, c(init_site,time_step)); Pj_s2


# ---- S3 ----

# M is the time dependent benefit matrix Site x time_step
# random generation or provide data:
p1_s3=c(sample(1:10, 5)) 
p2_s3=c(sample(1:10, 5)) 
p3_s3=c(sample(1:10, 5))
p4_s3=c(sample(1:10, 5))
p5_s3=c(sample(1:10, 5))
p6_s3=c(sample(1:10, 5))
dataR_s3 = rbind(p1_s3,p2_s3,p3_s3,p4_s3,p5_s3,p6_s3) # is transformed into the M benefit matrix by running down all rows, then on to the subsequent column
M_s3 <- matrix(nrow=init_site, ncol=time_step, data=dataR_s3); M_s3

# term is a vector representing the terminal benefit (reward) at each site (time_step+1) 
dataRT_s3 = c(sample(1:10, 6)); #note that this is in the order of parcels (runs down row)
term_s3 <- matrix(dataRT_s3, nrow = init_site, ncol = 1)

# Pj is the time dependent matrix representing the probability of a site being 
# converted at every time step 
# Pj(Sites x time_step)

pj1_s3=runif(6, min = 0.1, max = 0.9)
pj2_s3=runif(6, min = 0.1, max = 0.9)
pj3_s3=runif(6, min = 0.1, max = 0.9)
pj4_s3=runif(6, min = 0.1, max = 0.9) 
pj5_s3=runif(6, min = 0.1, max = 0.9)
pj6_s3=runif(6, min = 0.1, max = 0.9)
dataC_s3 = rbind(pj1_s3,pj2_s3,pj3_s3,pj4_s3,pj5_s3,pj6_s3)
Pj_s3 <- array(dataC_s3, c(init_site,time_step)); Pj_s3