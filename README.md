# PPR
MDP R implementation of the PPR problem

Run PPR_nonStationary_example.r
> 1. generates a PPR problem at random
>   Specify # of sites (<7) and # of time steps
> 2. solves finite horizon non stationary MDP
> 3. simulate a trajectory and plots results.

## Worth remembering: how to convert a site states \[site1 site2\] into an MDP state stateID
When using getState it returns state id from index 0, to use the state to check the action in policy, requires +1 as MDP states start at 1 not 0.

x= c(2,1,0) # x codes the state of 3 sites: site 1 Converted, site 2 is purchased, site 3 is available

getState(x) # call get state function to return the stateID
[1] 5       # state id provided from index 0 - must add +1 for querying policy/P/R/V

policy[6,2] # query policy for state id 6 at timestep 2
[1] 3        # optimal action is purchase site 3

#Similarly:

getSite(5,3) # getSite assumes stateID index starts at 0 - Checking we get x back
[1] 2 1 0

getSite(6,3) # check we don't get x when calling getSite(stateID+1, total number of sites)
[1] 0 2 0
