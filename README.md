# PPR
MDP R implementation of the PPR problem

Run PPR_nonStationary_example.r
> 1. generates a PPR problem at random
>   Specify # of sites (<7) and # of time steps
> 2. solves finite horizon non stationary MDP
> 3. simulate a trajectory and plots results.


When using getState it returns state id from index 0, to use the state to check the action in policy, requires +1 as MDP states start at 1 not 0.
