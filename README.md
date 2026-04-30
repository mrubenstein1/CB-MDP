# **Project Title**

Time-dependent decisions under non-stationarity: applications to the land acquisition problem

## **Citation**

Rubenstein, M. 2026. "Time-dependent decisions under non-stationarity: applications to the land acquisition problem". <https://doi.org/10.5066/P13NSY2X>

## **Description**

This code provides a simple quantitative demonstration of an imagined land acquisition problem. Conservation results (i.e., benefit or reward) is evaluated under three competing decision models and varying degrees of ecological non-stationarity. The decision models reflect a spectrum of accomodating time-dependence into decision making: an optimal, fully time-dependent Markov Decision Process (MDP) model; a greedy heuristic; and an intermediary myopic MDP. The cumulative benefit (i.e., cumulative rewards) calculated at the end of all time steps is used to compare model performance, and assess the conditions under which additional levels of decision model complexity is justified. The application of these models is imagined in the Prairie Pothole Region (PPR) of the United States, where land acquisition is critical to the conservation of migratory waterfowl. This analysis was used to assess the impact of incorporating time-dependent decision models into conservation decisions in the associated manuscript (Rubenstein et al, 2026. "Cognitive and Institutional Barriers to Effective Natural Resource Management under Global Change". Conservation Biology, in review). 

## Repository Structure

```         
CB-MDP/
├── PPR_nonStationary_example.r         # Implementation of the land acquisition problem under non-stationary. 
                                            # sets the benefit scenario (constant or variable), 
                                            # executes the non-stationary MDP, 
                                            # and compares results to the non-stationary models 
                                            # ("greedy_solver.r" and "mdp_finite_horizon_nonStationary.r")
├── PPR_toyproblem.R                    # Defines initial parameters for use across all models 
                                            # (time steps, number of sites, reward and transition matrices)
├── mdp_finite_horizon_nonStationary.r  # Defines the policy function for the non-stationary 
                                            # finite horizon MDP solver (optimal policy)
├── greedy_solver.r                     # Defines the policy function for the greedy heuristic 
├── mdp_myopic_forward_look_policy.r    # Defines the policy function for the stationary, myopic MDP 
├── explore_solution_PPR.r              # Defines function to simulate runs of the policy and create graphics
├── volatility.R                        # Runs iterative simulations across increasing degrees of 
│                                           # non-stationarity and compares model performance
├── graphics.r                          # Generates figures and visualizations
├── significance_tests.R                # Statistical tests for comparing model outputs; compares results with and without outliers

│
└── MDPToolbox/                         # Core MDP utility functions
    │                                   # Adapted from Chades et al. (2014), Ecography
    │                                   # https://doi.org/10.1111/ecog.00888
    │
    ├── dec2binvec.r                     # Decimal to binary vector conversion
    ├── binvec2dec.r                     # Binary vector to decimal conversion
    ├── getSite.r                        # Site index helper
    ├── getState.r                       # State index helper
    ├── mdp_example_PPR_non_stationary.r # Builds MDP transition/reward matrices for PPR problem
    ├── mdp_finite_horizon.r             # Standard finite horizon MDP solver
    ├── mdp_check.r                      # Input validation for MDP problem setup
    ├── mdp_check_square_stochastic.r    # Validates stochastic transition matrices
    ├── mdp_computePR.r                  # Computes transition probability and reward matrices
    ├── mdp_computePpolicyPRpolicy.r     # Policy-specific matrix computation
    ├── mdp_bellman_operator.r           # Bellman optimality operator
    ├── mdp_eval_policy_iterative.r      # Iterative policy evaluation
    ├── mdp_eval_policy_matrix.r         # Matrix-based policy evaluation
    ├── mdp_eval_policy_optimality.r     # Optimality condition checking
    ├── mdp_eval_policy_TD_0.r           # TD(0) policy evaluation
    ├── mdp_policy_iteration.r           # Policy iteration algorithm
    ├── mdp_policy_iteration_modified.r  # Modified policy iteration
    ├── mdp_value_iteration.r            # Value iteration algorithm
    ├── mdp_value_iteration_bound_iter.r # Value iteration with bounded iterations
    ├── mdp_value_iterationGS.r          # Gauss-Seidel value iteration
    ├── mdp_relative_value_iteration.r   # Relative value iteration
    ├── mdp_Q_learning.r                 # Q-learning algorithm
    ├── mdp_LP.r                         # Linear programming MDP solver
    ├── mdp_span.r                       # Span seminorm helper
    ├── mdp_example_forest.r             # Forest management example problem
    ├── mdp_example_rand.r               # Random MDP example
    ├── mdp_example_reserve.r            # Reserve design example problem
    ├── explore_solution_reserve.r       # Simulation helper for reserve example
    └── reserve_example.r                # Reserve problem implementation
```
**Disclaimer**

This software has been approved for release by the U.S. Geological Survey
(USGS). Although the software has been subjected to rigorous review, the USGS
reserves the right to update the software as needed pursuant to further analysis
and review. No warranty, expressed or implied, is made by the USGS or the U.S.
Government as to the functionality of the software and related material nor shall
the fact of release constitute any such warranty. Furthermore, the software is
released on condition that neither the USGS nor the U.S. Government shall be
held liable for any damages resulting from its authorized or unauthorized use

**Permissions**

This work is marked with Creative Commons Zero v1.0 Universal (https://creativecommons.org/publicdomain/zero/1.0/).

**Keywords**

non-stationarity, time-dependent decisions, climate change, cognitive biases, institutional barriers, land acquisition

**Funding Acknowledgement**

This work was conducted as a part of the “Markov decision processes in 
non-autonomous socio-ecological systems” Working Group supported by the 
John Wesley Powell Center for Analysis and Synthesis, funded by the U.S. Geological Survey.

