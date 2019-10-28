# Notes

These are my notes as I go through the results.

# Gas

- Is it fair to compare gas production in different locations in current time?  If gas production is serving as a proxy for gas potential, then yes.  It may be picking up differences in regulations and development costs across geographies within the US.

- Does the all possible pairwise comparison make sense.  It may be putting more weight on tribes with large geographic range.  I created inverse weights (1/n_obs_per_tribe) and the linear model shows no real difference between.

  + I'm not sure what the following syntax does with the error term: aov_model <- aov(gas ~ time + Error(tribe / time), data = gas_data_long)