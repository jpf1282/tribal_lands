# Notes

These are my notes as I go through the results.

# Gas

- Is it fair to compare gas production in different locations in current time?  If gas production is serving as a proxy for gas potential, then yes.  It may be picking up differences in regulations and development costs across geographies within the US.

- Does the all possible pairwise comparison make sense.  It may be putting more weight on tribes with large geographic range.  I created inverse weights (1/n_obs_per_tribe) and the linear model shows no real difference between.

  + I'm not sure what the following syntax does with the error term: aov_model <- aov(gas ~ time + Error(tribe / time), data = gas_data_long)
  
# Oil

- There are a few big outliers in oil (and gas).  See if the results hold when we drop them.  The distributions seem to be about lognormal.  Consider log transform.

- Dropping obs outside of the 99%tile (3sd) reduces significance (p-val increases to .06).  Not that we need to do this but it does suggest influential outliers.
  
  - I also ran a weighted version of the t-test that I think rebalances the data.  The difference is now significant

- My initial instinct is to consider the subset of obs where you have data on both t1 and t2

- Using the weighted model and clustering standard errors by tribe showed a statistically significant decline in oil

# Amenity rank

- Seems to indicate that amenity rank is better in T2 counties.

# Risk

- Risk results seem to indicate that tribes were moved to less risky lands.  Is this the same as the EPA risk?

- No difference when I run weighted LM with clustered errors

# Precip

- In general, I think this metric is less prone to criticism that human factors like choices to develop oil and gas as well as regulation are affecting the comparison.

- The aov model shows a statistical difference but given the mean and standard deviations, I don't see how that can be the case.  The Error(tribe/time) piece must do something.  

  - I see a positive result using my equivalent regression method with weights and standard errors clustered at the tribe level.
  
  