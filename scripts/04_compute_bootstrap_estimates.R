#04_compute_bootstrap_estimates

# to do: compute jacknife resamples

# note: compute estimates with trimmed and untrimmed weights



# compute actual estimates ----
# plain estimate from weighted survey




# bootstrap percentiles estimates ----
# authors compute: BS Mean - mean of resamples
# BS Median - median of resamples
# BS se - SD of resamples

# confidence intervals: 
# Normal.L95: Actual est. - qnorm(0.975) * SD from bootstrap resamples
# Normal.U95: Actual est. + qnorm(0.975) * SD from bootstrap resamples
# Percentile.L95: 0.025 percentile of bootstrap resamples 
# Percentile.U95: 0.975 percentile of bootstrap resamples 

# bootstrap bca ----

# inputs: 
# * point estimates from jacknife resamples (proportion of vote in jacknife resamples)       
# * point estimates from resamples (proportion of vote in resample)       

