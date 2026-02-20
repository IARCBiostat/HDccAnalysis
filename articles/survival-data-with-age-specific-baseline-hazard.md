# Survival data with age-specific baseline hazard

``` r
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

## Generate survival data with a piecewise baseline hazard

We simulate 1000 participants entering at age 50 and administratively
censored at age 70. However, the baseline hazard is piecewise-constant:

- ages \[0, 60): 0.005
- ages \[60, 100): 0.02

``` r
set.seed(1)
n <- 1000
entry_age <- 50
censor_age <- 70
beta <- c(0.4, 0.6)
p <- length(beta)
covariates <- matrix(rnorm(n * p), ncol = p)

baseline_hazard_by_age <- data.frame(
  age_lo = c(0, 60),
  age_hi = c(60, 100),
  rate   = c(0.005, 0.02)
)

sim <- HDccAnalysis::sim_cox_age_data(
  n = n,
  entry_age = entry_age,
  censor_age = censor_age,
  beta = beta,
  covariates = covariates,
  baseline_hazard_by_age = baseline_hazard_by_age
)

head(sim)
# # A tibble: 6 × 6
#      id entry_age censor_age event   Exp1    Exp2
#   <int>     <dbl>      <dbl> <int>  <dbl>   <dbl>
# 1     1        50       60.3     1 -0.626  1.13  
# 2     2        50       70       0  0.184  1.11  
# 3     3        50       70       0 -0.836 -0.871 
# 4     4        50       70       0  1.60   0.211 
# 5     5        50       70       0  0.330  0.0694
# 6     6        50       70       0 -0.820 -1.66  
```

### Quick checks

``` r
fit <- survival::coxph(survival::Surv(entry_age, censor_age,event) ~ Exp1 + Exp2, data = sim)
print(fit)
# Call:
# survival::coxph(formula = survival::Surv(entry_age, censor_age, 
#     event) ~ Exp1 + Exp2, data = sim)

#         coef exp(coef) se(coef)     z        p
# Exp1 0.40820   1.50410  0.06268 6.512 7.41e-11
# Exp2 0.53256   1.70329  0.06039 8.818  < 2e-16

# Likelihood ratio test=116.8  on 2 df, p=< 2.2e-16
# n= 1000, number of events= 247 
```

### Incidence rate

The achieved incidence rate (events per person-time):

``` r
attr(sim, "achieved_incidence_rate")
# [1] 0.01367849
```

### adjust the baseline hazard to acquire a target incidence rate

We can now run the simulation to achive a target inceidance rate while
keeping the relevant hazard difference of the age groups using the
`target_avg_baseline_hazard`:

``` r
set.seed(1)
sim <- HDccAnalysis::sim_cox_age_data(
  n = n,
  entry_age = entry_age,
  censor_age = censor_age,
  beta = beta,
  covariates = covariates,
  baseline_hazard_by_age = baseline_hazard_by_age,
  target_avg_baseline_hazard = 0.02
)

fit <- survival::coxph(survival::Surv(entry_age, censor_age,event) ~ Exp1 + Exp2, data = sim)
print(fit)
# Call:
# survival::coxph(formula = survival::Surv(entry_age, censor_age, 
#     event) ~ Exp1 + Exp2, data = sim)

#         coef exp(coef) se(coef)      z        p
# Exp1 0.40245   1.49549  0.05116  7.866 3.66e-15
# Exp2 0.57387   1.77512  0.05277 10.875  < 2e-16

# Likelihood ratio test=175  on 2 df, p=< 2.2e-16
# n= 1000, number of events= 367 
print(attr(sim, "achieved_incidence_rate"))
# [1] 0.0212904

beta_2 <- c(1.5, 1.5)
sim_2 <- HDccAnalysis::sim_cox_age_data(
  n = n,
  entry_age = entry_age,
  censor_age = censor_age,
  beta = beta_2,
  covariates = covariates,
  baseline_hazard_by_age = baseline_hazard_by_age,
  target_avg_baseline_hazard = 0.02
)

fit_2 <- survival::coxph(survival::Surv(entry_age, censor_age,event) ~ Exp1 + Exp2, data = sim_2)
print(fit_2)
# Call:
# survival::coxph(formula = survival::Surv(entry_age, censor_age, 
#     event) ~ Exp1 + Exp2, data = sim_2)

#         coef exp(coef) se(coef)     z      p
# Exp1 1.47227   4.35912  0.06993 21.05 <2e-16
# Exp2 1.38520   3.99564  0.07145 19.39 <2e-16

# Likelihood ratio test=726.9  on 2 df, p=< 2.2e-16
# n= 1000, number of events= 349 
print(attr(sim_2, "achieved_incidence_rate"))
# [1] 0.02121945
```
