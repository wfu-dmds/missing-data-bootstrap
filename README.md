# missing-data-bootstrap

## Week 1 Summary

* Most study only consider the variable being imputed rather than the relationship of y and x with the conditional imputation on x.
* misconceptions exist on both imputation methods.
* The general process introduced in 2.1 is that we need to capture the relationship between X and Z to determine the missingness of X, then by determining the relationship between X and the outcome, we hence figure out the true covariance.
* For the deterministic imputation method, we first find the relationship between X and Z, and using the function of Z to predict the missingness of X, and based on the probability of missingness, replenish the missing X statistics to aquire X_obs. And finally, we predict Y with X_obs.
    * If we are estimating the coefficient accordning to the Y~X_imp,det model, the variance is under/over estimated at the exactly same rate as the covariance is under/over estimated, thus the method provides unbiased relationship.
    * We may compare the estimated variance with the true variance assuming X being fully observed, the variance of the complete case analysis, and the true variance of sampling distribution.
    * According to the result, we may determine which approach is more efficient given the comparison result.
    * We may use different approach due to exactly what is the goal.
    * Deterministic imputation using the outcome will lead to biased estimation.
* Contrast with single prediction replacement of each missing value for the deterministic imputation, the imputed values in stochastic imputation are sampled from a predictive distribution of X, conditioned on Z and X_obs.\
    * Stochastic imputation with outcome Y captures the true estimate when X is fully observed.
    * The reason why inclue outcome in the imputation model is that it correct the covariance and thus the stachastic process will correct the variance to obtain unbiased estimate. Moreover, we need to model to be congenial, which we must include all variables that are in the final analysis model in the imputation model with the exactly same form.
* In the simulation, only the deterministic imputation without Y and the stochastic imputation with outcome Y outputs the true estimate, which matches the figure in the stochastic imputation part.
* There do exist so many cases in biomedical research that over/under estimate significant factors using inappropriate imputation approach, thus convincing the importance of this research to find both mathematical and theoretical connection between deterministic and stochastic imputation methods.
* Whether we are given the outcome variable while doing imputation can be a condition of using appropriate imputation approach.
