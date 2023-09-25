# missing-data-bootstrap

## Week 3 Summary

* Set up a simulation with a binomial Z with p = 0.5 and normally distributed X with a mean of Z and SD = 1. Y = 2 X + standard normal error. Z also determinines missingness with the probability that X is missing if Z = 0 of 0.25 and then we varied the probability that X is missing if Z = 1 from 0.05 to 1.
* Did a simulation examining the bootstrap confidence interval, "true" confidence interval, model based confidence interval and complete case confidence
* Below is a plot of the results
    * the bootstrap accurately estimated the true variance
    * the variability increased as the probability of missingness increased
    * the model based variance over estimated the truth for lower probabilieis of missingness (~ <0.7) and under-estimated when the probability of missingness was greater
    * The complete case variance was always smaller than the variance after imputation for all probabilities of missingness
      
 ![Screenshot 2023-09-24 223955](https://github.com/wfu-dmds/missing-data-bootstrap/assets/144035061/2a687d34-71fd-4e7b-8d53-f1e925df81af)


## Week 2 Summary

In the previous meeting, we have worked on creating functions in r for all three different approaches for dealing with missing data while building simple linear regression models. The first approach is the deterministic imputation, which we call that function as run_models. In this scenario, we primarily estimates the missing data by using the z column and leave any non-missing data in x unchanged. The second approach is the bootstrap one, which we extract a proportion of the data set with a fixed number of total rows and allowing repetition of the row, and fit the model to observe the coefficient. The third approach would be the complete case analysis, which we disregard all missing values and only look at what are not missing and fit model with those proportion of data. Another useful thing that I learned from last meeting was the use of "::" symbol since it allows to directly apply certain function from any package without implement the library first.

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
