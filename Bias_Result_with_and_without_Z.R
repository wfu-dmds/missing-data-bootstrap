library(tidyverse)

n <- 1000

#Deterministic Imputation
run_models <- function(data) {
  imputa <- lm(x_fit ~ Z, data)
  coeffi <- imputa$coefficients
  x_imp <- coeffi[1] + coeffi[2] * data$Z
  data$x_imp = ifelse(data$miss_x, x_imp, data$X)
  lm(Y ~ x_imp, data) #Add Z
}
#Bootstrap
bootstrap_one <- function(data) {
  data_boot <- data[sample(1:nrow(data), replace = TRUE), ]
  mod <- run_models(data_boot)
  mod$coefficients[2]
}
bootstrap_var <- function(data, B = 1000) {
  coef <- map_dbl(1:B, ~bootstrap_one(data))
  list(var = var(coef),
       coeffi_ = mean(coef))
}  

#Stochastic Imputation
stoch_imp <- function(data) {
  stochastic_imp <- mice(data, method = "norm", formulas = list(x_fit ~ Y + Z), printFlag = FALSE)
  fit <- with(stochastic_imp, lm(Y ~ x_fit)) #Add Z
  pooled <- pool(fit) |> broom::tidy()
  
  list(se = pooled$std.error[pooled$term == "x_fit"],
       coef = pooled$estimate[pooled$term == "x_fit"])
}

#Simulation
simulation <- function(a) 
{
  data <- tibble(
    Z = rbinom(n,1,p=0.5),
    # X = Z + rnorm(n),
    # Y = 2*X + rnorm(n),
    X = rnorm(n),
    Y = 2*X + Z + rnorm(n),
    miss_x = ifelse(Z, rbinom(n, 1, p=a), rbinom(n, 1, p=0.25)),
    x_fit = ifelse(miss_x, NA, X)
  )
  
  final_fit <- run_models(data)
  cc_fit <- lm(Y ~ x_fit, data) #Add Z
  
  tibble(
    prob = a,
    bias_det = abs(final_fit$coefficients[2] - 2),
    bias_sto = abs(stoch_imp(data)$coef - 2),
    bias_btsp = abs(bootstrap_var(data, B = 100)$coeffi_ - 2),
    bias_cc = abs(cc_fit$coefficients[2] - 2)
  )
}

probability <- seq(0, 1, by=0.05)

rep_sim_1 <- function (x) {
  o <- purrr::map_df(1:500, ~simulation(x)) # try 500
  tibble(
    prob_f = x,
    bias_deterministic = mean(o$bias_det),
    bias_stochastic = mean(o$bias_sto),
    bias_bootstrap = mean(o$bias_btsp),
    bias_cpltcase = mean(o$bias_cc)
  )
}

dff <- map_df(probability, rep_sim_1)

plot(dff$prob_f, dff$bias_deterministic, col="black",pch="o", ylim=c(0.025, 0.12), xlab="Probability of missing X with Z = 1", ylab= expression(paste("Bias result: ", hat(beta[1]), " - 2")), lty=2)
lines(dff$prob_f, dff$bias_deterministic,col = "black",lty=2, lwd = 2)
points(dff$prob_f, dff$bias_stochastic, col="orange", pch="#")
lines(dff$prob_f, dff$bias_stochastic, col="orange",lty=2, lwd = 2)
points(dff$prob_f, dff$bias_bootstrap, col="skyblue", pch="+")
lines(dff$prob_f, dff$bias_bootstrap, col="skyblue",lty=2, lwd = 2)
# points(dff$prob_f, dff$bias_cpltcase, col="#CC79A7", pch="x")
# lines(dff$prob_f, dff$bias_cpltcase, col="#CC79A7",lty=2, lwd = 2)
legend(0,0.12,legend=c("Deterministic Imputation","Stochastic Imputation","Bootstrap","Complete Case"), col=c("black","orange","skyblue","#CC79A7"),
       pch=c("o","#","+","x"), cex = 0.8,lty=c(1,2,3,4), ncol=1)
