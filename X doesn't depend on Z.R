library(tidyverse)

n <- 1000

#Deterministic Imputation
run_models <- function(data) {
  imputa <- lm(x_fit ~ Z, data)
  coeffi <- imputa$coefficients
  x_imp <- coeffi[1] + coeffi[2] * data$Z
  data$x_imp = ifelse(data$miss_x, x_imp, data$X)
  lm(Y ~ x_imp, data)
}
#Bootstrap
bootstrap_one <- function(data) {
  data_boot <- data[sample(1:nrow(data), replace = TRUE), ]
  mod <- run_models(data_boot)
  mod$coefficients[2]
}
bootstrap_var <- function(data, B = 1000) {
  coef <- map_dbl(1:B, ~bootstrap_one(data))
  var(coef)
}  

#Stochastic Imputation
stoch_imp <- function(data) {
  stochastic_imp <- mice(data, method = "norm", formulas = list(x_fit ~ Y + Z), printFlag = FALSE)
  fit <- with(stochastic_imp, lm(Y ~ x_fit))
  pooled <- pool(fit) |> broom::tidy()
  
  pooled$std.error[pooled$term == "x_fit"]
}
#Simulation
simulation <- function(a) 
{
  data <- tibble(
    Z = rbinom(n,1,p=0.5),
    X = rnorm(n),
    Y = 2*X + Z + rnorm(n),
    miss_x = ifelse(Z, rbinom(n, 1, p=a), rbinom(n, 1, p=0.25)),
    x_fit = ifelse(miss_x, NA, X)
  )
  
  final_fit <- run_models(data)
  cc_fit <- lm(Y ~ x_fit, data)
  
  tibble(
    prob = a,
    Beta_1 = final_fit$coefficients[2],
    det_imp_var = vcov(final_fit)[2, 2],
    sto_imp_var = stoch_imp(data)^2,
    bootstrap_var = bootstrap_var(data, B = 100),
    cc_var = vcov(cc_fit)[2, 2])
}

probability <- seq(0, 1, by=0.05)

rep_sim <- function (x) {
  o <- purrr::map_df(1:100, ~simulation(x)) 
  tibble(
    prob_f = x,
    true_var = var(o$Beta_1),
    estimated_var_det_imp = mean(o$det_imp_var),
    estimated_var_sto_imp = mean(o$sto_imp_var),
    estimated_boot_var = mean(o$bootstrap_var),
    estimated_cc_var = mean(o$cc_var)
  )
}

library(mice)

df_fnl <- map_df(probability, rep_sim)

plot(df_fnl$prob_f, df_fnl$true_var, col="black",pch="o", ylim = c(0, 0.03), xlab="Probability of missing X with Z = 1", ylab= expression(paste(hat(beta[1]), " variance")), lty=2)
lines(df_fnl$prob_f, df_fnl$true_var,col = "black",lty=2, lwd = 2)
points(df_fnl$prob_f, df_fnl$estimated_var_det_imp, col="orange", pch="*")
lines(df_fnl$prob_f, df_fnl$estimated_var_det_imp, col="orange",lty=2, lwd = 2)
points(df_fnl$prob_f, df_fnl$estimated_var_sto_imp, col="skyblue", pch="#")
lines(df_fnl$prob_f, df_fnl$estimated_var_sto_imp, col="skyblue",lty=2, lwd = 2)
points(df_fnl$prob_f, df_fnl$estimated_boot_var, col="#009E73", pch="+")
lines(df_fnl$prob_f, df_fnl$estimated_boot_var, col="#009E73",lty=2, lwd = 2)
points(df_fnl$prob_f, df_fnl$estimated_cc_var, col="#CC79A7", pch="x")
lines(df_fnl$prob_f, df_fnl$estimated_cc_var, col="#CC79A7",lty=2, lwd = 2)
legend(0,0.03,legend=c("True Variance","Deterministic Imputation","Stochastic Imputation","Bootstrap","Complete Case"), col=c("black","orange","skyblue","#009E73","#CC79A7"),
       pch=c("o","*","#","+","x"), cex = 0.8,lty=c(1,2,3,4,5), ncol=1)