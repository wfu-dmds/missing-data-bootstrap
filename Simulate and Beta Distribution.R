library(tidyverse)

n <- 1000
run_models <- function(data) {
  imputa <- lm(x_fit ~ Z, data)
  coeffi <- imputa$coefficients
  x_imp <- coeffi[1] + coeffi[2] * data$Z # deterministic
  data$x_imp = ifelse(data$miss_x, x_imp, data$X)
  lm(Y ~ x_imp, data)
}
bootstrap_one <- function(data) {
  data_boot <- data[sample(1:nrow(data), replace = TRUE), ]
  mod <- run_models(data_boot)
  mod$coefficients[2]
}
bootstrap_var <- function(data, B = 1000) {
  coef <- map_dbl(1:B, ~bootstrap_one(data))
  var(coef)
}  
sim <- function() {
  data <- tibble(
    Z = rbinom(n,1,p=0.5),
    X = Z + rnorm(n),
    Y = 2*X + rnorm(n),
    miss_x = ifelse(Z, rbinom(n, 1, p=0.95), rbinom(n, 1, p=0.25)),
    x_fit = ifelse(miss_x, NA, X)
  )
  
  final_fit <- run_models(data)
  cc_fit <- lm(Y ~ x_fit, data)

  tibble(
    Beta_0 = final_fit$coefficients[1],
    Beta_1 = final_fit$coefficients[2],
    var_lm_beta_1 = vcov(final_fit)[2, 2],
    bootstrap_var = bootstrap_var(data, B = 100),
    cc_var = vcov(cc_fit)[2, 2])
}

o <- purrr::map_df(1:100, ~sim())

o |>
  summarise(abs_bias = abs(2 - mean(Beta_1)),
            true_var = var(Beta_1),
            estimated_var_lm = mean(var_lm_beta_1),
            estimated_boot_var = mean(bootstrap_var),
            estimated_cc_var = mean(cc_var))

# hist(o$Beta_0, xlab = "beta 0", main = "Beta 0 distribution")
hist(o$Beta_1, xlab = expression(paste(hat(beta[1]), ": Coefficient for imputed X")), main = expression(paste(hat(beta[1]), " Distribution")))

