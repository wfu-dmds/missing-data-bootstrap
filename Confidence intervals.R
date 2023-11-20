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
  list(var = var(coef),
       coeffi_ = mean(coef))
}  

#Stochastic Imputation
stoch_imp <- function(data) {
  stochastic_imp <- mice(data, method = "norm", formulas = list(x_fit ~ Y + Z), printFlag = FALSE)
  fit <- with(stochastic_imp, lm(Y ~ x_fit))
  pooled <- pool(fit) |> broom::tidy()
  
  list(se = pooled$std.error[pooled$term == "x_fit"],
       coef = pooled$estimate[pooled$term == "x_fit"]
  )
}

#Simulation
simulation <- function(a) 
{
  data <- tibble(
    # Z = rbinom(n,1,p=0.5),
    # X = rnorm(Z, 1),
    # Y = 2*X + rnorm(n),
    Z = rbinom(n,1,p=0.5),
    X = rnorm(n),
    Y = 2*X + Z + rnorm(n),
    miss_x = ifelse(Z, rbinom(n, 1, p=0.7), rbinom(n, 1, p=0.25)), #change .25 to .5
    x_fit = ifelse(miss_x, NA, X)
  )
  
  final_fit <- run_models(data)
  cc_fit <- lm(Y ~ x_fit, data)
  stoch_imp <- stoch_imp(data)
  
  tibble(
    prob = a,
    det_imp_coef = final_fit$coefficients[2],
    det_imp_se = sqrt(vcov(final_fit)[2, 2]),
    sto_imp_coef = stoch_imp$coef,
    sto_imp_se = stoch_imp$se,
    bootstrap_coef = bootstrap_var(data, B = 100)$coeffi_,
    bootstrap_se = sqrt(bootstrap_var(data, B = 100)$var),
    cc_beta = cc_fit$coefficients[2],
    cc_se_ = sqrt(vcov(cc_fit)[2, 2])
  )
  
}

rep_sim <- function (x) {
  o <- purrr::map_df(1:500, ~simulation(x))
  tibble(
    det_imp_c = mean(o$det_imp_coef),
    det_imp_s = mean(o$det_imp_se),
    sto_imp_c = mean(o$sto_imp_coef),
    sto_imp_s = mean(o$sto_imp_se),
    bootstrap_c = mean(o$bootstrap_coef),
    bootstrap_s = mean(o$bootstrap_se),
    cc_c = mean(o$cc_beta),
    cc_s = mean(o$cc_se_)
  )
}

dtbl_1 <- map_df(1:1, ~rep_sim(0.7))

tbl <- tibble(
  index = c("Deterministic Imputation", "Stochastic Imputation", "Bootstrap", "Complete Case"),
  coeff = c(mean(dtbl_1$det_imp_c),mean(dtbl_1$sto_imp_c),mean(dtbl_1$bootstrap_c),mean(dtbl_1$cc_c)),
  se = c(mean(dtbl_1$det_imp_s),mean(dtbl_1$sto_imp_s),mean(dtbl_1$bootstrap_s),mean(dtbl_1$cc_s))
)

tbl <- tbl[order(tbl$index, decreasing = TRUE), ]

plot1 <- ggplot(tbl, aes(coeff, index)) + 
  labs(x="Confidence Interval", y="Imputation Methods") + 
  geom_pointrange(aes(xmin = coeff - 1.96 * se, xmax = coeff + 1.96 * se)) + 
  geom_vline(xintercept=2, color='skyblue', linetype='solid', alpha=5)

plot1 + annotate("text", x = 2.2, y = 4, label = paste("(", round(tbl$coeff[1]-1.96*tbl$se[1],3),", ",round(tbl$coeff[1]+1.96*tbl$se[1],3),")")) +
  annotate("text", x = 2.25, y = 3, label = paste("(", round(tbl$coeff[2]-1.96*tbl$se[2],3),", ",round(tbl$coeff[2]+1.96*tbl$se[2],3),")")) +
  annotate("text", x = 2.21, y = 2, label = paste("(", round(tbl$coeff[3]-1.96*tbl$se[3],3),", ",round(tbl$coeff[3]+1.96*tbl$se[3],3),")")) +
  annotate("text", x = 2.21, y = 1, label = paste("(", round(tbl$coeff[4]-1.96*tbl$se[4],3),", ",round(tbl$coeff[4]+1.96*tbl$se[4],3),")")) + 
  coord_cartesian(xlim = c(1.85, 2.33))

#Simulation
simulation1 <- function(a) 
{
  data <- tibble(
    # Z = rbinom(n,1,p=0.5),
    # X = rnorm(Z, 1),
    # Y = 2*X + rnorm(n),
    Z = rbinom(n,1,p=0.5),
    X = rnorm(n),
    Y = 2*X + Z + rnorm(n),
    miss_x = ifelse(Z, rbinom(n, 1, p=a), rbinom(n, 1, p=0.25)), #change .25 to .5
    x_fit = ifelse(miss_x, NA, X)
  )
  
  final_fit <- run_models(data)
  cc_fit <- lm(Y ~ x_fit, data)
  stoch_imp <- stoch_imp(data)
  
  t <- tibble(
    prob = a,
    det_imp_coef = final_fit$coefficients[2],
    det_imp_se = sqrt(vcov(final_fit)[2, 2]),
    sto_imp_coef = stoch_imp$coef,
    sto_imp_se = stoch_imp$se,
    bootstrap_coef = bootstrap_var(data, B = 100)$coeffi_,
    bootstrap_se = sqrt(bootstrap_var(data, B = 100)$var),
    cc_beta = cc_fit$coefficients[2],
    cc_se_ = sqrt(vcov(cc_fit)[2, 2])
  )
  
  tibble(
    det_cov_check = ifelse(2<(t$det_imp_coef+1.96*t$det_imp_se)&&2>(t$det_imp_coef-1.96*t$det_imp_se),TRUE,FALSE),
    sto_cov_check = ifelse(2<(t$sto_imp_coef+1.96*t$sto_imp_se)&&2>(t$sto_imp_coef-1.96*t$sto_imp_se),TRUE,FALSE),
    bs_cov_check = ifelse(2<(t$bootstrap_coef+1.96*t$bootstrap_se)&&2>(t$bootstrap_coef-1.96*t$bootstrap_se),TRUE,FALSE),
    cc_cov_check = ifelse(2<(t$cc_beta+1.96*t$cc_se_)&&2>(t$cc_beta-1.96*t$cc_se_),TRUE,FALSE)
  )
}

library(mice)
cov_chk <- map_df(1:1000, ~simulation1(0.7))

tibble(
  Deterministic_Coverage_Check = sum(cov_chk$det_cov_check)/1000,
  Stochastic_Coverage_Check = sum(cov_chk$sto_cov_check)/1000,
  Bootstrap_Coverage_Check = sum(cov_chk$bs_cov_check)/1000,
  CompleteCase_Coverage_Check = sum(cov_chk$cc_cov_check)/1000
)