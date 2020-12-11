# Problem set 6A

# Question 1 
# part A
n <- 10
u <- runif(n, 0, 1)
# Lambda = 0.5
lambda <- 0.5
y <- -log(1-u)/lambda 

# For Question 2
var_true <- (1/0.5^2)
var_hat_list <- vector()
sample_var_list <- vector()

# Part b
# Lambda hat = 1/mean(y)
lambda_hat <- 1/mean(y)

# part c
# bias and se of lambda_hat and RMSW
lambda_hat_list = vector()

nsim <- 10000
for (i in 1:nsim) {
  u_sim <- runif(n, 0, 1)
  y_sim <- -log(1-u_sim)/lambda
  lambda_hat_list[i] <- 1/mean(y_sim)
  var_hat_list[i] <- mean(y_sim)^2
  sample_var_list[i] <- var(y_sim)
}

# Bias
bias_lambda_hat <- mean(lambda_hat_list) - 0.5
bias_lambda_hat
# SE
se_lambda_hat <- sd(lambda_hat_list)
se_lambda_hat
# RMSE
rmse_lambda_hat <- sqrt(bias_lambda_hat^2 + se_lambda_hat^2)
rmse_lambda_hat

# Plotting to check normality
hist(lambda_hat_list)

# Question 2
# Bias and SE and MSE of sigma hat Sq 
bias_sigma_hat <- mean(var_hat_list) - var_true
bias_sigma_hat
# SE of sigma_hat
se_sigma_hat <- sd(var_hat_list)
se_sigma_hat
# RMSE
rmse_sigma_hat <- sqrt(bias_sigma_hat^2 + se_sigma_hat^2)
rmse_sigma_hat

# plotting
hist(var_hat_list)

# part c
# Bias
bias_sample_var <- mean(sample_var_list) - var_true
bias_sample_var
# SE
se_sample_var <- sd(sample_var_list)
se_sample_var
# RMSE
rmse_sample_var <- sqrt(bias_sample_var^2 + se_sample_var^2)
rmse_sample_var