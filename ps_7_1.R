# Problem set 7
# By Ahsan Muhammad

# Ques 1
# a
arr <- c(5, 6, 5, 8, 8, 4, 2, 4, 5, 4)
lambda_hat <- mean(arr)
lambda <- 5
# part b
lambda_hat_list <- vector()
lambda_list <- vector()
variance_hat_list <- vector()
nsim <- 10000
n <- 10
for(i in 1:nsim){
  y_pb <- rpois(n, lambda_hat)
  lambda_hat_list[i] <- mean(y_pb)
  variance_hat_list[i] <- var(y_pb)
  
  ### SIMULATION
  y_sim <- rpois(n, lambda)
  lambda_list[i] <- mean(y_sim)
}

# Bias, Se, RMSE of lambda hat
bias_lambda_hat <- mean(lambda_hat_list) - lambda_hat
bias_lambda_hat
# SE
se_lambda_hat <- sd(lambda_hat_list)
se_lambda_hat
#RMSE
rmse_lambda_hat <- sqrt(bias_lambda_hat^2 + se_lambda_hat^2)
rmse_lambda_hat

# Part c
# The sample of 10 obs estimates a mean and variance of 5.1
# Checking how well s^2 is as an estimate of variance

# Bias, Se, RMSE of s^2
bias_ssq <- mean(variance_hat_list) - lambda_hat
bias_ssq
# SE
se_ssq <- sd(variance_hat_list)
se_ssq
#RMSE
rmse_ssq <- sqrt(bias_ssq^2 + se_ssq^2)
rmse_ssq

###### TRUE BIAS
bias_true <- mean(lambda_list) - lambda
bias_true
