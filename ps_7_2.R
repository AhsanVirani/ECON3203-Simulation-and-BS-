# Question 2
arr <- c(5, 6, 5, 8, 8, 4, 2, 4, 5, 4)
root_lambda_hat <- sqrt(mean(arr))
lambda <- 5

# part b
root_lambda_hat_list <- vector()
root_variance_hat_list <- vector()
root_lambda_list <- vector()

nsim <- 10000
n <- 10
for(i in 1:nsim){
  y_pb <- rpois(n, 5.1)
  root_lambda_hat_list[i] <- sqrt(mean(y_pb))
  root_variance_hat_list[i] <- sqrt(var(y_pb))
  
  # SIM
  y_sim <- rpois(n, lambda)
  root_lambda_list[i] <- sqrt(mean(y_sim))
}

# Bias, Se, RMSE of root lambda hat
bias_root_lambda_hat <- mean(root_lambda_hat_list) - root_lambda_hat
bias_root_lambda_hat
# SE
se_root_lambda_hat <- sd(root_lambda_hat_list)
se_root_lambda_hat
#RMSE
rmse_root_lambda_hat <- sqrt(bias_root_lambda_hat^2 + se_root_lambda_hat^2)
rmse_root_lambda_hat

## OF S^2
bias_root_variance_hat <- mean(root_variance_hat_list) - root_lambda_hat
bias_root_variance_hat
# SE
se_root_variance_hat <- sd(root_variance_hat_list)
se_root_variance_hat
#RMSE
rmse_root_variance_hat <- sqrt(bias_root_variance_hat^2 + se_root_variance_hat^2)
rmse_root_variance_hat

#### TRUE BIAS
true_bias <- mean(root_lambda_list) - sqrt(lambda)
true_bias
