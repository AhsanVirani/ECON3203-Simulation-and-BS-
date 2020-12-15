# Ques 2
# n = 10 theta = 5
n <- 10
theta <- 5
y <- runif(n, 0, theta)
theta_hat_est <- max(y)

theta_hat_list <- vector()
theta_list <- vector()
nsim <- 10000
set.seed(12345)
for(i in 1:nsim){
  # Bootstrapping
  y_pb <- runif(n, 0, theta_hat_est)
  theta_hat_list[i] <- max(y_pb)
  # Simuation
  y_sim <- runif(n, 0, theta)
  theta_list[i] <- max(y_sim)
}

##### BOOTSTRAPPING
# bias
bias_theta_hat <- mean(theta_hat_list) - theta_hat_est
bias_theta_hat
# SE
se_theta_hat <- sd(theta_hat_list)
se_theta_hat
# RMSE
rmse_theta_hat <- sqrt(bias_theta_hat^2 + se_theta_hat^2)
rmse_theta_hat

# Plotting
hist(theta_hat_list)


# SIMULATION
bias_theta <- mean(theta_list) - theta
bias_theta
# SE
se_theta <- sd(theta_list)
se_theta
# RMSE 
rmse_theta <- sqrt(bias_theta^2 + se_theta^2)
rmse_theta

# Plotting
hist(theta_list)

