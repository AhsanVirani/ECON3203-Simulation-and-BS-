# Parametric Bootstrapping
# By Ahsan Muhammad

# Question 1 
# part A
n <- 10
u <- runif(n, 0, 1)
# Lambda = 0.5
lambda <- 0.5
y <- -log(1-u)/lambda 

# Part b
lambda_hat <- 1/mean(y)

# part C
lambda_hat_list <- vector()
lambda_list <- vector()
nsim <- 1000
set.seed(12345)
for(i in 1:nsim){
  u_bs <- runif(n, 0, 1)
  # Using bootstrap
  y_bs <- -log(1-u_bs)/lambda_hat   
  lambda_hat_list[i] <- 1/mean(y_bs)
  # Using Simulation
  y_sim <- -log(1-u_bs)/lambda   
  lambda_list[i] <- 1/mean(y_sim)
}
###### BOOTSTRAP
# bias
bias_bs <- mean(lambda_hat_list) - lambda_hat
bias_bs
# SE
se_bs <- sd(lambda_hat_list)
se_bs
# RMSE
rmse_bs <- sqrt(bias_bs^2 + se_bs^2)
rmse_bs

# Plotting
hist(lambda_hat_list)

##### SIMULATION
# bias
bias_sim <- mean(lambda_list) - lambda
bias_sim
# SE
se_sim <- sd(lambda_list)
se_sim
# RMSE
rmse_sim <- sqrt(bias_sim^2 + se_sim^2)
rmse_sim

# Plotting
hist(lambda_list)
