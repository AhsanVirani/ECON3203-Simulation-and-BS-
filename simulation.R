# Simulation 
# Problem set 4A
# By Ahsan Muhammad, Dated 30th Nov 2020

# Q1 part a) mu = 0.2, sigma = 0.15. Cal bias, SE, and RMSE

# Creating empty vectors to store mean, sd, median and sharpe ratio
y_bar_list = vector()
sd_list = vector()
sharpe_list = vector()
median_list = vector()

set.seed(123)
nSim <- 10000
for (i in 1:nSim) {
  y_sample = rnorm(5, 0.2, 0.15)
  # Sample mean as an estimator
  y_bar_list[i] = mean(y_sample)
  # Sample sd as an estimator
  sd_list[i] = sd(y_sample)
  # Sample sharpe as an estimator
  sharpe_list[i] = y_bar_list[i]/sd_list[i]
  # Question 2
  median_list[i] = median(y_sample)
}

# PART A. BIAS, SE, and, RMSE of the sample mean

# Bias of sample mean
bias_sample_mean <- mean(y_bar_list) - 0.2
bias_sample_mean
# Standard Error of sample mean
se_sample_mean <- sd(y_bar_list)
se_sample_mean
# RMSE of sample man
rmse_sample_mean <- sqrt(bias_sample_mean^2 + se_sample_mean^2)
rmse_sample_mean
print(c(bias_sample_mean, se_sample_mean, rmse_sample_mean))

# Plotting histogram to see normality asymtotic
hist(y_bar_list)

# PART B. BIAS, SE, and RMSE OF SAMPLE SD
# Bias of sample

# Bias of sample mean
bias_sd <- mean(sd_list) - 0.15
bias_sd
# Standard Error of sample mean
se_sd <- sd(sd_list)
se_sd
# RMSE of sample man
rmse_sd <- sqrt(bias_sd^2 + se_sd^2)
rmse_sd

hist(sd_list)

# PART C
bias_sharpe <- mean(sharpe_list) - (0.2/0.15)
bias_sharpe
se_sharpe <- sd(sharpe_list)
se_sharpe
rmse_sharpe <- sqrt(bias_sharpe^2 + se_sharpe^2)
rmse_sharpe

# QUESTION 2
bias_median <- mean(median_list) - 0.2
bias_median
se_median <- sd(median_list)
se_median
rmse_median <- sqrt(bias_median^2 + se_median^2)
rmse_median

# Q2 with n = 50
for (i in 1:nSim) {
  y_sample = rnorm(50, 0.2, 0.15)
  # Sample mean as an estimator
  y_bar_list[i] = mean(y_sample)
  # Sample sd as an estimator
  sd_list[i] = sd(y_sample)
  # Sample sharpe as an estimator
  sharpe_list[i] = y_bar_list[i]/sd_list[i]
  # Question 2
  median_list[i] = median(y_sample)
}

# SAMPLE MEAN AS AN ESTIMATOR

# Bias of sample mean
bias_sample_mean <- mean(y_bar_list) - 0.2
bias_sample_mean
# Standard Error of sample mean
se_sample_mean <- sd(y_bar_list)
se_sample_mean
# RMSE of sample man
rmse_sample_mean <- sqrt(bias_sample_mean^2 + se_sample_mean^2)
rmse_sample_mean

# SAMPLE MEDIAN AS AN ESTIMATOR
bias_median <- mean(median_list) - 0.2
bias_median
se_median <- sd(median_list)
se_median
rmse_median <- sqrt(bias_median^2 + se_median^2)
rmse_median