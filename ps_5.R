# Problem set 5
# by Ahsan Muhammad

# Ques 3

sample_corr <- vector()

n <- 100
nsim <- 10000
for(i in 1:nsim) {
  x = rnorm(n, 0, 1)
  y = 0.3*x + rnorm(n, 0, 1)
  sample_corr[i] = cor(x, y)
}

# Bias
mean(sample_corr) - 0.3
sd(sample_corr)

# For n = 100