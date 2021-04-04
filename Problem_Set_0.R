# Problem Set 0 ================================================================
# Author: Thomas Wiemann 

# This problem set serves as a brief illustration of the law of large numbers 
#     (LLN) and the central limit theorem (CLT). 

# Preliminaries ================================================================

# Here, we'd typically load required packages or our own custom code stored in 
#     other R scripts. For this problem set, base R suffices so we'll omit this
#     step here.

# Set random seed 
set.seed(2021)

# Exercise 1: Convergence to the mean ==========================================

# First, we set some parameters for the simulation exercise
nobs <- 10000 # maximum number of samples drawn 

# Second, we generate iid draws from various distributions. For simplicity, 
#     consider a standard normal N(0,1), a uniform on the interval (-1, 1), 
#     a Bernoulli B(p = 0.25).

draws_norm <- rnorm(nobs, mean = 0, sd = 1)
draws_unif <- runif(nobs, min = -1, max = 1)
draws_bern <- sample(c(0,1), size = nobs, replace = T, prob = c(0.75, 0.25))

# Calculate cumulative sums and divide by the number of draws to obtain the 
#     sample means after n draws, for n = {1, 2,..., 10000}. Note, these 
#     divisions are element-wise.

means_norm <- cumsum(draws_norm) / c(1:nobs)
means_unif <- cumsum(draws_unif) / c(1:nobs)
means_bern <- cumsum(draws_bern) / c(1:nobs)

# Normal draws
plot(means_norm, type = "l", main = expression(bar(X)[n]), col = "black",
     ylab = "", xlab = "n", las = 1)
lines(rep(0, nobs),  col = "red", lty = 5)

# Uniform draws
plot(means_unif, type = "l", main = expression(bar(X)[n]), col = "black",
     ylab = "", xlab = "n", las = 1)
lines(rep(0, nobs),  col = "red", lty = 5)

# Bernoulli draws
plot(means_bern, type = "l", main = expression(bar(X)[n]), col = "black",
     ylab = "", xlab = "n", las = 1)
lines(rep(0.25, nobs),  col = "red", lty = 5)

# Indeed, we see the sample means converging to the true means. 

# Exercise 2: Convergence in distribution ======================================

# As before, let's first define some parameters for the exercise. Here, our goal
#     is to visualize the distribution of the sample mean at various sample 
#     sizes. It thus makes sense to fix the number of simulated sample means at
#     a sufficiently high value (so that we can plot a proper histogram), and 
#     define a set of different sample sizes.

n_smplmeans <- 10000
nobs_set <-  c(5, 10, 100, 1000)

# To calculate the means, it's convenient to use a simple for-loop. Note that
#     we must first initialize a matrix to store our results in. This matrix
#     should be of appropriate dimension. Then, we loop over all possible values
#     in nobs_set. Note that we draw individual samples from the exponential 
#     distribution with rate 2 so that the expected value is 1/2.

res_sim <- matrix(NA, nrow = n_smplmeans, ncol = length(nobs_set))
for (j in 1:length(nobs_set)) {
  res_sim[, j] <- replicate(n_smplmeans, mean(rexp(nobs_set[j], 2)))
}#FOR

# Let us now plot the corresponding histograms.

hist(res_sim[, 1], 50, xlab = expression(bar(X)[n]), main = "n = 5")
hist(res_sim[, 2], 50, xlab = expression(bar(X)[n]), main = "n = 10")
hist(res_sim[, 3], 50, xlab = expression(bar(X)[n]), main = "n = 100")
hist(res_sim[, 4], 50, xlab = expression(bar(X)[n]), main = "n = 1,000")

# Indeed, we see that the distribution of the sample mean converges to a normal
#     distribution.