# Problem Set 0 ================================================================
# Author: Thomas Wiemann =======================================================

# Preliminaries ================================================================

# Load necessary libraries
library(ggplot2) # for plots

# Source custome code
source("mymethods/ols.R")

# Data =========================================================================

# Import data from csv file
df <- read.csv(file = "Data/test_data.csv", header = TRUE, sep = ",")

# Summary Statistics ===========================================================

# Print first 10 rows to get a first look at the variables. It looks like y, X1, 
#     and X2 are continious but X3 is discrete. Also, get a sense of the 
#     dimension of the data.

head(df, 10)

dim(df) # 1620 observations with 4 variables

# Calculate summary statistics for each variable. First, use the `summary`
#     command. Then, replicate the mean with the `apply` function. Also 
#     calculate the 10th, and 90th quantile of each  vaiable, as well as their 
#     standard deviation. Combine the summary statistics in a dataframe.

summary(df)

var_mean <- apply(X = df, MARGIN = 2, FUN = mean) 
var_sd <- apply(X = df, MARGIN = 2, FUN = sd) 
var_quantiles <- apply(X = df, MARGIN = 2, 
                       FUN = function(x) {
                         quantile(x, c(0.1, 0.9))
                       })#APPLY

df_sumstats <- data.frame(rbind(var_mean, var_sd, var_quantiles))

# The summary statistics calculated so far may not be particularly informative 
#     about X3, the discrete variable. Here, a tabulation may give better 
#     insights. It looks like the five distinct values of X3 are approximately
#     uniformly distributed in the sample.

table(df$X.3) # frequency of values within the data

table(df$X.3) / dim(df)[1] # relative frequencies

# Descriptive regression analysis ==============================================

# As a last part of the analysis here, we characterize the conditional 
#     distribution distribution of y given X via linear regression. R's built-in 
#     command for linear regression is `lm`, which we use here first. To obtain
#     the coefficient values and corresponding standard errors, we use the 
#     `summary` method (also built-in).

fit_lm <- lm(y ~ ., data = df) # linear regression of y on X and a constant
summary(fit_lm) # get coefficient values and standard errors

# Let us briefly check whether the regression output is sensible. For example, 
#     let's plot the regression residuals against our three variables X1-X3. We 
#     can use R's build in `plot` function for this. 

plot(x = df$X.1, y = fit_lm$residuals, 
     xlab = "X.1", ylab = expression(epsilon))
plot(x = df$X.2, y = fit_lm$residuals, 
     xlab = "X.2", ylab = expression(epsilon))
plot(x = df$X.3, y = fit_lm$residuals, 
     xlab = "X.3", ylab = expression(epsilon))

# For better plots with more customization options, the reccomended package is 
#     `ggplot2`. In the below, the third residual plot is reproduced and exported 
#     as a .pdf file (e.g., so that it can be included in a LaTeX document.)

pdf("Results/Problem_Set_0/Figures/plt_residualX3.pdf", 
    width=10, height=8, paper='special') 

df_plt <- data.frame(X.3 = df$X.3, eps = fit_lm$residuals)
ggplot(df_plt, aes(x = X.3, y = eps)) + 
  geom_point()  +
  ylab(expression(epsilon)) +
  xlab("X.3") +
  theme_classic(base_size = 30)

dev.off() # make sure to include this when using `pdf`

# The third plot, where we consider X3, suggests that we did not correctly 
#     capture the relationship between y and X3 with the linear model considered 
#     above. Note that we did not differentiate between the continious X1 and
#     X2, and the discrete X3. To allow for nonlinearities, it may be better to 
#     use indicator variables for different levels of X3. To do so, first set 
#     the variable type to factor. R will then include indicator variables when
#     calling `lm`.

df$X.3 <- as.factor(df$X.3)
fit_lm2 <- lm(y ~ ., data = df)
summary_2 <- summary(fit_lm2) # that looks a lot better! 

# As a last step, combine the desired results in a dataframe.

df_res <- data.frame(summary_2$coefficients)

# Exporting results to csv =====================================================

# Once the analysis is finished, it is often useful to store the results in .csv
#     files. Having already stored the desired numbers in dataframes, this is 
#     straightforward. Here, we export both the summary statistics and the 
#     regression results. This concludes Problem Set 0.

write.csv(df_sumstats, file="Results/Problem_Set_0/Tables/summary_stats.csv")
write.csv(df_res, file="Results/Problem_Set_0/Tables/regression_res.csv")

# Optional: Creating your own `lm` object ======================================

# One of the benefits of R over readily packaged statistical software (such as 
#     Stata or SPSS) is the ease with which statistical estimators can be 
#     implemented (or improved upon) by hand. To see how easy this can be, try 
#     writing your own `lm` function. I've written one called `ols`, which 
#     supports basic regression analysis of a univariate outcome on a set of 
#     regressors stored in a matrix. It does not have the helpful interface of 
#     the `lm` object. See the file stored as "R Code/ols.R" for details.

# First, let's get the full regression matrix where X3 is included as a set of 
#     indicator variables.
X_mat <- model.matrix(y ~ ., data = df)
head(X_mat, 10) # see the last columns for the indicator variables

# We may then regress y on X_mat as before. I've also written a `summary` method
#     for my `ols` object. The coefficient values and standard errors should be 
#     the same as before. 
fit_ols <- ols(y = df$y, X = X_mat)
summary(fit_ols)

# What's the use of writing your own estimator functions? On one hand, you don't
#     need to rely on software to provide you with the estimators you want to 
#     use. On the other, even when estimators are readily implemented, e.g., in 
#     a package, they are seldomly optimized for the task at hand. To save 
#     computational time, your own implementations can be of benefit -- even for
#     estimators as simple as linear regression. See the example below, where 
#     `ols` is faster than `lm`.

N <- 1000000 # set large sample size to see difference in time 
x <- rnorm(N) # simulate regressor 
y <- x + rnorm(N) # simulate outcome variable
system.time(lm(y ~ x)) # run lm
system.time(ols(y,  x)) # run ols