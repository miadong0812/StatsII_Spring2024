#Question 1
# Load necessary library
if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")
library(stats)

# 1. Set the seed for reproducibility
set.seed(123)

# 2. Generate 1,000 Cauchy random variables
data <- rcauchy(1000, location = 0, scale = 1)

# 3. Create the empirical CDF from the generated data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)

# 4. Calculate the theoretical CDF using a normal distribution
# Assuming a standard normal distribution (mean=0, sd=1)
theoreticalCDF <- pnorm(data, mean=0, sd=1)

# 5. Determine the test statistic D
D <- max(abs(empiricalCDF - theoreticalCDF))

# Display the test statistic
print(paste("Test Statistic D:", D))

# 6. Using ks.test for validation (This is optional and serves as a check)
ks_result <- ks.test(data, "pnorm", mean = 0, sd = 1)

# Display ks.test results
print(ks_result)

#Qestion 2
# Load necessary libraries
library(stats)  # For lm and optim functions

# Data Generation
set.seed(123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)

# OLS Regression using lm
lm_model <- lm(y ~ x, data = data)
print(summary(lm_model))

# Function to calculate residuals sum of squares (RSS)
rss <- function(beta, data) {
  y_pred <- beta[1] + beta[2] * data$x
  return(sum((data$y - y_pred) ^ 2))
}

# Initial parameter estimates for the Newton-Raphson algorithm
initial_params <- c(0, 1)  # Intercept and slope

# OLS Regression using Newton-Raphson (BFGS Method)
optim_result <- optim(par = initial_params, fn = rss, data = data, method = "BFGS")
print(optim_result)

# Compare the coefficients
cat("Coefficients from lm:\n")
print(coef(lm_model))
cat("Coefficients from BFGS:\n")
print(optim_result$par)
