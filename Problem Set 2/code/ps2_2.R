# PS2 - exercise 2//Econometrics II - 2023// Based on the code01_unit-root-problems
# Instructor: Vitor Possebom
# (2C class)// Goal: Unit root problems
# Thais Takeuchi
#-------------------------------------------------------------------------------
## Organize the working environment
#-------------------------------------------------------------------------------
# Clean the working environment
rm(list = ls())

# Load the required packages
if (!require(lpdensity)) {
  install.packages("lpdensity")
  library(lpdensity)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(lmtest)) {
  install.packages("lmtest")
  library(lmtest)
}

# Set seed
set.seed(2323)

# Parameters
capT <- 10000 # Number of observed periods (sample size)
a <- 0    # Intercept alpha
M <- 10000    # Number of MC repetitions
s <- 0.1      # Significance level
degr_freed_t_dist <- c(1, 5) # Degree of freedom of the t-dist
d <- 1 # Delta

# Function to run the simulation and calculate rejection rate
runSimulation <- function(degree) {
  rej <- numeric(M) # Rejection

  for (m in 1:M) {
    t <- 1:capT # Trend
    e <- rt(n = capT, df = degree) # Epsilon
    Y <- a + d * t + e
    reg <- lm(Y ~ t) # OLS regression
    t_stat <- abs(coeftest(reg)[2, 1] - d) / coeftest(reg)[2, 2]
    cv <- qnorm(1 - s/2) # Critical value

    rej[m] <- as.numeric(t_stat >= cv)
  }

  return(mean(rej))
}

# Run the Monte Carlo Simulation for each degree of freedom
rej_rates <- sapply(degr_freed_t_dist, runSimulation) # Rejection rates

# Print the rejection rates
for (i in seq_along(degr_freed_t_dist)) {
  degree <- degr_freed_t_dist[i]
  rej_rate <- round(100 * rej_rates[i], 3)

  cat("Rejection rate is equal to", rej_rate, "%",
      "when the degrees of freedom is", degree, "\n")
}

