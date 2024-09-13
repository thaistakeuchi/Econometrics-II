# Problem Set 1 - Problem 1// Econometrics 2 // Thais Takeuchi
# Instructor: Vitor Possebom
# Goal: Forecasting Brazilian GDP growth with an AR(p) model

###############################################################################
# Working environment
###############################################################################

# Clean the working environment
rm(list = ls())

# Load the packages
library("quantmod")
library("ggplot2")
library("lmtest")
library("xtable")

# Parameters
P <- 2 # Maximum number of lags in my AR models
Q <- 2 # Maximum number of lags in my MA models
alpha <- 0.05 # Significance level

###############################################################################
# Clean the data
###############################################################################
#Read the dataset from Ipeadata. Using an R project, the function read.csv
# can read the dataset because the file format uses "," as a column separator.
data <- read.csv("data/data_gdp_brazil.csv")

# Rename the variables: gdp growth is measured as % per year.
colnames(data) <- c("year", "gdp_growth")

# Create an xts object for time series data. The variable "year" is a date.
data_xts <- xts(
  x = data$gdp_growth,
  order.by = as.Date(as.character(data$year), format = "%Y")
)

###############################################################################
# Looking at the data
###############################################################################
# Plot the data:
gg <- ggplot(data = data, aes(x = year)) +
  geom_line(aes(y = gdp_growth), color = "#F5793A", size = 1.5) +
  scale_x_continuous(limits = c(1900, 2021),
                     breaks = seq(from = 1900, to = 2020, by = 20),
                     expand = c(0, 0)) +
  labs(x = "Year", y = "Yearly GDP Growth (%)") +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm"))
print(gg)

# Save this plot
ggsave("figures/figure_gdp_growth.pdf", width = 11, height = 8.5)

# Plot the autocorrelation of GDP growth
pdf(file = "figures/figure_autocorrelation.pdf", width = 11, height = 8.5)
acf(data_xts, ylab = "Autocorrelation", main = "Brazilian Yearly GDP Growth")
dev.off()

###############################################################################
# Estimate many AR(p) models using loops
###############################################################################

# Create a list to store the results of all my models
ar <- vector(mode = "list", length = P)
ma <- vector(mode = "list", length = Q)
arma <- vector(mode = "list", length = P)

# Loop over AR(p) models
for (p in 1:P){
  # Estimate an AR(p) model
  ar[[p]] <- arima(
    x = data_xts,
    order = c(p, 0, 0),
    include.mean = TRUE)}

# Loop over MA(q) models
for (q in 1:Q){
  # Estimate an MA(q) model
  ma[[q]] <- arima(
    x = data_xts,
    order = c(0, 0, q),
    include.mean = TRUE)}

# Loop over ARMA(p,q) models
for (p in 1:P){
  for (q in 1:Q){
    # Estimate an ARMA(p,q) model
    arma[[p]][[q]] <- arima(
      x = data_xts,
      order = c(p, 0, q),
      include.mean = TRUE)}
}

###############################################################################
# Report the estimated coefficients, BIC and AIC of each model
###############################################################################

# Create a matrix to store the AR_results
results_AR <- matrix(NA, nrow = P + 3, ncol = P)

# Rename the columns
colnames(results_AR) <- paste0("AR(", 1:P, ")")

# Rename the rows
rownames(results_AR) <- c("BIC", "AIC", "Intercept", paste0("Lag ", 1:P))

# Loop over AR(p) models
for (p in 1:P){
  # Store BIC: We want to minimize it.
  results_AR[1, p] <- BIC(ar[[p]])

  # Store AIC: We want to minimize it.
  results_AR[2, p] <- AIC(ar[[p]])

  # Store the intercept
  results_AR[3, p] <- ar[[p]]$coef[p + 1]

  # Store the AR coefficients
  results_AR[4:(4 + p - 1), p] <- ar[[p]]$coef[1:p]
}

# Save the table as a PDF
print(xtable(results_AR, type = "latex"), file = "figures/results_ar.tex")

--------------------------------------------------------------------------------
# Create a matrix to store the MA_results
results_MA <- matrix(NA, nrow = Q + 3, ncol = Q)

# Rename the columns
colnames(results_MA) <- paste0("MA(", 1:Q, ")")

# Rename the rows
rownames(results_MA) <- c("BIC", "AIC", "Intercept", paste0("Lag ", 1:Q))

# Loop over MA(q) models
for (q in 1:Q){
  # Store BIC: We want to minimize it.
  results_MA[1, q] <- BIC(ma[[q]])

  # Store AIC: We want to minimize it.
  results_MA[2, q] <- AIC(ma[[q]])

  # Store the intercept
  results_MA[3, q] <- ma[[q]]$coef[q + 1]

  # Store the AR coefficients
  results_MA[4:(4 + q - 1), q] <- ma[[q]]$coef[1:q]
}

# Save the table as a PDF
print(xtable(results_MA, type = "latex"), file = "figures/results_ma.tex")

--------------------------------------------------------------------------------
# Create a matrix to store the ARMA_results
results_ARMA <- matrix(NA, nrow = P + Q + 3, ncol = P * Q)

# Rename the columns
colnames(results_ARMA) <- apply(expand.grid(p = 1:P, q = 1:Q),
                                1,
                                function(x) paste0(
                                  "AR",
                                  "MA (",x[1], ",", x[2],")"))

# Rename the rows
rownames(results_ARMA) <- c("BIC", "AIC", "Intercept",
                            paste0("AR Lag ", 1:P),
                            paste0("MA Lag ", 1:Q))

# Save the table as a PDF
print(xtable(results_ARMA, type = "latex"), file = "figures/results_arma.tex")

for (p in 1:P) {
  for (q in 1:Q) {
    arma[[p]][[q]] <- arima(data_xts, c(p, 0, q), include.mean = TRUE)
    results_ARMA[1:3, (p-1)*Q+q] <- c(BIC(arma[[p]][[q]]), AIC(arma[[p]][[q]]), arma[[p]][[q]]$coef[p + 1])
    results_ARMA[4:(4 + p - 1), (p-1)*Q+q] <- arma[[p]][[q]]$coef[1:p]
    results_ARMA[(4 + p):(3 + p + q), (p-1)*Q+q] <- arma[[p]][[q]]$coef[(p + 2):(p + q + 1)]
  }
}

ar1 <- ar[[1]]; ar2 <- ar[[2]]; ma1 <- ma[[1]]; ma2 <- ma[[2]];
arma11 <- arma[[1]][[1]]; arma12 <- arma[[1]][[2]];
arma21 <- arma[[2]][[1]]; arma22 <- arma[[2]][[2]];

# Create a matrix to store the coefficient results
list <- list(ar1, ar2, ma1, ma2, arma11, arma12, arma21, arma22)
coeff <- vector(mode = "list",length = 8)
T = 8
for (t in 1:T){coeff[[t]]=coeftest(list[[t]])}

#coeff[[1]] is the AR(1) model
#coeff[[2]] is the AR(2) model
#coeff[[3]] is the MA(1) model
#coeff[[4]] is the MA(2) model
#coeff[[5]] is the ARMA(1,1) model
#coeff[[6]] is the ARMA(1,2) model
#coeff[[7]] is the ARMA(2,1) model
#coeff[[8]] is the ARMA(2,2) model

###############################################################################
# Forecast 10-years ahead using our models
###############################################################################

# Estimate our forecasted values using our models
models <- list(ar1, ar2, ma1, ma2, arma11, arma12, arma21, arma22)
forecasts <- lapply(models, function(model) predict(model, n.ahead = 10))
forecast_names <- c("forecast_ar1", "forecast_ar2", "forecast_ma1",
                    "forecast_ma2", "forecast_arma11", "forecast_arma12",
                    "forecast_arma21", "forecast_arma22")
forecast_list <- setNames(forecasts, forecast_names)

plot_data <- lapply(seq_along(forecasts), function(i) {
  data.frame(
    year = 2000:2030,
    gdp_growth = c(data$gdp_growth[data$year %in% 2000:2030], rep(NA, 10)),
    forecast = c(rep(NA, 21), forecast_list[[i]]$pred),
    CI_U = c(rep(NA, 21), forecast_list[[i]]$pred + qnorm(1 - alpha/2) * forecast_list[[i]]$se),
    CI_L = c(rep(NA, 21), forecast_list[[i]]$pred + qnorm(alpha/2) * forecast_list[[i]]$se)
  )
})

# Define function to create a plot for each element of plot_data
plot_forecast <- function(df) {
  ggplot(data = df, aes(x = year)) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Year") + ylab("Yearly GDP Growth (%)") +
    scale_x_continuous(
      limits = c(2000, 2031),
      expand = c(0,0),
      breaks = seq(from = 2000, to = 2030, by = 5)
    ) +
    geom_line(
      aes(y = gdp_growth, color = "Realized"),
      size = 1.5
    ) +
    geom_line(
      aes(y = forecast, color = "Forecast"),
      size = 1.5
    ) +
    geom_line(
      aes(y = CI_U),
      color = "#A95AA1",
      linetype = "dashed",
      size = 1.5
    ) +
    geom_line(
      aes(y = CI_L),
      color = "#A95AA1",
      linetype = "dashed",
      size = 1.5
    ) +
    scale_colour_manual(values = c(
      "#A95AA1", "#F5793A"
    )) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    )

}

# Use lapply to create a plot for each element of plot_data
plots <- lapply(plot_data, plot_forecast)

# Print each plot
for (i in seq_along(plots)) {
  print(plots[[i]])
}

print(plots[[1]])
print(plots[[2]])
print(plots[[3]])
print(plots[[4]])
print(plots[[5]])
print(plots[[6]])
print(plots[[7]])
print(plots[[8]])

# Save the plot as a PDF file
ggsave("figures/figure_gdp_growth_forecast_ar1.pdf", plot = plots[[1]], width = 11, height = 8.5)
ggsave("figures/figure_gdp_growth_forecast_ar2.pdf", plot = plots[[2]], width = 11, height = 8.5)
ggsave("figures/figure_gdp_growth_forecast_ma1.pdf", plot = plots[[3]], width = 11, height = 8.5)
ggsave("figures/figure_gdp_growth_forecast_ma2.pdf", plot = plots[[4]], width = 11, height = 8.5)
ggsave("figures/figure_gdp_growth_forecast_arma11.pdf", plot = plots[[5]], width = 11, height = 8.5)
ggsave("figures/figure_gdp_growth_forecast_arma12.pdf", plot = plots[[6]], width = 11, height = 8.5)
ggsave("figures/figure_gdp_growth_forecast_arma21.pdf", plot = plots[[7]], width = 11, height = 8.5)
ggsave("figures/figure_gdp_growth_forecast_arma22.pdf", plot = plots[[8]], width = 11, height = 8.5)
