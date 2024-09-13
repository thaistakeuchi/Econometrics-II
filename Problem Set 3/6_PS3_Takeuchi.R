#-------------------------------------------------------------------------------
# PS3 // Econometrics II // Problem 1 // Thais Takeuchi
# Instructor: Vitor Possebom
# The GOAL is to forecast Brazilian Annual GDP Growth in 2020
# This code was based on Matheus Junqueira's and clasroom code
#-------------------------------------------------------------------------------
# Organize the working enviroment
#-------------------------------------------------------------------------------
# Cleaning the working environment
#-------------------------------------------------------------------------------

rm(list = ls())

#-------------------------------------------------------------------------------
## Load the required packages
#-------------------------------------------------------------------------------

packages <- c("ggplot2", "stargazer", "dplyr", "quantmod", "lmtest", "xtable", 
              "forecast", "ARDL", "dynlm", "sandwich", "vars", "data.table", 
              "zoo", "tidyverse", "mFilter")

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

#-------------------------------------------------------------------------------
## Clean the data
#-------------------------------------------------------------------------------

dt <- fread("data/data_brazil.csv")

#-------------------------------------------------------------------------------
## Changing variable names:
## year and gdp_growth (gdp growth is measured as % per year)
#-------------------------------------------------------------------------------

colnames(dt) <- c("year", "gdp_growth", "exch_rate", "ipc")

#-------------------------------------------------------------------------------
## Subset of data - 1942 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
## Creating a ts object for work with time series
#-------------------------------------------------------------------------------

dt_2019 <- subset(dt, year >= 1942 & year <= 2019)

ts_gdp_2019 <- ts(dt_2019$gdp_growth,
                         start = c(1942),
                         end = c(2019),
                         frequency = 1)

ts_exch_rate_2019 <- ts(dt_2019$exch_rate,
                                 start = c(1942),
                                 end = c(2019),
                                 frequency = 1)

ts_ipc_2019 <- ts(dt_2019$ipc,
                           start = c(1942),
                           end = c(2019),
                           frequency = 1)

ADLdt_2019 <- ts.union(ts_gdp_2019, ts_exch_rate_2019, ts_ipc_2019)

#-------------------------------------------------------------------------------
## Creating a ts object for work with time series and plot the graphs
#-------------------------------------------------------------------------------

gdp_growth_ts <- ts(dt$gdp_growth,
                    start = c(1901),
                    end = c(2021),
                    frequency = 1)

exch_rate_ts <- ts(dt$exch_rate,
                            start = c(1901),
                            end = c(2021),
                            frequency = 1)

ipc_ts <- ts(dt$ipc,
                      start = c(1901),
                      end = c(2021),
                      frequency = 1)


year <- seq(1901,2021)

df <- data.frame(
  year = year,
  gdp_growth_1 = gdp_growth_ts,
  exch_rate_1 = exch_rate_ts,
  ipc_1 = ipc_ts
)

df_subset <- df[df$year > 1941 & df$year < 2021, ]

#-------------------------------------------------------------------------------
## Reduced-form VAR(1)
#-------------------------------------------------------------------------------

# Estimate our model
var1 <- VAR(ADLdt_2019, p = 1, type = c("both"))

# Collect the results
fitvar <- var1$varresult

# Build a table
stargazer(fitvar, type = "text" )

fitvar <- stargazer(fitvar, type = "latex")
writeLines(fitvar, "figures/table_summary_VAR1.txt")

# Looking if the model is stable - If all roots is less than one, the model is stable
roots(var1, modulus = TRUE) # Since all the roots have absolute values less than 1, then this model is stable. It means that we do not have explosive
# or nonstationary behavior


#-------------------------------------------------------------------------------
# Forecast based on our reduced-form VAR(1) model
#-------------------------------------------------------------------------------
# Forecast the next year
#-------------------------------------------------------------------------------

var_forecast <- predict(var1, n.ahead = 1)
summary(var_forecast$fcst$ts_gdp_2019)

# Create a matrix
forecast <- var_forecast$fcst$ts_gdp_2019[1, 1]
lower <- var_forecast$fcst$ts_gdp_2019[1, 2]
upper <- var_forecast$fcst$ts_gdp_2019[1, 3]
ci <- var_forecast$fcst$ts_gdp_2019[1,4]

# Build a table
forecast_matrix <- matrix(c(-3.88,forecast, lower, upper, ci), ncol = 5, byrow = FALSE)
print(forecast_matrix)
colnames(forecast_matrix) <- c("Observed GDP Growth","Forecast", "Lower Bound", "Upper Bound", "Confidence Interval")
forecast_latex <- stargazer(forecast_matrix, title = "Forecast Results", align = TRUE)
writeLines(forecast_latex, "figures/table_summary_VAR1_forecast.txt")

#-------------------------------------------------------------------------------
## Reduced-form VAR(2)
#-------------------------------------------------------------------------------

# Estimate our model
var2 <- VAR(ADLdt_2019, p = 2, type = c("both"))

# Collect the results
fitvar <- var2$varresult

# Build a table
stargazer(fitvar, type = "text" )

fitvar <- stargazer(fitvar, type = "latex")
writeLines(fitvar, "figures/table_summary_VAR2.txt")

# Looking if the model is stable - If all roots is less than one, the model is stable
roots(var2, modulus = TRUE) # Since all the roots have absolute values less than 1, then this model is stable. It means that we do not have explosive
# or nonstationary behavior


#-------------------------------------------------------------------------------
# Forecast based on our reduced-form VAR(2) model
#-------------------------------------------------------------------------------
# Forecast the next year
#-------------------------------------------------------------------------------

var_forecast <- predict(var2, n.ahead = 1)
summary(var_forecast$fcst$ts_gdp_2019)

# Create a matrix
forecast <- var_forecast$fcst$ts_gdp_2019[1, 1]
lower <- var_forecast$fcst$ts_gdp_2019[1, 2]
upper <- var_forecast$fcst$ts_gdp_2019[1, 3]
ci <- var_forecast$fcst$ts_gdp_2019[1,4]

# Build a table
forecast_matrix <- matrix(c(-3.88,forecast, lower, upper, ci), ncol = 5, byrow = FALSE)
print(forecast_matrix)
colnames(forecast_matrix) <- c("Observed GDP Growth","Forecast", "Lower Bound", "Upper Bound", "Confidence Interval")
forecast_latex <- stargazer(forecast_matrix, title = "Forecast Results", align = TRUE)
writeLines(forecast_latex, "figures/table_summary_VAR2_forecast.txt")

#-------------------------------------------------------------------------------
## Reduced-form VAR(3)
#-------------------------------------------------------------------------------

# Estimate our model
var3 <- VAR(ADLdt_2019, p = 3, type = c("both"))

# Collect the results
fitvar <- var3$varresult

# Build a table
stargazer(fitvar, type = "text" )

fitvar <- stargazer(fitvar, type = "latex")
writeLines(fitvar, "figures/table_summary_var3.txt")

# Looking if the model is stable - If all roots is less than one, the model is stable
roots(var3, modulus = TRUE) # Since all the roots have absolute values less than 1, then this model is stable. It means that we do not have explosive
# or nonstationary behavior


#-------------------------------------------------------------------------------
# Forecast based on our reduced-form VAR(3) model
#-------------------------------------------------------------------------------
# Forecast the next year
#-------------------------------------------------------------------------------

var_forecast <- predict(var3, n.ahead = 1)
summary(var_forecast$fcst$ts_gdp_2019)

# Create a matrix
forecast <- var_forecast$fcst$ts_gdp_2019[1, 1]
lower <- var_forecast$fcst$ts_gdp_2019[1, 2]
upper <- var_forecast$fcst$ts_gdp_2019[1, 3]
ci <- var_forecast$fcst$ts_gdp_2019[1,4]

# Build a table
forecast_matrix <- matrix(c(-3.88,forecast, lower, upper, ci), ncol = 5, byrow = FALSE)
print(forecast_matrix)
colnames(forecast_matrix) <- c("Observed GDP Growth","Forecast", "Lower Bound", "Upper Bound", "Confidence Interval")
forecast_latex <- stargazer(forecast_matrix, title = "Forecast Results", align = TRUE)
writeLines(forecast_latex, "figures/table_summary_var3_forecast.txt")

#-------------------------------------------------------------------------------
# Granger Causality Test
#-------------------------------------------------------------------------------

granger_1 <- causality(var2, cause = "ts_exch_rate_2019")
granger_1$Granger

granger_2 <- causality(var2, cause = "ts_ipc_2019")
granger_2$Granger

granger_3 <- causality(var2, cause = "ts_gdp_2019")
granger_3$Granger

#-------------------------------------------------------------------------------
# Structural VAR(2) 
#-------------------------------------------------------------------------------

right_order <- ADLdt_2019[, c("ts_exch_rate_2019", "ts_gdp_2019", "ts_ipc_2019")]


var4_order <- VAR(right_order, p = 2, type = c("both"))


#-------------------------------------------------------------------------------
# IRF - GDP 
#-------------------------------------------------------------------------------

# Compute the Structural IRF with bootstrapped standard errors
s_irf <- irf(
  var4_order, impulse = "ts_gdp_2019",
  n.ahead = 10, boot = TRUE, runs = 1000, ci=0.90,
  ortho = TRUE
)

#Loop over variables
for (v in 1:3) {
  # Construct a dataframe to plot the structural IRF
  temp <- data.frame(
    "period" = 0:10,
    "irf" = s_irf$irf$ts_gdp_2019[, v],
    "CI_U" = s_irf$Upper$ts_gdp_2019[, v],
    "CI_L" = s_irf$Lower$ts_gdp_2019[, v]
  )
  colnames(temp) <- c("period", "irf", "CI_U", "CI_L")
  
  # Define the label of the Y-axis
  #ylabel <- paste0("SIRF - Response: ", colnames(s_irf$irf$ts_gdp_2019)[v])
  
  # Plot the IRF
  gg <- ggplot(data = temp, aes(x = period)) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Year") + ylab("SIRF") +
    geom_ribbon(
      aes(ymin = CI_L, ymax = CI_U), fill = "grey80"
    ) + geom_hline(yintercept = 0, color = "black") +
    geom_line(
      aes(y = irf), size = 1.5, color = "red"
    )
  print(gg)
  
  # Define the name of the figure
  figvarname <- paste0(
    "figures/figure-sirf-of-response", colnames(s_irf$irf$ts_gdp_2019)[v], "-to-impulse_ts_gdp_2019.pdf"
  )
  
  # Save the last plot
  ggsave(figvarname, width = 11, height = 8.5)
}

#-------------------------------------------------------------------------------
# IRF - Exchange Rate 
#-------------------------------------------------------------------------------

# Compute the Structural IRF with bootstrapped standard errors
s_irf <- irf(
  var4_order, impulse = "ts_exch_rate_2019",
  n.ahead = 10, boot = TRUE, runs = 1000, ci=0.90,
  ortho = TRUE
)

#Loop over variables
for (v in 1:3) {
  # Construct a dataframe to plot the structural IRF
  temp <- data.frame(
    "period" = 0:10,
    "irf" = s_irf$irf$ts_exch_rate_2019[, v],
    "CI_U" = s_irf$Upper$ts_exch_rate_2019[, v],
    "CI_L" = s_irf$Lower$ts_exch_rate_2019[, v]
  )
  colnames(temp) <- c("period", "irf", "CI_U", "CI_L")
  
  # Define the label of the Y-axis
  #ylabel <- paste0("SIRF - Response: ", colnames(s_irf$irf$ts_gdp_2019)[v])
  
  # Plot the IRF
  gg <- ggplot(data = temp, aes(x = period)) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Year") + ylab("SIRF") +
    geom_ribbon(
      aes(ymin = CI_L, ymax = CI_U), fill = "grey80"
    ) + geom_hline(yintercept = 0, color = "black") +
    geom_line(
      aes(y = irf), size = 1.5, color = "red"
    )
  print(gg)
  
  # Define the name of the figure
  figvarname <- paste0(
    "figures/figure-sirf-response-of-", colnames(s_irf$irf$ts_exch_rate_2019)[v], "-to-impulse-ts_exch_rate_2019.pdf"
  )
  
  # Save the last plot
  ggsave(figvarname, width = 11, height = 8.5)
}

#-------------------------------------------------------------------------------
# IRF - IPC 
#-------------------------------------------------------------------------------

# Compute the Structural IRF with bootstrapped standard errors
s_irf <- irf(
  var4_order, impulse = "ts_ipc_2019",
  n.ahead = 10, boot = TRUE, runs = 1000, ci=0.90,
  ortho = TRUE
)

#Loop over variables
for (v in 1:3) {
  # Construct a dataframe to plot the structural IRF
  temp <- data.frame(
    "period" = 0:10,
    "irf" = s_irf$irf$ts_ipc_2019[, v],
    "CI_U" = s_irf$Upper$ts_ipc_2019[, v],
    "CI_L" = s_irf$Lower$ts_ipc_2019[, v]
  )
  colnames(temp) <- c("period", "irf", "CI_U", "CI_L")
  
  # Define the label of the Y-axis
  #ylabel <- paste0("SIRF - Response: ", colnames(s_irf$irf$ts_gdp_2019)[v])
  
  # Plot the IRF
  gg <- ggplot(data = temp, aes(x = period)) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Year") + ylab("SIRF") +
    geom_ribbon(
      aes(ymin = CI_L, ymax = CI_U), fill = "grey80"
    ) + geom_hline(yintercept = 0, color = "black") +
    geom_line(
      aes(y = irf), size = 1.5, color = "red"
    )
  print(gg)
  
  # Define the name of the figure
  figvarname <- paste0(
    "figures/figure-sirf-response-of-", colnames(s_irf$irf$ts_ipc_2019)[v], "-to-impulse-ts_ipc_2019.pdf"
  )
  
  # Save the last plot
  ggsave(figvarname, width = 11, height = 8.5)
}
