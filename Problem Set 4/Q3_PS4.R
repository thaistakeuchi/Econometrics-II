#PS4// Econometrics II// Thais Takeuchi // This code was based on the classroom code
# Instructor: Vitor Possebom
#Goal:Analyzing cointegrated variables in R
rm(list = ls())

#-------------------------------------------------------------------------------
#Load packages

packages <- c("ggplot2", "urca", "vars", "zoo", "stargazer", "data.table",
              "lubridate", "magrittr", "tidyverse", "forecast")

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}
#-------------------------------------------------------------------------------
## Working with the data

# Any column identified as "character" should be treated as a date column ("Data")
data_brazil <- fread("data/brazil_data.csv", colClasses = list(character = "Data"))

# Any column identified as "character" should be treated as a date column ("DATE")
data_usa <- fread("data/usa_data.csv", colClasses = list(character = "DATE"))

data_brazil <- data_brazil[, -4] # The column "V4" was removed from the 'data_brazil' object

# Rename variable names

# Retrieve current column names
col_names_brazil <- colnames(data_brazil)
col_names_usa <- colnames(data_usa)

# Assign new names to desired columns
col_names_brazil[1:3] <- c("date", "exchange_rate", "ipca")
col_names_usa[1:2] <- c("date", "cpi")

# Set the updated column names
colnames(data_brazil) <- col_names_brazil
colnames(data_usa) <- col_names_usa

# Convert the date column to Date format
data_brazil <- data_brazil %>%
  mutate(date = gsub("\\.", "-", date)) %>%              # Replace dots with dashes in the date column
  mutate(date = paste(date, "01", sep = "-")) %>%       # Append "01" to the values in the date column
  mutate(date = ymd(date))                              # Convert the values in the date column to Date format

data_usa$date <- ymd(data_usa$date)

start_date <- ymd("1995-01-01")
end_date <- ymd("2020-01-01")
data_brazil <- data_brazil[date >= start_date & date < end_date]
data_usa <- data_usa[date >= start_date & date < end_date]

base_date <- ymd("1995-01-01")

data_brazil[, `:=` (
  y_ipca = 100 * (log(ipca) - log(ipca[date == base_date])),
  y_exchange_rate = 100 * (log(exchange_rate) - log(exchange_rate[date == base_date]))
)]

data_usa[, y_cpi := 100*(log(cpi) - log(cpi[date == base_date]))]

data_merged <- merge(data_brazil, data_usa, by = "date") # Merge data_brazil and data_usa

# We re-order the variables so that they match a similar order to the variables
# in our VAR example in Lecture 4B.
data_brazil <- data_brazil[, c("date", "y_exchange_rate", "y_ipca")]
data_usa <- data_usa[, c("date", "y_cpi")]

#Merge data_brazil_log and data_usa_log
data_merged <- merge(data_brazil, data_usa, by = "date") # Merge data_brazil and data_usa
data_merged <- data_merged[, c("date", "y_exchange_rate", "y_ipca", "y_cpi")]

#Remove column "date", "ipca", "exchange_rate" and "cpi"
data_merged[, date := NULL]
data_merged[, ipca := NULL]
data_merged[, exchange_rate := NULL]
data_merged[, cpi := NULL]

#Reordering the variables again
data_merged <- data_merged[, c("y_exchange_rate", "y_ipca", "y_cpi")]
#-------------------------------------------------------------------------------
# Plotting the data
#-------------------------------------------------------------------------------
# Plotting the data
for (v in 1:3) {
  # Plot the variable
  gg <- ggplot(data_merged, aes(x = 1:nrow(data_merged))) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Month") +
    ylab(colnames(data_merged)[v]) +
    geom_line(aes(y = .data[[colnames(data_merged)[v]]]),
              size = 1.5, color = "#CC6677")
  print(gg)

  # Create a name for the file
  namefig <- paste0("figures/raw_", colnames(data_merged)[v], ".pdf")

  # Save the plot
  ggsave(namefig, width = 11, height = 8.5)
}

#-------------------------------------------------------------------------------
# cpi: Testing for a unit root
#-------------------------------------------------------------------------------
# Run the Augmented Dickey-Fuller Test: Since our plot suggest that there is a
# trend, we use the largest version of the test. Since we have montly data,
# we use 12 lags.
df_y_cpi <- ur.df(
  y = data_merged$y_cpi,
  type = 'trend',
  lags = 12
)

# Report the test
print(summary(df_y_cpi))

# The phi2 test statistic is equal to 6.46 while its 5% critical value is 4.71.
# Hence, we reject the phi2 null. The phi3 test statistic is equal to 1.63
# while its 5% critical value is 6.30. Hence, we do not reject the phi3 null.
# This result suggests that this variable follows a unit root process with
# drift.

#-------------------------------------------------------------------------------
# ipca: Testing for a unit root
#-------------------------------------------------------------------------------
# Run the Augmented Dickey-Fuller Test: Since our plot suggest that there is a
# trend, we use the largest version of the test. Since we have montly data,
# we use 12 lags.
df_y_ipca <- ur.df(
  y = data_merged$y_ipca,
  type = 'trend',
  lags = 12
)

# Report the test
print(summary(df_y_ipca))

# The phi2 test statistic is equal to 6.41 while its 5% critical value is 4.71.
# Hence, we reject the phi2 null. The phi3 test statistic is equal to 2.29
# while its 5% critical value is 6.30. Hence, we do not reject the phi3 null.
# This result suggests that this variable follows a unit root process with
# drift.

#-------------------------------------------------------------------------------
# exchange_rate: Testing for a unit root
#-------------------------------------------------------------------------------
# Run the Augmented Dickey-Fuller Test: Since our plot suggest that there is a
# trend, we use the largest version of the test. Since we have montly data,
# we use 12 lags.
df_y_exchange_rate <- ur.df(
  y = data_merged$y_exchange_rate,
  type = "trend",
  lags = 12
)

# Report the test
print(summary(df_y_exchange_rate))

# The phi2 test statistic is equal to 1.95 while its 5% critical value is 4.71.
# Hence, we do not reject the phi2 null. This result suggests that this variable
# follows a unit root process without a drift nor a trend.

# Since all variables are I(1) processes individually, we can proceed to testing
# for cointegration.

#-------------------------------------------------------------------------------
# Testing for cointegration
#-------------------------------------------------------------------------------
# Johansen's test: Instead of using the specification used by Johansen and
# Juselius (1990), we control for more lags instead of controlling directly
# for dummy variables.
vecm1 <- ca.jo(
  x = data_merged,
  ecdet = "none",
  type  = "eigen",
  K = 12,
  spec = "transitory"
)

# Extract the relevant information from the johansen_result object
test_statistics <- vecm1@teststat
critical_values <- vecm1@cval
r = c(2,1,0)
# Create a table
table_jo <- cbind(Test_Statistic = test_statistics, Critical_Values = critical_values)

# Print the table
print(table_jo)

# Convert the table_jo object to an xtable object
latex_table <- xtable(table_jo)

# Print the LaTeX code for the table
print(latex_table, type = "latex")


# Explanation:
# x: Variables that we care about.
# ecdet: Since most macroeconomic model suggest that those variables have a
# stable long-run equilibrium, we set this option to be "none".
# type: There are two tests for cointegration: "eigen" and "trace". In class,
# we only saw the test "eigen".
# K: Since we have montly data, we set our model to be a VAR(12)
# spec: There are two possible specification for the VECM. In class, we only
# discussed the transitory normalization.

# We start by testing the null hypothesis of no cointegrating relation (r = 0)
# against the alternative that there is at least one cointegrating relation.
# Our test statistic is 56.69 and our 5% critical value is 27.14. Consequently,
# we reject the null of no cointegration.

# Now, we test that we have only one cointegrating relation (r = 1) against the
# alternative that there are at least two cointegrating relations.
# Our test statistic is 15.38 and our 5% critical value is 21.07. Consequently,
# we do not reject the null that there is only one cointegrating relation.

