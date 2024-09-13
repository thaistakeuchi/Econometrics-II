#PS4// Econometrics II// Thais Takeuchi // This code was based on the classroom code
# Instructor: Vitor Possebom
#Goal: We investigate whether there is any cointegrating relation between our
# three variables, hence we use the Phillips-Ouliaris-Hansen test.

#-------------------------------------------------------------------------------
# Organize the working enviroment
#-------------------------------------------------------------------------------
# Cleaning the working environment
#-------------------------------------------------------------------------------

rm(list = ls())

#-------------------------------------------------------------------------------
## Load the required packages
#-------------------------------------------------------------------------------
packages <- c("ggplot2", "stargazer", "tidyverse", "quantmod", "lmtest",
              "xtable", "forecast", "ARDL", "dynlm", "sandwich", "vars",
              "data.table", "zoo", "mFilter", "lubridate", "stringr", "zoo",
              "urca")

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

data_merged[, z := y_ipca - y_exchange_rate - y_cpi]

#-------------------------------------------------------------------------------
## OLS Equation - IPCA ~ Exchange Rate + CPI
#-------------------------------------------------------------------------------

# Estimating model 1 = m1
m1 <- lm(formula = y_ipca ~ y_exchange_rate + y_cpi, data = data_merged)

summary(m1)

# Cointegration Vector
covec_m1 <- c(1, -m1$coefficients[2], -m1$coefficients[3])

# Residuals Vector
resvec_m1 <- residuals(m1)

plot(resvec_m1)

# ADF test on Residuals Vector

adf_m1 <- ur.df(
  y = resvec_m1,
  type = "none",
  selectlags = "BIC"
)

#-------------------------------------------------------------------------------
# The test statistic is tau1 in the slides. Its critical values are reported at the bottom.
#-------------------------------------------------------------------------------
print(summary(adf_m1))

#-------------------------------------------------------------------------------
# Creating an array that receives the results
#-------------------------------------------------------------------------------

summary_adf <- summary(adf_m1)
adf_matrix <- matrix(NA, nrow = 1, ncol = 4)
colnames(adf_matrix) <- c("Statistic","Critical Value (1%)", "Critical Value (5%)", "Critical Value (10%)")
row.names(adf_matrix) <- c("tau1")

adf_matrix[1,1] <- adf_m1@teststat[1,1]
adf_matrix[1,2] <- summary_adf@cval[1,1]
adf_matrix[1,3] <- summary_adf@cval[1,2]
adf_matrix[1,4] <- summary_adf@cval[1,3]

print(adf_matrix)

#-------------------------------------------------------------------------------
## It allows you to create well-formatted
## tables from R data frames or matrices, and export
## them directly in LaTeX code to be included in a LaTeX document.
#-------------------------------------------------------------------------------

table_latex <- xtable(
  adf_matrix,
  caption = "ADF Test no drift and no time trend",
  label = "ADF Test",
  align = rep("c", ncol(adf_matrix) + 1),
  digits = 3
)

print(table_latex, file = "tables/table_adf_test.txt")

#-------------------------------------------------------------------------------
## OLS Equation - Exchange Rate ~ IPCA + CPI
#-------------------------------------------------------------------------------

# Estimating
m2 <- lm(formula = y_exchange_rate ~ y_ipca + y_cpi, data = data_merged)

summary(m2)

# Cointegration Vector
covec_m2 <- c(1, -m2$coefficients[2], -m2$coefficients[3])

# Residuals Vector
resvec_m2 <- residuals(m2)

plot(resvec_m2)

# ADF test on Residuals Vector

adf_m2 <- ur.df(
  y = resvec_m2,
  type = "none",
  selectlags = "BIC"
)

#-------------------------------------------------------------------------------
# The test statistic is tau1 in the slides. Its critical values are reported at the bottom.
#-------------------------------------------------------------------------------
print(summary(adf_m2))

#-------------------------------------------------------------------------------
# Creating an array that receives the results
#-------------------------------------------------------------------------------

summary_adf_2 <- summary(adf_m2)
adf_matrix_2 <- matrix(NA, nrow = 1, ncol = 4)
colnames(adf_matrix_2) <- c("Statistic","Critical Value (1%)", "Critical Value (5%)", "Critical Value (10%)")
row.names(adf_matrix_2) <- c("tau1")

adf_matrix_2[1,1] <- adf_m2@teststat[1,1]
adf_matrix_2[1,2] <- summary_adf_2@cval[1,1]
adf_matrix_2[1,3] <- summary_adf_2@cval[1,2]
adf_matrix_2[1,4] <- summary_adf_2@cval[1,3]

print(adf_matrix_2)

#-------------------------------------------------------------------------------
## It allows you to create well-formatted
## tables from R data frames or matrices, and export
## them directly in LaTeX code to be included in a LaTeX document.
#-------------------------------------------------------------------------------

table_latex_2 <- xtable(
  adf_matrix_2,
  caption = "ADF Test no drift and no time trend",
  label = "ADF Test",
  align = rep("c", ncol(adf_matrix_2) + 1),
  digits = 3
)

print(table_latex_2, file = "tables/table_adf_test2.txt")

#-------------------------------------------------------------------------------
## OLS Equation - CPI ~ IPCA + Exchange Rate
#-------------------------------------------------------------------------------

# Estimating
m3 <- lm(formula = y_cpi ~ y_ipca + y_exchange_rate, data = data_merged)

summary(m3)

# Cointegration Vector
covec_m3 <- c(1, -m3$coefficients[2], -m3$coefficients[3])
print(covec_m3)

# Residuals Vector
resvec_m3 <- residuals(m3)

plot(resvec_m3)

# ADF test on Residuals Vector

adf_m3 <- ur.df(
  y = resvec_m3,
  type = "none",
  selectlags = "BIC"
)

#-------------------------------------------------------------------------------
# The test statistic is tau1 in the slides. Its critical values are reported at the bottom.
#-------------------------------------------------------------------------------
print(summary(adf_m3))

#-------------------------------------------------------------------------------
# Creating an array that receives the results
#-------------------------------------------------------------------------------

summary_adf_3 <- summary(adf_m3)
adf_matrix_3 <- matrix(NA, nrow = 1, ncol = 4)
colnames(adf_matrix_3) <- c("Statistic","Critical Value (1%)", "Critical Value (5%)", "Critical Value (10%)")
row.names(adf_matrix_3) <- c("tau1")

adf_matrix_3[1,1] <- adf_m3@teststat[1,1]
adf_matrix_3[1,2] <- summary_adf_3@cval[1,1]
adf_matrix_3[1,3] <- summary_adf_3@cval[1,2]
adf_matrix_3[1,4] <- summary_adf_3@cval[1,3]

print(adf_matrix_3)

#-------------------------------------------------------------------------------
## It allows you to create well-formatted
## tables from R data frames or matrices, and export
## them directly in LaTeX code to be included in a LaTeX document.
#-------------------------------------------------------------------------------

table_latex_3 <- xtable(
  adf_matrix_3,
  caption = "ADF Test no drift and no time trend",
  label = "ADF Test",
  align = rep("c", ncol(adf_matrix_2) + 1),
  digits = 3
)

print(table_latex_3, file = "tables/table_adf_test3.txt")

