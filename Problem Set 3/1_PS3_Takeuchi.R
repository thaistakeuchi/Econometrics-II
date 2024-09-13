#-------------------------------------------------------------------------------
# PS3 // Econometrics II // Problem 1 // Thais Takeuchi
# Instructor: Vitor Possebom
# The GOAL is to forecast Brazilian Annual GDP Growth in 2020
# This code was based on Matheus Junqueira's and classroom code
#-------------------------------------------------------------------------------
# Organize the working enviroment
#-------------------------------------------------------------------------------
# Cleaning the working environment
#-------------------------------------------------------------------------------

rm(list = ls())
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
## Load the required packages
#-------------------------------------------------------------------------------

packages <- c("ggplot2", "stargazer", "dplyr", "quantmod", "lmtest", "xtable", 
              "forecast", "ARDL", "dynlm", "sandwich", "vars", "data.table")

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
# data_2019 <- read.csv("C:/Users/thtak/Desktop/Econometria II/Problems Sets/Problem Set 3/data/data_brazil.csv")

#-------------------------------------------------------------------------------
## Changing variable names:
# date to year, real_gdp_growth_pct to gdp_growth (gdp growth is measured as % per year), 
# exchange_rate_real_dollar_annual_average to exch_rate and ipc_fipe_pct to ipc
#-------------------------------------------------------------------------------

colnames(dt) <- c("year", "gdp_growth", "exch_rate", "ipc")

dt_42to19 <- subset(dt, year >= 1942 & year <= 2019)

ts_gdp_2019 <- ts(dt_42to19$gdp_growth,
                         start = c(1942),
                         end = c(2019),
                         frequency = 1)

ts_exch_rate_2019 <- ts(dt_42to19$exch_rate,
                                 start = c(1942),
                                 end = c(2019),
                                 frequency = 1)

ts_ipc_2019 <- ts(dt_42to19$ipc,
                           start = c(1942),
                           end = c(2019),
                           frequency = 1)

ADLdt_42to19 <- ts.union(ts_gdp_2019, ts_exch_rate_2019, ts_ipc_2019)

ts_gdp <- ts(dt$gdp_growth,
                    start = c(1901),
                    end = c(2021),
                    frequency = 1)

#-------------------------------------------------------------------------------
# Estimate an ADL(2,1) - GDP growth as your dependent variable and Exchange 
# Rate as your predictor
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# ARDL 
#-------------------------------------------------------------------------------

model_gdp_exch_21 <- dynlm(ts_gdp_2019 ~ L(ts_gdp_2019) + L(ts_gdp_2019, 2) +
                                 L(ts_exch_rate_2019))


#-------------------------------------------------------------------------------
# Coefficient - Summary
#-------------------------------------------------------------------------------

summary(model_gdp_exch_21)

coef_model_gdp_exch_21 <- coeftest(model_gdp_exch_21, vcov. = sandwich)
print(coef_model_gdp_exch_21)

latex_table <- stargazer(coef_model_gdp_exch_21, type = "latex")

cat(latex_table)
writeLines(latex_table, "figures/table_summary_ADL_21_exchange_gdp.txt")

#-------------------------------------------------------------------------------
# Forecast
#-------------------------------------------------------------------------------

tail_2 <- tail(dt_42to19, 2)
matrix_tail_2 <- as.matrix(tail_2)
matrix_tail_2 <- matrix_tail_2[, -1]

model_gdp_exch_21_forecast <- coef(model_gdp_exch_21) %*% c(1, matrix_tail_2[2,1], matrix_tail_2[1,1], matrix_tail_2[2,2])

model_gdp_exch_21_forecast

window(ts_gdp, c(2020, 1), c(2020, 1)) - model_gdp_exch_21_forecast

#-------------------------------------------------------------------------------
# Estimate an ADL(2,2) - GDP growth as your dependent variable and Inflation 
# as your predictor
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# ARDL 
#-------------------------------------------------------------------------------

model_gdp_ipc_22 <- dynlm(ts_gdp_2019 ~ L(ts_gdp_2019) + L(ts_gdp_2019, 2) +
                            L(ts_ipc_2019) + L(ts_ipc_2019, 2))

#-------------------------------------------------------------------------------
# Coefficient - Summary
#-------------------------------------------------------------------------------

summary(model_gdp_ipc_22)

coef_model_gdp_ipc_22 <- coeftest(model_gdp_ipc_22, vcov. = sandwich)
print(coef_model_gdp_ipc_22)

latex_table <- stargazer(coef_model_gdp_ipc_22, type = "latex")

cat(latex_table)
writeLines(latex_table, "figures/table_summary_ADL_22_ipc_gdp.txt")
#-------------------------------------------------------------------------------
# Forecast
#-------------------------------------------------------------------------------

tail_2 <- tail(dt_42to19, 2)
matrix_tail_2 <- as.matrix(tail_2)
matrix_tail_2 <- matrix_tail_2[, -1]

model_gdp_ipc_22_forecast <- coef(model_gdp_ipc_22) %*% c(1, matrix_tail_2[2,1], matrix_tail_2[1,1], matrix_tail_2[2,3], matrix_tail_2[1,3])

model_gdp_ipc_22_forecast

window(ts_gdp, c(2020, 1), c(2020, 1)) - model_gdp_ipc_22_forecast

#-------------------------------------------------------------------------------
# Estimate an ADL(2,2,2) - GDP growth as your dependent variable and Inflation 
# and Exchange Rate as your predictor
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# ARDL 
#-------------------------------------------------------------------------------

model_3 <- dynlm(ts_gdp_2019 ~ L(ts_gdp_2019) + L(ts_gdp_2019, 2) +
                   L(ts_exch_rate_2019) + L(ts_exch_rate_2019, 2) + L(ts_ipc_2019) + L(ts_ipc_2019, 2))

#-------------------------------------------------------------------------------
# Coefficient - Summary
#-------------------------------------------------------------------------------

summary(model_3)

coef_model_3 <- coeftest(model_3, vcov. = sandwich)
print(coef_model_3)

latex_table <- stargazer(coef_model_3, type = "latex")

cat(latex_table)
writeLines(latex_table, "figures/table_summary_model_3.txt")

#-------------------------------------------------------------------------------
# Forecast
#-------------------------------------------------------------------------------

tail_2 <- tail(dt_42to19, 2)
matrix_tail_2 <- as.matrix(tail_2)
matrix_tail_2 <- matrix_tail_2[, -1]

model_3_forecast <- coef(model_3) %*% c(1, matrix_tail_2[2,1], matrix_tail_2[1,1], matrix_tail_2[2,2], 
                                        matrix_tail_2[1,2], matrix_tail_2[2,3], matrix_tail_2[1,3])

model_3_forecast

window(ts_gdp, c(2020, 1), c(2020, 1)) - model_3_forecast

#-------------------------------------------------------------------------------
# Estimate an ARMA(2,0) - GDP growth as your dependent
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# ARMA(2,0) 
#-------------------------------------------------------------------------------

model_4 <- dynlm(ts_gdp_2019 ~ L(ts_gdp_2019) + L(ts_gdp_2019, 2))

#-------------------------------------------------------------------------------
# Coefficient - Summary
#-------------------------------------------------------------------------------

summary(model_4)

coef_model_4 <- coeftest(model_4, vcov. = sandwich)
print(coef_model_4)

latex_table <- stargazer(coef_model_4, type = "latex")

cat(latex_table)
writeLines(latex_table, "figures/table_summary_model_4.txt")

#-------------------------------------------------------------------------------
# Forecast
#-------------------------------------------------------------------------------

tail_2 <- tail(dt_42to19, 2)
matrix_tail_2 <- as.matrix(tail_2)
matrix_tail_2 <- matrix_tail_2[, -1]

model_4_forecast <- coef(model_4) %*% c(1, matrix_tail_2[2,1], matrix_tail_2[1,1])

model_4_forecast

window(ts_gdp, c(2020, 1), c(2020, 1)) - model_4_forecast

