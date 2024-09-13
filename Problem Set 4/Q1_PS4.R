#PS4// Econometrics II// Thais Takeuchi // This code was based on the classroom code
# Instructor: Vitor Possebom

rm(list = ls())

#-------------------------------------------------------------------------------
#Load packages

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
#-------------------------------------------------------------------------------
## Question 1.1 - Subset the data to cover only the analyzed period (1995-2020).
# Filter data for the specified date range
start_date <- ymd("1995-01-01")
end_date <- ymd("2020-01-01")
data_brazil <- data_brazil[date >= start_date & date < end_date]
data_usa <- data_usa[date >= start_date & date < end_date]


# Print the first/last few rows of the filtered data frames
print(head(data_brazil))
print(head(data_usa))
print(tail(data_brazil))
print(tail(data_usa))

#-------------------------------------------------------------------------------
# Question 1.2 - For each variable X_{k,t}, k \in {1,2,3} in the dataset, define Y_{k,t} := 100·[log (X{k,t}) − log (X_{k,January1995})]
# X_{1,t} = ipca
# X_{2,t} = exchange rate
# X_{3,t} = cpi

# It creates two new columns, "y_ipca" and "y_exchange_rate", in data_brazil. The
# values in these columns are calculated based on the "ipca" and "exchange_rate"
# columns using the formula above. The formula computes the percentage change
# between the current value and the value at the reference date ("1995-01-01"),
# and multiplies it by 100. The result is assigned to the respective new columns.
base_date <- ymd("1995-01-01")

data_brazil[, `:=` (
  y_ipca = 100 * (log(ipca) - log(ipca[date == base_date])),
  y_exchange_rate = 100 * (log(exchange_rate) - log(exchange_rate[date == base_date]))
)]

# It creates a new column, "y_cpi", in data_usa. The values in this column are
# calculated based on the "cpi" column using the formula above.
# The formula computes the percentage change between the current value and the
# value at the reference date ("1995-01-01"), and multiplies it by 100. The
# result is assigned to the "y_cpi" column.
data_usa[, y_cpi := 100*(log(cpi) - log(cpi[date == base_date]))]

data_merged <- merge(data_brazil, data_usa, by = "date") # Merge data_brazil and data_usa

#-------------------------------------------------------------------------------
# Question 1.4 - Define Z_t = a^{\prime}Y_t, where Y_t = (Y_{1,t}; Y_{2,t}; Y_{3,t})^{\prime}
# It adds a new column "z" to the data_merged and assigns it the calculated values of the
# expression y_ipca - y_exchange_rate - y_cpi
data_merged[, z := y_ipca - y_exchange_rate - y_cpi]

#-------------------------------------------------------------------------------
# Question 1.5 - Plot the data for X_{k,t}, k \in {1,2,3}
# X_{1,t} = ipca
ggplot(data = data_merged, aes(x = date)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Month") + ylab("IPCA") +
  geom_line(
    aes(y = y_ipca), size = 1.5, color = "#661100"
  ) +
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y")

ggsave("figures/Y_ipca.png", width = 11, height = 8.5)

# X_{2,t} = exchange rate
ggplot(data = data_merged, aes(x = date)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Month") + ylab("Exchange Rate") +
  geom_line(
    aes(y = y_exchange_rate, group = 1), size = 1.5, color = "#661100"
  ) +
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y")

ggsave("figures/Y_exchange_rate.png", width = 11, height = 8.5)

ggplot(data = data_merged, aes(x = date)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Month") + ylab("CPI") +
  geom_line(
    aes(y = y_cpi), size = 1.5, color = "#661100"
  ) +
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y")

ggsave("figures/Y_cpi.png", width = 11, height = 8.5)

# Question 1.7 - Plot the data for Z_t
ggplot(data = data_merged, aes(x = date)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Month") + ylab("Z_t") +
  geom_line(
    aes(y = z), size = 1.5, color = "#661100"
  ) +
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y")

ggsave("figures/Y_z.png", width = 11, height = 8.5)

#-------------------------------------------------------------------------------
# Question 1.6 - Using ADF test, test whether your Y_{k,t} variables are each
# individually I(1). Be clear about the specification of your ADF test and about
# your null hypothesis, explaining how you choose the number of lags and your null
# hypothesis.

# Define the variables
variables <- c("y_ipca", "y_exchange_rate", "y_cpi", "z")

# Perform ADF test for each variable and create tables
for (variable in variables) {
  adf_result <- ur.df(
    y = data_merged[[variable]],
    type = "trend",
    lags = 12,
    selectlags = "BIC"
  )

  adf_table <- data.table(
    variable = variable,
    test = colnames(adf_result@teststat),
    statistic = adf_result@teststat[1, ],
    cval_1pct = adf_result@cval[, "1pct"],
    cval_5pct = adf_result@cval[, "5pct"],
    cval_10pct = adf_result@cval[, "10pct"]
  )

  adf_table[
    ,
    rejection := case_when(
      abs(statistic) > abs(cval_1pct) ~ "***",
      abs(statistic) > abs(cval_5pct) ~ "**",
      abs(statistic) > abs(cval_10pct) ~ "*",
      TRUE ~ NA_character_
    )
  ]

  adf_table[
    ,
    test := case_when(
      test == "tau3" ~ "$\\tau3$",
      test == "phi2" ~ "$\\phi2$",
      TRUE ~ "$\\phi3$"
    )
  ]

  setnames(
    adf_table,
    new = c(
      "Variable", "Test", "Statistic", "CV (1\\%)", "CV (5\\%)",
      "CV (10\\%)", "Rejection"
    )
  )

  temp_tex <- xtable(adf_table)
  cat(
    capture.output(print(temp_tex, include.rownames = FALSE, booktabs = TRUE, sanitize.colnames.function = identity, sanitize.text.function = identity)),
    "\n",
    file = paste0("tables/table_adf-test-", variable, ".txt"),
    append = TRUE
  )
}

