# PS2 - exercise 4//Econometrics II - 2023// Based on the lecture's code
# Instructor: Vitor Possebom
# (2C class)//Goal: Testing for unit roots
# Thais Takeuchi
#-------------------------------------------------------------------------------
## Organize the working environment
#-------------------------------------------------------------------------------
# Clean the working environment
rm(list = ls())

# Load the required packages
packages <- c("data.table", "urca", "ggplot2", "scales", "lubridate", "dplyr",
              "xtable","bdsmatrix", "cli", "colorspace", "crayon", "digest",
              "fansi","ggplot2", "glue", "lmtest", "magrittr", "maxLik",
              "moments","pillar", "plm", "rbibutils", "RColorBrewer", "Rdpack",
              "rlang", "scales", "tibble", "vctrs", "withr", "zoo")

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Reading and renaming columns:
dt_corn <- fread("data/corn-production-land-us.csv")

old <- c("Entity", "Code", "Year", "Corn, area harvested (hectares)",
         "Corn production (tonnes)")
new <- c("entity", "code", "year", "area_harv", "corn_prod")

setnames(dt_corn, old, new)

# Use data between 1950-2021
dt_corn <- dt_corn[year >= 1950]

# Defining which Augmented Dickey-Fuller Test to Run
# Data visualization using ggplot2:
gg <- ggplot(dt_corn, aes(x = year)) +
  theme_bw(base_size = 25) + # this line applies a black and white theme to the
  # plot with a base font size of 25
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("") + ylab("Corn production (tonnes)") + #These lines set the x-axis label to an empty string
  #(no label) and the y-axis label to "Corn production (tonnes)".
  scale_x_yearqtr(format = "%Y", n = 20) + # %Y: year, %q: quarter, n = 20:
  # limits the number of displayed labels to 20
  geom_line(aes(y = corn_prod), size = 1.5, color = "#F5793A") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #This line
# adjusts the appearance of the x-axis text by rotating it 90 degrees clockwise,
# aligning the text vertically in the middle (vjust = 0.5), and aligning it to the
# right side of the plot (hjust = 1).
print(gg)
ggsave("figures/figure_plot-corn-production.png", width = 11, height = 8.5)

#-------------------------------------------------------------------------------
# Augmented Dickey-Fuller Test: Drift and Time Trend
# Run the test using BIC to choose the number of lags
df_drift <- ur.df(
  y = dt_corn$corn_prod, # Vector to be tested for unit root
  type = 'trend',
  lags = 5, # Number of lags for endogenous variable to be included
  selectlags = c("BIC") # Lag selection can be achieved according to "BIC"
)

print(summary(df_drift))

