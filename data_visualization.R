# -------------------------------------------------------------------
# Visualising correlation: apartment living area vs listed price
# -------------------------------------------------------------------

# Install & load packages ----------------------------------------------------
if (!require(ggplot2)) install.packages("ggplot2")

library(readr)
library(ggplot2)
library(dplyr)
library(scales)

# 1. Load data ---------------------------------------------------------------

df <- read_csv(
  "C:/Users/User/Downloads/team data/team 156/complete_project/members/member_A_data/cleaned_data.csv",
  show_col_types = FALSE
)

# 2. Prepare data for a clearer plot -----------------------------------------
# - Remove missing values
# - Express price in millions of USD
# - Trim extreme outliers (top 1%) in both variables

df_plot <- df |>
  filter(
    !is.na(apartment_living_area_sqm),
    !is.na(price_in_USD)
  ) |>
  mutate(
    price_million = price_in_USD / 1e6
  )

p99_price <- quantile(df_plot$price_million, 0.99, na.rm = TRUE)
p99_area  <- quantile(df_plot$apartment_living_area_sqm, 0.99, na.rm = TRUE)

df_plot <- df_plot |>
  filter(
    price_million <= p99_price,
    apartment_living_area_sqm <= p99_area
  )
