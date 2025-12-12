# 01_repo_setup.R
# Creates shared folder once
dir.create("figures", showWarnings = FALSE)
message(" figures/ folder ready")

# 02_load_clean_data.R
# Produces cleaned_data.csv for everyone
library(readr); library(dplyr)
df <- read_csv("member_A_data.csv") |>
  mutate(across(c(apartment_living_area_sqm, price_in_USD), as.numeric)) |>
  filter(if_all(c(apartment_living_area_sqm, price_in_USD),
                ~ is.finite(.) && . > 0))
write_csv(df, "cleaned_data.csv")
message("cleaned_data.csv written")
