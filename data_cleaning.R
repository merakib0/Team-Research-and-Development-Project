# A01_data_cleaning.R
# Purpose: robustly parse and clean world_real_estate_data.csv
# Outputs:
# - members/member_A_data/cleaned_data.csv
# - members/member_A_data/cleaning_report.txt

library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# Paths (adjust if running from different working dir)
raw_path <- "C:/Users/User/Downloads/team data/team 156/world_real_estate_data(147k).csv"
out_dir <- 'C:/Users/User/Downloads/team data/team 156/complete_project/members/member_A_data'
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
clean_path <- file.path(out_dir, "cleaned_data.csv")

# Read raw (try to auto-detect)
df_raw <- read_csv(raw_path, guess_max = 20000, show_col_types = FALSE, col_types = cols(.default = col_character()))

# Copy of original cols for reporting
orig_nrow <- nrow(df_raw)
orig_cols <- names(df_raw)

# previous code has an error I fixed it
# Helper to parse numeric from strings like "110 m²", "1,200", "1 200"
parse_num <- function(x) {
  x2 <- as.character(x)
  x2 <- str_replace_all(x2, "\\s+", "")        # remove spaces
  x2 <- str_replace_all(x2, ",", ".")          # comma -> dot
  # Extract the first valid number pattern to avoid coercion warnings
  num_str <- str_extract(x2, "-?\\d+\\.?\\d*")
  as.numeric(num_str)
}

df <- df_raw %>%
  mutate(
    apartment_living_area_raw = coalesce(apartment_living_area, location, NA_character_),
    apartment_living_area_sqm = parse_num(apartment_living_area_raw),
    price_in_USD = parse_num(price_in_USD),
    # fallback columns
    country = coalesce(country, location, NA_character_)
  )

# Report initial NA counts
na_before <- sapply(df[c("apartment_living_area_sqm","price_in_USD")], function(x) sum(is.na(x)))

# Handle obvious unit mismatches: if living area > 10000 it's probably in sq ft? flag it.

# We'll keep them but record outliers; do not convert unless clear mapping known.

df <- df %>%

  filter(!is.na(price_in_USD) & !is.na(apartment_living_area_sqm) & apartment_living_area_sqm > 0)

# Optionally remove extreme outliers (very small or huge area/price)

# We'll keep them but create flags so downstream decide; do not drop by default.

df <- df %>%

  mutate(

    area_flag_outlier = (apartment_living_area_sqm > 10000) | (apartment_living_area_sqm < 5),

    price_flag_outlier = (price_in_USD > 1e8) | (price_in_USD < 100)

  ) %>%

  mutate(price_per_sqm = price_in_USD / apartment_living_area_sqm)

# Keep only useful columns for analysis

df_out <- df %>%

  mutate(

    city = coalesce(location, NA_character_),

    apartment_bedrooms = coalesce(apartment_bedrooms, NA),

    apartment_bathrooms = coalesce(apartment_bathrooms, NA)

  ) %>%

  select(country, city, apartment_living_area_sqm,

         price_in_USD, price_per_sqm, apartment_bedrooms,

         apartment_bathrooms,

         area_flag_outlier, price_flag_outlier)

write_csv(df_out, clean_path)

# Write cleaning report

report_lines <- c(

  paste0("Cleaning report generated: ", Sys.time()),

  paste0("Original rows: ", orig_nrow),

  paste0("Original columns: ", paste(orig_cols, collapse = ", ")),

  "NA counts before parse (approx):",

  paste0("  apartment_living_area: ", na_before["apartment_living_area_sqm"]),

  paste0("  price_in_USD: ", na_before["price_in_USD"]),

  paste0("Rows after filtering missing area or price: ", nrow(df_out)),

  "Outlier flags summary:",

  paste0("  area_flag_outlier count: ", sum(df_out$area_flag_outlier, na.rm = TRUE)),

  paste0("  price_flag_outlier count: ", sum(df_out$price_flag_outlier, na.rm = TRUE)),

  "",

  "Notes:",

  "- apartment_living_area parsed by removing non-digit characters; commas treated as decimal separators.",

  "- price_in_USD parsed similarly.",

  "- Rows with missing price or area dropped (documented above).",

  "- Extreme values are flagged but not removed; please state handling decision in report.",

  ""

)

write_lines(report_lines, report_path)

cat("Cleaning finished.\nClean file saved to:", clean_path, "\nReport saved to:", report_path, "\n")
