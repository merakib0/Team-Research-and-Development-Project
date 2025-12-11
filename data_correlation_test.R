library(readr)

library(broom)

library(dplyr)

# Read data

df <- read_csv(

  "C:/Users/User/Downloads/team data/team 156/complete_project/members/member_A_data/cleaned_data.csv",

  show_col_types = FALSE

)

# Correlation tests

pearson <- cor.test(

  df$apartment_living_area_sqm,

  df$price_in_USD,

  method = "pearson"

spearman <- cor.test(

  df$apartment_living_area_sqm,

  df$price_in_USD,

  method = "spearman",

  exact = FALSE   # <- removes the "ties" warning

)

# Linear model

lm_model <- lm(price_in_USD ~ apartment_living_area_sqm, data = df)

# Output directory

out_dir <- "C:/Users/User/Downloads/team data/team 156/complete_project/members/member_C_stats"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Save results (only join folder + file name!)
writeLines(capture.output(pearson),
           file.path(out_dir, "pearson_test.txt"))

writeLines(capture.output(spearman),
           file.path(out_dir, "spearman_test.txt"))

writeLines(capture.output(summary(lm_model)),
           file.path(out_dir, "test_results.txt"))

# OPTIONAL: either remove this line...
# source("C:/Users/User/Downloads/team data/team 156/complete_project/member_C_script.R")
