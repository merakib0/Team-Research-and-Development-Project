if (!require(ggplot2)) install.packages("ggplot2")

library(readr)

library(ggplot2)

library(scales)

df <- read_csv("C:/Users/User/Downloads/team data/team 156/complete_project/members/member_A_data/cleaned_data.csv", show_col_types = FALSE)

out_dir <- "C:/Users/User/Downloads/team data/team 156/complete_project/members/member_B_viz/figures"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

p1 <- ggplot(df, aes(x = apartment_living_area_sqm, y = price_in_USD)) +

  geom_point(alpha = 0.35, size = 1) +

  geom_smooth(method = "lm", se = TRUE, color = "black") +

  labs(title = "Apartment price (USD) vs Living area (sqm)",

       x = "Living area (sqm)", y = "Price (USD)") +

  theme_minimal()

ggsave(file.path(out_dir, "C:/Users/User/Downloads/team data/team 156/complete_project/members/member_B_viz/price_vs_area.png"), p1, width = 8, height = 5)

p3 <- ggplot(df, aes(x = price_per_sqm)) + geom_histogram(bins = 50) +

  labs(title = "Distribution of price per sqm", x = "Price per sqm (USD)", y = "Count") +

  theme_minimal()

ggsave(file.path(out_dir, "C:/Users/User/Downloads/team data/team 156/complete_project/members/member_B_viz/price_per_sqm_hist.png"), p3, width = 7, height = 4)
