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


# 3. Compute Pearson correlation (for subtitle) ------------------------------

corr_value <- cor(
  df_plot$apartment_living_area_sqm,
  df_plot$price_million,
  method = "pearson",
  use = "complete.obs"
)

corr_label <- paste0("Pearson r = ", round(corr_value, 2))

# 4. Create visualisation (5 million USD step on y-axis) ---------------------

max_price_m <- max(df_plot$price_million, na.rm = TRUE)
upper_break  <- ceiling(max_price_m / 5) * 5   # next multiple of 5 million

p_corr <- ggplot(
  df_plot,
  aes(x = apartment_living_area_sqm, y = price_million)
) +
  geom_point(alpha = 0.25, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "black") +
  scale_y_continuous(
    name   = "Listed price (million USD)",
    breaks = seq(0, upper_break, by = 5),       # 5 million USD steps
    labels = label_number(accuracy = 1, suffix = "M")
  ) +
  scale_x_continuous(
    name   = "Apartment living area (sqm)",
    breaks = pretty(df_plot$apartment_living_area_sqm)
  ) +
  labs(
    title    = "Relationship between apartment living area and listed price",
    subtitle = corr_label
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(margin = margin(b = 8))
  )

# 5. Save figure -------------------------------------------------------------

out_dir <- "C:/Users/User/Downloads/team data/team 156/complete_project/members/member_B_viz/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(out_dir, "price_vs_living_area_5M_steps.png"),
  plot     = p_corr,
  width    = 8,
  height   = 5,
  dpi      = 300
)

