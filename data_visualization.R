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

# 03_scatter_area_vs_price.R
library(ggplot2); library(scales)
df <- read_csv("cleaned_data.csv")
df_plot <- df |>
  filter(price_in_USD <= quantile(price_in_USD, .99),
         apartment_living_area_sqm <= quantile(apartment_living_area_sqm, .99))
r <- round(cor(df_plot$apartment_living_area_sqm, df_plot$price_in_USD/1e6), 2)

ggplot(df_plot, aes(apartment_living_area_sqm, price_in_USD/1e6)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(0, ceiling(max(df_plot$price_in_USD/1e6)/5)*5, 5),
                     labels = function(x) paste0(x, "M")) +
  labs(title = "Area vs Listed Price",
       subtitle = paste("Pearson r =", r, "(n =", nrow(df_plot), ", 99 % trim)")) +
  ggsave("figures/scatter_area_vs_price_5M_steps_trim99.png", width = 8, height = 5, dpi = 300)

# 04_hist_price_per_sqm_log.R
library(ggplot2); library(scales)
df <- read_csv("cleaned_data.csv") |>
  mutate(ppsqm = price_in_USD / apartment_living_area_sqm)

ggplot(df, aes(ppsqm)) +
  geom_histogram(bins = 60, color = "white") +
  scale_x_log10(labels = label_dollar()) +
  labs(title = "Price per sqm (log scale)") +
  ggsave("figures/hist_price_per_sqm_log.png", width = 8, height = 5, dpi = 300)
