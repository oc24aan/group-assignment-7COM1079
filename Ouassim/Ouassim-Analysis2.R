# R codes used for analysis and visualization 

# Load libraries
library(tidyverse)
library(lubridate)

# Read CSV file
df <- read_csv("london_weather.csv")

# Initial data inspection
glimpse(df)
summary(df)

# Convert 'date' from numeric to Date format
df$date <- as.Date(as.character(df$date), format = "%Y%m%d")
class(df$date)

# Create 'year' and 'month' columns for further analysis
df <- df %>%
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    ),
    season_year = paste(season, year)
  )

# Order seasons logically for plotting
df$season <- factor(df$season, levels = c("Winter", "Spring", "Summer", "Autumn"))

# Filter out rows with missing precipitation values
df <- df %>% filter(!is.na(precipitation))

# Plot 1: Histogram of overall daily precipitation with normal curve
mean_precip <- mean(df$precipitation, na.rm = TRUE)
sd_precip <- sd(df$precipitation, na.rm = TRUE)

p1 <- ggplot(df, aes(x = precipitation)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.7) +
  stat_function(fun = function(x) {
    dnorm(x, mean = mean_precip, sd = sd_precip) * nrow(df) * 1 # scale density to counts
  }, color = "red", size = 1) +
  labs(
    title = "Distribution of Daily Precipitation in London (1979–2020) with Normal Curve",
    x = "Daily Precipitation (mm)",
    y = "Frequency"
  ) +
  theme_minimal()

# Plot 2: Boxplot comparing precipitation by season with mean points
p2 <- ggplot(df, aes(x = season, y = precipitation, fill = season)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  labs(
    title = "Daily Precipitation by Season in London (1979–2020)",
    x = "Season",
    y = "Daily Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Calculate mean and median precipitation per season
season_stats <- df %>%
  group_by(season) %>%
  summarise(
    mean_precipitation = mean(precipitation, na.rm = TRUE),
    median_precipitation = median(precipitation, na.rm = TRUE)
  )
print(season_stats)

# Plot 3: Bar chart of mean precipitation per season
p3 <- season_stats %>%
  ggplot(aes(x = season, y = mean_precipitation, fill = season)) +
  geom_col(alpha = 0.8) +
  labs(
    title = "Mean Daily Precipitation by Season in London (1979–2020)",
    x = "Season",
    y = "Mean Daily Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Statistical tests: Wilcoxon pairwise tests

# 1. By season
wilcox_season <- pairwise.wilcox.test(df$precipitation, df$season, p.adjust.method = "holm")
print(wilcox_season$p.value)

# 2. By year (might be heavy if too many years)
wilcox_year <- pairwise.wilcox.test(df$precipitation, as.factor(df$year), p.adjust.method = "holm")
print(wilcox_year$p.value)

# 3. By season-year combination
wilcox_season_year <- pairwise.wilcox.test(df$precipitation, df$season_year, p.adjust.method = "holm")
print(wilcox_season_year$p.value)

# Create results directory if not exists
if(!dir.exists("results")) dir.create("results")

# Save p-values to CSV
write.csv(wilcox_season$p.value, "results/season_precipitation_pvalues.csv")
write.csv(wilcox_year$p.value, "results/year_precipitation_pvalues.csv")
write.csv(wilcox_season_year$p.value, "results/season_year_precipitation_pvalues.csv")

# Plot 4: Faceted histograms of precipitation by season
p4 <- ggplot(df, aes(x = precipitation)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~season) +
  labs(
    title = "Distribution of Daily Precipitation by Season",
    x = "Daily Precipitation (mm)",
    y = "Frequency"
  ) +
  theme_minimal()

# Save all plots
ggsave("results/histogram_precipitation_normal_curve.png", plot = p1, width = 8, height = 5)
ggsave("results/boxplot_precipitation_season.png", plot = p2, width = 8, height = 5)
ggsave("results/mean_precipitation_season.png", plot = p3, width = 8, height = 5)
ggsave("results/faceted_precipitation_histograms.png", plot = p4, width = 8, height = 5)

# Create and save separate histograms for each season
seasons <- levels(df$season)

for (s in seasons) {
  p <- ggplot(df %>% filter(season == s), aes(x = precipitation)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
    labs(
      title = paste("Histogram of Daily Precipitation in", s),
      x = "Daily Precipitation (mm)",
      y = "Frequency"
    ) +
    theme_minimal()
  
  ggsave(filename = paste0("results/precipitation_histogram_", tolower(s), ".png"),
         plot = p, width = 8, height = 5)
}
