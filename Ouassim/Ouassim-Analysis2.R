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
    )
  )

# Order seasons logically for plotting
df$season <- factor(df$season, levels = c("Winter", "Spring", "Summer", "Autumn"))

# Filter out rows with missing precipitation values
df <- df %>% filter(!is.na(precipitation))

# Create your results directory if it doesn't exist
dir.create("Ouassim/results", recursive = TRUE, showWarnings = FALSE)

# Plot 1: Histogram of overall daily precipitation
p1 <- ggplot(df, aes(x = precipitation)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Daily Precipitation in London (1979–2020)",
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

# Statistical test: Wilcoxon pairwise test to compare seasons
wilcox_test <- pairwise.wilcox.test(df$precipitation, df$season, p.adjust.method = "holm")
print(wilcox_test$p.value)

# Save p-values to CSV inside your folder
write.csv(wilcox_test$p.value, "Ouassim/results/season_precipitation_pvalues.csv")

# Plot 4 : Faceted histograms of precipitation by season
p4 <- ggplot(df, aes(x = precipitation)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~season) +
  labs(
    title = "Distribution of Daily Precipitation by Season",
    x = "Daily Precipitation (mm)",
    y = "Frequency"
  ) +
  theme_minimal()

# Save all plots inside your folder
ggsave("Ouassim/results/histogram_precipitation.png", plot = p1, width = 8, height = 5)
ggsave("Ouassim/results/boxplot_precipitation_season.png", plot = p2, width = 8, height = 5)
ggsave("Ouassim/results/mean_precipitation_season.png", plot = p3, width = 8, height = 5)
ggsave("Ouassim/results/faceted_precipitation_histograms.png", plot = p4, width = 8, height = 5)

# Create separate histograms for each season and save them individually inside your folder
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
  
  ggsave(filename = paste0("Ouassim/results/precipitation_histogram_", tolower(s), ".png"),
         plot = p, width = 8, height = 5)
}
