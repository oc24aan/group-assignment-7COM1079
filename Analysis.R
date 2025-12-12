# R codes used for analysis and visualization 

library(readr)
df <- read_csv("london_weather.csv")

# create year and month column
df$year <- as.numeric(substr(as.character(df$date), 1, 4))
df$month <- as.numeric(substr(as.character(df$date), 5, 6))

df$season <- "Autumn"
df$season[df$month %in% c(12, 1, 2)] <- "Winter"
df$season[df$month %in% c(3, 4, 5)] <- "Spring"
df$season[df$month %in% c(6, 7, 8)] <- "Summer"

df$season_year <- paste(df$season, df$year)

# Clear NA from the data :

sum(is.na(df$date))   
sum(is.na(df$precipitation)) #We have 6 NA 

df <- df[!is.na(df$date) & !is.na(df$precipitation), ]
sum(is.na(df$precipitation))

View(df)

# Histogram 

hist(df$precipitation[df$season == "Winter"], 
     main = "Histogram of Daily Precipitation in Winter", 
     xlab = "Precipitation (mm)", 
     ylab = "Frequency",
     col = "lightgray", 
     breaks = 50,
     las = 1,
)

hist(df$precipitation[df$season == "Spring"], 
     main = "Histogram of Daily Precipitation in Spring", 
     xlab = "Precipitation (mm)", 
     ylab = "Frequency",
     col = "lightgreen", 
     breaks = 50,
     las = 1,
)

hist(df$precipitation[df$season == "Summer"], 
     main = "Histogram of Daily Precipitation in Summer", 
     xlab = "Precipitation (mm)", 
     ylab = "Frequency",
     col = "orange", 
     breaks = 50,
     las = 1,
)

hist(df$precipitation[df$season == "Autumn"], 
     main = "Histogram of Daily Precipitation in Autumn", 
     xlab = "Precipitation (mm)", 
     ylab = "Frequency",
     col = "lightblue", 
     breaks = 50,
     las = 1,
)

# histogram with curve Figure 1 

png("seasonal_precipitation_histogram_with_curves.png",width = 800, height = 600)

h <- hist(df$precipitation, 
          main = "Histogram of Daily Precipitation in London by Season from 1979 to 2020", 
          xlab = "Precipitation (mm)", 
          ylab = "Frequency",
          col = "cornflowerblue", 
          breaks = 50,
          xlim = c(0, 70),
          ylim = c(0, 12000),
          las = 1
)

# Normal Curve with proper scale

x_lines <- seq(min(df$precipitation), 70, length = 100)
y_lines <- dnorm(x_lines, mean = mean(df$precipitation), sd = sd(df$precipitation))
bin_width <- h$breaks[2] - h$breaks[1]
y_scaled <- y_lines * length(df$precipitation) * bin_width
lines(x_lines, y_scaled, col = "red", lwd = 2)

dev.off()

# box plot Figure 2

png("box_plot.png",width = 800, height = 600)

boxplot(precipitation ~ season, data = df,
        main = "Daily Precipitation in London by Season from 1979 to 2020",
        xlab = "Season",
        ylab = "Precipitation (mm)",
        col = c("orange", "lightgreen", "lightblue", "lightgray"),
        border = "black")

dev.off()

# Table 1

tapply(df$precipitation, df$season, summary)

# Pairwise Wilcoxon Test

pairwise.wilcox.test(df$precipitation, df$season, p.adjust.method = "holm")
 


