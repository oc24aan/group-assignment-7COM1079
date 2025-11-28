# R codes used for analysis and visualization 

library(readr)
weather_df <- read_csv("london_weather.csv")
View(df)

weather_df$year <- as.numeric(substr(weather_df$date, 1, 4))
weather_df$month <- as.numeric(substr(weather_df$date, 5, 6))

weather_df$season <- ifelse(weather_df$month %in% c(12, 1, 2), "Winter",
                            ifelse(weather_df$month %in% c(3, 4, 5), "Spring",
                                   ifelse(weather_df$month %in% c(6, 7, 8), "Summer", "Autumn")))

weather_df <- weather_df[!is.na(weather_df$precipitation), ]
