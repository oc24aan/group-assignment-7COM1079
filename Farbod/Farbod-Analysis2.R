
df <- read.csv("london_weather.csv")


df$year <- as.numeric(substr(df$date, 1, 4))
df$month <- as.numeric(substr(df$date, 5, 6))


df$season <- "Autumn"
df$season[df$month %in% c(12,1,2)] <- "Winter"
df$season[df$month %in% c(3,4,5)] <- "Spring"
df$season[df$month %in% c(6,7,8)] <- "Summer"


df <- df[!is.na(df$precipitation), ]

# Pairwise Wilcoxon

pairwise_res <- pairwise.wilcox.test(df$precipitation, df$season,
                                     p.adjust.method = "holm")
print(pairwise_res)


# Boxplot

boxplot(precipitation ~ season, data = df,
        main = "Daily Precipitation in London by Season (1979–2020)",
        xlab = "Season",
        ylab = "Precipitation (mm)",
        col = c("orange","lightgreen","lightblue","lightgray"))


# Histogram + Density per Season

par(mfrow=c(2,2))
for(s in c("Winter","Spring","Summer","Autumn")){
  hist(df$precipitation[df$season==s],
       breaks=30,
       col="lightblue",
       main=paste("Histogram of Precipitation:", s),
       xlab="Precipitation (mm)",
       ylab="Frequency",
       freq=FALSE)
  lines(density(df$precipitation[df$season==s]), col="red", lwd=2)
}
par(mfrow=c(1,1))  # Reset


# Mean Precipitation Barplot

seasons <- c("Winter","Spring","Summer","Autumn")
means <- tapply(df$precipitation, df$season, mean)
ses   <- tapply(df$precipitation, df$season, function(x) sd(x)/sqrt(length(x)))

bar_centers <- barplot(means,
                       names.arg=seasons,
                       col=c("orange","lightgreen","lightblue","lightgray"),
                       ylim=c(0, max(means+ses)*1.1),
                       ylab="Mean Precipitation (mm)",
                       main="Mean Daily Precipitation by Season")

test_result <- pairwise.wilcox.test(df$precipitation, df$year, p.adjust.method = "holm")
y_p_value_table <- test_result$p.value
write.csv(y_p_value_table,"Farbod/p_values_years.csv")

test_result <- pairwise.wilcox.test(df$precipitation, df$season_year, p.adjust.method = "holm")
s_p_value_table <- test_result$p.value
write.csv(s_p_value_table, "Farbod/p_values_seasons.csv")

