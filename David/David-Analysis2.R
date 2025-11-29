library(readr)
df<- read_csv("london_weather.csv")
df$year<- as.numeric(substr(as.character(df$date),1,4))
df$month<- as.numeric(substr(as.character(df$date),5,6))
df$season[df$month %in% c(3,4,5)]<- "Spring"
df$season[df$month %in% c(6,7,8)]<- "Summer"
df$season[df$month %in% c(9,10,11)]<- "Autumn"
df$season[df$month %in% c(12,1,2)]<- "Winter"
df$season <- factor(df$season,
                    levels = c("Spring","Summer","Autumn","Winter"))

png("Box_plot.png")  

boxplot(df$precipitation ~ df$season, 
        xlab = "seasons",
        ylab = "Daily precipitation(mm)",
        main = "The Boxplot Diagram")

dev.off()



#R script for Histogram


y<-df$precipitation
h<- hist(y,     #variable to count
         40,    #number of bars
         
         main = "Histogram of daily precipitation(London weather)",  #Title of chart
         
         xlab = "Daily Precipitation(mm)",  #x-axis label
         ylab = "Frequency",                #y-axis label
         col = "skyblue"                   #fill color for the bars
)

#R script for Overlay
mn<-mean(y) #mean of the data (precipitation)
stdD<-sd(y) #standard deviation of the data
x<-seq(0.000,61.800) #x-axis points
y1<-dnorm(x,mean=mn,sd=stdD) # an idealized normal curve
y1<-y1*diff(h$mids[1:2]) * length(y)  #calculate the curve against actual lines
lines(x, y1,col ="black")  #plot calibrated line in black


