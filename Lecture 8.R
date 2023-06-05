library(ggplot2)
library(dplyr)
library(ggmap)


mvt <- read.csv("mvt.csv")
mvt <- na.omit(mvt)

str(mvt)
mvt$Date <- strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt <- mvt %>% mutate(Dow= factor(weekdays(Date)), Hour= Date$hour)

dow_counts <- as.data.frame(table(mvt$Dow))

mvt %>% ggplot(aes(Dow))+ geom_bar()

ggplot(dow_counts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

dow_counts$Var1 <- factor(dow_counts$Var1, ordered = T, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
str(dow_counts)

table(mvt$Hour, mvt$Dow)

dh_counts <- as.data.frame(table(Hour=mvt$Hour, Dow= mvt$Dow))

ggplot(dh_counts, aes(Hour, Dow)) +geom_tile(aes(fill=Freq)) +
  scale_fill_gradient(low="white", high= "red")


chi_borders <- c(bottom= min(mvt$Latitude), 
                 top= max(mvt$Latitude), 
                 left= min(mvt$Longitude), 
                  right= max(mvt$Longitude))
map <- get_stamenmap(chi_borders, zoom = 10, maptype = "terrain")
ggmap(map)

ggmap(map) +
  geom_point(data= mvt[1:100, ], aes(x=Longitude, y=Latitude), color="red")

grid <- as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
table(round(mvt$Longitude, 2), round(mvt$Latitude, 2))
str(grid)
grid <- grid %>% mutate(long= as.numeric(as.character(Var1)), lat=as.numeric(as.character(Var2)))


ggmap(map) +
  geom_point(data=grid %>% filter(Freq>0), aes(x=long, y=lat, color=Freq, size=Freq))+ 
  scale_color_gradient(low="yellow", high="red")


