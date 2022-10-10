#install packages
install.packages("tidyverse")    #to load the data
install.packages("ggplot2")      #to plot charts
install.packages("geosphere")    #to calculate the longitude and latitude
install.packages("dplyr")        #to load and read the data
install.packages("sp")           #for 2d and 3d data
install.packages("maps")         #to plot the map of Ghana
install.packages("rworldmap")    #to plot maps
install.packages("here")         #for easy file references      
install.packages("rnaturalearth") #to add internal borders
install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
install.packages("raster")       # to work with raster data   
install.packages("sf")           # to work with shape files
install.packages("viridis")      # for the colour scheme
install.packages("cartography")  # to help with the plotting of the maps
install.packages("tanaka")       # to make the Tanaka plots
install.packages("tmap")         # to make the static 2d maps
install.packages("SpatialPosition") # to calculate some spatial statistics
install.packages("leaflet")      # to make the interactive versions of the map
install.packages("rayshader")    # to make the 3D plot
install.packages("rayrender")    # to to render the 3D plot  
install.packages("tweenr")       # to interpolate data
install.packages("stringr")      # to work with text data
install.packages("scales")       # to create nice scales in the legend
install.packages("gdata")        # to delete objects from the environment
install.packages("zoo")          # to interpolate data
install.packages("showtext")     # to add fonts
install.packages("spData")
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
install.packages("grid")

library(tidyverse)    #to load the data
library(ggplot2)      #to plot charts
library(lubridate)    #to work with dates
library(geosphere)    #to calculate the longitude and latitude
library(dplyr)        #to load and read the data
library(sp)           #for 3d and 2d data
library(maps)         #to plot the map of Ghana
library(rworldmap)    #to plot maps
library(here)         #for easy file referencing
library(rnaturalearth)#to plot internal borders
library(raster)       # to work with raster data   
library(sf)           # to work with shape files
library(viridis)      # for the colour scheme
library(cartography)  # to help with the plotting of the maps
library(tanaka)       # to make the Tanaka plots
library(rworldmap)    # to get the shape file of Ghana
library(tidyverse)    # to do the data wrangling
library(tmap)         # to make the static 2d maps
library(SpatialPosition) # to calculate some spatial statistics
library(leaflet)      # to make the interactive versions of the map
library(rayshader)    # to make the 3D plot
library(rayrender)    # to to render the 3D plot  
library(tweenr)       # to interpolate data
library(stringr)      # to work with text data
library(scales)       # to create nice scales in the legend
library(lubridate)    # to work with dates
library(gdata)        # to delete objects from the environment
library(zoo)          # to interpolate data
library(showtext)     # to add fonts
library(spData)
library(spDataLarge)
library(grid)

#to load and read the data
tracking_df <- read.csv("~/Desktop/Swoove_work/Tracking_data/Trackers.csv") #this is the file path
str(tracking_df)
colnames(tracking_df)
head(tracking_df)
tail(tracking_df)
summary(tracking_df)

#check for distinct rider id
n_distinct(tracking_df$userId)

#first convert time to date time format 
tracking_df$Date_and_time <- as_datetime(tracking_df$time/1000)
tracking_df$date <- as_date(tracking_df$Date_and_time)


#I noticed the csv file became unsorted so I have to sort it out 
#but if it is already sorted out for whoever uses the code, you can just go ahead and calculate the distance
#I will also be arranging the file according to user id, then by date and time
tracking_df <- tracking_df[order(tracking_df$userId, tracking_df$Date_and_time),]

#to emsure the number format is not scientific
options(scipen=999)
getOption("digits")
options(digits = 2)
#create a boundary box(border) for the whole of ghana
long <- c(-2.7685546874999996, -2.9443359375, -2.57080078125, -3.27392578125, -2.7685546874999996,  
          -2.0654296875, 1.142578125, 0.5712890625, 0.41748046875, 0.3515625, -0.06591796875, -2.7685546874999996)
lat <- c(11.005904459659451, 10.660607953624776, 8.189742344383703, 6.5118147063479, 5.00339434502215,
         4.740675384778373, 6.053161295714067, 6.860985433763661, 9.492408153765544, 10.31491928581316, 11.113727282172755, 11.005904459659451)
ghana <- data.frame(long, lat)

#to ensure the cordinates fall within the borders of Ghana
tracking_df$existing <- point.in.polygon(tracking_df$longitude, tracking_df$latitude, ghana$long, ghana$lat)

#create a new data frame of the cordinates that are indeed in Ghana
tracks <- tracking_df %>% 
  filter(existing == 1)


#calculating the distance between the second point and the point before it
latitude <- tracks$latitude
longitude <- tracks$longitude
user_id <- tracks$userId
date <- tracks$date
dist <- data.frame(latitude, longitude,user_id, date)
distance_meters <- function(x) {
  y <- x - 1
  while (y > 0) {
    y <- x - 1
    if (all(dist$user_id[x] == dist$user_id[y]) & all(dist$date[x] == dist$date[y])) {
      meters <- distm(c(dist$longitude[x], dist$latitude[x]), c(dist$longitude[y], dist$latitude[y]), fun = distHaversine)
      return(abs(meters[1, 1]))
    } else {
      return(0)
    }
  }
  if (y == 0) {
    return(0)
  }
  if (y<0){
    return(0)
  }
}
tracks$distance_in_meters <- sapply(1:nrow(dist), distance_meters)


#to convert the distance from meters to kilometers
tracks$distance_in_km <- tracks$distance_in_meters/1000

#to calculating time difference of the time below and the one before it
time_chec <- tracks$Date_and_time
userId <- tracks$userId
date <- tracks$date
ti_di <- data.frame(time_chec, userId, date)

time_diff <- function(x) {
  y <- x - 1
  while (y > 0) {
    if (all(ti_di$userId[x] == ti_di$userId[y]) & all(ti_di$date[x] == ti_di$date[y])) {
      difference <- difftime(ti_di$time_chec[x],ti_di$time_chec[y],units = "secs")
      return(round(difference,digits = 1))
    } else {
      return(0)
    }
  }
  if (y == 0) {
    return(0)
  }
  if (y < 0) {
    return(0)
  }
}

tracks$time_diff_seconds <- sapply(1:nrow(ti_di), time_diff)

#to convert the time difference to munites
tracks$minutes <- tracks$time_diff_seconds/60

#to calculate the km each rider has riden
xaxis <- aggregate(tracks$distance_in_km ~ tracks$userId, FUN = sum)
yaxis <- aggregate(tracks$minutes ~ tracks$userId, FUN = sum)
axeses <- data.frame(yaxis, xaxis)

ggplot(data = axeses)+
  geom_point(mapping = aes(x= xaxis$`tracks$distance_in_km`, y = yaxis$`tracks$minute`))+
  geom_smooth(mapping = aes(x= xaxis$`tracks$distance_in_km`, y = yaxis$`tracks$minute`))+
  labs(title = "Relationship between the distance spent and the kilometers riden", x = "Distance in kilometers", y = "Time spent in minutes")

#to check the distance by each rider
xaxis <- aggregate(tracks$distance_in_km ~ tracks$userId, FUN = sum)
xaxis$`tracks$distance_in_km` <- round(xaxis$`tracks$distance_in_km`, digits = 2)
print(xaxis[order(-xaxis$`tracks$distance_in_km`),])

#to see the total time by each rider
yaxis <- aggregate(tracks$minutes ~ tracks$userId, FUN = sum)
yaxis$`tracks$minutes` <- round(yaxis$`tracks$minutes`, digits = 2)
print(yaxis[order(-yaxis$`tracks$minutes`),])

#to see how much time each rider has spent on the way doing absolutely nothing 
time_wasted <- tracks %>% 
  filter(distance_in_km == 0)
time_wasted$minutes <- time_wasted$time_diff_seconds/60

time_wasters <- aggregate(time_wasted$minutes ~ time_wasted$userId, FUN = sum)
time_wasters <- time_wasters[order(-time_wasters$`time_wasted$minutes`),]
time_wasters <- time_wasters %>% 
  filter(`time_wasted$minutes`>0)

#calculate the mean, median and mode
summary(axeses)

#so many anomalies, I will now filter the data for noise and see how it is according to year
tracks$year <- format(tracks$date, format = "%Y")

#debugging
ggplot(data = tracks) +
  geom_point(mapping = aes(x= distance_in_km, y = time_diff_seconds))+
  facet_wrap(~year)

#to filter for noise by removing time wasted on the trip
new_df <- tracks %>% 
  filter(distance_in_km>0)

#this filters the impossible anomalies, like people travelling 100-200kms in seconds
anomalies <- new_df %>% 
  filter(distance_in_km > 100)

anomalies$minutes <- anomalies$time_diff_seconds/60

#to visualize the data
ggplot(data = new_df) +
  geom_point(mapping = aes(x= distance_in_km, y = minutes))+
  geom_smooth(mapping = aes(x= distance_in_km, y = minutes))+
  labs(title = "Time vs Distance each year", x = "Distance in kilometers", y = "Time spent in minutes")+
  facet_wrap(~year)

#to see the distance ordered in descending order, from here, you can see that people were travelling 
#impossible distance in impossible time
newer_df <- tracks[order(-tracks$distance_in_km),]

#testing the route
#load shape file for ghana
Ghana <- read_sf(here("~/Desktop/Works/exported.json"))
tm_shape(Ghana) +
  tm_borders()

# Show the cities on the map
#the route
first <- new_df$longitude
second <- new_df$latitude
idee <- new_df$userId
datee <- new_df$date
froutes <- data.frame(first, second, idee, datee)

#dot the path
sp::plot(ne_states(country = "ghana"))
points(x=froutes$first, y = froutes$second, col = "red", cex = 2, pch = 20)