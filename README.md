# riders-tracking-analysis

This analysis was conducted to find the total kilometers every rider had travelled within the country.

Additional analysis to determine the time used by each rider. This was done to make sure the relationship between the time travelled and kiometers was reasonable.

### Loading the packages 

Next, I installed and loaded the packages needed to conduct my analysis.

`#install packages
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
library(grid)`

Next, I put the data in rstudio(attached, you will see the path where the file is saved on my device). I have the software downloaded on my computer, but if you are using cloud version of Rstudio, you can just upload it there. I'll attach a sample csv of the data here 

`#to load and read the data
tracking_df <- read.csv("~/Desktop/Trackers.csv") #this is the file path
str(tracking_df)
colnames(tracking_df)
head(tracking_df)
tail(tracking_df)
summary(tracking_df)`

The sample dataset is in this repo. Saved as "sample_data.csv".

Things to note: 

- to find the distance, the enry below must subtrack the one above;
- the time is in milliseconds and must be converted to date and time;
- the distinct d is the user_id not the x-id.

### Cleaning the data

Next, I ensured that the data was clean and in the right formats. Then I checked for the number of distinct riders:

`#check for distinct rider id
n_distinct(tracking_df$userId)`

Then, I changed the time from milliseconds to date and time format and date format as well.

`#first convert time to date time format 
tracking_df$Date_and_time <- as_datetime(tracking_df$time/1000)
tracking_df$date <- as_date(tracking_df$Date_and_time)`

Next, I noticed the csv file became unsorted so I had to sort it out but if it is already sorted out when you open the csv file, you can just go ahead and calculate the distance,

`#I will also be arranging the file according to user id, then by date and time
tracking_df <- tracking_df[order(tracking_df$userId, tracking_df$Date_and_time),]`

This could be one of the first things you do. I ran this code to endure that whatever output i get doesn't turn up scientific, and the second is to make sure the decimal points have just 2 digits.

`#to emsure the number format is not scientific
options(scipen=999)
getOption("digits")
options(digits = 2)`

Next, i created a boundary box by using the cordinares for the whole country to make sure that any cordinate outside of the country should be removed since we operate only within the country.

`#create a boundary box(border) for the whole of ghana
long <- c(-2.7685546874999996, -2.9443359375, -2.57080078125, -3.27392578125, -2.7685546874999996,  
          -2.0654296875, 1.142578125, 0.5712890625, 0.41748046875, 0.3515625, -0.06591796875, -2.7685546874999996)
lat <- c(11.005904459659451, 10.660607953624776, 8.189742344383703, 6.5118147063479, 5.00339434502215,
         4.740675384778373, 6.053161295714067, 6.860985433763661, 9.492408153765544, 10.31491928581316, 11.113727282172755, 11.005904459659451)
ghana <- data.frame(long, lat)

#to ensure the cordinates fall within the borders of Ghana
tracking_df$existing <- point.in.polygon(tracking_df$longitude, tracking_df$latitude, ghana$long, ghana$lat)`

Then, I created a dataframe to show pnly the data that is within the boundary of the country. i created another dataframe because i still wanted to preserve the original df.

`#create a new data frame of the cordinates that are indeed in Ghana
tracks <- tracking_df %>% 
  filter(existing == 1)`
  
Then, I created a loop to calculate the long and lat of the data and the one before it. I had to keep in mind to make sure that when the loop sees a new date and a new id, it must start from 0.

I calculated it in meters and then converted it to kilometers so I can answer the question.

`#calculating the distance between the second point and the point before it
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
tracks$distance_in_km <- tracks$distance_in_meters/1000`

I did the same analysis for the time as well. Mind you, the data had 1.4 million rows, so it took a good 7-10 minutes for the code to run. I calculated the time in seconds and still converted it to minutes afterwards. But you can convert it to minutes or hours directly by using ("units = 'secs', 'mins', 'hours'")

`#to calculate time difference of the time below and the one before it
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
tracks$minutes <- tracks$time_diff_seconds/60`

Then I checked the sum of kilometers each rider has tavelled as well as the time used to travel.

`#to calculate the km each rider has riden
xaxis <- aggregate(tracks$distance_in_km ~ tracks$userId, FUN = sum)
yaxis <- aggregate(tracks$minutes ~ tracks$userId, FUN = sum)
axeses <- data.frame(yaxis, xaxis)`

### The tracking data in the different parts of the country

I wanted to show what parts of Ghana the riders were delivering to and fro in Ghana. I wanted to see the cordinates of the riders that were actually moving and i filtered the data and plotted the chart.

Below, we can see the tracks of all the riders all over Ghana:

`new_df <- tracks %>% 
  filter(distance_in_km>0)
#the route
first <- new_df$longitude
second <- new_df$latitude
idee <- new_df$userId
datee <- new_df$date
froutes <- data.frame(first, second, idee, datee)
#plot the location
sp::plot(ne_states(country = "ghana"))
points(x=froutes$first, y = froutes$second, col = "red", cex = 2, pch = 20)`

With the chart, we can see if the tracking data is really accurate and it was as all the riders were travelling to places the company operates in.
