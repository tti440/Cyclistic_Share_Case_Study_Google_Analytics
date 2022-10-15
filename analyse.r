install.packages("ggplot2")
install.packages("tidyverse")
install.packages("mapview")

library(ggplot2)
library(tidyverse)
library(mapview)
library(sf)

# Making a year-based line map comparing Customer and Subscriber

year<-2020
while (year < 2023){
path <- paste("Cleaned_Data\\Divvy_Trips_",year,".csv", sep="")
dataset <- read.csv(path)
if ("usertype" %in% colnames(dataset)){
    grouped_count <- dataset %>%
        count(month, usertype)
        print("Generate Plot")
        ggplot(data=grouped_count, aes(x=month, y=n, group=usertype, color=usertype)) +
        geom_line() +
        geom_point() +
        ggtitle(year) +
        scale_x_discrete(
        "Month",
        limits=c(1,2,3,4,5,6,7,8,9,10,11,12)
)
        ggsave(paste("Monthly_usage_",year,".png", sep=""))
} else { 
   grouped_count <- dataset %>%
        count(month, member_casual)
print("Generate Plot")
grouped_count <- grouped_count %>%
        rename(usertype=member_casual)
grouped_count["usertype"] <- replace(grouped_count$usertype,grouped_count$usertype=="casual","Customer")
grouped_count["usertype"] <- replace(grouped_count$usertype,grouped_count$usertype=="member","Subscriber")

        ggplot(data=grouped_count, aes(x=month, y=n, group=usertype, color=usertype)) +
        geom_line() +
        geom_point()+
        ggtitle(year)+
        scale_x_discrete(
        "Month",
        limits=c(1,2,3,4,5,6,7,8,9,10,11,12)
)
        ggsave(paste("Monthly_usage_",year,".png", sep=""))
}
year <- year + 1
}

# Making a year-based top5 flequently used start_staion and end_station map comparing Customer and Subscriber
for(year in 2013:2022){
dataset <- read.csv(paste("Cleaned_Data\\Divvy_Trips_",year,".csv", sep=""))
if(year>2019){
dataset <- dataset %>%
        rename(from_station_name=start_station_name, to_station_name=end_station_name, usertype=member_casual)
dataset["usertype"] <- replace(dataset$usertype,dataset$usertype=="casual","Customer")
dataset["usertype"] <- replace(dataset$usertype,dataset$usertype=="member","Subscriber")
frequency_start_station <- dataset %>%
                        count(from_station_name, usertype, start_lat, start_lng)
frequency_end_station <- dataset %>%
                        count(to_station_name, usertype, end_lat, end_lng)
cutomer_start <- frequency_start_station %>%
                        filter(usertype=="Customer") %>%
                        arrange(desc(n)) %>%
                        head(5)
subscriber_start <- frequency_start_station %>%
                        filter(usertype=="Subscriber") %>%
                        arrange(desc(n)) %>%
                        head(5)
cutomer_end <- frequency_end_station %>%
                        filter(usertype=="Customer") %>%
                        arrange(desc(n)) %>%
                        head(5)
subscriber_end <- frequency_end_station %>%
                        filter(usertype=="Subscriber") %>%
                        arrange(desc(n)) %>%
                        head(5)
start_top5 <- rbind(cutomer_start,subscriber_start)
sf_start <- st_as_sf(x = start_top5, 
                        coords = c("start_lng", "start_lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
mapviewOptions(fgb = FALSE)
m<-mapview(sf_start, map.type="OpenStreetMap", zcol="usertype")
mapshot(m, file = paste(year,"_start_map.png", sep = ""))

end_top5 <- rbind(cutomer_end,subscriber_end)
sf_end <- st_as_sf(x = end_top5, 
                        coords = c("end_lng", "end_lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
mapviewOptions(fgb = FALSE)
m<-mapview(sf_end, map.type="OpenStreetMap", zcol="usertype")
mapshot(m, file = paste(year,"_end_map.png", sep = ""))
}else{
if(year==2018 | year==2019){
stations <- read.csv("Cleaned_Data\\Divvy_Stations_2017.csv")
}else{
stations <- read.csv(paste("Cleaned_Data\\Divvy_Stations_",year,".csv", sep=""))
}
frequency_start_station <- dataset %>%
                        count(from_station_name, usertype)
frequency_end_station <- dataset %>%
                        count(to_station_name, usertype)
summarized_station <- stations %>%
                        group_by(name) %>%
                        summarise(latitude=mean(latitude), longitude=mean(longitude))

lon_list <- c()
lat_list <- c()
for(row in 1:nrow(frequency_start_station)){
        station_name <- paste(frequency_start_station[row, "from_station_name"])

        lon <- summarized_station %>%
                        filter(str_detect(str_trim(gsub("[(*)]","",name),side=c("right")), str_trim(gsub("[(*)]","",station_name),side=c("right"))))%>%
                        select(longitude)
        lat <- summarized_station %>%
                        filter(str_detect(str_trim(gsub("[(*)]","",name),side=c("right")), str_trim(gsub("[(*)]","",station_name),side=c("right"))))%>%
                        select(latitude)
        lon_list<-append(lon_list, lon)
        lat_list<-append(lat_list, lat)
}
frequency_start_station$longitude<- lon_list
frequency_start_station$latitude<- lat_list

lon_list <- c()
lat_list <- c()
for(row in 1:nrow(frequency_end_station)){
        station_name <- paste(frequency_end_station[row, "to_station_name"])

        lon <- summarized_station %>%
                        filter(str_detect(str_trim(gsub("[(*)]","",name),side=c("right")), str_trim(gsub("[(*)]","",station_name),side=c("right"))))%>%
                        select(longitude)
        lat <- summarized_station %>%
                        filter(str_detect(str_trim(gsub("[(*)]","",name),side=c("right")), str_trim(gsub("[(*)]","",station_name),side=c("right"))))%>%
                        select(latitude)
        lon_list<-append(lon_list, lon)
        lat_list<-append(lat_list, lat)
}
frequency_end_station$longitude<- lon_list
frequency_end_station$latitude<- lat_list
cutomer_start <- frequency_start_station %>%
                        filter(usertype=="Customer") %>%
                        arrange(desc(n)) %>%
                        head(5)
subscriber_start <- frequency_start_station %>%
                        filter(usertype=="Subscriber") %>%
                        arrange(desc(n)) %>%
                        head(5)
cutomer_end <- frequency_end_station %>%
                        filter(usertype=="Customer") %>%
                        arrange(desc(n)) %>%
                        head(5)
subscriber_end <- frequency_end_station %>%
                        filter(usertype=="Subscriber") %>%
                        arrange(desc(n)) %>%
                        head(5)
start_top5 <- rbind(cutomer_start,subscriber_start)
end_top5 <- rbind(cutomer_end,subscriber_end)

sf_start <- st_as_sf(x = start_top5, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_end <- st_as_sf(x = end_top5, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

mapviewOptions(fgb = FALSE)
m<-mapview(sf_start, map.type="OpenStreetMap", zcol="usertype")
mapshot(m, file = paste(year,"_start_map.png", sep = ""))
mapviewOptions(fgb = FALSE)
m<-mapview(sf_end, map.type="OpenStreetMap", zcol="usertype")
mapshot(m, file = paste(year,"_end_map.png", sep = ""))
}}
total_year <- data.frame()
total_month <- data.frame()
total_dayofweek <- data.frame()
total_tripduration <- data.frame()
for(year in 2013:2022){
        dataset <- read.csv(paste("Cleaned_Data\\Divvy_Trips_",year,".csv", sep=""))
        if(year > 2019){
        dataset <- dataset %>% rename(usertype=member_casual)
                dataset["usertype"] <- replace(dataset$usertype,dataset$usertype=="casual","Customer")
                dataset["usertype"] <- replace(dataset$usertype,dataset$usertype=="member","Subscriber")}
        count_month <- dataset %>% count(month, usertype)
        total_month <- rbind(total_month, count_month)
        count_dayofweek <- dataset %>% count(dayofweek, usertype)
        total_dayofweek <- rbind(total_dayofweek, count_dayofweek)
        data_tripduration <- dataset %>% select(tripduration, usertype)
        total_tripduration <- rbind(total_tripduration, data_tripduration)
        year_usage <- dataset %>%
                        count(usertype) %>%
                        mutate("year"=year)
        total_year <- rbind(total_year, year_usage)
}
View(total_year)
total_year<-total_year%>%
subset(usertype != "Dependent")

ggplot(data=total_year, aes(x=year,y=n,color=usertype,fill=usertype))+
geom_bar(stat="identity",position = "dodge")+
scale_x_discrete(
        "Year",
        limits=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)
)
ggsave("Usage_based_on_year.png")
total_month <- total_month %>% 
                group_by(month, usertype) %>%
                summarise(total=sum(n), .groups="keep")
Customer_month <-subset(total_month, usertype=="Customer")
Subscriber_month <-subset(total_month, usertype=="Subscriber")
total_month <- rbind(Customer_month, Subscriber_month)
total_month <- total_month %>%
                        arrange(month)
ggplot(total_month, aes(x=month, y=total, color=usertype, fill=usertype))+
geom_bar(stat="identity",position = "dodge")+
scale_x_discrete(
        "Month",
        limits=c(1,2,3,4,5,6,7,8,9,10,11,12)
)
ggsave("Usage_of_Month.png")

total_dayofweek <- total_dayofweek %>% 
                group_by(dayofweek,usertype)%>%
                summarise(total=sum(n), .groups = "keep")

Customer_dayofweek <-subset(total_dayofweek, usertype=="Customer")
Subscriber_dayofweek <-subset(total_dayofweek, usertype=="Subscriber")
total_dayofweek <- rbind(Customer_dayofweek, Subscriber_dayofweek)
total_dayofweek <- total_dayofweek %>%
                        arrange(dayofweek)

ggplot(data=total_dayofweek, aes(x=dayofweek,y=total,color=usertype,fill=usertype))+
geom_bar(stat="identity",position = "dodge")+
scale_x_discrete(
        "Day of The week",
        limits=0:6,
        labels = c(
                "0" = "Monday",
                "1" = "Tuesday",
                "2" = "Wednesday",
                "3" = "Thursday",
                "4" = "Friday",
                "5" = "Saturday",
                "6" = "Sunday"
        )
)
ggsave("Usage_of_Dayofweek.png")

summary(total_tripduration%>%filter(usertype=="Subscriber"))
summary(total_tripduration%>%filter(usertype=="Customer"))
