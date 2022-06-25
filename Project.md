---
title: "Cyclistic Case Study - Google Data Analytics Course"
output: html_notebook
---

### **INTRODUCTION**

#### This is cyclistic bike-sharing case study from Google Data Analytics Certificate. This case study is about a bike-sharing programme with different types of bikes such as classic bikes, docked bikes and electric bikes in Chicago city from 2020 to 2021. The majority of riders opt for traditional bikes, and about 8% of riders use the assistive options. Cyclistic users are more likely to ride for leisure and  work each day bike-share program features more than 5,800 bicycles and 600 docking stations across Chicago and The bikes can be unlocked from one station and returned to any other station in the system anytime. they also provide a pass system for their customers: single-ride passes,full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are cyclistic members.


### **Scenario**

#### As a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, our team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, our team will design a new marketing strategy to convert casual riders into annual members.

### **Business Task**

1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?



### **Detailed Information** 

In R studio, First of all, I have installed some packages and libraries which can see below and read files from April 2020 to May 2021. Every file has 13 columns which represent the following information.

**Ride_id**  : unrepeatable number of each ride  

**Rideable_type** : There are three types of rides available:Docked type,class type and Electric-type 

**Started_at** : Starting period of trip with year and date(y-m-d h-m-s) form 

**Ended_at** : Ending periods of trip with year and time (y-m-d h-m-s) form  

**Start_station_name** : Name of the station where riders start their ride    

**Start_station_id** : A single number of every station where the ride going to start   

**End_station_name** : Name of the station where riders and their ride  

**End_station_id** : A single number of every station where the ride going to end    

**Start_lat** : Latitude  of starting station    
               [LATITUDE: It is the measurement of distance north or south of the equator and it's measured with                180 imaginary lines that form circles around the Earth east-west, parallel to the Equator.]   
               
**Start_lng** : Longitude of starting  station      
               [LONGITUDE: It is the measurement east or west of the prime meridian and it's measured by                       imaginary lines that run around the Earth vertically and meet at the North and South poles.                      These   lines know as meridian]   
               
**End_lat**   : Latitude  of end of station     

**End_lng**   : Longitude of end of station    

**Member_casual** : Two types of customers:
                    1) member  
                    2) casual


```{r}
install.packages("tidyverse")
install.packages("dplyr")
install.packages("janitor")
install.packages("lubridate")
install.packages("mapview")
install.packages("sf")
install.packages("geosphere")
install.packages("prophet")
install.packages("maps")
install.packages("mapproj")
install.packages("mapdata")
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(mapview)
library(sf)
library(geosphere)
library(ggplot2)
library(prophet)
library(maps)
library(mapdata)
library(mapproj)

```

```{r}
df1<-read.csv("~/pari/rdata/202004-divvy-tripdata.csv")
df2<-read.csv("~/pari/rdata/202005-divvy-tripdata.csv")
df3<-read.csv("~/pari/rdata/202006-divvy-tripdata.csv")
df4<-read.csv("~/pari/rdata/202007-divvy-tripdata.csv")
df5<-read.csv("~/pari/rdata/202008-divvy-tripdata.csv")
df6<-read.csv("~/pari/rdata/202009-divvy-tripdata.csv")
df7<-read.csv("~/pari/rdata/202010-divvy-tripdata.csv")
df8<-read.csv("~/pari/rdata/202011-divvy-tripdata.csv")
df9<-read.csv("~/pari/rdata/202012-divvy-tripdata.csv")
df10<-read.csv("~/pari/rdata/202101-divvy-tripdata.csv")
df11<-read.csv("~/pari/rdata/202102-divvy-tripdata.csv")
df12<-read.csv("~/pari/rdata/202103-divvy-tripdata.csv")
```

# **collection and cleaning of Data**   

All files have same  data type and order of column that's why we have combined all files into one file and we get 34,89,748 rows and 13 columns. In the first step of preparation, we have removed dirty data by removing empty rows and columns. I have detached the duplicate from ride_id to make it clean data.    


```{r}
bike_ride<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
dim(bike_ride)
bike_ride<-janitor::remove_empty(bike_ride,which = c("cols"))
bike_ride<-janitor::remove_empty(bike_ride,which = c("rows"))
dim(bike_ride)

duplicated(bike_ride$ride_id)
bike_ride<-bike_ride[!duplicated(bike_ride$ride_id),]
dim(bike_ride)
```

- After that,I have converted started_at and ended_at into datetime(ymd-hms) datatype form, if any rows have different datatype then we can make it in same form of datatype .   
     

- I have extracted starting and ending hour of cycling ,weekdays from started_at and ended_at column 

- I have created one more column trip_duration to find the timing of each trip where I have observed that around 10k rows show negative timing of cyclic trip, which is logically not possible. so, i have removed those rows.    


```{r}
bike_ride$started_at<-lubridate::ymd_hms(bike_ride$started_at)
bike_ride$ended_at<-lubridate::ymd_hms(bike_ride$ended_at)
str(bike_ride,20)
```

```{r}
bike_ride$start_station_id<-as.character(bike_ride$start_station_id)
bike_ride$end_station_id<-as.character(bike_ride$end_station_id)

```

## Create hours fields

```{r}
bike_ride$start_hour<-lubridate::hour(bike_ride$started_at)
bike_ride$end_hour<-lubridate::hour(bike_ride$ended_at)
bike_ride$start_date<-as.Date(bike_ride$started_at)
bike_ride$started_at<-as_datetime(bike_ride$started_at)
bike_ride$ended_at<-as_datetime(bike_ride$ended_at)
bike_ride$end_date<-as.Date(bike_ride$ended_at)
bike_ride$weekday<-weekdays(bike_ride$started_at) 


bike_ride$trip_duration<-difftime(bike_ride$ended_at,bike_ride$started_at,units = c("mins"))
bike_ride2<-bike_ride[!(bike_ride$trip_duration<0),]
str(bike_ride2)
is.numeric(bike_ride2$trip_duration)
bike_ride2$trip_duration<-as.numeric(bike_ride2$trip_duration)
is.numeric(bike_ride2$trip_duration)
bike_ride2<-na.omit(bike_ride2)  #Remove NA values
```
# **Analysis of Data**

- **Point Graph** indicates the trend of rides from April 2020 to March 2021. cyclic rides raised from April to August and reached near 3lac in 2020, after that it's started to fall at February 2021 and again it has increased on march. so, we can see that both riders have mostly preferred cycle rides in the summer season and these numbers of rides have completely down in the winter season.

```{r}
monthlygroup_duration=bike_ride2%>%
  mutate(month_year=format(as.Date(start_date),"%y-%m"))%>%
  group_by(member_casual,month_year)%>%
  summarise(number_of_ride=n(),average_duration=mean(trip_duration))%>%
  arrange(member_casual,month_year)



ggplot(data=monthlygroup_duration,aes(x=month_year,y=number_of_ride))+
   geom_point(aes(color=member_casual,shape=member_casual,size=member_casual))+
  theme(axis.text.x = element_text(angle = 60))+
  labs(title ="Ride Duration by Ride Type from 2020_21",subtitle = "Annualy",fill="rider_type")+
  xlab("Monthly Ride from 2020-21")+ylab("Numer of Ride")

```

In the next step, I have defined the minimum, maximum, median, and mean value of trip duration by the summary function. It can be seen that the highest trips have done in around 40 days, instead, some of the customers have zero trip duration. It is some limitation of data because zero trip duration is hard to possible.    


```{r}
summary(bike_ride2$trip_duration)
```

- This path graph also gives supports the statement that the trip duration of casual riders is three times greater than the trip duration of member riders. The trade of casual trip duration fluctuated and the trend of membertrip duration was slowly fallen.

- By analysing the path graph, member riders do ride for work purposes and casual riders do ride for leisure time.

```{r}
ggplot(data=monthlygroup_duration,aes(x=month_year,y=average_duration,group=member_casual))+geom_path(aes(color=member_casual))+geom_point(aes(color=member_casual))+theme(axis.text.x = element_text(angle = 60))+labs(title="Average of time by Ride Type from 2020_21",subtitle = "Monthly",fill="Rider_type")+xlab("Annual with Month")+ylab("Average duration(min)")
```

- I have created one of the pivot tables which indicated that member and casual both riders have around the same maximum and minimum timing of trip, but the mean and median trip duration of casual is around three times more than the member. It is clear that casual riders mostly used the cycle for leisure time and member riders utilized it for work purposes.   

```{r}

grouping_duration=bike_ride2%>%group_by(member_casual)%>%summarise(mean_duration=mean(trip_duration),median_duration=median(trip_duration),max_duration=max(trip_duration),min_duration=min(trip_duration),.groups = 'drop')
```
- This column graph compares the number of rides between casual and member riders from Monday to Sunday where member riders almost double than casual riders on weekdays, at the other hand both riders have similar numbers of rides on weekends with the highest pick of the week.


```{r}
##is.character(weekday)
bike_ride2$weekday<-ordered(bike_ride2$weekday,levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))


grouping_days_member_casual=bike_ride2%>%
  group_by(member_casual,weekday)%>%
  summarise(average_duration=mean(trip_duration),number_of_rides=n())%>%
              arrange(member_casual,weekday)



ggplot()+
  geom_col(data=grouping_days_member_casual,aes(x=weekday,y=number_of_rides,fill=member_casual),position="dodge")+labs(title = "numer of rides by rider type",subtitle = "everyday",fill="rider type")+xlab("weekday")+ylab("number of ride")


```

- The bar graph indicates rides of different bike types between casual and member riders. It is clear that the classic bike has became very popular with both types of riders than docked and electric bikes. Electric and ducked bikes have demanded around 50k in members, which is almost double that of casual riders.     

- By using this plot, we can keep stock of different bikes as per requirement and we should find the reason for this drastic difference between usage of bike rides.


```{r}
member_casual_ridetype=bike_ride2%>%
  group_by(member_casual,weekday,rideable_type)%>%
  summarise(average_duration=mean(trip_duration),number_of_rides=n())%>%
              arrange(member_casual,weekday)



ggplot(data=member_casual_ridetype,aes(x=weekday,y=number_of_rides,fill=rideable_type))+
  geom_bar(stat = "identity",position = position_dodge())+
  facet_wrap(~member_casual)+
  labs(title = "numer of rides by rider type",subtitle = "everyday",fill="bike type")+
  xlab("weekday")+ylab("number of ride")+
  theme(axis.text.x = element_text(angle=60))+
  scale_fill_manual(values = c('green','blue','red'))
```


```{r}

biketype_station_id=bike_ride2%>%group_by(rideable_type,start_station_id,start_station_name,member_casual)%>%
  summarise(number_of_ride_per_station=n())%>%arrange(member_casual,desc(number_of_ride_per_station))
head(biketype_station_id,10)



```    

**There is the top ten station name used by the casual rider**


### conclusion ###

This data have some limitation because of lack of information such as what is the reason rider avoid cycle riding on weekdays, why to dislike electric and docked bike by riders and so on. we get some misleading data like negative trip duration. In my opinion, we should collect more data for better data decision making. further that I removed dirty data and prepare it for aggregation, manipulation and analysis.  

- After analysis of all the data, I conclude that casual riders have utilized cyclic bikes for their leisure time and also used them for long rides, instead, member riders have used them for work purposes and ride at nearest places.    

- the second thing is that members and casual riders have used more bikes on hot days(between July to August ), which completely opposite on cold days. There are so many chances to slip from the bike in snow time and also bike's parts going to be junked .so, we need to maintain the bike's part specifically wheels. so, this is one reason to people choose casual membership rather than annual membership.we will solve this problem by making  good grip wheels to avoid slippery condition. we can also motivate people to use bikes as an exercise purpose in winter by organising advertise booth ,hoarding board ,etc.      

- we can see those casual riders use more bikes at weekends instead of weekdays. so, we can increase the rate of bikes at weekend. we can also establish a yearly pass system for member riders who get a heavy discount in summer and some special days. so, casual riders would be like to become annual member. moreover,we can give lure advertise for annual membership on those top ten station where have more casual riders.


