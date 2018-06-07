library(ggplot2)
library(dplyr)
library(lubridate)
time <- read.csv("time.csv",sep = "|",header = FALSE , stringsAsFactors = FALSE)

#hourly
hourly <- function(time, datee)
{
temp_hour <-  time %>% group_by(Date = as.Date(V3),hour=hour(V3)) %>% 
  summarise(count = mean(V2)) %>%  subset(Date==datee)
ggplot(temp_hour,aes(x = temp_hour$hour, y = temp_hour$count))+
  geom_line()+
  geom_point()+
  labs(x = "Hour",y = "Rating",title = datee)+
  stat_smooth(method = 'lm' ,se = FALSE , aes(colour = ""))+
  scale_color_brewer(name = "trendline", palette = "Dark2")
}

#daily
daily <- function(time , monthh)
{
temp_date <-  time %>% group_by(mon=month(as.Date(time$V3)),Date = as.Date(time$V3)) %>% 
  summarise(count = mean(V2)) %>%  subset(mon==monthh)
ggplot(temp_date,aes(x = mday(temp_date$Date) , y = temp_date$count))+
 geom_line()+
  geom_point()+
  labs(x = "Date",y = "Rating",title = month.abb[monthh])+
stat_smooth(method = 'lm' ,se = FALSE , aes(colour = ""))+
  scale_color_brewer(name = "trendline", palette = "Dark2")
}

#weekly
weekly <- function(time, monthh)
{
temp_week <- time %>% group_by(mon=month( as.Date(time$V3)), Date=floor_date(as.Date(time$V3),
                     "7 days")) %>%  summarise(count=mean(V2)) %>% subset(mon==monthh)
x = c("Week1","Week2","week3","week4","week5")
ggplot(temp_week,aes(x = x[1:nrow(temp_week)] , y = temp_week$count))+
  geom_line()+
  geom_point()+
  labs(x="",y = "Rating",title = month.abb[monthh])+
  stat_smooth(method = 'lm' ,se = FALSE , aes(colour = ""))+
  scale_color_brewer(name = "trendline", palette = "Dark2")
}

#monthly
monthly <- function(time, yearr)
{
temp_month <- time %>% group_by(year=year(as.Date(time$V3)),mon=month(as.Date(time$V3))) %>% 
  summarise(count=mean(V2))  %>%   subset(year == yearr)
ggplot(temp_month,aes(x = month.abb[temp_month$mon], y = temp_month$count))+
  geom_point()+
  labs(x="",y = "Rating",title = yearr)+
  stat_smooth(method = 'lm' ,se = FALSE , aes(colour = ""))+
  scale_color_brewer(name = "trendline", palette = "Dark2")
}

hourly(time,"2018-05-07")
daily(time, 5)
weekly(time,5)
monthly(time ,2018)
