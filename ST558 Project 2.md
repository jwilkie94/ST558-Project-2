ST558-Project-2
================
Jenna Wilkie
10/22/2021

# Reading in Data Set

To begin, we read in the data set, then created a subset of the data to
look at only business related news.

``` r
online_news_popularity<-read_csv("OnlineNewsPopularity.csv")

#filter for data where the chanel is business related
data<-online_news_popularity%>%filter(data_channel_is_bus == 1)
```
