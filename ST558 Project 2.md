ST558-Project-2
================
Jenna Wilkie
10/22/2021

# Libraries

The libraries used for this project were: *tidyverse*: A package useful
for data manipulation. Installation of the *tidyverse* package also
includes the *readr*, *tidyr*, *dplyr*, and *tibble* pakages.  
*caret*: A package useful for easily performing k-fold cross validation
of test sets.

``` r
library(tidyverse)
library(caret)
```

# Reading in Data Set

To begin, we read in the data set, then created a subset of the data to
look at only business related news.

``` r
online_news_popularity<-read_csv("OnlineNewsPopularity.csv")

#filter for data where the chanel is business related
bus<-online_news_popularity%>%filter(data_channel_is_bus == 1)
```
