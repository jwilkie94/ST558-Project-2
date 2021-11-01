# Libraries

The libraries used for this project were:   
**tidyverse**: A package useful for data manipulation.  Installation of the **tidyverse** package also includes the **readr**, **tidyr**, **dplyr**, and **tibble** pakages.      
**caret**: A package useful for easily performing k-fold cross validation of test sets.  
**randomForest**: A package that allows us to fit a random forest model.  
**gbm**: A package that allows us to fit the boosted tree model.  
**doParallel**: A package that allows us to use parallel computing for the random forest model to reduce the time it takes our code to run.    

# Repo Description

We created a repo for project 2. In this repo,  two group members can pull the request, do the work individually, and push to the main branch. This repo was designed to perform analyses of different types on articles from an online news source.  We generated four linear and ensemble models for comparison.  The data was split into a training and test set.  Each model was trained against the training set using cross validation.  The test data set was used for prediciton and the most accurate model was chosen based on the lowest RMSE value.  

# Code 

The code to be automated creating html documents are as below:

articleID<-unique(data$channel)  
output_file<-paste0(articleID,".md")  
params=lapply(articleID,FUN=function(x){ list(channel=x)})  
reports<-tibble(output_file,params)  
library(rmarkdown)  
apply(reports,MARGIN=1, FUN=function(x){render(input="ST558 Project 2.Rmd",output_format="github_document", output_file=x[[1]], params=x[[2]])})

Unfortunately we were unable to get the automation to work with the filter statement.  The entertainment articles are below, but the other htmls would not generate.  

# Links
The links for every analysis are as below:  
The analysis for [Business articles is available here](bus.html).  
The analysis for [Entertainment articles is available here](entertainment.html).  
The analysis for [Tech articles is available here](tech.html).  
The analysis for [World articles is available here](world.html).  
The analysis for [Socmed articles is available here](socmed.html).  




