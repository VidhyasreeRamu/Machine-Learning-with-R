---
title: 'Airline Delay Prediction'
output: html_document
=
---



#Go to the airlines data site:http://stat-computing.org/dataexpo/2009/the-data.html. 
#Read in the airlines data set for 2008 into a data frame.

```{r}
library(data.table)
#We are using fread as it used for faster read when having large file, in our case we have a 680 MB file which is quite huge. 
#Let's see how fast does this function reads the file
airline_df = fread("2008.csv")
#It took close to 14sec to load this file, i am really happy with this performanc :)
cat("Number rows in the Airline data:",nrow(airline_df))
```



#Remove all rows of the data frame with missing data.
```{r}
#We are using the complete.cases function, which return all the rows which are not having any missing values and we extract only them for further analysis 
airline_df = airline_df[complete.cases(airline_df),]
cat("Number of rows that we are going to use for analysis after removing NA rows:",nrow(airline_df))
```


#### Fit one regression model each to explain "DepDelay" and "ArrDelay".
```{r}
#We will first split the data for traing the model and to test.
subset_airline_df = airline_df[1:1000000,]
testdata_airline_df = airline_df[1000001:1524735,]

#Now we are ready with our data. So let's run our regression
#Before we go ahead with the Linear regression model we are trying to identify the variables which can act as independent variables to find Departure delay.
model_formula = (DepDelay ~ (LateAircraftDelay+CarrierDelay+WeatherDelay+NASDelay+factor(DayOfWeek)))
#Creating a model
DepDelayModel = lm(model_formula, subset_airline_df)
summary(DepDelayModel)

#Similarly we will be implementing for Arrival Delay
Arrdelayformula = (ArrDelay ~ (DepDelay+TaxiIn+Distance+AirTime+CarrierDelay+WeatherDelay+NASDelay+factor(DayOfWeek)+DepTime))
Arrdelaymodel = lm(Arrdelayformula, subset_airline_df)
summary(Arrdelaymodel)
```
```{r}
#Let's see a scatter plot which better explains the relationship between the variables
library(dplyr)
dept_delay_time_df = dplyr::select(subset_airline_df,DepDelay,LateAircraftDelay,CarrierDelay,WeatherDelay,NASDelay)
#plot(dept_delay_time_df)
arr_delay_time_df = dplyr::select(subset_airline_df,ArrDelay,DepDelay,TaxiIn,Distance,AirTime,CarrierDelay,WeatherDelay,NASDelay)
#plot(arr_delay_time_df)

```


#### Now take the fitted regression and predict delays using the remaining data from the no-missing data set 
```{r}
#Here we will test our model how well it is predicting based on our test data
ArrivalDelayPrediction = predict(Arrdelaymodel,testdata_airline_df)
DepDelayPrediction = predict(DepDelayModel,testdata_airline_df)

#Let's see the MSE (Mean Squared Error), in order to check MSE we are using METRICS package
library(Metrics)
cat("The MSE for Arrival Delay Model is:",mae(testdata_airline_df$ArrDelay,ArrivalDelayPrediction))
cat("\nThe MSE for Departure Delay Model is:",mae(testdata_airline_df$DepDelay,DepDelayPrediction))
```


