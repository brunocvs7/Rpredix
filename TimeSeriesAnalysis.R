
#This script supports time series analysis following the steps bellow:
# 1 - Aquiring data
# 2- Data preparation
  #2.1 Missing Values Treatment
  #2.2 Outlier Treatment
# 3 - Data discovery
  #3.1 ANalysig the stationarity
  #3.2 Analysing the autorcorrelation and partial autocorrelation



# Importing the packages needed to perform the analysis

source('handleNA.R')
source('checkStat.R')
source('lags.R')
source('ARnet.R')
source('RNA.R')
library(urca)
library(forecast)
library(tidyverse)
library(tseries)
library(imputeTS)
library(plotly)
##### 1

# Reading a CSV file in which We have our time series

file = "natural_gas_cs.csv"
time_series_df <- read.csv2(file, sep = ";",header = FALSE,
        na.strings = c("0", "-1", "NA", "-"))
time_series_df <- time_series_df %>% select(c(1,2))

time_series_df <- time_series_df %>% arrange(time_series_df$V1)
#FOrmating the dataframe

time_series_df$V1 <- time_series_df$V1 %>% as.Date()
time_series_df$V2 <- as.character(time_series_df$V2) %>%  as.numeric()

#Since We want to test Our model on the 2018 data, let's delete these observations.
time_series_df1 <- time_series_df[time_series_df$V1 < "2018-01-01",]
time_series_df2 <- time_series_df[time_series_df$V1> "2017-12-31",]
time_series_df2$V2 <- ifelse(time_series_df2$V2 == 0, NA,time_series_df2$V2 )
time_series_df$V2 <- ifelse(time_series_df$V2 == 0, NA,time_series_df$V2 )
##### 2

# Seeing if there is some NA values in the time series

time_series_df1$V2 %>% statsNA()
time_series_df2$V2 %>% statsNA()




# Ploting the data

plot_ly(time_series_df1, x = ~time_series_df1$V1, y = ~time_series_df1$V2, name = 'Time series plot', type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = list(type = "category"))

plot_ly(time_series_df2, x = ~time_series_df2$V1[1:15], y = ~time_series_df2$V2[1:15], name = 'Time series plot', type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = list(type = "category"))



# Missing Values Treatment

data_training <- handleNA(time_series_df1, action = "remove")
data_test <- handleNA(time_series_df2, action = "remove")



p <- plot_ly(data_test, x = ~data_test$V1, y = ~data_test$V2, name = 'Time series plot', type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = list(type = "category"))

p
# Check the stationarity of the time-series.

checkStat(data_test$V2)   # Once our cases lies in case 2, Let's consider our time-series stationary

# Analysing the autocorrelation function and partial autocorrelation function 

lags(data_test$V2)

# forecasting with aut.arima

fit <- auto.arima(data_test$V2, seasonal = TRUE)

fcast <- forecast(fit, h = 100)


plot(fcast)

# forecasting with HoltWinter

ga_ts <- ts(data_test$V2, start = c(2011,31), end = c(2017,31), frequency = 365)
forecast1 <- HoltWinters(ga_ts)
yy <- forecast(forecast1, h = 1)
plot(yy$mean)

