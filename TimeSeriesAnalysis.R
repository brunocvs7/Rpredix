
#This script supports time series analysis following the steps bellow:
# 1 - Aquiring data
# 2- Data preparation
  #2.1 Missing Values Treatment
  #2.2 Outlier Treatment
  #2.3 


# Importing the packages needed to perform the analysis
library(urca)
library(forecast)
library(tidyverse)
library(tseries)

##### 1

# Reading a CSV file in which We have our time series

file = "gas_natural_cs.csv"
time_series_df <- read.csv2(file, sep = ";",header = FALSE,
        na.strings = c("0", "-1", "NA", "-"))
time_series_df <- time_series_df %>% select(c(1,2))

##### 2

# Seeing if there is some NA values in the time series

time_series_df$V2 %>% as.numeric %>% statsNA()

#FOrmating the dataframe

time_series_df$V1 <- time_series_df$V1 %>% as.Date()
time_series_df$V2 <- time_series_df$V2 %>%  as.numeric()

# Ploting the data

ggplot(time_series_df, aes(V1, V2)) + geom_line() + xlab("") + ylab("Daily Views")


# Missing Values Treatment


