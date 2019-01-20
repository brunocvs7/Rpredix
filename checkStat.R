checkStat <- function (s) {
  
  # THis function appplys 2 methods to check the stationarity of a time series
  # If p >0.55 in kpss test, we reject the null hp0 and we think this series as non-stationary. If p <0.55 we assume the series is trend-st
  # If p < 0.55 in adf, we reject the null hp0 and we think this series as stationary. If p > 0.55 It is non-stat
  # Use also adf.test
  
  #Case 1: Both tests conclude that the series is not stationary -> series is not stationary
  #Case 2: Both tests conclude that the series is stationary -> series is stationary
  #Case 3: KPSS = stationary and ADF = not stationary  -> trend stationary, remove the trend to make series strict stationary
  #Case 4: KPSS = not stationary and ADF = stationary -> difference stationary, use differencing to make series stationary
  
  library(urca)
  library(forecast)
  library(tseries)
  x <- ur.kpss(s)
  print(x) 
  adf.test(s)
  

}