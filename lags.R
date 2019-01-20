lags <- function(s){
  library(forecast)
  
  Acf(s)
  Pacf(s)
  
}