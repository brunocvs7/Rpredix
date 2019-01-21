

perform <- function (Y, y) {
  
 
  
  RMSE <-  (sum((Y - y)^2) / length(y)) ^ 0.5  
  MAPE <- sum(abs((Y-y)/Y))*100/length(Y)
  SSE <- sum((Y - y)^2)
  
  R <- cor(Y,y)


  
  
  
 return(data.frame(RMSE = RMSE,MAPE  =MAPE,SSE = SSE, R = R))

}

