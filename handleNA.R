 handleNA <- function(data, action = "remove", s= FALSE, plot = FALSE){
   #This function require two arguments:
   #First argument is the data set,that is a df
   #with the first columns beeing the time and the second one beeing 
   #The values
   #Second argument is the action, that can be strictly "remove" or 
   #imput. By default, action argument is remove.
   #When remove is selected by the user, the data is simply appended
   #and the result is returned to a variable as a numeric vector
   #If the action is set to "imput" It will perfomed some methods
   #To imput and the best imputation will substitue the NA values
   #Finally the df is return containing the time in the 1st column
   #and the values in the second column
   
    if (action == "remove"){
      
      data<- data %>% na.omit()
    } else{
      
        if (s == FALSE){
          
          library(imputeTS)
          library(DMwR)
          ylocf <- na.locf(data[,2], option = "locf")
          
          ynocb <- na.locf(data[,2], option = "nocb")
          
          yli <- na.interpolation(data[,2], option = "linear")
          
          yspl <- na.interpolation(data[,2], option = "spline")
          
          imputs <- data.frame(ylocf, ynocb, yli, yspl)
          errors_metrics <- apply(imputs, 2, regr.eval, data$V1)
         y <- errors_metrics[3,] %>%  which.max() %>% imputs[]
         RMSE <- errors_metrics[3,] %>% which.max() %>% errors_metrics
         
          
        }
          
            
      
      
      
      
    }
   
   
   
   return()
 }
 