 handleNA <- function(data, action = "remove", s= FALSE){
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
   
   data = data
   action = action
   s = s
   
   if (action == "remove"){
      
      data<- data %>% na.omit()
    } else{
      
        if (s == FALSE){
          
          library(imputeTS)
          library(DMwR)
          library(dplyr)
         #Generate NA samples in the dataset to test the best impute model
          data1 <- na.omit(data)
          random <- sample_frac(as.data.frame(data1$V2), 0.2)
          rows <- as.numeric(rownames(random))
          actuals <- data1$V2
          data2 <- actuals
          data2[rows] <- NA
          
          #Here We teste the best model 
         
          ylocf <- na.locf(data2, option = "locf")
          
          ynocb <- na.locf(data2, option = "nocb")
          
          yli <- na.interpolation(data2, option = "linear")
          
          yspl <- na.interpolation(data2, option = "spline")
          
          imputs <- data.frame(ylocf, ynocb, yli, yspl)
          errors_metrics <- apply(imputs, 2, regr.eval, actuals)
          y <- errors_metrics[3,] %>%  which.min()
          
          if (y == 1){
            data$V2 = na.locf(data$V2, option = "locf")
          } else if (y == 2){
            data$V2 = na.locf(data$V2, option = "nocb")
          }else if (y == 3){
            data$V2 = na.interpolation(data$V2, option = "linear")
          } else{ data$V2 = na.interpolation(data$V2, option = "spline")}
         
          
        } else {
          
          
          library(imputeTS)
          library(DMwR)
          library(dplyr)
          #Generate NA samples in the dataset to test the best impute model
          
          data1 <- na.omit(data)
          random <- sample_frac(as.data.frame(data1$V2), 0.2)
          rows <- as.numeric(rownames(random))
          actuals <- data1$V2
          data2 <- actuals
          data2[rows] <- NA
          
          ylocf <- na.seadec(data2, algorithm = "locf")
          
          ynocb <- na.seadec(data2,algorithm =  "nocb")
          
          yli <- na.seadec(data2, algorithm = "interpolation")
          
          imputs <- data.frame(ylocf, ynocb)
          errors_metrics <- apply(imputs, 2, regr.eval, actuals)
          y <- errors_metrics[3,] %>%  which.min()
          
          if (y == 1){
            data$V2 = na.locf(data$V2, option = "locf")
          } else if (y == 2){
            data$V2 = na.locf(data$V2, option = "nocb")
          }else{
            data$V2 = na.interpolation(data$V2, option = "linear")
          }
          
        }
      
    }
   
   
   
   return(data)
 }
 