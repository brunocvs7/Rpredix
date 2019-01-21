RNA <- function (data, y, n1,  n2)
{
  
  #Data ? minha matriz, y ? a vari?vel que queremos inferenciar, n1 ? o n?mero m?ximo para neuronios na camada intermediaria 1
  # n2 ? o numero m?ximo para neuronios na camada 2
  
  library(dplyr)
  library(monmlp)
  source("perform.R")
  
  
  
  dataset <- data 
  
  #Sele??o aleat?ria de dados para treinamento 
  samplesize = 0.85 * nrow(dataset) %>% 
    round()
  
  
  index = nrow(dataset) %>% 
    seq_len() %>%
    sample( , size = samplesize )
  
  
  
  #####################################################################
  
  #Normaliza??o dos dados 
  
  max = apply(dataset , 2 , max)
  min = apply(dataset, 2 , min)
  
  scaled = as.data.frame(scale(dataset, center = min, scale = max - min))
  
  datatrain = dataset[ index, ]
  datatest = dataset[ -index, ]
  
  trainNN = scaled[index , ]
  testNN = scaled[-index , ]
  
  #####################################################################
  
  #Modelagem com RNA
  
  Hidden1 <- c()
  Hidden2 <- c()
  Algoritmo <- c()
  RMSEtr <- c()
  RMSEts <- c()
  MAPEtr <- c()
  MAPEts <- c()
  SSEtr <- c()
  SSEts <- c()
  Rtr <- c()
  Rts <- c()
  
  t <-0
  results <- data.frame()
 algoritmo <- c("BFGS", "CG","nlm", "nlminb", "Rvmmin", "Rcgmin") 
  
  nXtr <- trainNN %>% select(-y) %>% as.matrix()
  nYtr <- trainNN[y] %>% as.matrix()
  nXts <- testNN %>% select(-y) %>% as.matrix()
  nYts <- testNN[y] %>% as.matrix()
  Ytr <- datatrain[y]
  Yts <- datatest[y]
 
    for (k in algoritmo){
    
                   for (i in 1:n1){
                                    for (j in 0:n2) {
                                      
                                  
                                      nnet <- monmlp.fit(nXtr, nYtr, hidden1 =i, hidden2 = j, iter.max = 900,
                                                         n.trials = 1, n.ensemble = 1, bag = T,
                                                         cases.specified = NULL, iter.stopped = NULL,
                                                         scale.y = TRUE, Th = tansig, To = linear,
                                                         Th.prime = tansig.prime, To.prime = linear.prime,
                                                         monotone = 1, init.weights = NULL,
                                                         max.exceptions = 10, silent = FALSE, method = k,
                                                         control = list(trace = 0))
                                      
                                      
                                      nyts<- monmlp.predict(nXts, nnet) #Teste
                                      
                                      nytr <- monmlp.predict(nXtr,nnet) #Treinamento
                                      
                                      yts <- as.data.frame((nyts * (max(dataset[y]) - min(dataset[y]))) + min(dataset[y]))
                                      
                                      ytr <- as.data.frame((nytr * (max(dataset[y]) - min(dataset[y]))) + min(dataset[y]))
                                      
                                      performancetest <- perform(Yts,yts)
                                      
                                      
                                      
                                      if (performancetest$V1 > 0.1){
                                        
                                        performancetrain <- perform(Ytr, ytr)
                                        
                                      
                                        
                                        Hidden1[t] <- i
                                        Hidden2[t] <- j
                                        Algoritmo[t]<- k
                                        RMSEtr[t] <- performancetrain$RMSE
                                        RMSEts[t]<- performancetest$RMSE
                                        MAPEtr[t] <-performancetrain$MAPE
                                        MAPEts[t] <- performancetest$MAPE
                                        SSEtr[t] <- performancetrain$SSE
                                        SSEts[t]<- performancetest$SSE
                                        Rtr[t] <- performancetrain$V1
                                        Rts[t]<-performancetest$V1
                                        
                                        t <- t+1
                                                                   }
                                      
                                      
                                                     }
                   
                   
                                   }
    
    
    
    
                        }
  
  
  results <- data.frame(Hidden1, Hidden2, Algoritmo, RMSEtr, RMSEts, MAPEtr, MAPEts, SSEtr, SSEts, Rtr, Rts)
  results1 <- results
  
  
  
  return(results1)
  
  
}

