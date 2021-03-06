gen_matrix <- function(){
  
  prompt <- "enter the elements of the stochastic matrix row-wise as a space-separated list \n"
  D <- as.double(strsplit(readline(prompt), " ")[[1]])
  l <- length(D)
  P <- matrix(data = D, nrow=sqrt(l), ncol=sqrt(l), byrow=TRUE) 
  print(P)
  
  promt <- "enter the rates of transition row-wise as a space-separated list \n"
  L <- as.double(strsplit(readline(promt), " ")[[1]])
  print(L)
  
  G <- matrix(data = NA, nrow=sqrt(l), ncol=sqrt(l))
  
  
  i <- 1
  for(i in 1:sqrt(l)){
    
    j <- 1
    for(j in 1:sqrt(l)){
      
      if(i==j){
        G[i,j] <- -L[i]
      } 
      
      if(i != j) { 
        G[i,j] <- L[i] * P[i,j]
      }
      
      j <- j+1 
      
    }
    
    i <- i+1 
    
  }
 
  
  b <- c(rep(0,sqrt(l)),1)
  z <- c(rep(1,sqrt(l)))
  G <- t(G)
  A <- rbind(G,z)
  print("The limiting distribution vector is given as: ")
  print(round(qr.solve(A, b), sqrt(l)))
  
  
}

gen_matrix()