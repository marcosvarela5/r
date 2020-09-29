
prestamo <- function(P, n, i){
  
  R = P*((i)/(1-(1+i)^-n))
  
  return(R)
  
}

media <- function(x){
  n <- length(x)
  m <- sum(x)/n
  return(m)
}

varianza <- function(x){
  
  t <- length(x)
  s <- sum((x-mean(x))^2)/t
  
  return(s)
}

media_truncada <- function (a, p=0.2){
  
  n <- length(a)
  k <- round(p*n)
  i <- 1
  if((k<0)|(k>=(n/2))) stop('Valor de p incorrecto')
  a <- sort(a)
  for(i in (1:k)){
    a[i] <- NA
  }
  
  b <- a
  return(mean(b, 0, TRUE))
}


