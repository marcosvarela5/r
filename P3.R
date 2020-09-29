set.seed(1) #Establecer una semilla. 
sample(1:6,1) #Coge 1 numero aleatorio en el vector 1:6
runif(10, -1, 1) #Generar 10 numeros aleatorios en el intervalo -1, 1

#EJERCICIO 3.3

  #1.

glc<-function(n, semilla=1,a=0,b=7^5,m=2^31-1){
  
  u<-numeric(n)
  x<-semilla
  for(i in 1:n){
    x<-(a+b*x)%%m
    u[i]<-x/m
  }
  return (u)
}

  #2.

u<-glc(n=500,a=1,b=5,m=512)

  #3.
hist(u, freq=FALSE)
abline(h=1,col=2)

  #4

mean(u)

  #5.
mean((u>0.4)&(u<0.8))

  #6.
plot(u[-500], u[-1])

  #7.
u<-glc(n=500)
hist(u, freq=FALSE)
abline(h=1,col=2)
mean(u)
mean((u>0.4)&(u<0.8))
plot(u[-500], u[-1])


#EJERCICIO 3.4

  #1.
x<-runif(1,-1,1)
y<-runif(1,-1,1)
x+y<0

  #2.
N<-10000
x<-runif(N,-1,1)
y<-runif(N,-1,1)
x+y<0
mean(x+y<0)
  
  #3.
1/2

#EJERCICIO 3.5

N<-10000
x<-runif(N,-1,1)
y<-runif(N,-1,1)
mean(x^2+y^2<=1)

pi/4

#EJERCICIO 3.6

moneda<-c(cara=1, cruz=0)
N<-10000
x<-sample(moneda, size=N, replace=TRUE, prob=c(0.5,0.5))
mean(x)

y<-runif(N,0,1)
mean(y<0.5)

#EJERCICIO 3.7

n<- 10000
x1 <- rbinom(n, size=1, prob=0.8)
x2 <- rbinom(n, size=1, prob=0.9)
x3 <- rbinom(n, size=1, prob=0.6)
x4 <- rbinom(n, size=1, prob=0.5)
x5 <- rbinom(n, size=1, prob=0.7)

z1<- x1|x2
z2<- x3|x4
z3<- z1|z2
z4<-z3&x5
mean(z4)

#EJERCICIO 3.8

deMere<-function(n=4){
  lanz<-sample(1:6, size=4, replace=TRUE)
  return(6%in%lanz)
}

deMere()

N<-10000
mean(replicate(N,deMere()))
  1-(5/6)^4
  

