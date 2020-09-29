# Paradoja del cumpleaños
# Probabilidad de que al menos 2 personas de n compartan cumpleaños
 
cumple <- function(nmax=100){
dif <- numeric(nmax)
for(n in 1:nmax){
  dif[n] <- prod(365:(365-n+1))/365^n
}
p <- 1-dif
plot(1:nmax, p, main="Paradoja del cumpleaños", xlab="Número de personas",
ylab="Probabilidad de que al menos dos personas compartan cumpleaños")
abline(h=0.5, v=23)
return(p)
}
p <- cumple()
which(p==min(p[p>=0.5])) #Alternativa: método dicotómico
