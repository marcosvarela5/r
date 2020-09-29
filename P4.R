#PRÁCTICA 4 

############ PRIMEROS CÁLCULOS #############

  #1.X ??? B(10, 0.7), P(X = 0)
  
    dbinom(0, 10, 0.7)

  #2.X ??? B(10, 0.7), P(X ??? 3) y P(X < 3)

    sum(dbinom(0:3,10,0.7))
    pbinom(3, 10, 0.7)

  #3. X ??? B(10, 0.7), P(X > 2) y P(X ??? 2)
  #P(X>2) = 1- P(X<=2)

    sum(dbinom(3:10, 10, 0.7))
    1-pbinom(2,10,0.7)

  #4. X ??? B(100, 0.05), los cuartiles (cuantiles 0.25, 0.5 y 0.75)

    qbinom(c(0.25, 0.5, 0.75),100,0.05)

  #5. X ??? BN(10, 0.8), P(1 < X < 4) y P(1 ??? X ??? 4)

    sum(dnbinom(2:3, 10,0.8))

  #6. X ??? BN(10, 0.8), la mediana
  
    qnbinom(0.5, 10, 0.8)
  
  #7. X ??? P(?? = 3), P(X = 0) y P(X ??? 3)
    
    dpois(0,3)
  
  
######## REPRESENTACIONES GRÁFICAS #######
    
  
    x<-0:100
    p<-dbinom(x, 100, 0.5)
    plot(x,p,type='h')
    
  
    # 1.
    
    x<-0:10
    p<-dpois(x, 2)
    plot(x,p,type='h')
    
    #2
    
    x<-0:20
    p<-dpois(x, 10)
    plot(x,p,type='h') 
    
    ### Ejercicio 4.2.3
    
    #a) 
        dnbinom(2, 1, 0.8) #Saber la P de que acierte la primera
                           #pregunta y sea la la 3ª
        
    #b)
        dnbinom(2, 10, 0.8) #Saber la P de que acierte 10 (aprobar) 
                            #y al llevar 12 ya esté aprobado
        
        
    
####################### PARTE 2: 4.3 - VARIABLES ALEATORIAS CONTINUAS #########
        
    #APARTADO 4.3
        
       #a) P(X < 0.3) = P(X ??? 0.3)
        
          pnorm(0.3, mean=0, sd=1)
        
        
       #b) P(X > 1.4) = P(X ??? 1.4)
        
            1- pnorm(1.4) #o
            pnorm(1.4, lower.tail=FALSE)
            
       #c) P(0.3 < X < 1.4)
            
            pnorm(1.4) - pnorm(0.3)
            
            
    #APARTADO 4.3.1 - PRIMEROS CÁLCULOS
            
      #1. X ??? U(2, 5), P(X < 3.7)

            punif(3.7, min=2, max=5)
            
      #2. X ??? Exp(?? = 3), P(X < 0.5), P(X > 1.5) y P(0.5 < X < 1.5)
            
            pexp(0.5, rate=3)
            1-pexp(1.5, rate=3) | pexp(1.5, 3, lower.tail=FALSE)
            pexp(1.5, 3) - pexp(0.5, 3)
            
            
      #3. X ??? Exp(?? = 3), los cuantiles 0.05 y 0.95
            
            qexp(0.05, 3)
            qexp(0.95, 3)
            cuantiles <- qexp(c(0.05, 0.95), 3)
            pexp(cuantiles, 3)
      
            
      #4.1 X ??? N(1.5, 3.2), P(X < ???1.8)
            
            pnorm(-1.8, mean=1.5, sd=3.2)
            
            
     #4.2 X ??? N(1.5, 3.2), P(X > 2.7) = 1 - p(x <= 2.7)
            
            pnorm(2.7, 1.5, 3.2, lower.tail=FALSE)
            1 - pnorm(2.7, 1.5, 3.2)
            
            
     #4.3 X ??? N(1.5, 3.2), P(-1.8 < X < 2.7) = P(X < 2.7) - P(X < -1.8)
            
            pnorm(2.7, 1.5, 3.2) - pnorm(-1.8, 1.5, 3.2)
        
      
     #5. X ??? N(1.5, 3.2), los cuartiles
            
            qnorm(c(0.25, 0.50, 0.75), 1.5, 3.2)
            
            
     #6. X ??? N(1.5, 3.2), el valor a tal que P(X > a) = 0.3
            
            qnorm(0.7, 1.5, 3.2)
            qnorm(0.3, 1.5, 3.2, lower.tail=FALSE)
            
            
            
      
       
  #APARTADO 4.3.2 - REPRESENTACIONES GRÁFICAS
            
            
      curve(dunif(x,-1,2), xlim=c(-1,2), ylim=c(0,1), xlab="x", ylab="
      Función de densidade", main="Distribución uniforme U(-1,2)")

      
      #1. Representa grácamente la función de densidad 
      #de una distribución exponencial 
      #de ?? = 0.5.
      #¿Qué observas si subimos ?? a 1 y a 2? 
      #Puedes hacer uso del siguiente código en R:
      
      
      curve(dexp(x,rate=0.5),xlim=c(0,4),ylim=c(0,2))
      curve(dexp(x,rate=1),col="red", add=T)
      curve(dexp(x,rate=2),col="blue", add=T)
      legend("topright", c("Exp(0.5)", "Exp(1)", "Exp(2)"),
                col=c("black", "red", "blue"), lty=1, bty="n")
      
      #Representa gráficamente la función de densidad de una N(0, 1). ¿
      #Qué ocurre si aumentamos la varianza a 4? 
      #¿y si disminuye a 0.5? 
      #Representa las tres distribuciones en el mismo gráfico
      #(utiliza el código dado en los apartados anteriores).
      
      curve(dnorm(x,0,1),xlim=c(-4,4),ylim=c(0,0.70))
      curve(dnorm(x,0,2),col="red", add=T)
      curve(dnorm(x,0, sqrt(0.5)),col="blue", add=T)
      legend("topright", c("N(0,1)", "N(0,2)", "N(0,0.7))"),
             col=c("black", "red", "blue"), lty=1, bty="n")
      
      #Representa gráficamente la función de densidad de una N(0, 1)
      #Qué ocurre si aumentamos la
      #media a 2? ¿y si disminuye a -2?
      
      curve(dnorm(x,0,1),xlim=c(-6,6),ylim=c(0,0.5))
      curve(dnorm(x,2,1),col="red", add=T)
      curve(dnorm(x,-2,1),col="blue", add=T)
      legend("topright", c("N(0,1)", "N(2,1)", "N(-2,1))"),
             col=c("black", "red", "blue"), lty=1, bty="n")
      
      
      
   #EJERCICIO 4.3.3
      
      #1.
        media <- 2.5
        desv <- sqrt(0.05^2+0.1^2)
        curve(dnorm(x, media, desv), xlim=c(1.5, 3.5),
              main= "Grosor total de la envoltura del pendrive")
        
        #D = "Defectuoso" => (x<2.4)u(x>2.55)
        #ND = "No defectuoso" => 2.4<x<2.55
        
        P_ND <- pnorm(2.55, media, desv) - pnorm(2.4, media, desv)
        p_D <- 1- PD_ND
        
        #k tal que P(x>=k)=0.01? Cuantil 0.99
        #P(x>K) = 1 - P(x<=k) = 0.01 => P(x<=k) = 1-0.01 = 0.99
        #                                                => k = Q_ 0.99
        qnorm(0.01, media, desv, lower.tail=FALSE)
        qnorm(0.99, media, desv)
        
        # Y= "nº de pendrive defectuosos en un lote de 10" - B(n=10, p=p_D)
        # P(Y=5)
        
        dbinom(5, size=10, prob = p_D)
        
      
      
    #EJERCICIO 4.4
        
        # 1) x= "Nº de dixitos recibidos con erro"
        # X - B(200,0.1)
        x <- 0:200
        p <- dbinom(x, 200, 0.1)
        plot(x, p, type="h")
        
        # 2) P(X>10) = 1 - P(X<=10)
        sum(dbinom(11:200, 200, 0.1))
        1 - pbinom(10, 200, 0.1)
        
        #3) Y = "Nº de dixitos emitidos ata o primeiro erro"
        # Y - BN(1, 0.1)
        
        y <- 0:200
        p <- dnbinom(y, 1, 0.1)
        plot(y, p, type="h")
        
        #4) B(n, p) pode aproximarse para n grande e 0.1<=p<=0.9 por N(np, sqrt(np(1-p)))
        # N(200*0.1, sqrt(200*0.1*0.9)) = N(20, 4.24)
        x<-seq(0,200, lenght=1000)
        p<-dnorm(x, 20, 4.24)
        plot(x, p, type="l")
        windows()
        
        
        #5) X - N(20, 4.24), P(X>10) = 1 - P(X<=10)
        1 - pnorm(10, 20, 4.24)
        
        
    # EJERCICIO 4.5
        
        
        # X = "Nº visitas por minuto" - Poisson(lambda= 10)
        # E(X) = 10 = lambda
        
        #1.)
        
          # P(X>3) = 1 - P(X<=3)
          # P(X>3) = 1 - P(X<=3) = 1 - P[P(X=0)+P(X=1)+P(X=2)+P(X=3)]
        
        1 - ppois(3, lambda=10)
        ppois(3, 10, lower.tail=FALSE)
        
        
        #2.) Representar la función de masa de probabilidad de nº de visitas en 30 segundos
        
        x <- 0:15
        p <- dpois(x, lambda=5)
        plot(x, p, type="h", xlim=c(0,15), ylim=c(0, 0.5), lwd=3)
        lines(x, dpois(x, lambda=10), type="h", col="red", lty=2, lwd=3)
        lines(x, dpois(x, lambda=2.5), type="h", col="blue", lty=3, lwd=3)
        
        #3.) P de que en 30 s no haya ningun acceso a la web
        
        # P(Y=0)
        
        dpois(0, lambda=5)
        
        # T="tiempo en min transcurrido entre dos visitas" - Exp(10)
        # P(T>0.5) = 1- P(T<=0.5)
        
        1- pexp(0.5, rate=10)
        pexp(0.5, rate=10, lower.tail=FALSE)
        
            
        # Función de densidad de la distribución que mide el tiempo transcurrido
        # entre dos visitas consecutivas a la web
        
        curve(dexp(x, rate=10), xlim=c(0,1), ylim=c(0, 20), main="Exp(10)", xlab="T",
              ylab="Función de densidad")
        curve(dexp(x, rate=10*2), add=T, col="red")
        curve(dexp(x, rate=10/2), add=T, col="blue")
        
        
        # Cuantiles: Mediana(Q_0.5) y Q_0.10
        
        qexp(0.5, rate=10)
        
        # P(T>k) = 0.90
        
        qexp(0.1, rate=10)
        qexp(0.9, rate=10, lower.tail=FALSE)
    
    
    
    
    
    
    
    

