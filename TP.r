#Trabajo Práctico Probabilidad y Estadística (c)#
#Segundo Cuatrimestre 2017#
# Luis Greco - Nicolas Hertzulis - Ruslan Sanmartin Sobol#

#Fijo la "semilla" para que no nos de cosas distintos resultados cada vez que experimentamos
set.seed(1109)
options(warn=-1) #Le saco los warnings para que no joda, en la binomial tira muchos, pero no se va de rango. Todo ok
#Ejercicio 1
#Esto es un copypaste de la exponencial de la clase11 pero que en el return tiene la media


funcion.inversa <- function(u, lambda){
  sal <- -1*log(1-u)/lambda
  return(sal)
}
Generar.exponenciales_devolviendo_media <- function(n,lambda){
  U <- runif(n)
  sal <- funcion.inversa(U,lambda)
  return(mean(sal))
}
y <- seq(length = 3000)
for (i in 1:3000){
  y[i] <- Generar.exponenciales_devolviendo_media(i, 3)
}
z <- seq(length = 3000)
for (j in 1:3000){
  set.seed(1109)
  z[j] <- Generar.exponenciales_devolviendo_media(j, 3)
}


plot(y,col="blue")
plot(z,col="red")

#Ejercicio 2
#2 a)
mediasA <- seq(length=1000)
for (j in 1:1000){
x1 <- rexp(1, rate = 3)
x2 <- rexp(1, rate = 3)
mediasA[j] <- (x1+x2)/2
}
#Histograma
hist(mediasA)

#Boxplot
boxplot(mediasA)

#Q-Q Plot
qqnorm(mediasA)
qqline(mediasA)#La cola del plot

# Punto b)

mediasB <- seq(length = 1000)

for (i in 1:1000){
a <- c(rexp(1, rate = 3), rexp(1, rate = 3), rexp(1, rate = 3), rexp(1, rate = 3), rexp(1, rate = 3))
mediasB[i] = mean(a)
}
#Histograma
hist(mediasB)

#Boxplot
boxplot(mediasB)

#Q-Q Plot
qqnorm(mediasB)
qqline(mediasB)#La cola del plot


#Punto C
mediasC1 <- seq(length = 1000) #Acá van con n=30
mediasC2 <-seq(length = 1000) #Acá van con n=500

a <- seq(length = 30)
b <- seq(length = 500)

for (j in 1:1000){
  a <- rexp(30, 3)
  b <- rexp(500, 3)
  mediasC1[j] = mean(a)
  mediasC2[j] = mean(b)
}

#Histograma
hist(mediasC1)
hist(mediasC2)

#Boxplot
boxplot(mediasC1)
boxplot(mediasC2)

#Q-Q Plot
qqnorm(mediasC1)
qqline(mediasC1)#La cola del plot

qqnorm(mediasC2)
qqline(mediasC2)

#Punto D
c <- rnorm(1000, 0, 1)
hist(c)
boxplot(c)
qqnorm(c)
qqline(c)


#Punto E

#boxplot
boxplot(mediasA,mediasB,mediasC1,mediasC2,c)

#EJ 3

#Punto a)
mediaX1 <- mean(mediasA)
varX1 <- var(mediasA)

mediaX1
varX1

mediaX2 <- mean(mediasB)
varX2 <- var(mediasB)

mediaX3 <- mean(mediasC1)
varX3 <- var(mediasC1)

mediaX4 <- mean(mediasC2)
varX4 <- var(mediasC2)

#Punto b)

#Transformación: (1/3 es la esperanza de la exp(3) )
transformacionA <- (mediaX1 - (1/3))/(sqrt((1/9)/1000))
transformacionA

transformacionB <- (mediaX2 - (1/5))/(sqrt((1/25)/1000))
transformacionB

transformacionC1 <- (mediaX3 - (1/30))/(sqrt((1/900)/1000))
transformacionC1

transformacionC2 <- (mediaX3 - (1/500))/(sqrt((1/250000)/1000))
transformacionC2


boxplot(mediasA, mediasB, mediasC1, mediasC2)

qqnorm(mediasA)
qqline(mediasA)

qqnorm(mediasB)
qqline(mediasB)

qqnorm(mediasC1)
qqline(mediasC1)

qqnorm(mediasC2)
qqline(mediasC2)

#Punto c)
hist(mediasA, freq = FALSE, xlim = c(-10, 130), ylim = c(0, 0.01))
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="darkblue", lwd=2)
#La misma con otra escala
hist(mediasA, freq = FALSE, xlim = c(-10, 130), ylim = c(0, 0.4))
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="darkblue", lwd=2)



hist(mediasB, freq = FALSE, xlim = c(-10, 120))
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="darkblue", lwd=2)


hist(mediasC1, freq = FALSE, col = "grey", ylim = c(0, 0.4))
curve(dnorm(x, mean=111, sd=1), add=TRUE, col="darkblue", lwd=2)


hist(mediasC2, freq = FALSE, col = "grey", xlim = c(109, 113))
curve(dnorm(x, mean=111, sd=1), add=TRUE, col="darkblue", lwd=2)

#Punto d) pdf

#EJ 4


y <- seq(length = 3000)
for (i in 1:3000){
  y[i] <- rbinom(5,size=i,prob=1/9) 
}
z <- seq(length = 3000)
for (j in 1:3000){
  set.seed(1109)
  z[j] <- mean(rbinom(5,size=j,prob=1/9))
}


plot(y,col="blue")
plot(z,col="red")


#Ejercicio 2
#2 a)
mediasA <- seq(length=1000)
for (j in 1:1000){
  x1 <- rbinom(5,size=j, prob=1/9)
  x2 <- rbinom(5,size=j, prob=1/9)
  mediasA[j] <- (x1+x2)/2
}
#Histograma
hist(mediasA)

#Boxplot
boxplot(mediasA)

#Q-Q Plot
qqnorm(mediasA)
qqline(mediasA)#La cola del plot

# Punto b)

mediasB <- seq(length = 1000)

for (i in 1:1000){
  a <- c(rbinom(10,size=i, prob=1/9), rbinom(10,size=i, prob=1/9), rbinom(10,size=i, prob=1/9), rbinom(10,size=i, prob=1/9), rbinom(10,size=i, prob=1/9))
  mediasB[i] = mean(a)
}
#Histograma
hist(mediasB)

#Boxplot
boxplot(mediasB)

#Q-Q Plot
qqnorm(mediasB)
qqline(mediasB)#La cola del plot


#Punto C
mediasC1 <- seq(length = 1000) #Acá van con n=30
mediasC2 <-seq(length = 1000) #Acá van con n=500

a <- seq(length = 30)
b <- seq(length = 500)

for (j in 1:1000){
  a <- rbinom(30,size=i, prob=1/9)
  b <- rbinom(500,size=i, prob=1/9)
  mediasC1[j] = mean(a)
  mediasC2[j] = mean(b)
}

#Histograma
hist(mediasC1)
hist(mediasC2)

#Boxplot
boxplot(mediasC1)
boxplot(mediasC2)

#Q-Q Plot
qqnorm(mediasC1)
qqline(mediasC1)#La cola del plot

qqnorm(mediasC2)
qqline(mediasC2)

#Punto D
c <- rnorm(1000, 0, 1)
hist(c)
boxplot(c)
qqnorm(c)
qqline(c)

#Punto E

#boxplot
boxplot(mediasA,mediasB,mediasC1,mediasC2,c)

#EJ 3

#Punto a)
mediaX1 <- mean(mediasA)
varX1 <- var(mediasA)

mediaX1
varX1

mediaX2 <- mean(mediasB)
varX2 <- var(mediasB)

mediaX3 <- mean(mediasC1)
varX3 <- var(mediasC1)

mediaX4 <- mean(mediasC2)
varX4 <- var(mediasC2)

#Punto b)

#Transformación: (np es la esperanza es decir 5*1/9== 5/9 )
transformacionA <- (mediaX1 - (5/9))/(sqrt((25/81)/1000))
transformacionA

transformacionB <- (mediaX2 - (10/9))/(sqrt((100/81)/1000))
transformacionB
transformacionC1 <- (mediaX3 - (30/9))/(sqrt((900/81)/1000))
transformacionC1

transformacionC2 <- (mediaX3 - (500/9))/(sqrt((250000/81)/1000))
transformacionC2

#
boxplot(mediasA, mediasB, mediasC1, mediasC2)

qqnorm(mediasA)
qqline(mediasA)

qqnorm(mediasB)
qqline(mediasB)

qqnorm(mediasC1)
qqline(mediasC1)

qqnorm(mediasC2)
qqline(mediasC2)

#Punto c)
hist(mediasA, freq = FALSE, xlim = c(-10, 100), ylim = c(0, 0.5))
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="darkblue", lwd=2)
#Otro del mismo pero en otra escala
hist(mediasA, freq = FALSE, xlim = c(-10, 100), ylim = c(0, 0.03))
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="darkblue", lwd=2)


hist(mediasB, freq = FALSE, xlim = c(-10, 120))
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="darkblue", lwd=2)



hist(mediasC1, freq = FALSE, col = "grey", ylim = c(0, 0.4))
#111.1069 es la media muestral de eso. Lo estoy poniendo para que quede superpuesto, si hubiera puesto
#la Normal(0,1) hubiera quedado en otro lado del gráfico. Acá solo la desplacé un poco para que quede más lindo.
curve(dnorm(x, mean=111.1069, sd=1), add=TRUE, col="darkblue", lwd=2)



hist(mediasC2, freq = FALSE, col = "grey", xlim = c(108, 114))
curve(dnorm(x, mean=111.0976, sd=1), add=TRUE, col="darkblue", lwd=2)
