#Trabajo Práctico Probabilidad y Estadística (c)#
#Segundo Cuatrimestre 2017#
# Luis Greco - Nicolas Hertzulis - Ruslan Sobol#

#Fijo la "semilla" para que no nos de cosas distintos resultados cada vez que experimentamos
set.seed(1109)

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


plot(y)
plot(z)
#Ver los graficos bizarros que me da

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

a <- seq(length = 1000)
b <- seq(length = 1000)

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

