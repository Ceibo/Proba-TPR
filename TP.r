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
medias <- seq(length=1000)
for (j in 1:1000){
x1 <- rexp(1, rate = 3)
x2 <- rexp(1, rate = 3)
medias[j] <- (x1+x2)/2
}
#Histograma
hist(medias)

#Boxplot
boxplot(medias)

#Q-Q Plot
qqnorm(medias)
qqline(medias)#La cola del plot


