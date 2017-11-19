#Trabajo Práctico Probabilidad y Estadística (c)#
#Segundo Cuatrimestre 2017#
# Luis Greco - Nicolas Hertzulis - Ruslan Sanmartin Sobol#

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


plot(y,col="blue")
plot(z,col="red")
#Ver los graficos bizarros que me da
#Explicacion
# Se puede verificar mediante la Ley de Grandes Numeros, la media real y la media estimada, son casi exactas, con un posible error de (+0,05 o -0,05)
# En el plot de promedio, se obvserva como por la LGN, la misma converge a la media cuando su tamaño de muestras tiende a infinito.
# Se puede observar el comportamiento asintotico del promedio muestral.

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
boxplot(mediasA,mediasB,mediasC1,mediasC2)
boxplot(mediasA)
boxplot(mediasB)
boxplot(mediasC1)
boxplot(mediasC2)
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

#DE QUE ME PIDEN QUE HAGA LOS BOXPLOTS??? NO ME QUEDA CLARO
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
hist(mediasA, freq = FALSE)
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="darkblue", lwd=2)


hist(mediasB)
curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)



hist(mediasC1, freq = FALSE, col = "grey")
curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)


hist(mediasC2, freq = FALSE, col = "grey")
x = mediasA
curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)


#Punto d) HACER

# Se observa en el histograma `mediasA` no tiene una buena aproximacion,dado a que sera necesaria una muestra mayor, dado que carece de simetria, lo cual no nos da informacion respecto a si tiene a una normal
# Con respecto a los histogramas, `mediasB`, `mediasC1` y `mediasC2`, se puede observar como los graficos empiezan a represtan una "campana" al estilo de una normal, lo cual nos informa, que nuestra muestra fue lo suficientemente buena, para poder estimar dicha distribucion aleatoria.
# Aunque estaria bueno, poder agrander la muestra para el histograma `mediasB`, para que sea mas claro la forma de la campana, dado que en la muestra actual, la misma es minuscula.
#EJ 4
x <- rnorm(1000)
hist(x, freq = FALSE, col = "grey")
curve(dnorm(x, 3, 2), col = 2, add = TRUE)
