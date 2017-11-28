autos.data <- read.table("/home/sly/Desktop/autos.txt", header = TRUE)
girasol.data <- read.table("/home/sly/Desktop/girasol.txt", header = TRUE)
muebles.data <- read.table("/home/sly/Desktop/muebles.txt", header = TRUE)

attach(autos.data)
autoslm <- lm(precio ~ calidad)

# Ejericicio 2:
plot(precio, calidad)
abline(autoslm$coefficients)
#Sí, se ve una relacion lineal entre precio y calidad.  El modelo lineal tomaria como variables precio y calidad.

#R^2 es el coeficiente de deterinacion.
# Residual standard error: 15830 on 40 degrees of freedom = S
# Vendría a ser el valor observado del estimador consistente del desvío
# Multiple R-squared:  0.5643,	Adjusted R-squared:  0.5534 . Esto es el R^2, el coef. de determinación.
# Como es 0,5643, cuanto más cerca de cero, la predicción es mala y si está cerca de 1 es mejor.
# Bueno, decimos que estima de forma aceptable. Sí fuera R^2 = 1, quiere decir que la diferencia entr el punto
#real y el estimado es cero, entonces la estimación es perfecta.

 # Y = 7020.7 + 6299.2 * x Esta es nuestra recta para predecir
# Con x = 50000 queda 314967021 . Es turbio. La cuenta es >  sum(autoslm$coefficients*c(1,50000)) = 314964834
# La diferencia con nuestro resultados debe ser por los decimales.
sum(advlm$coefficients*c(1,250))

# d) Estimador de la varianza del error.
# El summary nos daba S = 15830. El estimador de la varianza del erorr es S^2.
# Entonces S^2 = 250588900

#Para instalar el ISLR:
#1) install.packages('ISLR')
#2) library('ISLR')

#El p-valor del summary nos da 9.97e-09. Entnces si alpha > pvalor (que viene del Tobs) rechazamos H0.
# Entonces vamos a rechazar H0 la mayoría de las veces. Hay evidencia para decir que el precio y la calidad están relacionados.




  

# Tenemos el estimador de cuadrados minimos


summary(autoslm) #Acá tira los estimadores


#EJERCICIO 3

x <- rnorm(100, 0, 1)

eps <- rnorm(100, 0, 0.25)

#c)
y = -1 + 0.5*x + eps
#Acá B0 = -1 y B1 = 0,5

#d)
plot(x, y)
ej3lm <- lm(x ~ y)
summary(ej3lm)
#Entonces B0 = 1,50397 y B1 = 1,53364