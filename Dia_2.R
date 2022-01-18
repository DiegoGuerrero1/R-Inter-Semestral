# Día 2 18/01/2022
# Diego Guerrero 
# R version 4.0.5 (2021-03-31) -- "Shake and Throw"

## Repaso día 1 
rm(list = ls())

mtcars.db <- read.csv("mtcars.csv") #Leer database

str(mtcars.db) #ver estructura 

mtcars.db

rownames(mtcars.db) #Primero vemos los nombres de las filas 

rownames(mtcars.db)<- mtcars.db$X #Reemplazamos la indexación por el nombre "X", usando rownames y reemplazándolo por la columna X accediendo con $

names(mtcars.db) #Vemos que columna es "X".

mtcars.db <- mtcars.db[-1] #Quitamos X por medio de indexación 

mtcars.db


names(mtcars.db)[c(1,4,6,9)] <- c("Millas_por_galón","Caballos_de_fuerza","Peso", "Transmision") #Usamos indexación y metemos un vector en el orden de las columnas

names(mtcars.db) #Checar 

hist(mtcars.db$Millas_por_galón) #histograma 

mtcars.db$Transmision <- as.factor(mtcars.db$Transmision)

str(mtcars.db)

plot(y = mtcars.db$Caballos_de_fuerza, x = mtcars.db$Peso, col = mtcars.db$Transmision, xlab = "Peso(Ton)", ylab = "Caballos de fuerza")


# Día 2 

#caja y bigote millas por galón boxplot(mph)

boxplot(mtcars.db$Millas_por_galón ~ mtcars.db$Transmision, col="blue") #x es variable numperica y y nominal 

#manuales (1) no tienen una distribución normal, la mediana no está a la mitad dela caja. El promedio no coincidirá con la mediana. 

?boxplot

boxplot(mtcars.db$Millas_por_galón ~ mtcars.db$Transmision, col="blue", horizontal = T, notch = T) #x es variable numperica y y nominal 

#Prueba T. Saber si son diferentes. 
#Relación de la señal(medias) contra el ruido(varianza, que tanto de exparcen). Acentuación -> todos los valores se agrupan en la media. 
#Dependiendo dela naturaleza del trabajo se desea que se mantenga o no agrupados en la media. 

# t = Señal/Ruido 


mtcar.ttest <- t.test(mtcars.db$Millas_por_galón~mtcars.db$Transmision)#Es negativo pero se puede ver por valor absoluto. Entre mpas grande más diferencia 
#Un valor de P mayor a 0.05 significa que las medias son iguales. 
#Como el valor de P es menor a 0.5 significa que son diferentes. 
plot(mtcar.ttest)

# Ejercicio 1. ¿La forma del motor tiene influencia en los caballos de fuerza?

str(mtcars.db)
mtcars.db$vs <- as.factor(mtcars.db$vs)
boxplot(mtcars.db$Caballos_de_fuerza ~ mtcars.db$vs, col="blue", horizontal = F, notch = F) #x es variable numperica y y nominal 

## Prueba de t 
t.test(mtcars.db$Caballos_de_fuerza~mtcars.db$vs) #Las medianas son muy diferentes, la probabilidad de que sean iguales es de 0.00000182


# Distribuciones 

cars.db <- read.csv2("cars.csv")
str(cars.db)

View(cars.db)

dotchart(cars.db$speed)
dotchart(cars.db$dist)

plot(cars.db)

hist(cars.db$speed)
hist(cars.db$dist)

## Suavizar el histograma 

plot(density(cars.db$speed)) #Se ve mejor la distribución 

plot(density(cars.db$dist)) # X= variable de interés 

# Parte dos


library("MASS")

#Ajustamos distribuciones para datos univariados 
?fitdistr

fitdistr(cars.db$speed, densfun = "normal") #desviación estandar y la media
fitdistr(cars.db$dist, densfun = "negative binomial")

library(visualize)

visualize.norm(mu = median(cars.db$speed), sd = sd(cars.db$speed), stat = c(10,20), section = "bounded") #P : Probabilidad de que se encuentre en ese intervalo 

visualize.norm(mu = median(cars.db$speed), sd = sd(cars.db$speed), stat = 20, section = "upper") 

?visualize

## Estandarización de datos 

#Restar la media y dividir entre la desviación 

### ejercicio 7

cars.db$dist.estandar <- (cars.db$dist - mean(cars.db$dist))/sd(cars.db$dist)

hist(cars.db$dist.estandar)

plot(density(cars.db$dist.estandar))
plot(density(cars.db$dist))

hist(cars.db$dist)

### ejercicio 8: Distribución exponencial

lluvia.db <- read.csv("lluvia.csv")
str(lluvia.db)

hist(lluvia.db$mm.lluvia)

fitdistr(lluvia.db$mm.lluvia, densfun = "exponential")
?fitdistr
str(lluvia.db)
summary(lluvia.db)
lluvia.db$mm.lluvia.norm <- lluvia.db$mm.lluvia*100

visualize.exp(theta = as.numeric(lluvia.db$mm.lluvia),stat = 3.0,section = "upper" ) #checar
?visualize.exp

#Correlaciones 

#Usamos covarianza para la correlacion 
cov(cars.db$dist, cars$speed)
cor(cars.db$dist, cars$speed) #Una relacion 1:1 es perfecta, a medida que se acerca al 1 es alta. 

#Otras db 


cor(iris.db[1:4])

plot(iris.db$Petal.Length, iris.db$X)

cor(mtcars.db[c(1,4,6)])


## Ejercicio 9 

adv.db <- read.csv("advertising.csv")

str(adv.db)

cor(adv.db)

plot(x = adv.db$TV, y = adv.db$Sales)

plot(x = adv.db$Newspaper, y = adv.db$Sales)

#Regresion lineal 

#Bo = venta cero (intercepto)
#Pendiente de la variable 

adv.lm <- lm(adv.db$Sales~adv.db$TV)

#Ventas = 6.97 + 0.5546*TV + error

summary(adv.lm)

plot(adv.db$Sales~adv.db$TV)+
  abline(a = 6.97, b = 0.05546, col = "red")+
  text(x = 20, y = 25, "r² = 0.8112 ", cex = 0.7) #81% es explicado por el modelo 

# r^2 = sqrt(cor(adv.db$Sales, adv.db$TV)) 

#Ejercicio 10 

## Símbolos (pch=n, con 0 <= n =< 25) Líneas (lty = n)

plot(adv.db$Sales~adv.db$TV, pch = 4)+
  abline(a = 6.97, b = 0.05546, col = "blue", lty = 2)+
  text(x = 20, y = 25, "r² = 0.8112 ", cex = 0.7)

#Agregar leyendas 

plot(adv.db$Sales~adv.db$TV, pch = 4)+
  abline(a = 6.97, b = 0.05546, col = "blue", lty = 2)+
  text(x = 20, y = 25, "r² = 0.8112 ", cex = 0.7)


#Ejercicio 11
str(iris.db)
iris.db$Species <- as.factor(iris.db$Species)
iris$Species <- as.factor(iris$Species)

lm.setosa <- lm(iris.db[iris.db$Species == "setosa", ]$Petal.Length ~ iris.db[iris.db$Species == "sepalo", ]$Petal.Width )

str(iris.db)

iris.setosa <- iris[iris$Species == "setosa", ]
iris.versicolor <- iris[iris$Species == "versicolor", ]
iris.virginica <- iris[iris$Species == "virginica", ]

#Opción 2: Generar una función

lm.by.specie <- function(database, specie){
  database.rec <- database[database$Species == specie, ]
  lm.specie <- lm(database.rec$Petal.Length~database.rec$Petal.Width)
  return(lm.specie)
}

lm.setosa <- lm(iris.setosa$Petal.Length~iris.setosa$Petal.Width)
lm.setosa

lm.by.specie(iris, "setosa")

lm.versicolor <-lm.by.specie(iris, "versicolor")
lm.versicolor

lm.virginica <- lm.by.specie(iris, "virginica")
lm.virginica 

plot(iris.db$Petal.Length~iris.db$Petal.Width, col = iris.db$Species)+ 
  abline(a = 1.3276, b = 0.5465, col = "black", lty = 2)+
  abline(a = 1.781, b=1.869,col = "red", lty = 3)+
  abline(a = 4.2407, b = 0.6473, col = "green", lty = 3)
  

iris_sepalo <- iris[iris$Ancho.Sepalo >= 2.5, ]




