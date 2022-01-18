# Introducción 

## ¿Qué es R?

- Lenguaje de programación basado el lenguaje *S*
  - Software de visualiación 
- Ambiente de trabajo 
-     Abro un espacio para crear objetos. 
- Distribuido, mantenido por **R Development core Team* con sede en el Instituto de estadística y Matemáticas de la Universidad de Viena. 


# Símbolos

#El igual va para ambos lados. La dirección a veces es importante 
# Objetos 
#- Vector: 
 # Estructura más sencilla de datos. Una clección de deatos del mismo tipo. 

x <- c(1,2,3,4,5)
y <- c("Diego", "Gabo", "Rorigo", "Liz")

x[3]
y[2]

z <- x[3]
z <- x[5]
z




mat <- matrix(1:9, nrow = 3, ncol = 3)
mat


# Dataframes
#Diferentes tipos de datos 


db <- data.frame(id = letters[1:10], x = 1:10, y = 11:20)
View(db)


#EL abmiente R es una lista, hay que borrar el ambiente 


rm(list = ls())


# Crear una funión 


fahrenheit_a_celcius <- function(temp_F){
  temp_C <-(temp_F - 32)*5/9
  return(temp_C)
}
cel <- fahrenheit_a_celcius(90)

cel





## Ejercicio 
calcular la mnultiplicación de dos raices cuadradas

prim_ej <- function(n,m){
  return( sqrt(n)*sqrt(m))
}

prim_ej(2,3)

err_ej <- function(my_vec){
  err_est <- sqrt(sd(my_vec)/length(my_vec))
  return(err_est)
}

err_ej(c(1,2,3,4,5))

sum_ej <- function(my_vec){
  my_vec <- my_vec + 1
  return(my_vec)
}

sum_ej(c(0))



Calcular el error estándar de la media (desviación standar/ raíz cuadrada de n )

Sumar 1 a los valores de un vector y despues calcular el logaritmo natural 





# Estructura de un script 


rm(list = ls())   #Limpiar la memoria 
setwd("~/Documents/R-Course/R-Intersemestral")

# Ejercicio serio 

iris <- read.csv("iris.csv")
str(iris)
iris$Petal.Length <- 
  iris[ ,4]


iris<-iris[ , -1] #Quitar columna 
iris$fecha <- "2022-01-17"#Añadir columna
iris



# Operadores relacionales





iris
iris_set <-iris[iris$Species == "setosa", ]
iris_set

iris$Species
str(iris)
levels(iris$Species)

iris_set <-iris[iris$Sepal.Width>=3,]

iris_set <- iris[iris$Species != "setosa",]
iris_set

iris_set <- iris[iris$Sepal.Width >= 3 & iris$Sepal.Width >= 2,]
iris_set

#En función de y~x 



# Ejercicio 



names(iris)
names(iris)<-c("Longitud.Sepalo", "Ancho.Sepalo", "Longitud.Petalo","Ancho.Petalo","Especie","Fecha")

names(iris)[4]

row.names(iris)[1]<-"a"
row.names(iris)

#CAlcular el numero de elementos por categoría 
table(iris$Especie)



# Graficas básicas 

hist(iris$Longitud.Sepalo) #Histograma
#plot(iris) # Hacer un plot de toda la tabla 
iris
dotchart(iris$Longitud.Petalo, groups = as.factor(iris$Especie)) #Como se comportan las especies respecto a la longitud del petalo 

plot(x = iris$Longitud.Sepalo, y = iris$Ancho.Sepalo)
plot(x = iris$Longitud.Sepalo, y = iris$Ancho.Sepalo,col=iris$Especie)
str(iris)

iris$Especie <- as.factor(iris$Especie)
plot(x = iris$Longitud.Sepalo, y = iris$Ancho.Sepalo,col=iris$Especie, xlab = "Longitud del sépalo(cm)",ylab = "Ancho del sépalo (cm)")


#Caja y bigote

boxplot(iris$Longitud.Sepalo~iris$Especie, col = c("red","orange", "pink"))

#Ejercicio 

#1 
iris
iris_sepalo <- iris[iris$Ancho.Sepalo >= 2.5, ]

str(iris_sepalo)
summary(iris_sepalo)

plot(y = iris_sepalo$Longitud.Petalo, x = iris_sepalo$Longitud.Sepalo, col = iris_sepalo$Especie, xlab = "Longtud del sépalo (cm)", ylab= "Longitud del pétalo (cm)")


#SEgundo ejercicio 
wage <-read.csv("Wage.csv")
View(wage)

wage_wx <-wage[ ,-1]
names(wage_wx)

wage_wx <- wage_wx[wage_wx$wage<= 280, ]

summary(wage_wx)
wage_wx$education <- as.factor(wage_wx$education)
plot(x = wage_wx$age, y = wage_wx$wage, col= wage_wx$education, xlab = "Edad", ylab = "Salario(USD)")
