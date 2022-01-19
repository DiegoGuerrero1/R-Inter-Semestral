# Día 3 19/01/2022
# Diego Guerrero 
# R version 4.0.5 (2021-03-31) -- "Shake and Throw"

rm(list = ls()) #Borrando ambiente 
setwd("/home/guerrero/Documents/R-Course/R-Intersemestral")

# Repaso día 2
alumnos.df <- data.frame(nombres=c("Juan","Ines","Pablo"), calificacion = c(8,6,7,8,6,9), Fecha=c("1985-09-24", "1978-02-04", "1988-12-31"))#Fechas: aa/mm/dd
str(alumnos.df)
#remove(newdf)

alumnos.df$Fecha <- as.Date(alumnos.df$Fecha) #Casting 
str(alumnos.df)#Checamos estructura

plot(alumnos.df$calificacion~alumnos.df$Fecha)#Volvemos a plotear 

#Alternativa 
alumnos.df.rev <- data.frame(nombres=c("Juan","Ines","Pablo", "Ivan", "José", "Gabo"), calificacion = c(8.6,7.8,6.9, 5.6,7.8,9.1), Fecha=c("24-09-1985", "04-02-1978", "31-12-1988", "02-10-2001","04-01-2002","15-02-2002"))#Fechas: aa/mm/dd
str(alumnos.df.rev)
plot(alumnos.df.rev)

alumnos.df.rev$Fecha <- as.Date(alumnos.df.rev$Fecha, tryFormats = "%d-%m-%Y") #y = aa, Y=aaaa, se tiene que poner "-" o "/" dependiendo de cómo lo pusiste

alumnos.df <-  alumnos.df.rev
str(alumnos.df.rev)#Estructura 
plot(alumnos.df.rev$calificacion~alumnos.df.rev$Fecha) #Plot


## Ordenar por filas 

x <- c(1300, 1, 2344, 500, 340, 200, 100, 1, 99)
y <- sort(x, decreasing = T)
y
?sort()

alumnos.df[order(alumnos.df$nombres),] #Ordenamos por orden alfabético 

alumnos.df[order(alumnos.df$Fecha),] #Ordenamos por fehca de nacimiento 

alumnos.df[order(alumnos.df$nombres, alumnos.df$calificacion),] 


alumnos.df[7,] = c("Ines", 10, "2001-01-12") #Agregamos una Ines 

str(alumnos.df)



# Transponer un df 

alumnos.df.t <- t(alumnos.df) 
str(alumnos.df.t)



## Funciones de la familia apply 

# Permiten manipular segmentos de nuestros datos, sacar datos de nuestra tabla sin separarlas previemente. También funcionan para listas 

# Estructura : apply(data.frame, ¿Por filas o columnas?, Función a aplicar,... ¿Que hacer con datos faltantes?)


animal.db <- read.csv2("DataApply.csv")
read


#Agneau : Borrego, Vache : Vaca. 

rownames(animal.db) <- paste(animal.db$RefIndiv, animal.db$Methode)

rownames(animal.db) #Ya están combinadas 



animal.db<-animal.db[,-1]
animal.db$Prom <- apply(X = animal.db[ ,3:7], 1, mean)

animal.db <- animal.db[,-9]

#Sacar calificacion por evaluador en un nuevo dataframe 

examin.db <- apply(X = animal.db[,3:7], 2, mean)
examin.db

#Datos faltantes 

examin.db <- apply(X = animal.db[,3:7], 2, mean, na.rm = T) #Función parecida apply pero sirve para vectores, listas y dataframes 
View(examin.db)

examin.df <- as.data.frame(examin.db)

names(examin.df) <- "Promedio"
examin.df
write.csv(examin.df, "Promedio por examinadores.csv")

#Mover columnas de lugar 

examin.db[ ,c(2,1)] #fila, vec con orden 


#sapply : apply por columnas y funciona con listas y vectoreas aparte de data frames 

#tapply: crea resumenes de los datos de acuerdo a un factor 

tapply(animal.db$Examinateur1, animal.db$Animal, mean, na.rm = T)

## Si queremos por más factores, se meten por una lista 

tapply(animal.db$Examinateur1, list(animal.db$Animal, animal.db$Methode), mean, na.rm = T)


# FORMATOS DE DATAFRAMES 

library(reshape2)



animal.melt <- melt(animal.db[ ,-8], id.vars = c("Animal", "Methode")) #Conservamos las variables Animal y Methode, se crea una columan para el examinador 

names(animal.melt)[3] <- "Examinador"
names(animal.melt)
#Crear promedio de cada exmainador por tipo de animal 

str(animal.melt)
animal.melt$value <- as.numeric(animal.melt$value)






tapply(animal.melt$value, animal.melt$Examinador, mean, na.rm=TRUE)

tapply(animal.melt$value, list(animal.melt$Examinador, animal.melt$Animal),mean, na.rm = TRUE)

tapply(animal.melt$value, list(animal.melt$Examinador, animal.melt$Animal, animal.melt$Methode), mean , na.rm = TRUE)

plot(animal.melt$value ~ animal.melt$Examinador)



## Ejercicio 3: 

calabazas.db <- read.csv("calabazas.csv")

str(calabazas.db)


#Crear promedio de cada vitamina C para cafa una de las variedades para cada fecha 
tapply(calabazas.db$VitC, list(calabazas.db$Cult, calabazas.db$Date), sd , ra.nm = TRUE)


airq.db <- read.csv2("airquality.csv")
str(airq.db)
airq.db$Wind <- as.numeric(airq.db$Wind)

#Transformar a formato largo conservando el mes y el día como columnas 

airq.melt <- melt(airq.db, id.vars = c("Month", "Day"))
str(airq.melt)

#Sacar el promeido de cada mes para cada una de las variables atmosféricas 

tapply(airq.melt$value, list(airq.melt$variable,airq.melt$Month), mean, na.rm =TRUE) 




## ejercicio Extra 

#Cargar datos de advertising2.csv

adv2.db <- read.csv2("advertising2.csv")
adv2.db

str(adv2.db)

#transformar a formato largo dejando fijas store y sales 


adv2.melt <- melt(adv2.db, id.vars = c("Store","Sales"))
str(adv2.melt)
adv2.melt$Sales <- as.numeric(adv2.melt$Sales)
adv2.melt$value <- as.numeric(adv2.melt$value)
adv2.melt$Store <- as.factor(adv2.melt$Store)

plot(adv2.melt$Sales~adv2.melt$value, col = adv2.melt$variable)

