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

