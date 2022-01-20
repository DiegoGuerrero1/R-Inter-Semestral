rm(list = ls()) #limpiamos entorno 
setwd("~/Documents/R-Course/R-Intersemestral")
#llamamos a la librería 
library(ggplot2)
#cargamos los datos 
mpg.db <- read.csv("dia4/mpg.csv")

str(mpg.db)

?mpg


#Plantilla:
#gpglot(data=<DATA>)+
#<GEOM_FUNCTION>(mapping = aes(<MAPPINGS>)) 
ggplot(data = mpg.db, aes(x = displ, y = hwy))+
  geom_point()

ggplot(data = mpg.db, aes(x = displ, y = cty))+
  geom_point()

#### Ejercicio 1 
# 1. Traducir títulos del data frame 

names(mpg.db)<- c("x", "fabricante", "modelo", "desplazamiento", "año", "cilindros", "transmisión", "tracción","ciudad","autopista","combustible","tipo")
names(mpg.db)



#2. Crear una gráfica de dispersión de cilindros vs autopista
# Autopista = millas por galón , cilindros = número de cilindros 
ggplot(data= mpg.db, aes(x = cilindros, y = autopista))+
  geom_point()

#El de cuatro cilindros es más eficiente, da más millas por galón. 


# 3. Gráfica de dispersión de grabricante vs autopista
ggplot(data= mpg.db, aes(x = fabricante, y = autopista))+
  geom_point()




#4. Crear una gráfica tipo vs transmisión 

ggplot(data = mpg.db, aes(tipo, autopista))+
  geom_point()
