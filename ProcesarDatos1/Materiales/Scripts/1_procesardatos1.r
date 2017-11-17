rm(list=ls())
setwd("~")

# No es necesario siempre poner todo el choro del primer Script (aunque es buena práctica).
# Sí acostúmbrense a poner siempre al menos 3 cosas: nombre, fecha, y para qué hacen el script

#################################
# Fierro + Merino + Elton       #
# 04/04/17                      #
# Clase DEVF - Procesar Datos 1 #
#################################

#######################
### LOS DIRECTORIOS ###
#######################
# Si se acuerdan, los directorios no son más que las rutas donde se encuentran los archivos que vamos a utilizar en nuestra computadora.
# La "base" de donde parten nuestros directorios es el working directory (lo que especificamos arriba como setwd("~").
# Podríamos escribir todo el directorio completo cada vez que importemos o exportemos un archivo, pero mejor hagamos algo más fácil:

dir1 <- "/Users/Alecs/Dropbox (Data4)/Batch 15/2_ProcesarDatos1/Materiales/Inp"
dir2 <- "/Users/Alecs/Dropbox (Data4)/Batch 15/2_ProcesarDatos1/Materiales/Out"

# ¿Por qué 2 directorios? Veamos nuestra carpeta :)

# Ahora sí, importemos datos

##########################################
### ¿Cómo abrimos bases de datos en R? ###
##########################################

# Primero un .txt, o archivo de texto. Estos suelen estár separados por tabs (estas cositas "|"), pero siempre es mejor revisar en un editor de texto, porque puede cambiar. ¿Cómo que separados por...?

data.TXT <- read.table(paste(dir1, "Import_venezuela.txt", sep="/"), sep="\t",header=T, stringsAsFactors=F)
# Por qué nos marca un error? Fíjense en la última variable
head(data.TXT)

# Igual lee la base, pero si quieren pueden hacer que no salga el Warning con quote=NULL. 
data.TXT <- read.table(paste(dir1, "Import_venezuela.txt", sep="/"), sep="\t", quote=NULL, header=T, stringsAsFactors=F)

# OJO -> ERROR != WARNING. Los errores paran lo que estés corriendo, los warnings sólo te advierten, pero el código sigue corriendo.
# Además, si nombro un objeto igual a otro que ya existía, R no te avisa que ya tienen un objeto con el mismo nombre, simplemente lo reemplaza por el último que creaste.

# Ahora un csv (comma separeted value), que es un archivo donde las variables están separadas por una coma ","
data.CSV <- read.csv(paste(dir1, "Import_venezuela.csv", sep="/"), stringsAsFactors=F)
head(data.CSV)

# Y por último, otro de los formatos más comunes, nuestro ya conocido Excel. La función read.xlsx no es de las que trae R por default, viene en un paquete.
 
#install.packages("xlsx")
require(xlsx)

data.XLSX <- read.xlsx(paste(dir1, "Import_venezuela.xlsx", sep="/"), sheetIndex=1, header=T, stringsAsFactors=F)
head(data.XLSX)
# Empezamos con problemas...¿Cuáles ven?

rm(data.CSV, data.TXT, data.XLSX)

####################################
### Limpiar y manipular una base ###
####################################
# Los datos rara vez existen ya como los necesitamos. Limpiarlos y acomodarlos es uno de los pasos más importantes.

data <- read.table(paste(dir1, "Import_venezuela.txt", sep="/"), sep="\t", quote=NULL, header=T, stringsAsFactors=F)

# Tiramos la última variable, que no hacer referencia a nada. ¿por qué no también 2007 que también tiene puros NA´s?
data$X <- NULL
str(data)
str(data[,-13])

# Ahora renombremos nuestras variables a algo más "amigable"
names(data) # Así vemos cuáles son los nombres de nuestras variables
names(data) <- c("pais","y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010","y2011")
# ponemos la "y" antes del año porque R no acepta valores numéricos como nombres de variables.

# str() es uno de los comandos más útiles. Nos muestra no sólo qué tipo de objeto estamos viendo, sino qué contiene
str(data)
# Como vemos, todas nuestras variables (casi) son caracteres, esto es, R las identifica como texto, aunque sean números.
# Por esto, tenemos que convertir a númericos los valores de importaciones. Si no no podríamos hacer operaciones con ellos.
head(data) # ¿Qué hay "raro" en nuestras variables numéricas? Puntos (al menos es raro que haya 2 puntos)
# Si convertimos así las variables a número, antes de quitar los puntos, R nos va a convertir muchos valores a NA, esto porque hay valores de carácter (el punto) junto con los números. Veamos

tempo <- data[,c("pais", "y2001")]
head(tempo)

tempo$y2001 <- as.numeric(tempo$y2001)
head(tempo)

rm(tempo)

# Quitemos entonces primero los puntos. Probemos con 1 año primero
tempo <- data[,c("pais", "y2001")]
tempo$y2001 <- gsub(".", "", tempo$y2001, fixed=T)
head(tempo)
rm(tempo)

# Ahora sí, quitemos los de todos
data$y2001 <- gsub(".", "", data$y2001, fixed=T)
data$y2002 <- gsub(".", "", data$y2002, fixed=T)
data$y2003 <- gsub(".", "", data$y2003, fixed=T)
data$y2004 <- gsub(".", "", data$y2004, fixed=T)
data$y2005 <- gsub(".", "", data$y2005, fixed=T)
data$y2006 <- gsub(".", "", data$y2006, fixed=T)
data$y2007 <- gsub(".", "", data$y2007, fixed=T)
data$y2008 <- gsub(".", "", data$y2008, fixed=T)
data$y2009 <- gsub(".", "", data$y2009, fixed=T)
data$y2010 <- gsub(".", "", data$y2010, fixed=T)
data$y2011 <- gsub(".", "", data$y2011, fixed=T)

# y luego a número
data$y2001 <- as.numeric(data$y2001)
data$y2002 <- as.numeric(data$y2002)
data$y2003 <- as.numeric(data$y2003)
data$y2004 <- as.numeric(data$y2004)
data$y2005 <- as.numeric(data$y2005)
data$y2006 <- as.numeric(data$y2006)
data$y2007 <- as.numeric(data$y2007)
data$y2008 <- as.numeric(data$y2008)
data$y2009 <- as.numeric(data$y2009)
data$y2010 <- as.numeric(data$y2010)
data$y2011 <- as.numeric(data$y2011)

# Hay que crear entonces una variable de tipo factor con un valor numérico y los nombres de los paises como etiquetas

data$pais2 <- as.factor(data$pais)
head(data) #Se ven iguales peeero...
str(data)

data$pais2 <- NULL

# RECODIFICAR VARIABLES
# Supongamos que queremos cambiar los 0 de la variable y2001 por NA´s
table(data$y2001, useNA="always") # tenemos 69 0´s
data$y2001a <- data$y2001 # Creamos otra variable igual, para no modificar la original

data$y2001a==0



data$y2001a[data$y2001a==0] <- NA
table(data$y2001a, useNA="always")

data$y2001a <- NULL

# Reordenar las observaciones/renglones de nuestra base
tempo <- data[order(data$pais),]
head(data)
head(tempo) # las reordena en orden alfabético

# y si queremos ordenar de menor a mayor y2011
tempo <- data[order(data$y2011),]

# y de mayor a menor?
tempo <- data[order(data$y2011, decreasing=T),]

# y por dos o más variables

tempo <- data[order(data$pais, data$y2001),]
tempo2 <- data[order(data$y2001, data$pais),]

# El primer criterio que toma para ordenar siempre es la primera variable, y así sucesivamente

# Reordenar las variables/columnas de nuestra base
# supongamos que quiero mis variables en orden alfabético
tempo <- data[,order(names(data))]
str(tempo)
# y ahora supongamos que quiero que y2011 esté justo a la derecha de pais
tempo <- data[,c("pais", "y2011")]
# Sí, sólo me quedaron las 2 variables que especifiqué. Si quisiera reordenar toda la base tengo que escribir todo
tempo <- data[,c("pais", "y2011", "y2010", "y2009", "y2008", "y2007", "y2006", "y2005", "y2004", "y2003", "y2002", "y2001")]

rm(tempo,tempo2)

# DESCRIPTIVOS BÁSICOS: mínimo, 1er cuartil, mediana, media, 3er cuartil y máximo. DE UNA VARIABLE
summary(data$y2001)

# de todas las variables de la base
summary(data)

# RESHAPE
# Como imaginaran un reshape se refiere a cambiar la base de forma. Hay 2 formas de base de datos principalmente: wide y long
# La base que tenemos ahorita está en formato wide (tenemos los años horizontalmente como variables, no tenemos una sola variable que contenga todos los años)

# Si quisiera hacer una variable "year" que contenga todos los años, tengo que hacer un reshape long

data.long <- reshape(data, varying = paste("y", c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011"), sep=""),  direction="long",  sep="")

names(data.long) <- c("pais", "year", "imports", "id")
data.long$id <- NULL

# Guardamos como csv
write.csv(data.long, paste(dir2, "BaseVenLong.csv", sep="/"), row.names=F)

# si tuviéramos los años long, y los quisiéramos wide... reshaoe wide
data.wide <- reshape(data.long, timevar="year", idvar="pais", direction="wide", sep="_")

rm(data, data.long, data.wide)

# ETIQUETAR
data <- read.csv(paste(dir2, "BaseVenLong.csv", sep="/"), stringsAsFactors=F)
# etiquetar variables
# Una forma es con el paquete Hmisc. Lo malo es que los labels sólo van a servir para funciones que vengan de este paquete
#install.packages("Hmisc")
require(Hmisc)
label(data$pais) <- "País exportador"
label(data$year) <- "Año"
label(data$imports) <- "Importaciones totales de Venezuela"
describe(data)

# etiquetar valores de las variables
# imaginen que por alguna razón queremos tener una variable que nos indique cuáles son años superiores a 2006 y cuales no.
data$dummy06[data$year<=2006] <- 0
data$dummy06[data$year>2006] <- 1

data$dummy06 <- factor(data$dummy06, levels = c(0,1), labels = c("Anterior a 2006", "Después de 2006"))
head(data)
tail(data)

# LAGS Y FORWARDS
data <- read.csv(paste(dir2, "BaseVenLong.csv", sep="/"), stringsAsFactors=F)

install.packages("DataCombine")

require(DataCombine)

# primero el lag
data1 <- data[order(data$pais, data$year),]
data1 <- slide(data1, Var="imports", GroupVar="pais", slideBy=-1)
head(data1,50)

# Ahora el forward
data1 <- data[order(data$pais, data$year),]
data1 <- slide(data1, Var="imports", GroupVar="pais", slideBy=1)
head(data1,50)

rm(data1)

# ¿Cómo creamos variables con la media, desviación estándar, etc.?
# una variable del promedio por año
data$imp_yr_mean <- ave(data$imports, data$year)
data$imp_ctry_mean <- ave(data$imports, data$pais) # Sale todo NA´s, ¿por?
data$imp_ctry_mean <- ave(data$imports, data$pais, FUN=function(x) mean(x, na.rm=T))

# Desviación estándard
data$imp_yr_sd <- ave(data$imports, data$year, FUN=sd)

# mediana
data$imp_yr_med <- ave(data$imports, data$year, FUN=median)

# Mínimo 
data$imp_yr_min <- ave(data$imports, data$year, FUN=min)

# Máximo
data$imp_yr_max <- ave(data$imports, data$year, FUN=max)

# por 2 variables, por ejemplo, país-año (aunque aquí no tiene sentido porque el promedio por esos 2 grupos va ser igual al valor de imports)
data$imp_yrctry_mean <- ave(data$imports, data$year, data$pais, FUN=function(x) mean(x, na.rm=T))





