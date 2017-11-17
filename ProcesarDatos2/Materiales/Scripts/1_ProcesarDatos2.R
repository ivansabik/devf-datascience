rm(list=ls())
setwd("~")

#################################
# Fierro + Merino + Elton       #
# 02/02/17                      #
# Clase DEVF - Procesar Datos 2 #
#################################

# Prendamos los paquetes que vamos a usar. En cada parte van a ver para qué nos sirven distintos paquetes
# install.packages(c("foreign", "readxl","readstata13", "doBy", "plyr", "psych", "ggplot2"))
require(foreign)
require(readxl) 
require(readstata13)
require(doBy)
require(plyr)
require(psych)
require(ggplot2)

###################
### DIRECTORIOS ###
###################

dir1 <- "/Users/Alecs/Dropbox (Data4)/Batch 13/3_ProcesarDatos2/Materiales/Inp/2006"
dir2 <- "/Users/Alecs/Dropbox (Data4)/Batch 13/3_ProcesarDatos2/Materiales/Inp/2009"
dir3 <- "/Users/Alecs/Dropbox (Data4)/Batch 13/3_ProcesarDatos2/Materiales/Inp/2012"
dir4 <- "/Users/Alecs/Dropbox (Data4)/Batch 13/3_ProcesarDatos2/Materiales/Inp"
dir5 <- "/Users/Alecs/Dropbox (Data4)/Batch 13/3_ProcesarDatos2/Materiales/Out"


##################
# Diputados 2006 #
##################

dip06 <- read.table(paste(dir1, "dip2006.txt", sep="/"), sep="|", header=T, stringsAsFactors=F)

# RENOMBRAMOS ALIANZAS POR PARTIDOS
names(dip06)

# Primero entonces hay que volver todos minúsculas
names(dip06) <- tolower(names(dip06))
names(dip06)

# Ahora sí...
names(dip06)[names(dip06)=="apm"] <- "pri"
names(dip06)[names(dip06)=="pbt"] <- "prd"
names(dip06)[names(dip06)=="asdc"] <- "psd"
names(dip06)[names(dip06)=="total_votos"] <- "total"
names(dip06)[names(dip06)=="no_votos_nulos"] <- "nulo"
names(dip06)[names(dip06)=="no_votos_can_nreg"] <- "nreg"
names(dip06)[names(dip06)=="no_votos_validos"] <- "valid"
names(dip06)[names(dip06)=="lista_nominal"] <- "lista"

# veamos qué tipo de variables tenemos en nuestra base
str(dip06)
# las claves de estado y municipio son numéricas, hay que hacerlas caracter para generar ID de municipio
# Los IDs de municipio de INEGI tienen 5 dígitos: los 2 primeros son la clave del estado y los 3 siguientes de municipio. Aquí vamos a hacer IDs de INEGI, aunque en realidad vamos a llamarlos por el momento IDs nada más, ya que como veremos más adelante, los IDs de INEGI y los del IFE (que tenemos aquí) no siempre coinciden
dip06$edo <- formatC(dip06$id_estado, width = 2, format = "d", flag = "0")
dip06$mun <- formatC(dip06$municipio, width = 3, format = "d", flag = "0")

dip06$id <- paste(dip06$edo, dip06$mun, sep="")

# vamos a quedarnos con las variables que nos interesan nada más...un subset, pues. Podemos subsetear y ordenarlas de una vez, poniendo las variables que queremos, en el orden en que las queremos.
dip06 <- dip06[,c("id", "edo", "mun", "pri", "pan", "prd", "psd", "nulo", "nreg", "valid", "lista", "total")]

str(dip06)
# Calcular los votos por municipio. Tenemos que colapsar la base
dip06 <- summaryBy(pri + prd + pan + nulo + nreg + valid + lista + total ~ id, FUN=sum, data=dip06, na.rm=TRUE, keep.names=T)

str(dip06)

# Generamos votos porcentuales
dip06$d_pri <- dip06$pri/dip06$valid
dip06$d_pan <- dip06$pan/dip06$valid
dip06$d_prd <- dip06$prd/dip06$valid
dip06$d_nulo <- dip06$nulo/dip06$valid
dip06$d_part <- dip06$valid/dip06$lista

# Nos quedamos lo que nos interesa (en este caso los porcentajes por municipio)
dip06 <- dip06[,c("id", "d_pri", "d_pan", "d_prd", "d_nulo", "d_part")]


###################
# Presidente 2006 #    Aquí hacemos lo mismo
###################

pres06 <- read.table(paste(dir1, "pres2006.txt", sep="/"), sep="|", header=T, stringsAsFactors=F)
names(pres06) <- tolower(names(pres06))
names(pres06)[names(pres06)=="apm"] <- "pri"
names(pres06)[names(pres06)=="pbt"] <- "prd"
names(pres06)[names(pres06)=="asdc"] <- "psd"
names(pres06)[names(pres06)=="total_votos"] <- "total"
names(pres06)[names(pres06)=="no_votos_nulos"] <- "nulo"
names(pres06)[names(pres06)=="no_votos_can_nreg"] <- "nreg"
names(pres06)[names(pres06)=="no_votos_validos"] <- "valid"
names(pres06)[names(pres06)=="lista_nominal"] <- "lista"
pres06$edo <- formatC(pres06$id_estado, width = 2, format = "d", flag = "0")
pres06$mun <- formatC(pres06$municipio, width = 3, format = "d", flag = "0")
pres06$id <- paste(pres06$edo, pres06$mun, sep="")
pres06 <- pres06[,c("id", "edo", "mun", "pri", "pan", "prd", "psd", "nulo", "nreg", "valid", "lista", "total")]
pres06 <- summaryBy(pri + prd + pan + nulo + nreg + valid + lista + total ~ id, FUN=sum, data=pres06, na.rm=TRUE, keep.names=T)
pres06$p_pri <- pres06$pri/pres06$valid
pres06$p_pan <- pres06$pan/pres06$valid
pres06$p_prd <- pres06$prd/pres06$valid
pres06$p_nulo <- pres06$nulo/pres06$valid
pres06$p_part <- pres06$valid/pres06$lista
pres06 <- pres06[,c("id", "p_pri", "p_pan", "p_prd", "p_nulo", "p_part")]

# OJO: aquí tenemos municipios 000, que son casillas especiales. En elecciones de presidente puedes votar en lugares que no sea tu lugar de residencia (o donde estás registrado para el INE/IFE).

table(pres06$id)

# Tiramos el municipio... imaginen que nunca lo tuvieron por separado, qué hacemos?

pres06$tempo <- substr(pres06$id, 3, 5)

# Esta es otra forma de subsetear
pres06 <- subset(pres06, tempo!="000")

pres06$tempo <- NULL


###################
# Diputados 2012  #  Limpiamos primero 2012 porque es muy parecida a 2006; 2009 sí cambia
###################
# OJO CON EL DIRECTORIO

dip12 <- read.table(paste(dir3, "dip2012.txt", sep="/"), sep="|", header=T, stringsAsFactors=F)

names(dip12) <- tolower(names(dip12))

dip12$edo <- formatC(dip12$id_estado, width = 2, format = "d", flag = "0")
dip12$mun <- formatC(dip12$id_municipio, width = 3, format = "d", flag = "0")

table(dip12$edo, useNA="ifany")
table(dip12$mun, useNA="ifany") # Ya vimos que no hay NA´s

dip12$id <- paste(dip12$edo, dip12$mun, sep="")

# Fíjense que ahora tenemos los votos de los 3 partidos grandes por separado (por la forma en que se podía votar). Hay que sumarlos
# Primero los del PRI
dip12$pritot <- rowSums(dip12[c("pri", "pvem", "pri_pvem")], na.rm=T)
dip12$pri <- dip12$pvem <- dip12$pri_pvem <- NULL
names(dip12)[names(dip12)=="pritot"] <- "pri"

# Ahora los del PRD
dip12$prdtot <- rowSums(dip12[c("prd", "pt", "mc", "prd_pt_mc", "prd_pt", "prd_mc", "pt_mc")], na.rm=T)
dip12$prd <- dip12$pt <- dip12$mc <- dip12$prd_pt_mc <- dip12$prd_pt <- dip12$prd_mc <- dip12$pt_mc <- NULL
names(dip12)[names(dip12)=="prdtot"] <- "prd"

names(dip12)[names(dip12)=="num_votos_nulos"] <- "nulo"
names(dip12)[names(dip12)=="numero_votos_validos"] <- "valid"
names(dip12)[names(dip12)=="lista_nominal"] <- "lista"

dip12 <- summaryBy(pri + prd + pan + nulo + valid + lista ~ id, FUN=sum, data=dip12, na.rm=TRUE, keep.names=T)

dip12$d_pri <- dip12$pri/dip12$valid
dip12$d_pan <- dip12$pan/dip12$valid
dip12$d_prd <- dip12$prd/dip12$valid
dip12$d_nulo <- dip12$nulo/dip12$valid
dip12$d_part <- dip12$valid/dip12$lista

dip12 <- dip12[,c("id", "d_pri", "d_pan", "d_prd", "d_nulo", "d_part")]


####################
# Presidente 2012  #   CASI LO MISMO
####################

pres12 <- read.table(paste(dir3, "pres2012.txt", sep="/"), sep="|", header=T, stringsAsFactors=F)

names(pres12) <- tolower(names(pres12))

table(pres12$id_estado, useNA="ifany")
table(pres12$id_municipio, useNA="ifany") # aquí si tenemos municipios NA´s (acuérdense de las casillas especiales)
pres12 <- subset(pres12, is.na(pres12$id_municipio)==F)

pres12$edo <- formatC(pres12$id_estado, width = 2, format = "d", flag = "0")
pres12$mun <- formatC(pres12$id_municipio, width = 3, format = "d", flag = "0")

pres12$id <- paste(pres12$edo, pres12$mun, sep="")

# Sumamos votos del PRI
pres12$pritot <- rowSums(pres12[c("pri", "pvem", "pri_pvem")], na.rm=T)
pres12$pri <- pres12$pvem <- pres12$pri_pvem <- NULL
names(pres12)[names(pres12)=="pritot"] <- "pri"

# Y los del PRD
pres12$prdtot <- rowSums(pres12[c("prd", "pt", "mc", "prd_pt_mc", "prd_pt", "prd_mc", "pt_mc")], na.rm=T)
pres12$prd <- pres12$pt <- pres12$mc <- pres12$prd_pt_mc <- pres12$prd_pt <- pres12$prd_mc <- pres12$pt_mc <- NULL
names(pres12)[names(pres12)=="prdtot"] <- "prd"

names(pres12)[names(pres12)=="num_votos_nulos"] <- "nulo"
names(pres12)[names(pres12)=="numero_votos_validos"] <- "valid"
names(pres12)[names(pres12)=="lista_nominal"] <- "lista"

pres12 <- summaryBy(pri + prd + pan + nulo + valid + lista ~ id, FUN=sum, data=pres12, na.rm=TRUE, keep.names=T)

pres12$p_pri <- pres12$pri/pres12$valid
pres12$p_pan <- pres12$pan/pres12$valid
pres12$p_prd <- pres12$prd/pres12$valid
pres12$p_nulo <- pres12$nulo/pres12$valid
pres12$p_part <- pres12$valid/pres12$lista

pres12 <- pres12[,c("id", "p_pri", "p_pan", "p_prd", "p_nulo", "p_part")]


##################
# Diputados 2009 #  ESTA SI ES MUY DIFERENTE
##################
# Los datos para 2009, no vienen como .txt, sino como .xls. 
# Viene por estados, en hojas diferentes de excel para cada uno

# Hagamoslo primero solo para una base, Coahuila que sí tiene coalición pri+pvem

# Si se fijan habíamos usado el paquete "xlsx" para leer exceles. Me encontre con "readxl", que tiene la función read_excel, y abre mucho más rápido los exceles en R. Estos nos importa porque tenemos un loop que tiene que abrir y procesar 32 bases de datos.

prueba09 <- read_excel(paste(dir2, "5.xlsx", sep="/"), sheet=1, col_names=T, col_types=NULL, na="", skip=0)
prueba09 <- as.data.frame(prueba09)

names(prueba09) <- tolower(names(prueba09))

names(prueba09)[names(prueba09)=="primero mƒxico"] <- "coali_pri_pvem"
names(prueba09)[names(prueba09)=="salvemos mƒxico"] <- "coali_conv_pt"
names(prueba09)[names(prueba09)=="nueva alianza"] <- "nueva.alianza"
names(prueba09)[names(prueba09)=="no registrados"] <- "no.registrados"

table(prueba09$coali_pri_pvem, useNA="ifany")
# Por qué si coalición es distinto de NA?
prueba09$pri[is.na(prueba09$coali_pri_pvem)==F] <- prueba09$pri + prueba09$coali_pri_pvem

prueba09 <- summaryBy(pan + pri + prd + pvem + pt + convergencia + nueva.alianza + psd + coali_pri_pvem + coali_conv_pt + no.registrados + nulos + total ~ municipio, FUN=sum, data=prueba09, na.rm=TRUE, keep.names=T)

prueba09$d_pri <- prueba09$pri/prueba09$total
prueba09$d_pan <- prueba09$pan/prueba09$total
prueba09$d_prd <- prueba09$prd/prueba09$total
prueba09$d_nulo <- prueba09$nulos/prueba09$total

prueba09 <- prueba09[,c("municipio", "d_pri", "d_pan", "d_prd", "d_nulo")]

rm(prueba09)

# Ahora hagamos lo mismo, pero con un LOOP, para todas las bases al mismo tiempo

dip09 <- data.frame()

pb <- txtProgressBar(min=1, max=32, style=3)
for(x in 1:32){
	tempo <- read_excel(paste(dir2, paste(x, ".xlsx", sep=""), sep="/"), sheet=1, col_names=T, col_types=NULL, na="", skip=0)
	tempo <- as.data.frame(tempo)
	names(tempo) <- tolower(names(tempo))
    names(tempo)[names(tempo)=="primero mƒxico"] <- "coali_pri_pvem"
    names(tempo)[names(tempo)=="salvemos mƒxico"] <- "coali_conv_pt"
    names(tempo)[names(tempo)=="nueva alianza"] <- "nueva.alianza"
    names(tempo)[names(tempo)=="no registrados"] <- "no.registrados"
    tempo$pri[is.na(tempo$coali_pri_pvem)==F] <- tempo$pri + tempo$coali_pri_pvem
    tempo <- summaryBy(pan + pri + prd + pvem + pt + convergencia + nueva.alianza + psd + coali_pri_pvem + coali_conv_pt + no.registrados + nulos + total ~ municipio, FUN=sum, data=tempo, na.rm=TRUE, keep.names=T)
    tempo$d_pri <- tempo$pri/tempo$total
    tempo$d_pan <- tempo$pan/tempo$total
    tempo$d_prd <- tempo$prd/tempo$total
    tempo$d_nulo <- tempo$nulos/tempo$total
    tempo$edo <- x
    tempo <- tempo[,c("municipio", "d_pri", "d_pan", "d_prd", "d_nulo", "edo")]		
	dip09 <- rbind.fill(dip09, tempo)
	rm(tempo)
	setTxtProgressBar(pb, x)
}
close(pb)
rm(pb, x)

head(dip09)
str(dip09)

dip09$edo <- formatC(dip09$edo, width = 2, format = "d", flag = "0")
dip09$year <- 2009

dip09 <- dip09[, c(6,1,2,3,4,5,7)]

# Lo malo de esta base, es que no trae IDs por municipio
# ¿Que hacer? No queda mas que ponerlos a mano
# VER ARCHIVO DE EXCEL
# Por suerte ya lo hicimos, se llama "2009" *
# Nuevo comando para leer bases .dta (formato de STATA). Viene del paquete readstata13

dip09 <- read.dta13(paste(dir2, "2009.dta", sep="/"))
head(dip09)

###########################
# Base que identifica IDs #
###########################

# Los IDs que utiliza el INEGI son diferentes que los IDs que utiliza 
# el IFE. Hay que homogeneizar. 
# Por suerte, tenemos un .csv que matchea los dos codigos.
# VER CSV

ife <- read.csv(paste(dir4, "IFEtoINEGI.csv", sep="/"), stringsAsFactors=F)

ife$ID <- formatC(ife$id.ife, width = 5, format = "d", flag = "0")
ife$IDinegi <- formatC(ife$id.inegi, width = 5, format = "d", flag = "0")

ife$id.ife <- ife$id.inegi <- NULL

head(ife)

#####################
# Merge bases inegi #
#####################

# Vamos a juntar primero las bases de datos que tenemos con municipios con la que tiene los IDs del IFE/INE. En las bases de 2006 y 2012 tenemos los IDs del IFE (son bases electorales). En 2009 ya pusimos INEGI a manita, acuérdense. 

# Diputados 2006
head(dip06)
dip06 <- merge(dip06, ife, by.x="id", by.y="ID", all=T)
head(dip06)
tail(dip06)

# Presidente 2006
head(pres06)
pres06 <- merge(pres06, ife, by.x="id", by.y="ID", all=T)
head(pres06)
tail(pres06)

# Diputados 2012
head(dip12)
dip12 <- merge(dip12, ife, by.x="id", by.y="ID", all=T)
head(dip12)
tail(dip12)

# Presidente 2012
head(pres12)
pres12 <- merge(pres12, ife, by.x="id", by.y="ID", all=T)
head(pres12)
tail(pres12)

##########################
#    Merge bases IFE     # 
##########################
# Ahora vamos a juntar todas las bases: 2006, 2009 (que no tiene elección de presidente), y 2012

# 2006
elec06 <- merge(dip06, pres06, by="id", all=T)
names(elec06)[names(elec06)=="id"] <- "IDife"
names(elec06)[names(elec06)=="IDinegi.x"] <- "IDinegi"
elec06$IDinegi.y <- NULL

# Generamos una variabes de año para identificar el año de la elección cuando peguemos todas
elec06$year <- 2006

elec06 <- elec06[, c(7,1,13,2,3,4,5,6,8,9,10,11,12)]

rm(dip06, pres06)

# 2012
elec12 <- merge(dip12, pres12, by="id", all=T)
names(elec12)[names(elec12)=="id"] <- "IDife"
names(elec12)[names(elec12)=="IDinegi.x"] <- "IDinegi"
elec12$IDinegi.y <- NULL

# Generamos una variabes de año para identificar el año de la elección cuando peguemos todas
elec12$year <- 2012

elec12 <- elec12[, c(7,1,13,2,3,4,5,6,8,9,10,11,12)]

rm(dip12, pres12)

# Queremos pegar las bases de los 3 años. Tenemos dos opciones:

# 1) Con rbind.fill, que nos rellenará con NAs las columnas que no hagan match. Aquí primero juntamos y luego limpiamos

juntas1 <- rbind.fill(elec06, dip09, elec12)
juntas1$mun <- NULL
# y nos queda 2009 sin los IDife, ya que sólo pusimos a mano los de INEGI. Podemos volver a juntar
juntas1 <- merge(juntas1, ife, by="IDinegi", all=T)
juntas1$IDife <- NULL
names(juntas1)[names(juntas1)=="ID"] <- "IDife"

juntas1 <- juntas1[, c(1,13,2,3,4,5,6,7,8,9,10,11,12)]

rm(juntas1)

# 2) La otra es primero limpiar y luego juntar, ya con un simple rbind. Para poder apendear tenemos que poner en el mismo formato 2009. So...
head(elec06)
head(elec12)
head(dip09)

dip09 <- merge(dip09, ife, by="IDinegi", all=T)
dip09$mun <- NULL
names(dip09)[names(dip09)=="ID"] <- "IDife"
dip09$d_part <- NA
dip09$p_pri <- NA
dip09$p_pan <- NA
dip09$p_prd <- NA
dip09$p_nulo <- NA
dip09$p_part <- NA
                   
elec09 <- dip09[, c(1,7,2,3,4,5,6,8,9,10,11,12,13)]

rm(dip09, ife)

# Ahora sí, juntamos

elec06to12  <- rbind(elec06, elec09, elec12)

elec06to12 <- elec06to12[order(elec06to12$IDinegi, elec06to12$year),]

rm(elec06, elec09, elec12) 

# Y la guardamos, para no tener que correr todo el Script cada que queramos esta base.
write.csv(elec06to12, paste(dir5, "elec06to12.csv", sep="/"), row.names=F)

rm(elec06to12)

#############################################################################
# Que tenemos hasta ahorita. Tan solo la base limpia de elecciones en 2006, #
# 2009 y 2012. ¿Que mas le podemos agregar de interes?                      #
#############################################################################

data <- read.csv(paste(dir5, "elec06to12.csv", sep="/"), stringsAsFactors=F)
data$IDinegi <- formatC(data$IDinegi, width = 5, format = "d", flag = "0")
data $IDife <- formatC(data$IDife, width = 5, format = "d", flag = "0")

#############################################
# Uso de Loops para generar partido ganador #
#############################################

# Por ejemplo, nos puede interesar saber, por municipio y por año, quien gano 

# Una forma es hacerlo uno por uno
data$d_votogan <- do.call(pmax, data[, 4:7])
data$d_gan <- as.character(NA)
data$d_gan[data$d_votogan==data$d_pri & is.na(data$d_pri)==F] <- "PRI" 
data$d_gan[data$d_votogan==data$d_pan & is.na(data$d_pan)==F] <- "PAN" 
data$d_gan[data$d_votogan==data$d_prd & is.na(data$d_prd)==F] <- "PRD" 
data$d_gan[data$d_votogan==data$d_nulo & is.na(data$d_nulo)==F] <- "NULOS" 

table(data$d_gan, useNA="ifany")

# Y ahora para presidente
data$p_votogan <- do.call(pmax, data[, 9:12])
data$p_gan <- as.character(NA)
data$p_gan[data$p_votogan==data$p_pri & is.na(data$p_pri)==F] <- "PRI" 
data$p_gan[data$p_votogan==data$p_pan & is.na(data$p_pan)==F] <- "PAN" 
data$p_gan[data$p_votogan==data$p_prd & is.na(data$p_prd)==F] <- "PRD" 
data$p_gan[data$p_votogan==data$p_nulo & is.na(data$p_nulo)==F] <- "NULOS" 

table(data$p_gan, useNA="ifany")

#############################################
# Extraer elementos de estadísticos básicos #
#############################################

# Podemos extraer elementos (como el promedio, min, max, etc.) luego de correr estadísticos descriptivos

# Ejemplo - Participación PRI
pri_part <- subset(data, d_gan=="PRI" & is.na(data$d_part)==F) 
sum <- summary(pri_part$d_part)
str(sum) # es una tabla
min <- sum[1]
max <- sum[6]
mean <- sum[4]

# paquete psych
sum2 <- describe(pri_part$d_part)
str(sum2) # es una base de datos!!!

rm(sum, sum2)

# Podemos también extraer los objetos, por ejemplo queremos hacer una gráfica con los estadísticos básicos de la participación en la elección presidencial de 2012 en los municipios donde ganó el PRI, por estado

head(data)

data$edo <- substr(data$IDinegi, 1, 2)
data$edo <- as.numeric(data$edo)

data.tempo <- subset(data, p_gan=="PRI" & year==2012)

data.tempo$control <- rownames(data.tempo)
data.tempo$sd <- as.numeric(NA)
data.tempo$mean <- as.numeric(NA)
data.tempo$max <- as.numeric(NA)
data.tempo$min <- as.numeric(NA)

pb <- txtProgressBar(min=1, max=32, style=3)
for(x in 1:32){
	tempo <- subset(data.tempo, edo==x)
	sum <- describe(tempo$p_part)
	data.tempo$sd[data.tempo$edo==tempo$edo] <- sum$sd
    data.tempo$mean[data.tempo$edo==tempo$edo] <- sum$mean
    data.tempo$max[data.tempo$edo==tempo$edo] <- sum$max
    data.tempo$min[data.tempo$edo==tempo$edo] <- sum$min
    rm(tempo, sum) 
	setTxtProgressBar(pb, x)
}
close(pb)
rm(pb, x)


