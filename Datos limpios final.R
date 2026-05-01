
#Importamos Librerias
library(readxl)
library(FactoClass)
library(factoextra)
library(plotly)
library(knitr)
library(ggplot2)
library(dplyr)
library(Factoshiny)
library(DT) # para tablas interactivas
library(plotly) # para gráficos interactivos

#Importamos la base de datos
setwd("C:/Users/jorge/Downloads")
ruta_del_archivo <- "EAC_CIFRAS_2022_ANONIMIZADA_FINAL.csv"
datos_orig <- read.csv(ruta_del_archivo, sep = ";", dec = ",")

#En nuestro estudio, tomaremos aquellas empresas Activas. POr lo que, no tomaremos encienta a empresas con produccion bruta = 0. 
#Asi que, eliminaremos aquellos datos que tengan produccion bruta =0 (Son alrededor de 200 datos).
datos <- datos_orig[!is.na(as.numeric(datos_orig$BRUTA)) & as.numeric(datos_orig$BRUTA) != 0, ]

#Crearemos dos nuevas bases de datos, en este caso, para las variables discretas y continuas de nuestro interes.
#IDOJ1
discretas <- select(datos, CORRELA_16, IDAIO, PROMUJ, PROHOM, PERMUJ, PERHOM, DIRMUJ, DIRHOM, AGENCIA, APRENDIZ, PUBLICI, SOCIOS, PERSONOM, DIRECTO, TOTMUJ, TOTHOM)
continuas <- select(datos, BRUTA, CONSUI, SUELDOS, PRESTAC, VENTA, AGREGA, SUEPLAN, PREPLAN, COTIZA, TOTREM, ROTACION, INVPRO, CTO, CTOINS, GASTOS, GASTOSNOP)
                                                                    
# convertimos los datos a tipo numerico. 
continuas <- data.frame(lapply(continuas, function(x) as.numeric(as.character(x))))
# convertimos a sus respectivos valores en pesos colombianos
continuas <-1000*continuas

#Ahora agregamos nuevas variables a continuas
Clasifemp <-discretas$CORRELA_16
continuas <- cbind(Clasifemp, continuas)

#asignamos a los NA el valor de cero
continuas[is.na(continuas)] <- 0 

# Ahora, clasificamos los datos de tipo discret

#Años empresa
años <- as.integer(discretas$IDAIO)
años[años < 1972] <- 1
años[años >= 1972 & años < 1982] <- 2
años[años >= 1982 & años < 1992] <- 3
años[años >= 1992 & años < 2002] <- 4
años[años >= 2002 & años < 2012] <- 5
años[años >= 2012 & años < 2024] <- 6

años <- factor(años, labels=c( "Mas de 50","Entre 40 y 50", "Entre 30 y 40", "Entre 30 y 20", "Entre 20 y 10", "Menos de 10"))
discretas$IDAIO <- años
#personal permanete
permanente <- as.integer(discretas$PERSONOM)
permanente [permanente >= 1 & permanente < 10 ] <- 1
permanente [permanente >= 10 ] <- 2
permanente <- factor(permanente, labels=c("(0)PERM", "(1-10)PERM", "(>10)PERM"))
discretas$PERSONOM <- permanente 

# Personal DIRECTO
directo <- as.integer(discretas$DIRECTO)
directo[directo >= 1 & directo<= 10 ] <- 1
directo[directo  > 10 ] <- 2
directo <- factor(directo, labels=c("(0)DIREC", "(1-10)DIREC", "(>10)DIREC"))
discretas$DIRECTO <- directo

#Total hombres
thombres <- as.integer(discretas$TOTHOM)
thombres[is.na(thombres)] <- 0 #Asignarle el valor de cero a los nas
thombres[thombres >= 1 & thombres<= 10 ] <- 1
thombres[thombres  > 10 ] <- 2
thombres <- factor(thombres, labels=c("(0)HOMBRES", "(1-10)HOMBRES", "(>10)HOMBRES"))
discretas$TOTHOM <- thombres


#Total Mujeres
tmujeres <- as.integer(discretas$TOTMUJ)
tmujeres[is.na(tmujeres)] <- 0 #Asignarle el valor de cero a los nas
tmujeres[tmujeres >= 1 & tmujeres < 10 ] <- 1
tmujeres[tmujeres >= 10 ] <- 2
tmujeres <- factor(tmujeres, labels=c("(0)MUJERES", "(1-10)MUJERES", "(>10)MUJERES"))
discretas$TOTMUJ <- tmujeres

# numero de socios
n_socios <- as.numeric(datos$SOCIOS)
n_socios[n_socios == 0 ] <- 0 #Asignarle el valor de cero a los nas
n_socios[n_socios > 0 & n_socios < 4] <- 1
n_socios[n_socios >= 4 & n_socios<100] <- 2
n_socios <- factor(n_socios, labels=c("No Registra","Pocos Socios", "Bastantes Socios"))
discretas$SOCIOS <- n_socios

#clasificamos las empresas segun su tamañó. Es este caso, basaremos la clasificaion "Decreto 957 de 2019".
pbruta <- 1000*as.numeric(datos$BRUTA)
#precio uvt añó 2022
uvt22 = 38004
TMÑEMP <- unlist(pbruta/uvt22)
TMÑEMP[TMÑEMP <= 44769 ] <- 1
TMÑEMP[TMÑEMP > 44769 & TMÑEMP <= 431196] <- 2
TMÑEMP[TMÑEMP > 431196 & TMÑEMP <= 2160692] <- 3
TMÑEMP[TMÑEMP > 2160692] <- 4
TMÑEMP <- factor(TMÑEMP, labels=c("MIcroe", "PQempr", "MEdempr", "EmprGran"))
discretas <- cbind(discretas, TMÑEMP)

#Clasificamos publicidad, basandonos en el porcentaje gastado respecto a la produccion bruta.
pbruta <- abs(pbruta/1000)
publicidad <- as.numeric(discretas$PUBLICI)
contador = as.integer(1)

for(i in publicidad){
  if (i/pbruta[contador] > 0 & i/pbruta[contador] < 0.02){
    publicidad[contador] <- 1 
  }else if (i/pbruta[contador] >= 0.02 ) {
    publicidad[contador] <- 2 
  }
  contador <- contador+1
  
}

publicidad <- factor(publicidad, labels=c("Nopub", "Pubbaja", "Pubmod"))
discretas$PUBLICI <- publicidad

#OTRAS POSIBLES CLASIFICACIONES


# #Socios Mujeres
smujeres <- as.integer(discretas$PROMUJ)
smujeres[smujeres >= 1 ] <- 1
smujeres <- factor(smujeres, labels=c("SisociosM", "NosociosM"))
discretas$PROMUJ<- smujeres
# 
#Socios Hombres
shombres <- as.integer(discretas$PROHOM)
shombres[shombres >= 1 ] <- 1
shombres <- factor(shombres, labels=c("SisociosH", "NosociosH"))
discretas$PROHOM<- shombres

#personal permanete mujeres
pmujeres <- as.integer(discretas$PERMUJ)
pmujeres[pmujeres >= 1 & pmujeres < 10 ] <- 1
pmujeres[pmujeres >= 10 ] <- 2
pmujeres <- factor(pmujeres, labels=c("(0)PerM", "(1-10)PerM", "(>10)PerM"))
discretas$PERMUJ <- pmujeres

# Personal permanente Hombres
phombres <- as.integer(discretas$PERHOM)
phombres[phombres >= 1 & phombres<= 10 ] <- 1
phombres[phombres  > 10 ] <- 2
phombres <- factor(phombres, labels=c("(0)PerH", "(1-10)PerH", "(>10)PerH"))
discretas$PERHOM <- phombres
# 
# Personal Temporal Mujeres
tmujeres <- as.integer(discretas$DIRMUJ)
tmujeres[tmujeres >= 1 & tmujeres < 10 ] <- 1
tmujeres[tmujeres >= 10 ] <- 2
tmujeres <- factor(tmujeres, labels=c("(0)tempM", "(1-10)tempM", "(>10)tempM"))
discretas$DIRMUJ <- tmujeres

# Personal Temporal Hombres
thombres <- as.integer(discretas$DIRHOM)
thombres[thombres >= 1 & thombres<= 10 ] <- 1
thombres[thombres  > 10 ] <- 2
thombres <- factor(thombres, labels=c("(0)tempH", "(1-10)tempH", "(>10)tempH"))
discretas$DIRHOM <- thombres
# 
# Personal temporal cont a través de agencias
ptagenc <- as.integer(discretas$AGENCIA)
ptagenc[ptagenc >= 1 ] <- 1
ptagenc <- factor(ptagenc, labels=c("NoperAgencia", "SiperAgencia" ))
discretas$AGENCIA <- ptagenc
# 
# Aprendices
aprendiz <- as.integer(discretas$APRENDIZ)
aprendiz[aprendiz >= 1 & aprendiz< 3 ] <- 1
aprendiz[aprendiz  >= 3 ] <- 2
aprendiz <- factor(aprendiz, labels=c("(0)Apren", "(1-3)Apren", "(>3)Apren"))
discretas$APRENDIZ <- aprendiz




summary(discretas)
#Reclasificamos al tipo de empresas

# Tipempr <- discretas$CORRELA_16
# tipo1 <- list(454, 451, 453)
# tipo2 <- list(462, 464, 465, 466)
# tipo3 <- list("4711-472", 4719, 473, "4741-4742", 4752, "4759-4761", "4771-4751", 4772, 4773)
# 
# Tipempr[Tipempr %in% tipo1] <- 1
# Tipempr[Tipempr %in% tipo2] <- 2
# Tipempr[Tipempr %in% tipo3] <- 3
# 
# Tipempr <- factor(as.integer(Tipempr), labels=c("Comercio vehiculos", "Comercio al por mayor", "Comercio al por menor"))
# discretas$CORRELA_16 <- Tipempr
# 
# table(discretas$IDOJ1)
# K <- unclass(table(discretas$TMÑEMP, discretas$CORRELA_16))
# K_add <- addmargins(K)

#PCAshiny(continuas)
#res.PCA<-PCA(continuas,quali.sup=c(1),graph=FALSE)
#plot.PCA(res.PCA,choix='var',cex=1.2,cex.main=1.2,cex.axis=1.2 )
#plot.PCA(res.PCA,axes=c(1,3),choix='var')
#plot.PCA(res.PCA,axes=c(2,3),choix='var')
