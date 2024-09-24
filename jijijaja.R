# cambia idioma de la consola de R a español:
Sys.setenv(LANG="es")

#Sin notacion cientifica y con 3 decimales
options(digits = 3, scipen = 999) 
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
setwd("C:/Users/thetr/OneDrive/Documentos/R/Proyecto EDM")
ruta_del_archivo <- "EAC_CIFRAS_2022_ANONIMIZADA_FINAL.csv"
datos_orig <- read.csv(ruta_del_archivo, sep = ";", dec = ",")

#En nuestro estudio, tomaremos aquellas empresas Activas. POr lo que, no tomaremos encienta a empresas con produccion bruta = 0. 
#Asi que, eliminaremos aquellos datos que tengan produccion bruta =0 (Son alrededor de 200 datos).
datos <- datos_orig[!is.na(as.numeric(datos_orig$BRUTA)) & as.numeric(datos_orig$BRUTA) != 0, ]

#Crearemos dos nuevas bases de datos, en este caso, para las variables discretas y continuas de nuestro interes.
#IDOJ1
discretas <- select(datos, CORRELA_16, IDAIO, SOCIOS, PERSONOM, DIRECTO, TOTMUJ, TOTHOM, PUBLICI, PROMUJ, PROHOM,PERMUJ, PERHOM, DIRMUJ, DIRHOM, AGENCIA, APRENDIZ)
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

# Socios Mujeres
smujeres <- as.integer(discretas$PROMUJ)
smujeres[smujeres >= 1 ] <- 1
smujeres <- factor(smujeres, labels=c("SisociosM", "NosociosM"))
discretas$PROMUJ<- smujeres

# Socios Hombres
shombres <- as.integer(discretas$PROHOM)
shombres[shombres >= 1 ] <- 1
shombres <- factor(shombres, labels=c("SisociosH", "NosociosH"))
discretas$PROHOM<- shombres

# #personal permanete mujeres
pmujeres <- as.integer(discretas$PERMUJ)
pmujeres[pmujeres >= 1 & pmujeres < 10 ] <- 1
pmujeres[pmujeres >= 10 ] <- 2
pmujeres <- factor(pmujeres, labels=c("(0)PerM", "(1-10)PerM", "(>10)PerM"))
discretas$PERMUJ <- pmujeres

# # Personal permanente Hombres
phombres <- as.integer(discretas$PERHOM)
phombres[phombres >= 1 & phombres<= 10 ] <- 1
phombres[phombres  > 10 ] <- 2
phombres <- factor(phombres, labels=c("(0)PerH", "(1-10)PerH", "(>10)PerH"))
discretas$PERHOM <- phombres

# # Personal Temporal Mujeres
tmujeres <- as.integer(discretas$DIRMUJ)
tmujeres[tmujeres >= 1 & tmujeres < 10 ] <- 1
tmujeres[tmujeres >= 10 ] <- 2
tmujeres <- factor(tmujeres, labels=c("(0)tempM", "(1-10)tempM", "(>10)tempM"))
discretas$DIRMUJ <- tmujeres

# # Personal Temporal Hombres
thombres <- as.integer(discretas$DIRHOM)
thombres[thombres >= 1 & thombres<= 10 ] <- 1
thombres[thombres  > 10 ] <- 2
thombres <- factor(thombres, labels=c("(0)tempH", "(1-10)tempH", "(>10)tempH"))
discretas$DIRHOM <- thombres

# # Personal temporal cont a través de agencias 
ptagenc <- as.integer(discretas$AGENCIA)
ptagenc[ptagenc >= 1 & ptagenc < 10 ] <- 1
ptagenc[ptagenc >= 10 ] <- 2
ptagenc <- factor(ptagenc, labels=c("(0)PTAgenc", "(1-10)PTAgenc", "(>10)PTAgenc"))
discretas$AGENCIA <- ptagenc

# # Aprendices
aprendiz <- as.integer(discretas$APRENDIZ)
aprendiz[aprendiz >= 1 & aprendiz<= 10 ] <- 1
aprendiz[aprendiz  > 10 ] <- 2
aprendiz <- factor(aprendiz, labels=c("(0)Apren", "(1-10)Apren", "(>10)Apren"))
discretas$APRENDIZ <- aprendiz

#Total hombres
thombres <- as.integer(discretas$TOTHOM)
thombres[thombres >= 1 & thombres<= 10 ] <- 1
thombres[thombres  > 10 ] <- 2
thombres <- factor(thombres, labels=c("(0)HOMBRES", "(1-10)HOMBRES", "(>10)HOMBRES"))
discretas$TOTHOM <- thombres

#Total Mujeres
tmujeres <- as.integer(discretas$TOTMUJ)
tmujeres[tmujeres >= 1 & tmujeres < 10 ] <- 1
tmujeres[tmujeres >= 10 ] <- 2
tmujeres <- factor(tmujeres, labels=c("(0)MUJERES", "(1-10)MUJERES", "(>10)MUJERES"))
discretas$TOTMUJ <- tmujeres

# numero de socios
n_socios <- as.numeric(datos$SOCIOS)
n_socios[n_socios == 0 ] <- 0
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

Tipempr <- discretas$CORRELA_16
tipo1 <- list(454, 451, 453)
tipo2 <- list(462, 464, 465, 466)
tipo3 <- list("4711-472", 4719, 473, "4741-4742", 4752, "4759-4761", "4771-4751", 4772, 4773)

Tipempr[Tipempr %in% tipo1] <- 1
Tipempr[Tipempr %in% tipo2] <- 2
Tipempr[Tipempr %in% tipo3] <- 3
# 
Tipempr <- factor(as.integer(Tipempr), labels=c("Comercio vehiculos", "Comercio al por mayor", "Comercio al por menor"))
discretas$CORRELA_16 <- Tipempr
#Variables activas
Y = discretas[, 1:11]
#1. Número de ejes para el ACM: Usaremos 2, esto basandonos en el criterio de benzecri
#2. Número de ejes para la clasicación: en el histograma de valores propios, usaremos 16
fc <- FactoClass(Y, dudi.acm)

barplot(as.vector(fc$dudi$eig), col="darkblue")

barplot(rev(tail(fc$indices$Indice,15)), horiz=TRUE, col="darkblue")


summary ( fc$ cluster ) -> nk
round ( nk/ sum ( nk )* 100 ,1)
# Valores test variables cuantitativas
#fc$carac.cont
# Valores test categorías de v. cualitativas 
fc$carac.cate
xtable(fc$clus.summ[,1:4],digits=c(0,0,0,3,3))

barplot(as.vector(fc$dudi$eig), col="darkblue")
plotFactoClass(fc, x=1, y=2, cex.col=0.5,
               roweti="", infaxes="no", cstar=0)




