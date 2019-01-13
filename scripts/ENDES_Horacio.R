###########################################
#                                         #
#     ENDES 2017 EN R: APLICACION HTA     #
#         Akram Hernández-Vásquez       #
#        akram.hernandez.v@upch.pe        #
#                                         #
###########################################

#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algún dataset 
# o informacion cargada
rm(list = ls())

##############################
# 1. Instalación de paquetes #
##############################

# Instalación de Paquetes
#install.packages("dplyr")
#install.packages("foreign")
#install.packages("haven")
#install.packages("survey")
library(dplyr)
library(foreign)
library(haven)
library(survey)

##############################
# 2. Función _merge en R     #
##############################

# Función stata.merge en R similar al _merge en Stata
stata.merge <- function(x,y, name){
  x$new1 <- 1
  y$new2 <- 2
  df <- merge(x,y, by = name, all = TRUE)
  df$stat.merge.variable <- rowSums(df[,c("new1", "new2")], na.rm=TRUE)
  df$new1 <- NULL
  df$new2<- NULL
  df
}

########################################
# 3. Lectura de datos y unión de datos #
########################################
setwd("C:/Users/horac/Google Drive/ENDES_Horacio/Proyecto_ENDES/data")
#Cargamos el cuestionario salud "CSALUD01"
CSALUD01 <- read.spss("CSALUD01.sav")
write.csv(CSALUD01,file="CSALUD01.csv",row.names=FALSE)
CSALUD01 <-  read.csv("CSALUD01.csv")
orden_CSALUD01_CASEID <- CSALUD01[order(CSALUD01$HHID, CSALUD01$QSNUMERO), ]

#Cargamos el cuestionario individual "RE223132"
RE223132 <- read_sav("RE223132.SAV")
write.csv(RE223132,file="RE223132.csv",row.names=FALSE)
RE223132 <-  read.csv("RE223132.csv")
### Horacio por favor adaptar la línea 59 a 64 a R ###
numero <- str_length(CASEID)
# generate num_adj=numero-3
# generate HHID=substr(CASEID,1,num_adj)

# generate QSNUMERO=substr(CASEID,-2,2)
# destring QSNUMERO, replace
### 
orden_RE223132_CASEID <- RE223132[order(RE223132$HHID, RE223132$QSNUMERO), ]
table(RE223132$CASEID)


#Cargamos el cuestionario individual "REC42"
REC42 <- read_sav("REC42.SAV")
write.csv(REC42,file="REC42.csv",row.names=FALSE)
REC42 = read.csv("REC42.csv")
### Horacio por favor adaptar la línea 59 a 64 a R ###
# generate numero=length(CASEID)
# generate num_adj=numero-3
# generate HHID=substr(CASEID,1,num_adj)

# generate QSNUMERO=substr(CASEID,-2,2)
# destring QSNUMERO, replace
### 
orden_RREC42_CASEID <- REC42[order(REC42$HHID, REC42$QSNUMERO), ]
table(REC42$CASEID)


#Cargamos el cuestionario hogar "RECH23"
RECH23 <- read_sav("RECH23.SAV")
write.csv(RECH23,file="RECH23.csv",row.names=FALSE)
RECH23 = read.csv("RECH23.csv")
orden_RECH23_HHID <- RECH23[order(RECH23$HHID), ]

#Cargamos el cuestionario hogar "RECH0"
RECH0 <- read.spss("RECH0.SAV")
write.csv(RECH0,file="RECH0.csv",row.names=FALSE)
RECH0 = read.csv("RECH0.csv")
orden_RECH0_HHID <- RECH0[order(RECH0$HHID), ]

#Cargamos el cuestionario hogar "RECH1"
RECH1 <- read.spss("RECH1.SAV")
write.csv(RECH1,file="RECH1.csv",row.names=FALSE)
RECH1 = read.csv("RECH1.csv")
RECH1$QSNUMERO <- RECH1$HVIDX
orden_RECH1_HHID <- RECH1[order(RECH1$HHID, RECH1$QSNUMERO), ]


#Juntamos las bases RECH1 y CSALUD01 "MyData1"  merge 1:1 HHID QSNUMERO
MyData1 <- stata.merge(orden_RECH1_HHID, orden_CSALUD01_CASEID, "HHID", "QSNUMERO")

MyData1 <- merge(orden_RECH23_HHID, orden_RECH0_HHID, "HHID", "QSNUMERO")

orden_MyData1_HHID <- MyData1[order(MyData1$HHID, MyData1$QSNUMERO), ]  #esta es para el merge final
table(MyData1$HHID)
table(MyData1$stat.merge.variable)


#Juntamos las bases RE223132 y RECH42 "MyData2"  merge 1:1 HHID QSNUMERO


#Juntamos las bases RECH0 y RECH23 "MyData3"  merge 1:1 HHID


#Juntamos las bases MyData1 y MyData2 "MyData4"   merge 1:1 HHID QSNUMERO


#Juntamos las bases MyData3 y MyData4 "MyData5" merge 1:m HHID

