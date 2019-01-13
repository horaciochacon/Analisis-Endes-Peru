###########################################
#                                         #
#     ENDES 2017 EN R: APLICACION HTA     #
#         Akram Hernández-Vásquez         #
#        akram.hernandez.v@upch.pe        #
#         Horacio Chacón-Torrico          #
#         horacio.chacon.t@upch.pe        #
#                                         #
###########################################

#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algún dataset 
# o informacion cargada

rm(list = ls())

#####################################
# 1. Preparación de las Librerias   #
#####################################

library(foreign)
library(tidyverse)
library(plyr)
library(survey)

########################################
# 2. Lectura de datos                  #
########################################

# Cargando las Bases como Tibble

CSALUD01 <- as.tibble(read.spss("data/CSALUD01.sav",to.data.frame = T))
RE223132 <-  as.tibble(read.spss("data/RE223132.SAV",to.data.frame = T))
REC42 <-  as.tibble(read.spss("data/REC42.SAV",to.data.frame = T))
RECH1 <-  as.tibble(read.spss("data/RECH1.SAV",to.data.frame = T))
RECH23 <-  as.tibble(read.spss("data/RECH23.SAV",to.data.frame = T))
RECH0 <-  as.tibble(read.spss("data/RECH0.SAV",to.data.frame = T))


# Ordenamos CSALUD01

CSALUD01 <- arrange(CSALUD01,CSALUD01$HHID, CSALUD01$QSNUMERO)

# En RE223132 creamos HHID y QSNUMERO y ordenamos
# HHID es la subcadena de texto sin los 3 últimos caracteres de CASEID
# QSNUMERO es la subcadena restante (2 últimos caracteres) de CASEID

RE223132$HHID <- str_sub(RE223132$CASEID,1,(str_length(RE223132$CASEID)-3))
RE223132$QSNUMERO <- str_sub(RE223132$CASEID,-2,-1)
RE223132$QSNUMERO <- as.numeric(RE223132$QSNUMERO)
RE223132 <- arrange(RE223132,RE223132$HHID, RE223132$QSNUMERO)

# En REC42 creamos HHID y QSNUMERO y ordenamos

REC42$HHID <- str_sub(REC42$CASEID,1,(str_length(REC42$CASEID)-3))
REC42$QSNUMERO <- str_sub(REC42$CASEID,-2,-1)
REC42$QSNUMERO <- as.numeric(REC42$QSNUMERO)
REC42 <- arrange(REC42,REC42$HHID, REC42$QSNUMERO)

# Ordenamos RECH23

RECH23 <- arrange(RECH23,RECH23$HHID)

# Ordenamos RECH0

RECH0 <- arrange(RECH0,RECH0$HHID)

# Ordenamos RECH1
RECH1$QSNUMERO <- RECH1$HVIDX
RECH1 <- arrange(RECH1,RECH1$QSNUMERO)


########################################
# 3. Unión de Datos                    #
########################################

# Juntamos las bases RECH1 y CSALUD01 "MyData1" merge 1:1 HHID QSNUMERO
# Akram: por lo que veo hay que unir la base RECH1 que tiene información de cada
# una de las personas. Pero esta base no tiene un QSNUMERO, sino un HVIDX que lo
# estoy tomando como si fuera este QSNUMERO. Asi el join 1:1 se basa en esas dos
# columnas. 

#PRUEBAS

##CS <- select(CSALUD01,'HHID','QSNUMERO'); table(CS$QSNUMERO) ; sum(is.na(RE22))
##RE22 <- select(RE223132,'HHID','QSNUMERO') ; table(RE22$QSNUMERO) ; sum(is.na(RE22))
##REC42


#PRUEBAS

MyData1 <- join(RECH1, CSALUD01, by = c("HHID","QSNUMERO"),type="left")
MyData1 <- arrange(MyData1,MyData1$HHID,MyData1$QSNUMERO)

# Juntamos las bases RE223132 y RECH42 "MyData2"  merge 1:1 HHID QSNUMERO

MyData2 <- join(RE223132, REC42, by = c("HHID", "QSNUMERO"),type='left')
MyData2 <- arrange(MyData2,MyData2$HHID,MyData2$QSNUMERO)

# Juntamos las bases RECH0 y RECH23 "MyData3"  merge 1:1 HHID

MyData3 <- join(RECH0, RECH23, by = "HHID",type='left')
MyData3 <- arrange(MyData3,MyData3$HHID)

# Juntamos las bases MyData1 y MyData2 "MyData4"   merge 1:1 HHID QSNUMERO

MyData4 <- join(MyData1,MyData2,by = c("HHID","QSNUMERO"),type='left')
MyData4 <- arrange(MyData4,MyData4$HHID,MyData4$QSNUMERO)

# Juntamos las bases MyData3 y MyData4 "MyData5" merge 1:m HHID

MyData5 <- join(MyData3,MyData4,by = 'HHID', type = 'left')

########################################
# 4. Filtros                           #
########################################

MyData6 <- filter(MyData5,V213!="Yes" | is.na(V213))

MyData6 <- filter(MyData6,QSRESINF == "Iniciar entrevista")

#Se omite el filtro de mayor o igual o 15 años
       

# Borramos los objetos que no utilizaremos
rm(CSALUD01,MyData1,MyData2,MyData3,MyData4,MyData5,RECH1,RECH0,RECH23,REC42,RE223132)

########################################
# 5. Creación diseño de Encuesta       #
########################################

#----------------------------------------
#Seteamos la encuesta mediante el paquete "survey"
#Incluir las variables HV001, hv022, PESO15_AMAS

design <-svydesign(id=~HV001, strata=~HV022, weights=~PESO15_AMAS, data=MyData6)    #VER QUE A VECES 

##############################################
# 5. Preparación de las variables de interés #
##############################################

#----------------------------------------
#Generando valores promedio de la PA sistólica
MyData6$PAMS <-(MyData6$QS905S+MyData6$QS903S)/2

#----------------------------------------
#Generando valores promedio de la PA diastólica
MyData6$PAMD <-(MyData6$QS905D+MyData6$QS903D)/2

#----------------------------------------
#Generando la variable hipertensión arterial y tabulamos los casos
MyData6$HTA_JNC <-(MyData6$PAMS>=140) | (MyData6$PAMD>=90)
MyData6$HTA_ideal <- (MyData6$PAMS<120) | (MyData6$PAMD<80)
table(MyData6$HTA_JNC)


# Generamos la Variable Cigarrillo

MyData6$TBQ <- if_else(MyData6$QS200 == "Si",TRUE,if_else(MyData6$QS200 == 'No sabe/ no recuerda',NA,FALSE))
MyData6$TBQ_ideal <- if_else(MyData6$QS200 == "No",TRUE,if_else(MyData6$QS200 == 'No sabe/ no recuerda',NA,FALSE))

# Generamos la Variable Diabetes

MyData6$DBT <- if_else(MyData6$QS109 == "Si",TRUE,if_else(MyData6$QS109 == 'No sabe/ no recuerda',NA,FALSE))
MyData6$DBT_ideal <- if_else(MyData6$QS109 == "No",TRUE,if_else(MyData6$QS109 == 'No sabe/ no recuerda',NA,FALSE))

# Generamos la Variable Peso V437 y QS900

# Validamos que no hay dos mediciones de peso distintas en un mismo sujeto
table(!is.na(MyData6$V437),!is.na(MyData6$QS900))

MyData6$PESO <- ifelse(!is.na(MyData6$V437),MyData6$V437/10,ifelse(!is.na(MyData6$QS900),MyData6$QS900,NA))

# Comprobamos gráficamente los datos
ggplot(data=MyData6, mapping = aes(x=MyData6$HV104,y=PESO)) + geom_boxplot()

# Generamos la Variable Talla V438 y QS901

# Validamos que no hay dos mediciones de peso distintas en un mismo sujeto
table(!is.na(MyData6$V438),!is.na(MyData6$QS901))

MyData6$TALLA <- ifelse(!is.na(MyData6$V438),MyData6$V438/10,ifelse(!is.na(MyData6$QS901),MyData6$QS901,NA))

# Comprobamos gráficamente los datos
ggplot(data=MyData6, mapping = aes(x=MyData6$HV104,y=TALLA)) + geom_boxplot()

# Generamos la Variable IMC

MyData6$IMC <- MyData6$PESO/(MyData6$TALLA/100)^2

MyData6$Obesidad <- ifelse(MyData6$IMC>=30,TRUE,FALSE)

MyData6$IMC_ideal <- ifelse(MyData6$IMC<25,TRUE,FALSE)

# Comprobamos gráficamente los datos
ggplot(data= filter(MyData6,IMC<100), mapping = aes(x=HV104,y=IMC)) + geom_boxplot() 

#Usar comando which para encontrar errores

# Fruta


MyData6$Frutas_dias_ultima_semana <- ifelse(MyData6$QS213U == 'Numero de dias',MyData6$QS213C,ifelse(MyData6$QS213U == 'No comio',0,NA))

MyData6$Frutas_Unidades_dia <- MyData6$QS214C

MyData6$Fruta_por_semana <- as.numeric(as.character(MyData6$Frutas_dias_ultima_semana))*as.numeric(as.character(MyData6$Frutas_Unidades_dia))
MyData6$Fruta_por_semana <- ifelse(is.na(MyData6$Fruta_por_semana),0,MyData6$Fruta_por_semana)

# Jugo de Fruta


MyData6$Jugo_Frutas_dias_ultima_semana <- ifelse(MyData6$QS215U == 'Numero de dias',MyData6$QS215C,ifelse(MyData6$QS215U == 'No tomo',0,NA))

MyData6$Jugo_Frutas_Vasos_dia <- MyData6$QS216C

MyData6$Jugo_Fruta_Vasos_por_semana <- as.numeric(as.character(MyData6$Jugo_Frutas_dias_ultima_semana))*as.numeric(as.character(MyData6$Jugo_Frutas_Vasos_dia))*2 
MyData6$Jugo_Fruta_Vasos_por_semana <-  ifelse(is.na(MyData6$Jugo_Fruta_Vasos_por_semana),0,MyData6$Jugo_Fruta_Vasos_por_semana)

# Ensalada de Fruta


MyData6$Ensalada_Frutas_dias_ultima_semana <- ifelse(MyData6$QS217U == 'Numero de dias',MyData6$QS217C,ifelse(MyData6$QS217U == 'No comio',0,NA))

MyData6$Ensalada_Frutas_porciones_dia <- MyData6$QS218C

MyData6$Ensalada_Fruta_porciones_por_semana <- as.numeric(as.character(MyData6$Ensalada_Frutas_dias_ultima_semana))*as.numeric(as.character(MyData6$Ensalada_Frutas_porciones_dia))*2
MyData6$Ensalada_Fruta_porciones_por_semana <- ifelse(is.na(MyData6$Ensalada_Fruta_porciones_por_semana),0,MyData6$Ensalada_Fruta_porciones_por_semana)

# Ensalada de Verdura


MyData6$Ensalada_Verduras_dias_ultima_semana <- ifelse(MyData6$QS219U == 'Numero de dias',MyData6$QS219C,ifelse(MyData6$QS219U == 'No comio',0,NA))

MyData6$Ensalada_Verduras_Unidad <- MyData6$QS220U

MyData6$Ensalada_Verduras_Unidades_dia <- ifelse(!is.na(MyData6$QS220CC),MyData6$QS220CC,ifelse(!is.na(MyData6$QS220CV),MyData6$QS220CV,NA))

MyData6$porcion_ensalada_verdura <- ifelse(MyData6$Ensalada_Verduras_Unidad == 'Numero de cucharadas',
                                           MyData6$Ensalada_Verduras_Unidades_dia/4, ifelse(MyData6$Ensalada_Verduras_Unidad == 'Numero de porciones',
                                                                                            MyData6$Ensalada_Verduras_Unidades_dia,NA))


MyData6$Ensalada_Verdura_por_semana <- as.numeric(as.character(MyData6$Ensalada_Verduras_dias_ultima_semana))*as.numeric(as.character(MyData6$porcion_ensalada_verdura))
MyData6$Ensalada_Verdura_por_semana <- ifelse(is.na(MyData6$Ensalada_Verdura_por_semana),0,MyData6$Ensalada_Verdura_por_semana)

# Porciones de alimentos sanos

MyData6$fruta_o_ensalada_porciones_semana <- MyData6$Ensalada_Verdura_por_semana+MyData6$Ensalada_Fruta_porciones_por_semana+
  MyData6$Jugo_Fruta_Vasos_por_semana+MyData6$Fruta_por_semana

MyData6$Dieta_ideal <- ifelse(MyData6$fruta_o_ensalada_porciones_semana >= 35,TRUE,FALSE)

# Corregimos la variable edad

MyData6$HV105 <- as.character(MyData6$HV105)
MyData6$HV105 <- ifelse(MyData6$HV105 == '97+','97',MyData6$HV105)
MyData6$HV105 <- as.numeric(MyData6$HV105)

MyData6$grupo_etareo <-  cut(MyData6$HV105, breaks = c(0,15,25,35,45,55,65,100),right = F,labels = c('0-14','15-24','25-34','35-44','45-54','55-65','65+'))


# Plots de prueba

ggplot(data= filter(MyData6,IMC<100), mapping = aes(x=HV106,y=IMC)) + geom_boxplot() 

ggplot(data = MyData6, mapping = aes(x=as.numeric(as.character(MyData6$HV105)))) + geom_histogram(bins = 82)

ggplot(data = MyData6, mapping = aes(x=HV104,y=as.numeric(as.character(MyData6$HV105)))) + geom_boxplot() 

ggplot(data= filter(MyData6,IMC<100), mapping = aes(x=HV116,y=IMC)) + geom_boxplot() 

ggplot(data = MyData6, mapping = aes(x = QS27)) + geom_bar() + coord_flip()

ggplot(data = MyData6, mapping = aes(x = Obesidad, y = Fruta_por_semana)) + geom_boxplot()

ggplot(data = filter(MyData6,IMC<100), mapping = aes(y = IMC, x = Frutas_Unidades_dia)) + geom_point() + geom_smooth()

ggplot(data = filter(MyData6,IMC<100) , mapping = aes(y = IMC, x = as.numeric(as.character(HV105)))) + geom_point() + geom_smooth()


# Verificación ANEXOS INEI

table(MyData6$HV104) # Sexo
table(MyData6$HV025) # Urbano/Rural


table(MyData6$DBT,useNA = 'ifany')
table(MyData6$QS200,useNA = 'ifany')
table(MyData6$QS205C,useNA = 'ifany') # Cigarrillos diario N = 481
table(MyData6$QS213U,useNA = 'ifany') 

# Seleccionamos las variables: Sexo, Area urbano/rural, edad (crear grupo etareo), Area de residencia, nivel educativo
# Dominio de Residencia, quintil de bienestar, Costa/sierra/selva * Urbano/rural, region, departamento

df1 <- select(MyData6, HHID, QSNUMERO,HV001, HV022, PESO15_AMAS,sexo = HV104, edad = HV105, grupo_etareo, urb_rur = HV025, 
       nivel_educativo = HV106,HV270,HV271, anos_estudio = HV108, region_natural = SHREGION, departamento = HV024,
       HTA_JNC,HTA_ideal, DBT,DBT_ideal, PESO, TALLA, IMC, IMC_ideal, TBQ,TBQ_ideal, Frutas_dias_ultima_semana,Frutas_Unidades_dia, Fruta_por_semana,
       Jugo_Frutas_dias_ultima_semana, Jugo_Frutas_Vasos_dia, Jugo_Fruta_Vasos_por_semana, Ensalada_Frutas_dias_ultima_semana,
       Ensalada_Frutas_porciones_dia,Ensalada_Fruta_porciones_por_semana,Ensalada_Verduras_dias_ultima_semana,Ensalada_Verduras_Unidad,
       Ensalada_Verduras_Unidades_dia,Ensalada_Verdura_por_semana,porcion_ensalada_verdura,fruta_o_ensalada_porciones_semana,
       Dieta_ideal)

attach(df1)

df1$CV_ideal <-  DBT_ideal*HTA_ideal*TBQ_ideal*IMC_ideal*Dieta_ideal
df1$CV_ideal <- as.logical(df1$CV_ideal)
df1$CV_score <-  DBT_ideal+HTA_ideal+TBQ_ideal+IMC_ideal+Dieta_ideal

df1 <- filter(df1,!is.na(CV_ideal))

# Escribimos el la base final en el .csv

write.csv(df1, 'data_output/df1.csv')

# Ponderación

design <-svydesign(id=~HV001, strata=~HV022, weights=~PESO15_AMAS, data=df1) 

svyciprop(~I(Dieta_ideal==TRUE), design, method="mean", df=degf(design))

class(df1$HTA_ideal)





   