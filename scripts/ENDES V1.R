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
library(plotrix)
library(DHS.PE)
library(haven)

########################################
# 2. Lectura de datos                  #
########################################





# Cargando las Bases como Tibble

CSALUD01 <- as.tibble(read.spss("data/CSALUD01.sav",to.data.frame = T))
MUJER_RE223132 <-  as.tibble(read.spss("data/RE223132.SAV",to.data.frame = T))
MUJER_REC42 <-  as.tibble(read.spss("data/REC42.SAV",to.data.frame = T))
HOGAR_RECH1 <-  as.tibble(read.spss("data/RECH1.SAV",to.data.frame = T))
VIVIENDA_RECH23 <-  as.tibble(read.spss("data/RECH23.SAV",to.data.frame = T))
HOGAR_RECH0 <-  as.tibble(read.spss("data/RECH0.SAV",to.data.frame = T))


# Cargando las Bases con funcion

CSALUD01 <- as.tibble(consulta_endes(2017,414,'CSALUD01'))
MUJER_RE223132 <-  as.tibble(consulta_endes(2017,67,'RE223132'))
MUJER_REC42 <-  as.tibble(consulta_endes(2017,70,'REC42'))
HOGAR_RECH1 <-  as.tibble(consulta_endes(2017,64,'RECH1'))
VIVIENDA_RECH23 <-  as.tibble(consulta_endes(2017,65,'RECH23'))
HOGAR_RECH0 <-  as.tibble(consulta_endes(2017,64,'RECH0'))


# En MUJER_RE223132 creamos HHID y QSNUMERO y ordenamos
# HHID es la subcadena de texto sin los 3 últimos caracteres de CASEID
# QSNUMERO es la subcadena restante (2 últimos caracteres) de CASEID

MUJER_RE223132$HHID <- str_sub(MUJER_RE223132$CASEID,1,(str_length(MUJER_RE223132$CASEID)-3))
MUJER_RE223132$QSNUMERO <- str_sub(MUJER_RE223132$CASEID,-2,-1)
MUJER_RE223132$QSNUMERO <- as.numeric(MUJER_RE223132$QSNUMERO)

# En MUJER_REC42 creamos HHID y QSNUMERO y ordenamos

MUJER_REC42$HHID <- str_sub(MUJER_REC42$CASEID,1,(str_length(MUJER_REC42$CASEID)-3))
MUJER_REC42$QSNUMERO <- str_sub(MUJER_REC42$CASEID,-2,-1)
MUJER_REC42$QSNUMERO <- as.numeric(MUJER_REC42$QSNUMERO)

# Renombramos HVIDX

HOGAR_RECH1$QSNUMERO <- HOGAR_RECH1$HVIDX


########################################
# 3. Unión de Datos                    #
########################################

# Juntamos las bases HOGAR_RECH1 y CSALUD01 "BASE1" merge 1:1 HHID QSNUMERO

BASE1 <- join(HOGAR_RECH1, CSALUD01, by = c("HHID","QSNUMERO"),type="left")

# Juntamos las bases MUJER_RE223132 y RECH42 "BASE2"  merge 1:1 HHID QSNUMERO

BASE2 <- join(MUJER_RE223132, MUJER_REC42, by = c("HHID", "QSNUMERO"),type='left')

# Juntamos las bases HOGAR_RECH0 y VIVIENDA_RECH23 "BASE3"  merge 1:1 HHID

BASE3 <- join(HOGAR_RECH0, VIVIENDA_RECH23, by = "HHID",type='left')

# Juntamos las bases BASE1 y BASE2 "BASE4"   merge 1:1 HHID QSNUMERO

BASE4 <- join(BASE1,BASE2,by = c("HHID","QSNUMERO"),type='left')

# Juntamos las bases BASE3 y BASE4 "BASE5" merge 1:m HHID

BASE5 <- join(BASE3,BASE4,by = 'HHID', type = 'left')

########################################
# 4. Filtros                           #
########################################

BASE_FINAL <- filter(BASE5,V213!="Yes" | is.na(V213))
BASE_FINAL <- filter(BASE_FINAL,QSRESINF == "Iniciar entrevista")

# Borramos los objetos que no utilizaremos
rm(CSALUD01,BASE1,BASE2,BASE3,BASE4,BASE5,HOGAR_RECH1,HOGAR_RECH0,VIVIENDA_RECH23,MUJER_REC42,MUJER_RE223132)

##############################################
# 5. Preparación de las variables de interés #
##############################################

#Generando valores promedio de la PA sistólica
BASE_FINAL$PAMS <-(BASE_FINAL$QS905S+BASE_FINAL$QS903S)/2

#Generando valores promedio de la PA diastólica
BASE_FINAL$PAMD <-(BASE_FINAL$QS905D+BASE_FINAL$QS903D)/2

#Generando la variable HTA y HTA ideal
BASE_FINAL$HTA_JNC <-(BASE_FINAL$PAMS>=140) | (BASE_FINAL$PAMD>=90)
BASE_FINAL$HTA_ideal <- (BASE_FINAL$PAMS<120) | (BASE_FINAL$PAMD<80)

# Generamos la Variable Tabaquismo y Tabaquismo ideal

BASE_FINAL$TBQ <- if_else(BASE_FINAL$QS200 == "Si",TRUE,if_else(BASE_FINAL$QS200 == 'No sabe/ no recuerda',NA,FALSE))
BASE_FINAL$TBQ_ideal <- if_else(BASE_FINAL$QS200 == "No",TRUE,if_else(BASE_FINAL$QS200 == 'No sabe/ no recuerda',NA,FALSE))

# Generamos la Variable Diabetes y Diabetes ideal

BASE_FINAL$DBT <- if_else(BASE_FINAL$QS109 == "Si",TRUE,if_else(BASE_FINAL$QS109 == 'No sabe/ no recuerda',NA,FALSE))
BASE_FINAL$DBT_ideal <- if_else(BASE_FINAL$QS109 == "No",TRUE,if_else(BASE_FINAL$QS109 == 'No sabe/ no recuerda',NA,FALSE))

# Generamos la Variable Peso V437 y QS900

# Validamos que no hay dos mediciones de peso distintas en un mismo sujeto
table(!is.na(BASE_FINAL$V437),!is.na(BASE_FINAL$QS900))

BASE_FINAL$PESO <- ifelse(!is.na(BASE_FINAL$V437),BASE_FINAL$V437/10,ifelse(!is.na(BASE_FINAL$QS900),BASE_FINAL$QS900,NA))

# Comprobamos gráficamente los datos
ggplot(data=BASE_FINAL, mapping = aes(x=BASE_FINAL$HV104,y=PESO)) + geom_boxplot()

# Generamos la Variable Talla V438 y QS901
# Validamos que no hay dos mediciones de peso distintas en un mismo sujeto
table(!is.na(BASE_FINAL$V438),!is.na(BASE_FINAL$QS901))

BASE_FINAL$TALLA <- ifelse(!is.na(BASE_FINAL$V438),BASE_FINAL$V438/10,ifelse(!is.na(BASE_FINAL$QS901),BASE_FINAL$QS901,NA))

# Comprobamos gráficamente los datos
ggplot(data=BASE_FINAL, mapping = aes(x=BASE_FINAL$HV104,y=TALLA)) + geom_boxplot()

# Generamos la Variable IMC, Obesidad e IMC ideal

BASE_FINAL$IMC <- BASE_FINAL$PESO/(BASE_FINAL$TALLA/100)^2

BASE_FINAL$Obesidad <- ifelse(BASE_FINAL$IMC>=30,TRUE,FALSE)

BASE_FINAL$IMC_ideal <- ifelse(BASE_FINAL$IMC<25,TRUE,FALSE)

# Comprobamos gráficamente los datos
ggplot(data= filter(BASE_FINAL,IMC<100), mapping = aes(x=HV104,y=IMC)) + geom_boxplot() 


# Fruta
BASE_FINAL$Frutas_diasxsemana <- ifelse(BASE_FINAL$QS213U == 'Numero de dias',BASE_FINAL$QS213C,ifelse(BASE_FINAL$QS213U == 'No comio',0,NA))
BASE_FINAL$Frutas_un_dia <- BASE_FINAL$QS214C
BASE_FINAL$Fruta_semana <- BASE_FINAL$Frutas_diasxsemana*as.numeric(as.character(BASE_FINAL$Frutas_un_dia))
BASE_FINAL$Fruta_semana <- ifelse(is.na(BASE_FINAL$Fruta_semana),0,BASE_FINAL$Fruta_semana)

# Jugo de Fruta
BASE_FINAL$Jugo_Frutas_diasxsemana <- ifelse(BASE_FINAL$QS215U == 'Numero de dias',BASE_FINAL$QS215C,ifelse(BASE_FINAL$QS215U == 'No tomo',0,NA))
BASE_FINAL$Jugo_Frutas_Vasos_dia <- BASE_FINAL$QS216C
BASE_FINAL$Jugo_Fruta_Vasos_semana <- BASE_FINAL$Jugo_Frutas_diasxsemana*as.numeric(as.character(BASE_FINAL$Jugo_Frutas_Vasos_dia))*2 
BASE_FINAL$Jugo_Fruta_Vasos_semana <-  ifelse(is.na(BASE_FINAL$Jugo_Fruta_Vasos_semana),0,BASE_FINAL$Jugo_Fruta_Vasos_semana)

# Ensalada de Fruta
BASE_FINAL$Ensalada_Frutas_diasxsemana <- ifelse(BASE_FINAL$QS217U == 'Numero de dias',BASE_FINAL$QS217C,ifelse(BASE_FINAL$QS217U == 'No comio',0,NA))
BASE_FINAL$Ensalada_Frutas_porciones_dia <- BASE_FINAL$QS218C
BASE_FINAL$Ensalada_Fruta_porciones_semana <- BASE_FINAL$Ensalada_Frutas_diasxsemana*as.numeric(as.character(BASE_FINAL$Ensalada_Frutas_porciones_dia))*2
BASE_FINAL$Ensalada_Fruta_porciones_semana <- ifelse(is.na(BASE_FINAL$Ensalada_Fruta_porciones_semana),0,BASE_FINAL$Ensalada_Fruta_porciones_semana)

# Ensalada de Verdura
BASE_FINAL$Ensalada_Verduras_diasxsemana <- ifelse(BASE_FINAL$QS219U == 'Numero de dias',BASE_FINAL$QS219C,ifelse(BASE_FINAL$QS219U == 'No comio',0,NA))
BASE_FINAL$Ensalada_Verduras_Unidad <- BASE_FINAL$QS220U
BASE_FINAL$Ensalada_Verduras_un_dia <- ifelse(!is.na(BASE_FINAL$QS220CC),BASE_FINAL$QS220CC,ifelse(!is.na(BASE_FINAL$QS220CV),BASE_FINAL$QS220CV,NA))
BASE_FINAL$porcion_ensalada_verdura <- ifelse(BASE_FINAL$Ensalada_Verduras_Unidad == 'Numero de cucharadas',
                                              BASE_FINAL$Ensalada_Verduras_un_dia/4, ifelse(BASE_FINAL$Ensalada_Verduras_Unidad == 'Numero de porciones',
                                                                                            BASE_FINAL$Ensalada_Verduras_un_dia,NA))
BASE_FINAL$Ensalada_Verdura_semana <- BASE_FINAL$Ensalada_Verduras_diasxsemana*as.numeric(as.character(BASE_FINAL$porcion_ensalada_verdura))
BASE_FINAL$Ensalada_Verdura_semana <- ifelse(is.na(BASE_FINAL$Ensalada_Verdura_semana),0,BASE_FINAL$Ensalada_Verdura_semana)

# Porciones de alimentos sanos
BASE_FINAL$fruta_o_ensalada_porciones_semana <- BASE_FINAL$Ensalada_Verdura_semana+BASE_FINAL$Ensalada_Fruta_porciones_semana+
  BASE_FINAL$Jugo_Fruta_Vasos_semana+BASE_FINAL$Fruta_semana

BASE_FINAL$Dieta_ideal <- ifelse(BASE_FINAL$fruta_o_ensalada_porciones_semana >= 35,TRUE,FALSE)

# Corregimos la variable edad

BASE_FINAL$HV105 <- as.character(BASE_FINAL$HV105)
BASE_FINAL$HV105 <- ifelse(BASE_FINAL$HV105 == '97+','97',BASE_FINAL$HV105)
BASE_FINAL$HV105 <- as.numeric(BASE_FINAL$HV105)

#Elaboramos los grupos etareos
BASE_FINAL$grupo_etareo <-  cut(BASE_FINAL$HV105, breaks = c(0,15,25,35,45,55,65,100),right = F,labels = c('0-14','15-24','25-34','35-44','45-54','55-65','65+'))


# Seleccionamos las variables: Sexo, Area urbano/rural, edad (crear grupo etareo), Area de residencia, nivel educativo
# Dominio de Residencia, quintil de bienestar, Costa/sierra/selva * Urbano/rural, region, departamento

SALUD_CV <- select(BASE_FINAL, HHID, QSNUMERO, HV001, HV022, PESO15_AMAS, SEXO = HV104, 
                   EDAD = HV105, GRUPO_ETAREO = grupo_etareo, AREA_RESI = HV025, REGION_NATURAL = SHREGION,
                   DEPARTAMENTO = HV024, LUGAR_RESIDENCIA = HV026, NIVEL_EDUCATIVO = HV106, ANOS_ESTUDIO = HV108, 
                   WI_CATEGORIZADO =HV270, WI_CONTINUO = HV271, PAMD, PAMS, HTA_ideal, Dieta_ideal,
                   FRUTA_VERDURA_SEMANA = fruta_o_ensalada_porciones_semana,
                   DBT_ideal,IMC, IMC_ideal,TBQ_ideal)
                    

SALUD_CV$CV_ideal <-  SALUD_CV$DBT_ideal*SALUD_CV$HTA_ideal*SALUD_CV$TBQ_ideal*SALUD_CV$IMC_ideal*SALUD_CV$Dieta_ideal
SALUD_CV$CV_ideal <- as.logical(SALUD_CV$CV_ideal)
SALUD_CV$CV_score <-  SALUD_CV$DBT_ideal+SALUD_CV$HTA_ideal+SALUD_CV$TBQ_ideal+SALUD_CV$IMC_ideal+SALUD_CV$Dieta_ideal

SALUD_CV <- filter(SALUD_CV,!is.na(SALUD_CV$CV_ideal))


SALUD_CV$estado_civil <- ifelse(SALUD_CV$estado_civil == 'Never married', 'Soltero/a',
                                ifelse(SALUD_CV$estado_civil == 'Married' | SALUD_CV$estado_civil == 'Living together', 'Casado/a o conviviente',
                                       'Viudo/a, divorciado/a o separado/a'))

SALUD_CV$IMC_ideal2 <-  cut(SALUD_CV$IMC, breaks = c(0,25,30,200),
                                right = F,labels = c('Ideal','Intermediate','Poor'))

SALUD_CV$Dieta_ideal2 <-  cut(SALUD_CV$FRUTA_VERDURA_SEMANA, breaks = c(0,14,28,200),
                            right = F,labels = c('Poor','Intermediate','Ideal'))

SALUD_CV$HTA_ideal2 <- ifelse(SALUD_CV$PAMD >= 90 | SALUD_CV$PAMS >= 140, 
                              "Poor", ifelse(SALUD_CV$PAMD >= 80 | SALUD_CV$PAMS >= 120,
                                             "Intermediate", "Ideal"))

# Escribimos el la base final en el .csv

write.csv(SALUD_CV, 'data_output/SALUD_CV.csv')
write_dta(SALUD_CV, 'data_output/SALUD_CV.dta')

# Ponderación

design <-svydesign(id=~HV001, strata=~HV022, weights=~PESO15_AMAS, data=SALUD_CV) 

svyby(~CV_ideal,by=~grupo_etareo,design = design,FUN=svyciprop,vartype=c('se','ci'))

svyby(~CV_ideal,by=~urb_rur,design = design,FUN=svyciprop,vartype=c('se','ci'))

svyby(~CV_ideal,by=~region_natural,design = design,FUN=svyciprop,vartype=c('se','ci'))

svyby(~CV_ideal,by=~sexo,design = design,FUN=svyciprop,vartype=c('se','ci'))

svyby(~CV_ideal,by=~HV022,design = design,FUN=svyciprop,vartype=c('se','ci'))

quintil_bienestar <- svyby(~CV_ideal,by=~HV270,design = design,FUN=svyciprop,vartype=c('se','ci'))


ggplot(quintil_bienestar, aes(x = HV270, y = CV_ideal)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = ci_u, ymin = ci_l))+
  xlab('Quintil de Bienestar') + 
  ylab('Proporción de Salud CV ideal')


svyciprop(~I(Dieta_ideal==TRUE), design, method="mean", df=degf(design))

class(SALUD_CV$HTA_ideal)


# Tabla 1.1
attach(SALUD_CV)


# Sexo

table(sexo)
table(sexo, CV_ideal)
svyciprop(~I(sexo=='Male'), design, method="mean", df=degf(design))
svyciprop(~I(sexo=='Female'), design, method="mean", df=degf(design))
svyby(~CV_ideal,by=~sexo,design = design,FUN=svyciprop,vartype=c('se','ci'))

# Grupo Etareo

table(grupo_etareo)
table(grupo_etareo,CV_ideal,useNA = 'ifany')
svyciprop(~I(grupo_etareo=='15-24'), design, method="mean", df=degf(design))
svyciprop(~I(grupo_etareo=='25-34'), design, method="mean", df=degf(design))
svyciprop(~I(grupo_etareo=='35-44'), design, method="mean", df=degf(design))
svyciprop(~I(grupo_etareo=='45-54'), design, method="mean", df=degf(design))
svyciprop(~I(grupo_etareo=='55-65'), design, method="mean", df=degf(design))
svyciprop(~I(grupo_etareo=='65+'), design, method="mean", df=degf(design))
svyby(~CV_ideal,by=~grupo_etareo,design = design,FUN=svyciprop,vartype=c('se','ci'))


# Estado Civil

table(SALUD_CV$estado_civil,useNA = 'ifany')
table(SALUD_CV$estado_civil,CV_ideal,useNA = 'ifany')
svyciprop(~I(estado_civil=='Soltero/a'), design, method="mean", df=degf(design))
svyciprop(~I(estado_civil=='Casado/a o conviviente'), design, method="mean", df=degf(design))
svyciprop(~I(estado_civil=='Viudo/a, divorciado/a o separado/a'), design, method="mean", df=degf(design))
svyby(~CV_ideal,by=~estado_civil,design = design,FUN=svyciprop,vartype=c('se','ci'))


# Área de Residencia

table(urb_rur)
table(SALUD_CV$urb_rur,CV_ideal,useNA = 'ifany')
svyciprop(~I(urb_rur=='Urban'), design, method="mean", df=degf(design))
svyciprop(~I(urb_rur=='Rural'), design, method="mean", df=degf(design))
svyby(~CV_ideal,by=~urb_rur,design = design,FUN=svyciprop,vartype=c('se','ci'))


# Nivel Educativo

table(nivel_educativo)
table(SALUD_CV$nivel_educativo,CV_ideal,useNA = 'ifany')
svyciprop(~I(nivel_educativo=='No education, preschool'), design, method="mean", df=degf(design))
svyciprop(~I(nivel_educativo=='Primary'), design, method="mean", df=degf(design))
svyciprop(~I(nivel_educativo=='Secondary'), design, method="mean", df=degf(design))
svyciprop(~I(nivel_educativo=='Higher'), design, method="mean", df=degf(design))
svyby(~CV_ideal,by=~nivel_educativo,design = design,FUN=svyciprop,vartype=c('se','ci'))


# Dominio geográfico

table(region_natural,useNA = 'ifany')
table(region_natural,CV_ideal,useNA = 'ifany')
svyciprop(~I(region_natural=='Lima metropolitana'), design, method="mean", df=degf(design))
svyciprop(~I(region_natural=='Resto Costa'), design, method="mean", df=degf(design))
svyciprop(~I(region_natural=='Sierra'), design, method="mean", df=degf(design))
svyciprop(~I(region_natural=='Selva'), design, method="mean", df=degf(design))

svyby(~CV_ideal,by=~nivel_educativo,design = design,FUN=svyciprop,vartype=c('se','ci'))
