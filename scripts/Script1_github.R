# Nombre:     Script de manipulación y unión ENDES utilizando librería ENDES.PE
# Versión:    1.0
# Autores:    Akram Hernández-Vasquez, Horacio Chacón-Torrico
# Objetivos:  Cargar las bases de datos de la ENDES, unirlas en una sola base y
#             construir las variables para el análisis (obesidad, hipertensión y dieta) 
# Fecha:      28/02/19

library(tidyverse)
library(haven)
library(survey)
library(ENDES.PE)

# Cargamos las bases de datos de la ENDES

SALUD     <- consulta_endes(periodo = 2017, codigo_modulo = 414, base = "CSALUD01", guardar = FALSE)
MUJER_OBS <- consulta_endes(periodo = 2017, codigo_modulo = 67, base = "RE223132", guardar = FALSE)
MUJER_LAC <- consulta_endes(periodo = 2017, codigo_modulo = 70, base = "REC42", guardar = FALSE)
PERSONA   <- consulta_endes(periodo = 2017, codigo_modulo = 64, base = "RECH1", guardar = FALSE)
VIVIENDA  <- consulta_endes(periodo = 2017, codigo_modulo = 65, base = "RECH23", guardar = FALSE)
HOGAR     <- consulta_endes(periodo = 2017, codigo_modulo = 64, base = "RECH0", guardar = FALSE)
MUJER_ANT <- consulta_endes(periodo = 2017, codigo_modulo = 74, base = "RECH5", guardar = FALSE)

# Transformamos las variables de unión (DESCOMPOSICIÓN DEL CASEID EN HHID Y QSNUMERO):
# 1. Descartamos últimos 3 caracteres de CASEID y lo convertimos a HHID
# 2. Extraemos los últimos 2 caracteres de CASEID y lo convertimos a QSNUMERO
# 3. Convertimos la variable de texto a numérica

MUJER_OBS$HHID      <- str_sub(MUJER_OBS$CASEID,1,(str_length(MUJER_OBS$CASEID)-3))
MUJER_OBS$QSNUMERO  <- str_sub(MUJER_OBS$CASEID,-2,-1)
MUJER_OBS$QSNUMERO  <- as.numeric(MUJER_OBS$QSNUMERO)
MUJER_LAC$HHID      <- str_sub(MUJER_LAC$CASEID,1,(str_length(MUJER_LAC$CASEID)-3))
MUJER_LAC$QSNUMERO  <- str_sub(MUJER_LAC$CASEID,-2,-1)
MUJER_LAC$QSNUMERO  <- as.numeric(MUJER_LAC$QSNUMERO)
PERSONA$QSNUMERO    <- PERSONA$HVIDX
MUJER_ANT$QSNUMERO  <- MUJER_ANT$HA0

# Realizamos la unión mediante los identificadores o llaves HHID y QSNUMERO

BASE1 <- left_join(PERSONA, SALUD, by = c("HHID","QSNUMERO"))
BASE2 <- left_join(MUJER_OBS, MUJER_LAC, by = c("HHID", "QSNUMERO"))
BASE1 <- left_join(BASE1, MUJER_ANT, by = c("HHID", "QSNUMERO"))
BASE3 <- left_join(HOGAR, VIVIENDA, by = "HHID")
BASE4 <- left_join(BASE1,BASE2,by = c("HHID","QSNUMERO"))
BASE5 <- left_join(BASE3,BASE4,by = 'HHID')

# Filtramos la base de datos final (BASE5)

BASE_FINAL <- filter(BASE5,QSRESINF == 1) # Filtramos entrevistas completas del CSALUD (n = 32 514)

# Descartamos las bases de datos que ya no son necesarias para ahorrar memoria 

rm(SALUD,BASE1,BASE2,BASE3,BASE4,BASE5,PERSONA,HOGAR,VIVIENDA,MUJER_LAC,
   MUJER_OBS, MUJER_ANT)

# Calculamos la variable Hipertensión Arterial 

BASE_FINAL$PAMS <-(BASE_FINAL$QS905S+BASE_FINAL$QS903S)/2 # Obtenemos la PAM sistólica de dos mediciones
BASE_FINAL$PAMD <-(BASE_FINAL$QS905D+BASE_FINAL$QS903D)/2 # Obtenemos la PAM diastólica de dos mediciones
BASE_FINAL$HIPERTENSION  <- (BASE_FINAL$PAMS>=140) | (BASE_FINAL$PAMD>=90) # Definimos el criterio de HTA

# Calculamos la variable  Obesidad

BASE_FINAL <- BASE_FINAL %>% 
  mutate(PESO = ifelse(QS902 == 1 & (QS900 >= 1 & QS900 < 999), QS900,
                       ifelse(QS902 == 4, HA2/10, NA)),
         TALLA = ifelse(QS902 == 1 & (QS901 >= 1 & QS901 < 999), QS901,
                        ifelse(QS902 == 4, HA3/10, NA)))

BASE_FINAL <- BASE_FINAL %>% 
  mutate(ha13tmp = case_when(HA13 == 0 ~ 1,
                             HA13 == 3 ~ 2,
                             HA13 == 4 ~ 3,
                             HA13 == 6 ~ 6),
         RQS902 = ifelse(QS902 == 4, ha13tmp, QS902))


BASE_FINAL <- BASE_FINAL %>% 
  mutate(IMC = PESO/(TALLA^2)*10000) %>% 
  mutate(OBESIDAD = IMC >= 30, SOBREPESO = IMC >= 25 & IMC < 30) %>% 
  filter(V213 != 1 | is.na(V213), RQS902 == 1)

# Calculamos la variable dieta

# Fruta
BASE_FINAL <- BASE_FINAL %>%
  mutate(fruta_dxs = ifelse(QS213U == 1, QS213C, NA), fruta_uxd = QS214C,
         fruta_semana = fruta_dxs * fruta_uxd)

# Jugo de Fruta
BASE_FINAL <- BASE_FINAL %>%
  mutate(jugof_dxs = ifelse(QS215U == 1, QS215C, NA), jugof_vxd = QS216C,
         jugof_semana = jugof_dxs * jugof_vxd * 2)

# Ensalada de Fruta
BASE_FINAL <- BASE_FINAL %>%
  mutate(ensaladaf_dxs = ifelse(QS217U == 1, QS217C, NA), ensaladaf_pxd = QS218C,
         ensaladaf_semana = ensaladaf_dxs * ensaladaf_pxd * 2)

# Ensalada de Verdura
BASE_FINAL <- BASE_FINAL %>%
  mutate(ensaladav_dxs = ifelse(QS219U == 1, QS219C, NA), ensaladav_u = QS220U,
         ensaladav_uxd = coalesce(as.numeric(QS220CV), as.numeric(QS220CC)),
         ensaladav_porcion = ifelse(ensaladav_u == 2, ensaladav_uxd / 4,
                                    ifelse(ensaladav_u == 1, ensaladav_uxd, NA)),
         ensaladav_semana = ensaladav_dxs * ensaladav_porcion)

# Porciones de alimentos sanos
BASE_FINAL <- BASE_FINAL %>% 
  mutate(frutaensalada_pxs = rowSums(cbind(ensaladav_semana,ensaladaf_semana,
                                           jugof_semana, fruta_semana), 
                                     na.rm = TRUE)) %>% 
  mutate(DIETA = ifelse(frutaensalada_pxs == 0, NA, 
                        as.numeric(frutaensalada_pxs >= 31.5)))

# Generamos la base de datos final solo con las variables necesarias

ENDES <- select(BASE_FINAL, HHID, QSNUMERO, CONGLOMERADO = HV001, ESTRATO = HV022, 
                PONDERACION = PESO15_AMAS, SEXO = HV104, AREA_RESIDENCIA = HV025, 
                QUINTIL_BIENESTAR = HV270, REGION_NATURAL = SHREGION,HIPERTENSION, PESO,
                TALLA, IMC, OBESIDAD,DIETA)

# Especificamos el diseño muestral de la encuesta

diseño <- svydesign(id =~ CONGLOMERADO, strata =~ ESTRATO, weights=~ PONDERACION, data=ENDES) 


