library(tidyverse)
library(haven)
library(survey)

# Cargamos las bases de datos de la ENDES

SALUD     <- read_sav("data/CSALUD01.sav", encoding = 'UTF-8')
MUJER_OBS <- read_sav("data/RE223132.SAV", encoding = 'UTF-8')
MUJER_LAC <- read_sav("data/REC42.SAV", encoding = 'UTF-8')
PERSONA   <- read_sav("data/RECH1.SAV", encoding = 'UTF-8')
VIVIENDA  <- read_sav("data/RECH23.SAV", encoding = 'UTF-8')
HOGAR     <- read_sav("data/RECH0.SAV", encoding = 'UTF-8')

# Transformamos las variables de unión

MUJER_OBS$HHID      <- str_sub(MUJER_OBS$CASEID,1,(str_length(MUJER_OBS$CASEID)-3))
MUJER_OBS$QSNUMERO  <- str_sub(MUJER_OBS$CASEID,-2,-1)
MUJER_OBS$QSNUMERO  <- as.numeric(MUJER_OBS$QSNUMERO)
MUJER_LAC$HHID      <- str_sub(MUJER_LAC$CASEID,1,(str_length(MUJER_LAC$CASEID)-3))
MUJER_LAC$QSNUMERO  <- str_sub(MUJER_LAC$CASEID,-2,-1)
MUJER_LAC$QSNUMERO  <- as.numeric(MUJER_LAC$QSNUMERO)
PERSONA$QSNUMERO    <- PERSONA$HVIDX

# Realizamos la unión

BASE1 <- left_join(PERSONA, SALUD, by = c("HHID","QSNUMERO"))
BASE2 <- left_join(MUJER_OBS, MUJER_LAC, by = c("HHID", "QSNUMERO"))
BASE3 <- left_join(HOGAR, VIVIENDA, by = "HHID")
BASE4 <- left_join(BASE1,BASE2,by = c("HHID","QSNUMERO"))
BASE5 <- left_join(BASE3,BASE4,by = 'HHID')

# Filtramos la base de datos final

BASE_FINAL <- filter(BASE5,V213!= 1 | is.na(V213)) # Descartamos las gestantes
BASE_FINAL <- filter(BASE_FINAL,QSRESINF == 1) # Filtramos entrevistas completas

# Eliminamos las bases que ya no son necesarias

rm(SALUD,BASE1,BASE2,BASE3,BASE4,BASE5,PERSONA,HOGAR,VIVIENDA,MUJER_LAC,MUJER_OBS)

# Calculamos la variable Hipertensión y Obesidad

BASE_FINAL$PAMS <-(BASE_FINAL$QS905S+BASE_FINAL$QS903S)/2
BASE_FINAL$PAMD <-(BASE_FINAL$QS905D+BASE_FINAL$QS903D)/2
BASE_FINAL$HTA  <-(BASE_FINAL$PAMS>=140) | (BASE_FINAL$PAMD>=90)

BASE_FINAL$PESO  <- ifelse(!is.na(BASE_FINAL$V437),BASE_FINAL$V437/10,
                          ifelse(!is.na(BASE_FINAL$QS900),BASE_FINAL$QS900,NA))
BASE_FINAL$TALLA <- ifelse(!is.na(BASE_FINAL$V438),BASE_FINAL$V438/10,
                           ifelse(!is.na(BASE_FINAL$QS901),BASE_FINAL$QS901,NA))
BASE_FINAL$IMC <- BASE_FINAL$PESO/(BASE_FINAL$TALLA/100)^2
BASE_FINAL$OBESIDAD <- ifelse(BASE_FINAL$IMC>=30,TRUE,FALSE)

# Generamos la base de datos final solo con las variables necesarias

ENDES <- select(BASE_FINAL, HHID, QSNUMERO, conglomerado = HV001, estrato = HV022, 
                ponderacion = PESO15_AMAS, sexo = HV104, urb_rur = HV025, 
                quintil_bienestar = HV270, region_natural = SHREGION, HTA, PESO,
                TALLA, IMC, OBESIDAD)

# Especificamos el diseño muestral de la encuesta

diseño <- svydesign(id =~ conglomerado, strata =~ estrato, weights=~ ponderacion, 
                    data=ENDES) 
