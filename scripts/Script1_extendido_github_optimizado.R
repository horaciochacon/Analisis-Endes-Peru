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

# Realizamos la unión con la función unir_endes

BASE1 <- unir_endes(PERSONA, SALUD, 'individual')
BASE2 <- unir_endes(MUJER_OBS, MUJER_LAC, 'individual')
BASE3 <- unir_endes(HOGAR, VIVIENDA, 'hogar')
BASE4 <- unir_endes(BASE1, BASE2, 'individual')
BASE5 <- unir_endes(BASE3, BASE4, 'hogar')

# Filtramos la base de datos final (BASE5)

BASE_FINAL <- BASE5 %>%
  filter(V213 != 1 | is.na(V213)) %>% # Descartamos a las gestantes
  filter(QSRESINF == 1) # Filtramos entrevistas completas del CSALUD (n = 32 514)

# Descartamos las bases de datos que ya no son necesarias para ahorrar memoria

rm(SALUD, BASE1, BASE2, BASE3, BASE4, BASE5, PERSONA, HOGAR, VIVIENDA, MUJER_LAC, MUJER_OBS)

# Calculamos la variable Hipertensión Arterial y Obesidad

BASE_FINAL <- BASE_FINAL %>%
  mutate(PAMS = (QS905S + QS903S) / 2, PAMD = (QS905D + QS903D) / 2) %>% # Obtenemos la PAM/D sistólica de dos mediciones
  mutate(HIPERTENSION = (PAMS >= 140) | (PAMD >= 90)) %>% # Definimos el criterio de HTA
  mutate(
    PESO = ifelse(!is.na(V437), V437 / 10, QS900), # Extraemos el peso de dos variables
    TALLA = ifelse(!is.na(V438), V438 / 10, QS901), IMC = PESO / (TALLA / 100)^2, # Extraemos la talla de dos variables
    OBESIDAD = ifelse(IMC >= 30, TRUE, FALSE)
  ) # Definimos IMC según criterio (IMC mayor o igual a 30)

# Fruta
BASE_FINAL <- BASE_FINAL %>%
  mutate(
    Frutas_dias_ultima_semana = ifelse(QS213U == 1, QS213C, NA), Frutas_Unidades_dia = QS214C,
    Fruta_por_semana = ifelse(QS213U == 3, 0, Frutas_dias_ultima_semana * Frutas_Unidades_dia)
  )

# Jugo de Fruta
BASE_FINAL <- BASE_FINAL %>%
  mutate(
    Jugo_Frutas_dias_ultima_semana = ifelse(QS215U == 1, QS215C, NA), Jugo_Frutas_Vasos_dia = QS216C,
    Jugo_Fruta_Vasos_por_semana = ifelse(QS215U == 3, 0, Jugo_Frutas_dias_ultima_semana * Jugo_Frutas_Vasos_dia * 2)
  )

# Ensalada de Fruta
BASE_FINAL <- BASE_FINAL %>%
  mutate(
    Ensalada_Frutas_dias_ultima_semana = ifelse(QS217U == 1, QS217C, NA), Ensalada_Frutas_porciones_dia = QS218C,
    Ensalada_Fruta_porciones_por_semana = ifelse(QS217U == 3, 0,
      Ensalada_Frutas_dias_ultima_semana * Ensalada_Frutas_porciones_dia * 2
    )
  )

# Ensalada de Verdura
BASE_FINAL <- BASE_FINAL %>%
  mutate(
    Ensalada_Verduras_dias_ultima_semana = ifelse(QS219U == 1, QS219C, NA), EV_Unidad = QS220U,
    Ensalada_Verduras_Unidades_dia = coalesce(as.numeric(QS220CV), as.numeric(QS220CC)),
    porcion_ensalada_verdura = ifelse(EV_Unidad == 2, Ensalada_Verduras_Unidades_dia / 4,
      ifelse(EV_Unidad == 1, Ensalada_Verduras_Unidades_dia, NA)
    ),
    Ensalada_Verdura_por_semana = ifelse(QS219U == 3, 0, Ensalada_Verduras_dias_ultima_semana * porcion_ensalada_verdura)
  )

# Porciones de alimentos sanos
BASE_FINAL <- BASE_FINAL %>%
  mutate(
    fruta_o_ensalada_porciones_semana = Ensalada_Verdura_por_semana +
    Ensalada_Fruta_porciones_por_semana + Jugo_Fruta_Vasos_por_semana + Fruta_por_semana,
    DIETA_IDEAL = fruta_o_ensalada_porciones_semana >= 35
  )

# Generamos la base de datos final solo con las variables necesarias

ENDES <- select(BASE_FINAL, HHID, QSNUMERO,
  CONGLOMERADO = HV001, ESTRATO = HV022,
  PONDERACION = PESO15_AMAS, SEXO = HV104, AREA_RESIDENCIA = HV025,
  QUINTIL_BIENESTAR = HV270, REGION_NATURAL = SHREGION, HIPERTENSION, PESO,
  TALLA, IMC, OBESIDAD, DIETA_IDEAL
)