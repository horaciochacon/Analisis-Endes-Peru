library(ggthemes)
library(purrr)
library(gdata)

# Elaboramos las tablas de prevalencia de hipertensión por área residencia, regiónnatural, sexo y quintil de bienestar

svyby(~HIPERTENSION,by=~AREA_RESIDENCIA,design = diseño,FUN=svyciprop,vartype=c('se','ci'))
svyby(~HIPERTENSION,by=~REGION_NATURAL,design = diseño,FUN=svyciprop,vartype=c('se','ci'))
svyby(~HIPERTENSION,by=~SEXO,design = diseño,FUN=svyciprop,vartype=c('se','ci'))
Hipertensión <- svyby(~HIPERTENSION,by=~SEXO+QUINTIL_BIENESTAR,design = diseño,FUN=svyciprop,vartype=c('se','ci'))

# Elaboramos las tablas de prevalencia de obesidad por área residencia, regiónnatural, sexo y quintil de bienestar

svyby(~OBESIDAD,by=~AREA_RESIDENCIA,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T)
svyby(~OBESIDAD,by=~REGION_NATURAL,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T)
svyby(~OBESIDAD,by=~SEXO,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T)
Obseidad <- svyby(~OBESIDAD,by=~SEXO+QUINTIL_BIENESTAR,design = diseño,FUN=svyciprop,
                           vartype=c('se','ci'),na.rm=T)

# Agregamos las tablas de obesidad e hipertensión por sexo y quintil de bienestar como insumo para el gráfico

Hipertensión <- set_names(Hipertensión, c('SEXO','QUINTIL_BIENESTAR','y','se','ci_l','ci_u'))
Obesidad <- set_names(Obseidad, c('SEXO','QUINTIL_BIENESTAR','y','se','ci_l','ci_u'))
QUINTIL_BIENESTAR <- combine(Hipertensión,Obesidad)
QUINTIL_BIENESTAR <- mutate(QUINTIL_BIENESTAR, SEXO = ifelse(SEXO==1,'Masculino','Femenino'))

# Graficamos las prevalencias puntuales y los intervalos de confianza al 95% con la tabla previa

ggplot(QUINTIL_BIENESTAR, aes(x=QUINTIL_BIENESTAR, y=y, colour = as.factor(SEXO))) +
  geom_point(size = 2) + facet_grid(source ~ as.factor(SEXO)) +
  geom_errorbar(width=.1, aes(ymin=ci_l, ymax= ci_u)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.35)) +
  labs(x = 'Quintil de bienestar', y = 'Prevalencia')  +
  theme(legend.position="none") 

# Validando los coeficientes de variación
cv(svyby(~HIPERTENSION,by=~AREA_RESIDENCIA,design = diseño,FUN=svyciprop,vartype=c('se','ci')))
cv(svyby(~HIPERTENSION,by=~REGION_NATURAL,design = diseño,FUN=svyciprop,vartype=c('se','ci')))
cv(svyby(~HIPERTENSION,by=~SEXO,design = diseño,FUN=svyciprop,vartype=c('se','ci')))
cv(svyby(~HIPERTENSION,by=~SEXO+QUINTIL_BIENESTAR,design = diseño,FUN=svyciprop,vartype=c('se','ci')))
cv(svyby(~OBESIDAD,by=~AREA_RESIDENCIA,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T))
cv(svyby(~OBESIDAD,by=~REGION_NATURAL,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T))
cv(svyby(~OBESIDAD,by=~SEXO,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T))
cv(svyby(~OBESIDAD,by=~SEXO+QUINTIL_BIENESTAR,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T))

