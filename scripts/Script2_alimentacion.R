library(ggthemes)
library(purrr)
library(gdata)

svyby(~HTA,by=~urb_rur,design = diseño,FUN=svyciprop,vartype=c('se','ci'))
svyby(~HTA,by=~region_natural,design = diseño,FUN=svyciprop,vartype=c('se','ci'))
svyby(~HTA,by=~Dieta_ideal,design = diseño,FUN=svyciprop,vartype=c('se','ci'))
HTA <- svyby(~HTA,by=~Dieta_ideal+quintil_bienestar,design = diseño,FUN=svyciprop,vartype=c('se','ci'))

svyby(~OBESIDAD,by=~urb_rur,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T)
svyby(~OBESIDAD,by=~region_natural,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T)
svyby(~OBESIDAD,by=~Dieta_ideal,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T)
Obseidad <- svyby(~OBESIDAD,by=~Dieta_ideal+quintil_bienestar,design = diseño,FUN=svyciprop,
                           vartype=c('se','ci'),na.rm=T)

HTA <- set_names(HTA, c('Dieta_ideal','quintil_bienestar','y','se','ci_l','ci_u'))
Obesidad <- set_names(Obseidad, c('Dieta_ideal','quintil_bienestar','y','se','ci_l','ci_u'))
quintil_bienestar <- combine(HTA,Obesidad)
quintil_bienestar <- mutate(quintil_bienestar, Dieta_ideal = ifelse(Dieta_ideal==1,'Dieta Ideal','No Dieta Ideal'))

ggplot(quintil_bienestar, aes(x=quintil_bienestar, y=y, colour = as.factor(Dieta_ideal))) +
  geom_point() + facet_grid(source ~ as.factor(Dieta_ideal)) +
  geom_errorbar(width=.1, aes(ymin=ci_l, ymax= ci_u), colour="darkred") + 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.35)) +
  labs(x = 'Quintil de Bienestar', y = 'Prevalencia',
       title=" Prevalencia de Hipertensión y Obesidad por Quintil de Bienestar", 
       caption="Fuente: Encuesta Demográfica y de Salud Familiar (Año 2017).")  +
  theme(legend.position="none") 

