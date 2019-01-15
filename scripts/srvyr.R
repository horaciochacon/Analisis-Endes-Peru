library(srvyr)
library(tidyverse)
library(sjlabelled)
library(ggthemes)
library(purrr)
library(gdata)


modelo <- ENDES %>% as_survey_design(id = CONGLOMERADO, strata = ESTRATO, weights = PONDERACION)

modelo %>%  
  group_by(AREA_RESIDENCIA) %>% 
  summarise(Prevalencia = survey_mean(HIPERTENSION, vartype  = c("ci","cv"))) %>% 
  mutate(AREA_RESIDENCIA = as_character(AREA_RESIDENCIA))

modelo %>%  
  group_by(REGION_NATURAL) %>% 
  summarise(Prevalencia = survey_mean(HIPERTENSION, vartype  = c("ci","cv")))
 
modelo %>%  
  group_by(SEXO) %>% 
  summarise(Prevalencia = survey_mean(HIPERTENSION, vartype  = c("ci","cv"))) %>% 
  mutate(SEXO = as_character(SEXO))


modelo %>% group_by(SEXO, QUINTIL_BIENESTAR) %>% 
  summarise(Prevalencia = survey_mean(OBESIDAD, na.rm = T, vartype = c("ci"))) %>% 
  mutate(Origen = "Obesidad") %>% 
  union(modelo %>% group_by(SEXO, QUINTIL_BIENESTAR) %>% 
              summarise(Prevalencia = survey_mean(HIPERTENSION, na.rm = T, vartype = c("ci"))) %>% 
          mutate(Origen = "Hipertension")) %>% 
  mutate(SEXO = as_character(SEXO)) %>% 
  ggplot(aes(x = QUINTIL_BIENESTAR, y = as.numeric(Prevalencia))) +
  geom_point(size = 2) + facet_grid(Origen ~ as.factor(SEXO)) +
  geom_errorbar(width=.1, aes(ymin=Prevalencia_low, ymax= Prevalencia_upp)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.35)) +
  labs(x = 'Quintil de bienestar', y = 'Prevalencia')  +
  theme(legend.position="none") 


