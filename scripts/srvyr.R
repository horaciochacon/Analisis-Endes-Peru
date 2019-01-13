library(srvyr)
library(tidyverse)
library(sjlabelled)

modelo <- ENDES %>% as_survey_design(id = CONGLOMERADO, strata = ESTRATO, weights = PONDERACION)

modelo %>%  group_by(AREA_RESIDENCIA) %>% summarise(proportion = survey_mean(HIPERTENSION))

modelo %>% group_by(SEXO, QUINTIL_BIENESTAR) %>% 
  summarise(proportion = survey_mean(OBESIDAD, na.rm = T, vartype = c("cv","ci"))) %>%
  mutate(SEXO = as_character(SEXO), QUINTIL_BIENESTAR = as_character(QUINTIL_BIENESTAR)) %>% 
  ggplot(aes(x = QUINTIL_BIENESTAR, y = as.numeric(proportion))) + geom_bar(stat = 'identity') +
  facet_wrap(~SEXO,nrow = 2) + xlab("Quintil de bienestar") + ylab("Prevalencia de obesidad") + 
  theme_minimal()


