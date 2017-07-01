#----#carga librerias----

library(dplyr)

#---#agrupar por equipo---
  
#Idea_1: sumar valores y ponderarlos por 90 min. Que es la duracion de un partido.
  
#----#preparacion de datos----

equipos_tot <- Basetotal %>%  dplyr::select(fecha,team.1,minutos_jugados:atajada_penal) %>% group_by(fecha,team.1)

equipos_tot <- equipos_tot %>%summarise_each(funs(sum,n_distinct(fecha,team.1)))%>%dplyr::select(fecha:minutos_jugados_n_distinct)

equipos_tot <- equipos_tot %>% mutate(partidos=minutos_jugados_n_distinct)

equipos_tot$minutos_jugados_n_distinct <- NULL

equipos_tot_df <- as.data.frame(equipos_tot)

equipos_tot_df <- equipos_tot_df %>% dplyr::select(-fecha)%>% group_by(team.1) %>%summarise_each(funs(sum))

