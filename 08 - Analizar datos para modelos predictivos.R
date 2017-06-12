#----#carga de librerias#----

library(funModeling)
library(dplyr)


#----# data profiling----
my_data_status=df_status(Basetotal)

write.csv(my_data_status,file = "campos.csv")

# Removing variables with 100% of zero values
vars_to_remove=subset(my_data_status, my_data_status$p_zeros == 100)
vars_to_remove["variable"] #no hubo goles en contra en los 3 torneos

## Keeping all except vars_to_remove
Basetotal=Basetotal[, !(names(Basetotal) %in% vars_to_remove[,"variable"])]

#Deteccion de outliers

summary(Basetotal)

# checking the value for the top 1% of highest values (percentile 0.99), which is ~ 7.05
quantile(Basetotal$pase_correcto, 0.99)


#----#selecting variables----
library(ggplot2)
sv <- ggplot(Basetotal) + geom_boxplot(aes(x=titular, y=minutos_jugados, color=titular))

sv

#----#cross plot-----

cross_minutos=cross_plot(Basetotal, str_input="minutos_jugados", str_target="titular")

vars_to_analyze=c("torneo", "team.1", "rol_id_rol","pase_correcto")

cross_plot(data=Basetotal, str_target="titular", str_input=vars_to_analyze)



#proporcion de suplentes que jugaron en relacion a los titulares

prop.sup <- data.frame(rol_id_rol=c('1','2','3','4'),value=c(1.3,7.6,25.8,36.8))

prop.sup$porcentaje <- c(prop.sup$value/sum(prop.sup$value))

#----#boxplot----

plotar(data=Basetotal, str_input="minutos_jugados", str_target="titular", plot_type = "boxplot")

ggplot(data = Basetotal,mapping = aes(x=rol_id_rol,y=minutos_jugados,col=rol_id_rol))+geom_boxplot()


#----#density plot----

plotar(data=Basetotal, str_input="minutos_jugados", str_target="titular", plot_type = "histdens")

plotar(data=Basetotal, str_input="pase_correcto", str_target="titular", plot_type = "histdens")

#----base para modelo predictivo----

Basetotal$victoria_l <- ifelse(Basetotal$fixt_local_goles - Basetotal$fixt_visitante_goles>0,1,0)

base_titulares <- Basetotal %>% filter(titular=='S')

base_locales <- base_titulares %>% filter(J_local=='L') %>% group_by(even_id_evento,local.1) %>% 
  summarise(loc_goles=sum(goles_convertidos),loc_asistencias=sum(asistencias),loc_disp_afuera=sum(disparo_afuera),
            loc_disp_palo=sum(disparo_palo),loc_disp_atajado=sum(disparo_atajado),loc_penal_errado=sum(penal_errado),
            loc_faltas=sum(faltas),loc_offsides=sum(offsides),loc_amarillas=sum(amarillas),loc_doble_ama=sum(doble_amarilla),
            loc_despejes=sum(despejes),loc_quites=sum(quites),loc_atajadas=sum(atajadas),loc_ataj_penal=sum(atajada_penal))

base_visitantes <- base_titulares %>% filter(J_local=='V') %>% group_by(even_id_evento,visitante.1) %>% 
  summarise(vis_goles=sum(goles_convertidos),vis_asistencias=sum(asistencias),vis_disp_afuera=sum(disparo_afuera),
            vis_disp_palo=sum(disparo_palo),vis_disp_atajado=sum(disparo_atajado),vis_penal_errado=sum(penal_errado),
            vis_faltas=sum(faltas),vis_offsides=sum(offsides),vis_amarillas=sum(amarillas),vis_doble_ama=sum(doble_amarilla),
            vis_despejes=sum(despejes),vis_quites=sum(quites),vis_atajadas=sum(atajadas),vis_ataj_penal=sum(atajada_penal))

base_partido <- cbind(base_locales,base_visitantes[,2:15])

#armado de la base para enviar

base_modelado_locales <- base_locales[,1:2] %>% left_join(avg_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_local"="equipo"))
base_modelado_visitantes <- base_visitantes[,1:2] %>% left_join(avg_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_visitante"="equipo"))

base_modelado_completa <- base_modelado_locales %>% left_join(base_modelado_visitantes,by="even_id_evento")


base_modelado_completa <- base_modelado_completa %>% left_join(lkp_eventos[,c("even_id_evento","fixt_local_goles","fixt_visitante_goles")],by="even_id_evento")

base_modelado_completa <- base_modelado_completa %>% mutate(resultado_local=ifelse(fixt_local_goles>fixt_visitante_goles,1,0))
