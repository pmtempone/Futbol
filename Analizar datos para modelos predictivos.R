----#carga de librerias#----

library(funModeling)


----# data profiling----
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


----#selecting variables----

sv <- ggplot(Basetotal) + geom_boxplot(aes(x=titular, y=minutos_jugados, color=titular))

sv

----#cross plot-----

cross_minutos=cross_plot(Basetotal, str_input="minutos_jugados", str_target="titular")

vars_to_analyze=c("torneo", "team.1", "rol_id_rol","pase_correcto")

cross_plot(data=Basetotal, str_target="titular", str_input=vars_to_analyze)

----#boxplot----

plotar(data=Basetotal, str_input="minutos_jugados", str_target="titular", plot_type = "boxplot")

ggplot(data = Basetotal,mapping = aes(x=rol_id_rol,y=minutos_jugados,col=rol_id_rol))+geom_boxplot()


----#density plot----

plotar(data=Basetotal, str_input="minutos_jugados", str_target="titular", plot_type = "histdens")

plotar(data=Basetotal, str_input="pase_correcto", str_target="titular", plot_type = "histdens")

