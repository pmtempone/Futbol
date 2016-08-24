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


