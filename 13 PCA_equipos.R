#crear un nuevo set de datos

base_locales_pca <- base_modelado_locales[,3:61]

base_locales_pca[!complete.cases(base_locales_pca),] <- 0

borrar <- df_status(base_locales_pca)

locales_pca <- prcomp(base_locales_pca[borrar$variable[borrar$p_zeros<100]],scale. = TRUE)
summary(locales_pca)


plot(locales_pca)
biplot(locales_pca$x[base_modelado_completa$resultado_local=='V',],
       locales_pca$x[base_modelado_completa$resultado_local!='V',],col = c("blue","red"))


base_visitantes_pca <- base_modelado_visitantes[,3:61]

base_visitantes_pca[!complete.cases(base_visitantes_pca),] <- 0

borrar <- df_status(base_visitantes_pca)

visitantes_pca <- prcomp(base_visitantes_pca[borrar$variable[borrar$p_zeros<100]],scale. = TRUE)
summary(visitantes_pca)


plot(visitantes_pca)

biplot(visitantes_pca$x[base_modelado_completa$resultado_local=='V',],
       visitantes_pca$x[base_modelado_completa$resultado_local!='V',],col = c("blue","red"))



#-----base con PCA-----

base_modelado_locales_pca <- cbind(base_locales[,1:2],locales_pca$x[,1:15])
base_modelado_visitantes_pca <- cbind(base_visitantes[,1:2],visitantes_pca$x[,1:15])
base_modelado_completa <- base_modelado_locales_pca %>% left_join(base_modelado_visitantes_pca,by="even_id_evento")

base_modelado_completa <- base_modelado_completa %>% left_join(lkp_eventos[,c("even_id_evento","fixt_local_goles","fixt_visitante_goles")],by="even_id_evento")

base_modelado_completa <- base_modelado_completa %>% mutate(resultado_local=ifelse(fixt_local_goles>fixt_visitante_goles,1,0)) %>% select(-fixt_local_goles,-fixt_visitante_goles)

