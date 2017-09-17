
freqs <- table(ifelse(base_modelado_completa$fixt_local_goles>base_modelado_completa$fixt_visitante_goles,"V",
                            ifelse(base_modelado_completa$fixt_local_goles==base_modelado_completa$fixt_visitante_goles,"E","D")))/length(ifelse(base_modelado_completa$fixt_local_goles>base_modelado_completa$fixt_visitante_goles,"V",
                                                                                                                                                  ifelse(base_modelado_completa$fixt_local_goles==base_modelado_completa$fixt_visitante_goles,"E","D")))
info(freqs)
library(entropy)

entropy.empirical(freqs)


table(ifelse(base_modelado_completa$fixt_local_goles>base_modelado_completa$fixt_visitante_goles,"V","O"))

entropy(ifelse(base_modelado_completa$fixt_local_goles>base_modelado_completa$fixt_visitante_goles,"V",
               ifelse(base_modelado_completa$fixt_local_goles==base_modelado_completa$fixt_visitante_goles,"E","D")))

-(0.29*log(0.29,base = 3)+2*(0.355*log(0.355,base = 3)))

-(6/15)*log((6/15),base = 2)-(9/15)*log((9/15),base = 2)

-(0.2543193)*log(0.2543193,base = 3)-(0.3006220)*log(0.3006220,base = 3)-(0.4450587)*log(0.4450587,base = 3)

entropy::entropy(c(0.29,0.355,0.355))
