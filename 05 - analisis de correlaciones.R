----#librerias-----

library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif
library(CombMSC)
library(ggplot2)
library(caret)
library(car)
library(ellipse)
library(QuantPsyc)
library(leaps)
library(xlsx)
library(dummies)
library(ggcorrplot)
library(fmsb)
options(scipen = 999)

-----#correlacion entre variables----

dev.copy(png,'grafico10_corrplot.png')
corrplot((cor(matjugadores,use="complete.obs")),method = "circle",type="upper")

dev.off()


ggcorrplot((cor(matjugadores,use="complete.obs")), hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Players", 
           ggtheme=theme_bw)

---#star plot de habilidades----

ggplot(matjugadores, aes(x = variable, y = value, colour = id, group = id)) +
  geom_line() +
  coord_polar(theta = "x", direction = -1) +
  scale_y_continuous(labels = percent)



radarchart(rbind(rep(0.5,14),rep(0,14),as.data.frame(matjugadores)[50,])  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.5,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)


