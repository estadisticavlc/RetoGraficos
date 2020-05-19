lista_de_paquetes <- c("curl","ggplot2", "hrbrthemes", "viridis","grid", "plyr", "dplyr") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(hrbrthemes)
library(viridis)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/LugarNacimiento.csv"),sep=";",dec=",")

# Calculamos los valores porcentuales por columna
porc_col=100*prop.table(as.matrix(datos[,2:ncol(datos)]),2)

# Creamos data.frame para hacer el gráfico
datos.plot=c()
lugares=as.character(datos[,1])
for (i in c(1:length(lugares))){
   for (j in 1:length(edades)){
      datos.plot=rbind(datos.plot,c(lugares[i],j,porc_col[i,j]))           
   }
}
datos.plot=as.data.frame(datos.plot)
colnames(datos.plot)=c("Lugar","Edad","Porc")
datos.plot$Edad=as.numeric(as.character(datos.plot$Edad))
datos.plot$Porc=as.numeric(as.character(datos.plot$Porc))

# Dibujamos el gráfico de áreas apiladas

edades=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90 o más")
p=ggplot(datos.plot, aes(x=Edad, y=Porc, fill=Lugar)) + 
        geom_area(alpha=0.6 , size=.5, colour="white") +
        scale_fill_viridis(discrete = T, name=NULL) +
        scale_x_continuous(breaks=c(1:10),labels=edades, expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        ylab("Porcentaje (%)")+
        xlab("Edad")+
        labs(title="Población de València según lugar de nacimiento y edad", 
             caption="Fuente: Padrón Municipal de Habitantes a 1 de enero de 2019. Elaboración: Oficina d'Estadística. Ajuntament de València.")+
        theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
              plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0),
              axis.text.x = element_text(size=6),
              axis.text.y = element_text(size=6))
p
ggsave(filename = paste0("20200520 Grafico de areas apiladas.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


