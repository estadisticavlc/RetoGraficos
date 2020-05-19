lista_de_paquetes <- c("curl","ggplot2","grid", "plyr", "dplyr") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/MortalidadCausa.csv"),sep=";",dec=",",encoding = "UTF-8")

edades=c("35-39","40-44","45-49",
         "50-54","55-59","60-64","65-69","70-74","75-80","80-84","85 o más")
causas=as.character(datos$Causa.muerte)
datos=datos[,-1]

datos.plot=c()
for (i in 1:length(causas)){
   for (j in 1:length(edades)){
      datos.plot=rbind(datos.plot,c(causas[i],edades[j],datos[i,j]))
   }
}
datos.plot=as.data.frame(datos.plot)
colnames(datos.plot)=c("Causa","Edad","Tasa")
datos.plot$Causa=factor(datos.plot$Causa,levels=rev(causas))
datos.plot$Edad=factor(datos.plot$Edad,levels=edades)
datos.plot$Tasa=as.numeric(as.character(datos.plot$Tasa))

# Dibujamos el heatmap

p=ggplot(datos.plot, aes(Edad, Causa, fill= Tasa)) + 
        geom_tile()+
        theme_bw()+
        scale_fill_viridis_c(option="plasma") +
        scale_x_discrete(expand=c(0,0))+
        scale_y_discrete(expand=c(0,0))+
        labs(title="Mortalidad proporcional según causa y edad en la ciudad de València (2017)", 
             y="Causa de mortalidad (21 causas)", 
             x="Edad", 
             fill="%",
             caption="Fuente: Servici d'Estudis Epidemiològics i Estadístiques Sanitàries. Conselleria de Sanitat\n\nElaboración: Oficina d'Estadística. Ajuntament de València.")+
        theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
              plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0),
              axis.text = element_text(size=6))
p
ggsave(filename = paste0("20200522 Grafico mapas de calor o heatmap.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


