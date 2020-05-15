lista_de_paquetes <- c("curl","ggplot2", "grid", "plyr", "dplyr", "ggridges") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggridges)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/Indicadores.csv"),sep=",",dec=",")
datos=datos[datos$codigo=="Ind28",]
datos=datos[datos$ANO>=1938,]
datos$VALOR=as.numeric(as.character(datos$VALOR))
nombres_meses=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre",
        "Noviembre","Diciembre")
datos$MES=nombres_meses[datos$MES]
datos$MES=factor(datos$MES,levels=rev(nombres_meses))

# Dibujamos el gráfico ridgeline
p=ggplot(datos, aes(x = VALOR, y = MES, fill = MES)) +
   geom_density_ridges(alpha=0.6) +
   theme_ridges() + 
   theme(legend.position = "none") +
   labs(title="Temperatura mensual media en València (desde 1938)", 
        y="", 
        x="", 
        caption="Fuente: Agencia Estatal de Meteorología (AEMET). Elaboración: Oficina d'Estadística. Ajuntament de València.")+
   theme(plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0))+
   xlab("ºC")
p
ggsave(filename = paste0("20200518 Grafico ridgeline.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


