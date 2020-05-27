lista_de_paquetes <- c("curl", "HistData","ggplot2", "grid", "gridExtra", "magrittr", "dplyr","knitr", "tidyverse") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(gridExtra)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/Indicadores.csv"),sep=",",dec=",")

datos_plot<-datos[which(datos$codigo=="Ind14" | datos$codigo=="Ind15"),c("ANO","MES","codigo", "VALOR")]
datos_plot<-datos_plot[which(datos_plot$ANO==2019),]
datos_plot$VALOR<-as.numeric(as.character(datos_plot$VALOR))
datos_plot$FECHA<-as.Date(paste0(datos_plot$ANO,"-",datos_plot$MES,"-1"))
datos_plot$TRANSPORTE<- factor(datos_plot$codigo, levels=c('Ind14','Ind15'), labels=c('Aeropuerto','Puerto'))
datos_plot$MES<-factor(datos_plot$MES, levels = 1:12, labels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))


# Dibujamos el gráfico diagrama de área polar o de Florence Nightingale
options(scipen=999)
p=ggplot(data = datos_plot,
         aes(x = MES, y = VALOR, fill = TRANSPORTE)) +
   geom_bar(width = 1, stat = "identity", position = "stack", colour = "gray70") +
   geom_text(aes(label=format(VALOR, big.mark = ".", scientific = FALSE)),size=1.5)+
   labs(title="Pasajeros en el Aeropuerto y en el Puerto de València. 2019",
        x=NULL,
        y=NULL,
        caption="Fuente: Aeropuertos Españoles y Navegación Aérea (AENA). Puerto Autónomo de València.\n\nElaboración: Oficina d'Estadística. Ajuntament de València.")+
   theme(plot.title = element_text(size=14,hjust = 0.5,face="bold"),
         plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0.5),
         legend.position = "right",
         axis.text.y = element_blank(),
         axis.ticks = element_blank())+
   scale_y_sqrt() +
   scale_fill_brewer(type = "qual", palette = "Pastel2", breaks = c("EMT","Aeropuerto","Puerto"), name="Medio de transporte")+
   coord_polar(start = 3 * pi / 2)

p
ggsave(filename = paste0("20200610 Grafico diagrama de area polar o de Florence Nightingale.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')

