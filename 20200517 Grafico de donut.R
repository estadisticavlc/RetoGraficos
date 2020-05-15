lista_de_paquetes <- c("curl","ggplot2", "grid", "RColorBrewer", "plyr") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(plyr)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/MicrodatosMarzo2020.csv"),sep=";",dec=",")
datos$PES=as.numeric(as.character(datos$PES))
# Agregar No sabe con No contesta en P7
datos$P7[datos$P7==99]=98
valores <- sort(as.numeric(unique(datos$P7)))
# Obtener frecuencias relativas, teniendo en cuenta el peso de cada entrevista
pesos <- aggregate(PES ~ P7, data = datos, sum)
datos_plot=data.frame(valores,freq=pesos$PES)
datos_plot$freq=100*datos_plot$freq/sum(datos_plot$freq)
datos_plot$valores=c("Es barato","Es adecuado","Es caro","NS/NC")
datos_plot$ymax <- cumsum(datos_plot$freq)
datos_plot$ymin <- c(0, head(datos_plot$ymax, n=-1))
datos_plot$labelPosition <- (datos_plot$ymax + datos_plot$ymin)/2
datos_plot$label <- paste0(datos_plot$valores,": ",round(datos_plot$freq,1),"%")

# Dibujamos el gráfico de donut
p=ggplot(datos_plot, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=as.factor(valores))) +
   geom_rect() +
   geom_label(x=3.5, aes(y=labelPosition, label=label), size=2) +
   scale_fill_manual(values = brewer.pal(5,"Pastel1")) +
   coord_polar(theta="y") +
   xlim(c(2, 4)) +
   theme_void() +
   labs(title="En relación al precio de su alquiler o hipoteca, ¿cree usted que…?", 
        caption="Fuente: Barómetro de Opinión Ciudadana de Marzo de 2020. Elaboración: Oficina d'Estadística. Ajuntament de València.") +
   theme(legend.position = "none")+
   theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
         plot.caption = element_text(color = "black",face = "italic", size = 6, hjust = 0))
p
ggsave(filename = paste0("20200517 Grafico de donut.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


