lista_de_paquetes <- c("curl", "waffle", "plyr", "dplyr", "ggplot2", "RColorBrewer") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerias necesarias
library(curl)
library(plyr)
library(dplyr)
library(waffle)
library(ggplot2)
library(RColorBrewer)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/ActividadesProfesionales.csv"),
               sep=";",dec=",",encoding = "UTF-8")

parts=datos$Total
names(parts)=datos$Descripcion.actividad.económica

# Dibujamos el grafico de waffle
p=waffle(100*parts/sum(parts), rows=5, colors=c(brewer.pal(12,"Paired"),brewer.pal(3,"Dark2")), 
         title="Actividades economicas profesionales en València (2019)",
         xlab=NULL,
         legend_pos = "bottom")+
  theme(legend.text = element_text(size = 4.9),
        plot.caption = element_text(size = 5, face = "italic"),
        legend.key.width=unit(0.3, "cm"),
        legend.key.height=unit(0.3, "cm"))+
  labs(caption = "Nota: Cada cuadrado representa a unas 300 personas.\n\nFuente: Impuesto de Actividades Economicas. Ayuntamiento de València.\n\nElaboración: Oficina d'Estadística. Ajuntament de València.")
p
ggsave(filename = paste0("20200527 Grafico de waffle.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')





