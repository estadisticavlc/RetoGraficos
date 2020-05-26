lista_de_paquetes <- c("curl","ggplot2", "grid", "dplyr","GGally", "viridis") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(dplyr)
library(GGally)
library(viridis)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/MicrodatosMayo2020.csv"),sep=";",dec=",")

# Creamos datos para el plot
datos.plot=c()
for (i in c(1:3)){
   fila=c()
   for (pregunta in c("P5_1","P5_2","P5_3","P5_4","P5_5")){
      fila=c(fila,100*length(which(datos[,pregunta]==1 & datos$Edat3==i))/length(which(datos$Edat3==i)))
   }
   datos.plot=rbind(datos.plot,c(fila,i))
}
datos.plot=as.data.frame(datos.plot)
colnames(datos.plot)=c("P5_1","P5_2","P5_3","P5_4","P5_5","Edad")
datos.plot$Edad=c("Menores de 40","De 40 a 64","Mayores de 64")
datos.plot$Edad=factor(datos.plot$Edad,levels=c("Menores de 40","De 40 a 64","Mayores de 64"))

# Dibujamos el gráfico de coordenadas paralelas
p=ggparcoord(datos.plot,
             columns = 1:5, groupColumn = 6, order = "anyClass",
             scale="globalminmax",
             showPoints = TRUE, 
             alphaLines = 1) + 
   geom_line(size=2) +
   scale_color_brewer(palette = "Set2") +
   theme_bw()+
   labs(title="¿Con qué frecuencia realiza las siguientes medidas de prevención de la COVID-19?",
        x=NULL,
        y="Porcentaje que responde 'Siempre'",
        caption="Fuente: Barómetro de Opinión Ciudadana de Mayo de 2020. Elaboración: Oficina de Estadística. Ajuntament de València.")+
   scale_x_discrete(labels=c("P5_1" = "Lavarse las manos antes y después de salir", "P5_2" = "Toser y estornudar en el codo",
                             "P5_3" = "Respetar la distancia de 2 metros", "P5_4" = "Utilizar mascarilla fuera de casa", 
                             "P5_5" = "Utilizar guantes fuera de casa"))+
   theme(plot.title = element_text(size=14,hjust = 0.5,face="bold"),
         plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0.5),
         legend.position = "right",
         axis.text.x = element_text(color="black", angle=30, hjust = 1, size=8),
         axis.text.y = element_text(color="black"))
p
ggsave(filename = paste0("20200609 Grafico de coordenadas paralelas.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')

