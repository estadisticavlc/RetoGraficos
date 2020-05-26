lista_de_paquetes <- c("circlize","RColorBrewer") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(circlize)
library(RColorBrewer)

# Creamos la lista de datos

origin <- c("España","Resto UE(28)","Europa No UE(28)","África","América del Norte","América Central","América del Sur","Asia")
destination <- rep("València",8)
data <- data.frame(origin, destination)
data$origin<-factor(data$origin, 
                    levels = c("España","Resto UE(28)","Europa No UE(28)","África","América del Norte","América Central","América del Sur","Asia"))
adjacencyData <- with(data, table(origin, destination))
# Introducir datos en valores porcentuales 
adjacencyData[,1]=c(46.1,12.7,3.6,4.1,1.4,5.2,18.9,7.9)

# Hacer el gráfico

png("20200608 Grafico diagrama de cuerdas.png", width = 9, height = 5, units = 'in', res = 300)
set.seed(1234)
chordDiagram(adjacencyData, transparency = 0.5, annotationTrack = c("name", "grid"), scale = T)
title(main = "Altas por inmigración interurbana según país/continente de nacionalidad (2018)", cex.main=0.9)
mtext("Fuente: Altas y bajas registradas en el Padrón Municipal de Habitantes. Elaboración: Oficina de Estadística. Ajuntament de València.", 
      side=1, cex = 0.6, font = 3)
dev.off()