lista_de_paquetes <- c("curl", "igraph","grid", "plyr", "dplyr") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(grid)
library(plyr)
library(dplyr)
library(igraph)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/CambiosDomicilio_DT_2018.csv"),sep=";",dec=",")
datos_red<-datos
rownames(datos_red)<-datos$Distrito.origen
datos_red<-datos_red[,-1]
colnames(datos_red)<-rownames(datos_red)
for(i in 1:nrow(datos_red)){
   top_cambios<-which(datos_red[i,] %in% sort(datos_red[i,], decreasing = T)[1:4])
   datos_red[i,top_cambios] <- 1
   datos_red[i,-top_cambios]<-0
}
datos_red<-as.matrix(datos_red)
network <- graph_from_adjacency_matrix(datos_red , diag=F )
V(network)$size<-log(apply(datos[,-1], 1, sum))
E(network)$color<-"#9ecae1"

# Dibujamos el gráfico redes
png("20200531 Grafico redes.png", width = 9, height = 5, units = 'in', res = 300)
par(cex.sub=0.5)
plot(network, 
     cex.sub=0.01,
     edge.arrow.size=.3,
     vertex.size=10,
     vertex.color="#2171b5",
     vertex.label.color="black",
     vertex.label.cex=.5,
     vertex.label.dist=2,
     vertex.label.font=1,
     vertex.label.degree=-pi/2,
     layout=layout.circle, 
     main="Cambios de domicilio entre los distritos de València (2018)", 
     sub="Nota: Para cada uno de los distritos se representan los cuatro principales distritos de destino.\n\nFuente: Altas y bajas registradas en el Padrón Municipal de Habitantes.\n\nElaboración: Oficina d'Estadística. Ajuntament de València.")
dev.off()

