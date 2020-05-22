lista_de_paquetes <- c("curl","ggplot2", "grid", "plyr", "dplyr") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/RentaDistrito.csv"),sep="\t",dec=",")
colnames(datos)[colnames(datos)=="Total"]="Valor"
# Definimos nueva columna para el valor para la ciudad
datos$Ciudad=datos$Valor[1]
# Quitamos la fila correspondiente al dato para la ciudad
datos=datos[-1,]
# Reemplazamos por los nombres de los distritos
nombres_distrito=c("Ciutat Vella","l'Eixample","Extramurs","Campanar",
                   "la Saïdia","el Pla del Real","l'Olivereta","Patraix",
                   "Jesús","Quatre Carreres","Poblats Marítims",
                   "Camins al Grau","Algirós","Benimaclet",
                   "Rascanya","Benicalap","Pobles del Nord",
                   "Pobles de l'Oest","Pobles del Sud")
datos[,1]=nombres_distrito
# Definir una variable de dos niveles: por encima y por debajo de la media
datos$nivel=as.factor(as.numeric(datos$Valor>datos$Ciudad))
# Ordenar de mayor a menor renta
datos=datos[order(datos$Valor),]
datos$Distrito=factor(datos$Distrito,levels=datos$Distrito)

# Dibujamos el gráfico de paleta o lollipop
p=ggplot(datos, aes(x = Valor, y = Distrito, color = nivel)) +
         geom_segment(aes(x = Ciudad, y = Distrito, xend = Valor, yend = Distrito),show.legend = FALSE) +
         geom_point(show.legend = FALSE)+
         theme_bw()+
         labs(title="Renta media por persona en los distritos de València (2017)", 
         y=NULL, 
         x=NULL, 
         caption="Nota: La línea vertical representa la renta media por persona en la ciudad (12453 euros).\n\nFuente: Instituto Nacional de Estadística. Elaboración: Oficina d'Estadística. Ajuntament de València.")+
         scale_color_manual(values=c("#d73027","#4575b4"))+
         scale_x_continuous(breaks = seq(10000,20000,2000))+
         geom_vline(xintercept = datos$Ciudad[1], col="gray70")
p
ggsave(filename = paste0("20200523 Grafico de paleta o lollipop.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


