lista_de_paquetes <- c("curl","treemap", "plyr", "dplyr", "RColorBrewer", "treemapify") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(plyr)
library(dplyr)
library(treemap)
library(RColorBrewer)
library(treemapify)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/AreesVulnerables2019.csv"),sep=";",dec=",")
agregar_datos=aggregate(Zones.verdes~X.U.FEFF.DT,data=datos,FUN=sum)
colnames(agregar_datos)[1]="Distrito"
poblacion=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/Poblacion2019.csv"),sep=";",dec=",")
agregar_poblacion=aggregate(Total~Distrito,data=poblacion,FUN=sum)
agregar_datos$Tasa=1000*agregar_datos$Zones.verdes/agregar_poblacion$Total
agregar_datos$Nombre_distrito=c("Ciutat Vella","l'Eixample","Extramurs","Campanar",
                   "la Saïdia","el Pla del Real","l'Olivereta","Patraix",
                   "Jesús","Quatre Carreres","Poblats Marítims",
                   "Camins al Grau","Algirós","Benimaclet",
                   "Rascanya","Benicalap","Pobles del Nord",
                   "Pobles de l'Oest","Pobles del Sud")
agregar_datos$Nombre_distrito=factor(agregar_datos$Nombre_distrito,levels=c("Ciutat Vella","l'Eixample","Extramurs","Campanar",
                                                                            "la Saïdia","el Pla del Real","l'Olivereta","Patraix",
                                                                            "Jesús","Quatre Carreres","Poblats Marítims",
                                                                            "Camins al Grau","Algirós","Benimaclet",
                                                                            "Rascanya","Benicalap","Pobles del Nord",
                                                                            "Pobles de l'Oest","Pobles del Sud"))

# Dibujamos el gráfico de rectángulos o treemap

png("20200525 Grafico de rectangulos o treemap.png", width = 9, height = 5, units = 'in', res = 300)
ggplot(agregar_datos, aes(area = Tasa, fill = Tasa, label = Nombre_distrito)) +
        scale_fill_gradient2(low = "#addd8e",high = "#238443")+
        geom_treemap(color="black",size=2) +
        geom_treemap_text(reflow = T, colour = "white") +
        theme(legend.position = "bottom") +
        labs(title = "Número de árboles por distrito en València (2018)",
             caption = "Fuente: Àrees Vulnerables a la ciutat de València 2019 (Oficina d'Estadistica de l'Ajuntament de València, 2020) / Servici de Jardineria. Ajuntament de València.\n\nElaboración: Oficina d'Estadística. Ajuntament de València.",
             fill = "Tasa (por 1000 habitantes)"
             )
dev.off()