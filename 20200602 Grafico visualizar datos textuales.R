lista_de_paquetes <- c("curl","tm", "SnowballC", "wordcloud", "RColorBrewer") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

datos=scan(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/NubePalabras.txt"),
           character(0),encoding = "UTF-8")
datos=gsub("_"," ",datos)

# Datos para el plot: palabras - frecuencias
datos.plot=data.frame(palabras=sort(unique(datos)),freq=as.numeric(table(datos)))

# Dibujamos el gráfico de visualizar datos textuales

png("20200602 Grafico visualizar datos textuales.png", width = 9, height = 5, units = 'in', res = 300)
set.seed(1234)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0.5, 4))
plot.new()
text(x=0.5, y=0.5, "¿Qué es lo que más le gusta de su distrito?", cex = 2)
wordcloud(words = datos.plot$palabras, freq = datos.plot$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"),scale = c(1.8,0.8))
text(x=0.5, y=0, "Fuente: Barómetro de Opinión Ciudadana de Diciembre de 2020. Elaboración: Oficina d'Estadística. Ajuntament de València.", cex = 0.5)

dev.off()



