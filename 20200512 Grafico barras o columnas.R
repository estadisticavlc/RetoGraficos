lista_de_paquetes <- c("curl","ggplot2","grid") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/MicrodatosMarzo2020.csv"),sep=";",dec=",")

# Dibujamos el gráfico de barras/columnas
p=ggplot(data=datos, aes(x=P3)) +  
   geom_bar(aes(y = 100*(..count..)/sum(..count..)),fill="#4781b3") +
   theme_bw() +
   labs(title="¿Cuántas personas viven en su casa contándose a usted?", 
        y="Porcentaje", 
        x="Personas", 
        caption="Fuente: Barómetro de Opinión Ciudadana de Marzo de 2020. Elaboración: Oficina d'Estadística. Ajuntament de València")+
   theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
         plot.caption = element_text(color = "black",face = "italic", size = 6)) +
   scale_x_continuous(breaks=c(0:10))
p
ggsave(filename = paste0("20200512 Grafico barras o columnas.png"), p,
       width = 5, height = 5, dpi = 300, units = "in", device='png')


