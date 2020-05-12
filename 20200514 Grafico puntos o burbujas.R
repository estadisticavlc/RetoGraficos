lista_de_paquetes <- c("curl","ggplot2", "grid") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/MicrodatosMarzo2020.csv"),sep=";",dec=",")
# Los valores No sabe/No contesta de las preguntas a representar se definen como faltantes 
datos$P5[datos$P5%in%c(98,99)]=NA
media_P5_DT=aggregate(P5~Districte,data=datos,FUN=mean)[,2]
datos$P6[datos$P6%in%c(98,99)]=NA
# Interesa hallar el porcentaje de 601 euros o más (valor 4 o superior)
datos$P6_rec=datos$P6
datos$P6_rec[datos$P6<=3]=0
datos$P6_rec[datos$P6>=4]=1
sum_P6_DT=aggregate(P6_rec~Districte,data=datos,FUN=sum)[,2]
porc_P6_DT=100*sum_P6_DT/as.numeric(table(datos$Districte))

datos_plot<-data.frame(porc_P6_DT, media_P5_DT)

# Dibujamos el gráfico de líneas
p=ggplot(data=datos_plot, aes(porc_P6_DT,media_P5_DT)) +  
   geom_point(colour="#4781b3", size=2) +
   theme_bw() +
   labs(title=NULL, 
        y="Satisfacción media (distrito) de vivir en su hogar (0-10)", 
        x="Porcentaje de personas (distrito) que pagan 601 euros o más de hipoteca o alquiler", 
        caption="Fuente: Barómetro de Opinión Ciudadana de Marzo de 2020. Elaboración: Oficina d'Estadística. Ajuntament de València")+
   theme(plot.caption = element_text(color = "black",face = "italic", size = 6),
         axis.title = element_text(size=8.5))
p
ggsave(filename = paste0("20200514 Grafico puntos o burbujas.png"), p,
       width = 5, height = 5, dpi = 300, units = "in", device='png')


