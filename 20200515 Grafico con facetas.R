lista_de_paquetes <- c("curl","ggplot2", "grid") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)


# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/MicrodatosMarzo2020.csv"),sep=";",dec=",")
nombres_distrito=c("Ciutat Vella","l'Eixample","Extramurs","Campanar",
                   "la Saïdia","el Pla del Real","l'Olivereta","Patraix",
                   "Jesús","Quatre Carreres","Poblats Marítims",
                   "Camins al Grau","Algirós","Benimaclet",
                   "Rascanya","Benicalap","Pobles del Nord",
                   "Pobles de l'Oest","Pobles del Sud")
datos$Districte=nombres_distrito[datos$Districte]
datos$Districte=factor(datos$Districte,levels=nombres_distrito)
datos$PES=as.numeric(as.character(datos$PES))
pesos_dt<-aggregate(PES ~ Districte, data = datos, sum)
pesos<-aggregate(PES ~ P3+Districte, data = datos, sum)
datos_plot<-merge(pesos,pesos_dt,by = "Districte")
datos_plot$Porcentaje<-100*datos_plot[,3]/datos_plot[,4]

# Dibujamos el gráfico con facetas
p=ggplot(data=datos_plot, aes(x=P3, y=Porcentaje)) +  
   geom_bar(stat = "identity",fill="#4781b3") +
   scale_x_continuous(breaks=c(0:10)) +
   facet_wrap(~ Districte, nrow = 4) +
   theme_bw() +
   labs(title="¿Cuántas personas viven en su casa contándose a usted?", 
        y="Porcentaje", 
        x="Personas", 
        caption="Fuente: Barómetro de Opinión Ciudadana de Marzo de 2020. Elaboración: Oficina d'Estadística. Ajuntament de València.")+
   theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
         plot.caption = element_text(color = "black",face = "italic", size = 6, hjust = 0)) 

p
ggsave(filename = paste0("20200515 Grafico con facetas.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


