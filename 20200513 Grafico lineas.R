lista_de_paquetes <- c("curl","ggplot2", "grid") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/Indicadores.csv"),sep=",",dec=",",encoding = "UTF-8")
valores<-as.numeric(as.character(datos$VALOR[datos$codigo=="Ind01"]))
fechas=as.Date(paste0(as.character(datos$ANO[datos$codigo=="Ind01"]),"-",as.character(datos$MES[datos$codigo=="Ind01"]),"-15"))
datos_plot<-data.frame(fechas, valores)

# Dibujamos el gráfico de líneas
p=ggplot(data=datos_plot, aes(fechas,valores)) +  
   geom_line(size=3, colour="#4781b3") +
   theme_bw() +
   labs(title="Evolución del paro registrado en la ciudad de València (desde enero de 2006)", 
        y="Número de personas", 
        x=NULL, 
        caption="Fuente: Servicio Valenciano de Ocupación y Formación (LABORA). Elaboración: Oficina d'Estadística. Ajuntament de València.")+
   theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
         plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0))+
   scale_x_date(breaks = as.Date(paste0(2006:2020,"-01-01")),date_labels = "%Y")
p
ggsave(filename = paste0("20200513 Grafico lineas.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


