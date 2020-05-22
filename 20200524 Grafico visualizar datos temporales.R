lista_de_paquetes <- c("curl","ggplot2", "grid") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/Indicadores.csv"),sep=",",dec=",")
datos$VALOR<-as.numeric(as.character(datos$VALOR))
datos$FECHA=as.Date(paste0(as.character(datos$ANO),"-",as.character(datos$MES),"-15"))
# Seleccionamos periodo temporal e indicadores
datos=datos[datos$ANO>=2008,]
datos=datos[datos$codigo%in%c("Ind16","Ind17"),]
datos$INDICADOR=""
datos$INDICADOR[datos$codigo=="Ind16"]="Autobuses EMT"
datos$INDICADOR[datos$codigo=="Ind17"]="Metrovalencia"

# Dibujamos las series temporales
options(scipen=5)
p=ggplot(data=datos, aes(FECHA,VALOR,col=codigo)) +
   geom_line(size=1.5,aes(color=INDICADOR)) + 
   theme_bw() +
   labs(title="Evolución mensual de los pasajeros de EMT y Metrovalencia desde 2008", 
        y="Pasajeros", 
        x=NULL, 
        caption="Fuente: Empresa Municipal de Transportes de València / Metrovalencia.\n\nElaboración: Oficina d'Estadística. Ajuntament de València.")+
   theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
         plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0),
         legend.title = element_blank())+
   scale_x_date(breaks = as.Date(paste0(2008:2020,"-01-01")),date_labels = "%Y")+
   scale_color_manual(values=c("#df4444","#dfc944"))
p
ggsave(filename = paste0("20200524 Grafico visualizar datos temporales.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


