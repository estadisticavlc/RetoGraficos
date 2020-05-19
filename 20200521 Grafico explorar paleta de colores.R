lista_de_paquetes <- c("curl","ggplot2", "wesanderson","grid", "plyr", "dplyr") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(wesanderson)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/Indicadores.csv"),sep=",",dec=",")
datos$VALOR<-as.numeric(as.character(datos$VALOR))
datos$FECHA=as.Date(paste0(as.character(datos$ANO),"-",as.character(datos$MES),"-15"))
# Seleccionamos periodo temporal e indicadores
datos=datos[datos$ANO>=2008,]
datos=datos[datos$codigo%in%c("Ind01","Ind07","Ind11","Ind13"),]
# Identificar cada codigo con el nombre del indicador
datos$INDICADOR=""
datos$INDICADOR[datos$codigo=="Ind01"]="Personas registradas en el paro"
datos$INDICADOR[datos$codigo=="Ind07"]="Personas afiliadas a la Seguridad Social como autónomas"
datos$INDICADOR[datos$codigo=="Ind11"]="Pernoctaciones en establecimientos hoteleros"
datos$INDICADOR[datos$codigo=="Ind13"]="Matriculaciones de turismos"

# Dibujamos el gráfico. Se usa una paleta del paquete wesanderson
paleta <- wes_palette(name = "GrandBudapest2", type = "continuous")
options(scipen=5)
p=ggplot(data=datos, aes(FECHA,VALOR)) +  
        geom_line(size=1.5,aes(color=INDICADOR),show.legend = FALSE) +
        theme_bw() +
        labs(title="Evolución de algunos indicadores socioeconómicos en València (desde enero de 2008)", 
             y=NULL, 
             x=NULL, 
             caption="Fuentes: Dirección General de Tráfico. Ministerio del Interior / Encuesta de Ocupación Hotelera (INE) / Ministerio de Inclusión, Seguridad Social y Migraciones / Servicio Valenciano de Ocupación y Formación (LABORA).\n\nElaboración: Oficina d'Estadística. Ajuntament de València.")+
        theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
              plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0),
              axis.text.x = element_text(size=7))+
        facet_wrap(~INDICADOR, scales = "free") +
        scale_x_date(breaks = as.Date(paste0(2006:2020,"-01-01")),date_labels = "%Y")+
        scale_color_manual(values=paleta)
p
ggsave(filename = paste0("20200521 Grafico explorar paleta de colores.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


