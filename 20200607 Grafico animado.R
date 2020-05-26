lista_de_paquetes <- c("curl", "gganimate","ggplot2","magick") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(gganimate)
library(ggplot2)
library(magick)

theme_set(theme_bw())

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/Indicadores.csv"),sep=",",dec=",")
valores<-as.numeric(as.character(datos$VALOR[datos$codigo=="Ind11"]))
fechas<-as.Date(paste0(as.character(datos$ANO[datos$codigo=="Ind11"]),"-",as.character(datos$MES[datos$codigo=="Ind11"]),"-15"))
datos_plot<-data.frame(fechas, valores)
datos_plot<-datos_plot[which(datos_plot$fechas<"2020-1-1"),]
datos_plot$anyo<-as.numeric(format(datos_plot$fecha, "%Y"))
datos_plot$name<-factor(datos_plot$name,levels = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"))

# Dibujamos el gráfico animado
options(scipen=999)
p <- ggplot(datos_plot, aes(x = fechas, y=valores)) +
        geom_line(show.legend = FALSE, alpha = 0.8, colour="steelblue", size=1) +
        geom_point()  +
        scale_x_date(date_labels = "%m/%Y") +
        labs(title="Pernoctaciones en establecimientos hoteleros. 1999-2019",
             x="",
             y="",
             caption="Fuente: Encuesta de Ocupación Hotelera (INE). Elaboración: Oficina de Estadística. Ajuntament de València.")+
        theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),
              axis.text=element_text(size=16),
              axis.title=element_text(size=18,face="bold"),
              plot.caption = element_text(color = "black",face = "italic", size = 12, hjust=0.5),
              legend.position = "none") + transition_reveal(fechas) 
animate(p, width = 900, height = 500)
anim_save("20200607 Grafico animado.gif")
