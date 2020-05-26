lista_de_paquetes <- c("curl","ggplot2", "grid", "dplyr","RColorBrewer","scales") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(dplyr)
library(RColorBrewer)
library(scales)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/Indicadores.csv"),sep=",",dec=",")
valores<-as.numeric(as.character(datos$VALOR[datos$codigo=="Ind17"]))
fechas=as.Date(paste0(as.character(datos$ANO[datos$codigo=="Ind17"]),"-",as.character(datos$MES[datos$codigo=="Ind17"]),"-15"))
datos_plot<-data.frame(name=fechas, value=valores)
datos_plot<-datos_plot[which(datos_plot$name<"2020-1-1"),]
datos_plot$name<-format(datos_plot$name, "%B")
datos_plot$name<-factor(datos_plot$name,levels = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"))


# Dibujamos el gráfico de violín

options(scipen=999)
p=datos_plot %>%
   ggplot( aes(x=name, y=value, fill=name)) +
   geom_violin(width=1.4, color="gray10") +
   geom_boxplot(width=0.1, color="gray10", alpha=0.2) +
   scale_fill_brewer(palette="Paired") +
   scale_y_continuous(labels = function(x) format(x, big.mark = ".",
                                                  scientific = FALSE))+
   labs(title="Distribución del número de pasajeros de Metrovalencia por mes. 1995-2019",
        x="Mes",
        y="Pasajeros",
        caption="Fuente: Metrovalencia. Elaboración: Oficina de Estadística. Ajuntament de València.")+
   theme(plot.title = element_text(size=14,hjust = 0.5,face="bold"),
         plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0.5),
         legend.position = "none")
theme_set(
   theme_bw()
)
p
ggsave(filename = paste0("20200605 Grafico de violin.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


