lista_de_paquetes <- c("curl","ggplot2", "ggalluvial","grid", "plyr", "dplyr") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(ggalluvial)
library(grid)
library(plyr)
library(dplyr)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/CambiosDomicilio_DT_2018.csv"),sep=";",dec=",")
nombres_distritos=datos$Distrito.origen
datos=datos[,-1]
# Se seleccionan distritos del 1 al 16
datos=datos[1:16,1:16]

# Crear datos plot
datos.plot=c()
for (i in 1:16){
   for (j in 1:16){
      datos.plot=rbind(datos.plot,c(i,j,datos[i,j]))           
   }
}
datos.plot=as.data.frame(datos.plot)
colnames(datos.plot)=c("DTO","DTD","Valor")
datos.plot$DTO=nombres_distritos[datos.plot$DTO]
datos.plot$DTD=nombres_distritos[datos.plot$DTD]
datos.plot$DTO=factor(datos.plot$DTO,levels=nombres_distritos)
datos.plot$DTD=factor(datos.plot$DTD,levels=nombres_distritos)

# Dibujamos el gráfico diagrama de sankey
p=ggplot(datos.plot,
       aes(y = Valor, axis1 = DTO, axis2 = DTD)) +
        geom_alluvium(width = 1/12) +
        geom_stratum(width = 1/12, fill="white", color = "grey") +
        geom_text(stat = "stratum", label.strata = TRUE, size=1.8,color="black") +
        scale_fill_brewer(palette = "PuRd")+
        theme_bw() +
        scale_x_discrete(limits = c("Origen", "Destino"), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        ggtitle("Cambios de domicilio entre los distritos de València (2018)")+
        labs(y=NULL,
             caption="Nota: Se han excluido los distritos Pobles del Nord, Pobles de l'Oest y Pobles del Sud por haberse registrado en ellos un número de cambios de domicilio notablemente inferior al del resto de distritos.\n\nFuente: Altas y bajas registradas en el Padrón Municipal de Habitantes. Elaboración: Oficina d'Estadística. Ajuntament de València.") +
        theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
              plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0),
              axis.text.y = element_blank(),
              legend.text = element_text(size=1),
              axis.ticks.length.y = unit(0, "cm"))
p
ggsave(filename = paste0("20200528 Grafico diagrama de sankey.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')