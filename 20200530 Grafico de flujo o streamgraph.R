lista_de_paquetes <- c("curl","ggplot2", "ggTimeSeries", "viridis","grid", "plyr", "dplyr") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(ggTimeSeries)
library(viridis)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/ContaminantesPolitecnica.csv"),sep=";",dec=",")

# Crear dataframe para hacer el grafico
datos.plot=c()
for (i in 1:12){
   for (j in c(1:5)){
      datos.plot=rbind(datos.plot,c(i,colnames(datos)[j],datos[i,j]))           
   }
}
datos.plot=as.data.frame(datos.plot)
colnames(datos.plot)=c("Mes","Contaminante","Valor")
datos.plot$Contaminante=factor(datos.plot$Contaminante,levels=colnames(datos))
datos.plot$Mes=as.numeric(as.character(datos.plot$Mes))
datos.plot$Valor=as.numeric(as.character(datos.plot$Valor))

time <- as.numeric(rep(seq(1,7),each=7))  # Eje X de abcisas
value <- runif(49, 10, 100)               # Eje Y de ordenadas
group <- rep(LETTERS[1:7],times=7)        # Un shape por grupo del eje X
data <- data.frame(time, value, group)


# Dibujamos el gráfico de flujo o streamgraph

p=ggplot(datos.plot,
         aes(x = Mes,
             y = Valor,
             group = Contaminante,
             fill = Contaminante)) +
        scale_fill_viridis(discrete = T, name=NULL, labels=colnames(datos),option = "D") +
        scale_x_continuous(breaks = 1:12,labels=c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                                                "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))+
        scale_y_continuous(breaks = c(-40,0,40),labels = c(40,80,120))+
        stat_steamgraph() +
        theme_bw()+
        labs(title="Evolución mensual de los principales contaminantes atmosféricos en València (2018)", 
             y="Microgramos por metro cúbico de aire", 
             x=NULL, 
             caption="Nota: Los datos corresponden a la estación atmosférica situada en la Universidad Politécnica.\n\nFuente: Conselleria de Medio Ambiente. Generalitat Valenciana. Elaboración: Oficina d'Estadística. Ajuntament de València.")+
        theme(plot.title = element_text(size=10,hjust = 0.5,face="bold"),
              plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0))
p
ggsave(filename = paste0("20200530 Grafico de flujo o streamgraph.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


