lista_de_paquetes <- c("curl","ggplot2", "grid", "plyr", "dplyr", "metR") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(metR)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/AreesVulnerables2019.csv"),sep=";",dec=",")

datos.loess <- loess(IAE ~ renda_mitjana * Var_poblacio_2015_2019, data = datos)

# Secuencia de valores para renta y variacion de poblacion. Se excluyen valores extremos

seq_renta=seq(quantile(datos$renda_mitjana,0.025),quantile(datos$renda_mitjana,0.975),100)
seq_var=seq(quantile(datos$Var_poblacio_2015_2019,0.025),quantile(datos$Var_poblacio_2015_2019,0.975),0.25)

datos.fit <-  expand.grid(renda_mitjana = seq_renta, 
                          Var_poblacio_2015_2019 = seq_var)
datos.pred <-  predict(datos.loess, newdata = datos.fit)

datos.plot=c()
for (j in 1:length(seq_var)){
   for (i in 1:length(seq_renta)){
        datos.plot=rbind(datos.plot,c(seq_renta[i],seq_var[j]))
   }
}
datos.plot=data.frame(x=datos.plot[,1],y=datos.plot[,2])
datos.plot$z=as.numeric(datos.pred)

# Dibujamos el mapa de contorno

p=ggplot(datos.plot,aes(x, y, z=z)) +
   geom_contour_fill()+
   theme_bw()+
   labs(title="Relación entre la tasa de actividad económica, la renta media y la variación de la población entre 2015 y 2019 en València", 
        y="Variación de la población en el periodo 2015-2019", 
        x="Renta media por persona (euros)", 
        caption="Nota: Datos a nivel de sección censal. La tasa de actividad económica se calcula como el número de actividades económicas por cada 1000 habitantes.\n\nFuente: Àrees Vulnerables a la ciutat de València 2019 (Oficina d'Estadistica de l'Ajuntament de València, 2020). Elaboración: Oficina d'Estadística. Ajuntament de València.")+
   scale_x_continuous(breaks = seq(8000,24000,2000), expand=c(0,0))+
   scale_y_continuous(breaks = seq(-5,10,2.5), expand=c(0,0))+
   theme(plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0),
         title = element_text(size=8),
         axis.title = element_text(size=8),
         legend.title=element_text(size=10))+
   scale_fill_viridis(name="Tasa")
p
ggsave(filename = paste0("20200519 Grafico mapa contorno.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


