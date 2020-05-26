lista_de_paquetes <- c("curl","ggplot2", "grid") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/Indicadores.csv"),sep=",",dec=",")
valores<-as.numeric(as.character(datos$VALOR[datos$codigo=="Ind29"]))
fechas=as.Date(paste0(as.character(datos$ANO[datos$codigo=="Ind29"]),"-",as.character(datos$MES[datos$codigo=="Ind29"]),"-15"))
datos_plot<-data.frame(fechas, valores)

# Dibujamos el gráfico de líneas

p=ggplot(data=datos_plot, aes(fechas,valores)) +
   geom_area(fill="deepskyblue3", alpha=0.5) + 
   geom_line(colour="deepskyblue3") +
   theme_bw() +
   labs(title="Serie histórica de precipitaciones mensuales en la ciudad de València. 1938-2020", 
        y="l/m2", 
        x="", 
        caption="Fuente: Agencia Estatal de Meteorología. Elaboración: Oficina d'Estadística. Ajuntament de València.")+
   theme(plot.title = element_text(size=12,hjust = 0.5,face="bold"),
         plot.caption = element_text(color = "black",face = "italic", size = 8, hjust=0.5))+
   annotate("text", 
            x = c(as.Date("1952-1-1"),as.Date("1945-1-1"),as.Date("1980-1-1"),as.Date("1990-1-1"), as.Date("2016-1-1")), 
            y = c(365, 275, 210, 350, 300), 
            label = c("10/1965 ostenta el récord (365,6 l/m2)", 
                      "13 de los 20 meses más \n lluviosos corresponden \n a octubre. Entre ellos el de \n 1957, año de la gran riada \n (306,5 l/m2)",
                      "1978 fue el \n año más seco \n (183,3 l/m2)",
                      "La precipitación máxima en un año \n se alcanzó en 1989 con 976,6 l/m2",
                      "En lo que va de siglo, \n octubre de 2007 ha sido \n el mes con más lluvia \n (336,9 l/m2)"), 
            hjust=0.5, 
            fontface = 'italic',
            colour="deepskyblue4",
            size=3.5)
p
ggsave(filename = paste0("20200601 Grafico con anotaciones.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


