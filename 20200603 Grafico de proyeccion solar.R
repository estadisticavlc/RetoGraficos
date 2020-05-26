lista_de_paquetes <- c("ggplot2", "data.table") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias

library(ggplot2)
library(data.table)

# Leemos los datos
df <- data.frame(
        'level1'=c('Preescolar', 'Preescolar', 'Preescolar', 'Preescolar', 'Primaria', 'Primaria', 'Primaria', 'Primaria', 'Primaria', 'Primaria', 'ESO', 'ESO', 'ESO', 'ESO', 'Bachillerato', 'Bachillerato','Ciclo Formativo', 'Ciclo Formativo','Ciclo Formativo', 'Ciclo Formativo'), 
        'level2'=c('0-2 años', '3 años', '4 años', '5 años', '1.º Primaria', '2.º Primaria', '3.º Primaria', '4.º Primaria', '5.º Primaria', '6.º Primaria', '1.º ESO', '2.º ESO', '3.º ESO', '4.º ESO', '1.º Bachillerato', '2.º Bachillerato', '1.º Grado Medio', '2.º Grado Medio', '1.º Grado Superior', '2.º Grado Superior'), 
        'value'=c(9288,5527,5850,5838,6587,6751,6767,7037,7192,7273,7797,7658,6944,6652,5512,5528,4835,3848,6863,6521)/sum(c(9288,5527,5850,5838,6587,6751,6767,7037,7192,7273,7797,7658,6944,6652,5512,5528,4835,3848,6863,6521)))
df$level1<-factor(df$level1,levels = c('Preescolar', 'Primaria', 'ESO', 'Bachillerato', 'Ciclo Formativo'))
df$level2<-factor(df$level2,levels=c('0-2 años', '3 años', '4 años', '5 años', '1.º Primaria', '2.º Primaria', '3.º Primaria', '4.º Primaria', '5.º Primaria', '6.º Primaria', '1.º ESO', '2.º ESO', '3.º ESO', '4.º ESO','1.º Bachillerato', '2.º Bachillerato', '1.º Grado Medio', '2.º Grado Medio', '1.º Grado Superior', '2.º Grado Superior'))
colors<-c("#006d2c", "#990000", "#045a8d", "#dd1c77", "#fc4e2a",
          "#ccece6",  "#99d8c9",  "#66c2a4", "#2ca25f",
          "#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#e34a33", "#b30000",
          "#d0d1e6","#a6bddb","#74a9cf","#2b8cbe",
          "#c994c7", "#df65b0",
          "#ffffb2", "#fed976", "#feb24c", "#fd8d3c")
colorNames <- c(unique(as.character(df$level1)), unique(as.character(df$level2)))
names(colors) <- colorNames

# Dibujamos el gráfico de proyección solar
p=ggplot(df, aes(y=value)) +
        geom_bar(aes(fill=level1, x=0), width=.5, stat='identity') + 
        geom_bar(aes(fill=level2, x=.25), width=.25, stat='identity') + 
        coord_polar(theta='y') + 
        theme(legend.position = "bottom", legend.title.align=0.5) +
        scale_fill_manual(name="Curso \n escolar",values=colors, breaks = c('Preescolar','0-2 años', '3 años', '4 años', '5 años', 'Primaria','1.º Primaria', '2.º Primaria', '3.º Primaria', '4.º Primaria', '5.º Primaria', '6.º Primaria', 'ESO','1.º ESO', '2.º ESO', '3.º ESO', '4.º ESO', 'Bachillerato','1.º Bachillerato', '2.º Bachillerato', 'Ciclo Formativo','1.º Grado Medio', '2.º Grado Medio', '1.º Grado Superior', '2.º Grado Superior')) +
        labs(title="Alumnado según curso escolar. 2018/2019",  
             caption="Fuente: Conselleria de Educación, Investigación, Cultura y Deporte. Elaboración: Oficina de Estadística. Ajuntament de València.")+
        theme(plot.title = element_text(size=14,hjust = 0.5,face="bold"),
              plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0.5))
theme_set(
        theme_void() 
)
p
ggsave(filename = paste0("20200603 Grafico de proyeccion solar.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')


