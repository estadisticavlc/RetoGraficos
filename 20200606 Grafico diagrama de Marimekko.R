lista_de_paquetes <- c("curl", "plyr", "reshape","ggplot2", "RColorBrewer") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(plyr)
library(reshape)
library(ggplot2)
library(RColorBrewer)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/MicrodatosMayo2020.csv"),sep=";",dec=",")
datos=datos[datos$P19_CAP==0 & datos$P19_NO_SAP==0,]

df <- data.frame(segment = c("Confinamiento", 
                             "Falta de vida social",
                             "Falta de libertad y rutinas",
                             "Trabajo",
                             "Miedo a la enfermedad",
                             "Efectos psicológicos",
                             "Mejora relaciones familiares",
                             "Problemas de conciliación",
                             "Más tiempo libre",
                             "Cambios en las relaciones personales",
                             "Efectos sociopolíticos"), 
                 segpct = c(100*length(which(datos$P19_CONFINAMENT==1))/nrow(datos), 
                            100*length(which(datos$P19_FALTA_VIDA_SOCIAL==1))/nrow(datos), 
                            100*length(which(datos$P19_FALTA_LLIBERTAT_RUTINES==1))/nrow(datos), 
                            100*length(which(datos$P19_TREBALL==1))/nrow(datos),
                            100*length(which(datos$P19_POR_MALALTIA==1))/nrow(datos), 
                            100*length(which(datos$P19_EFECTES_PSICOLOGICS==1))/nrow(datos), 
                            100*length(which(datos$P19_MILLORA_REL_FAMILIARS==1))/nrow(datos), 
                            100*length(which(datos$P19_PROBLEMES_CONCILIAR==1))/nrow(datos),
                            100*length(which(datos$P19_MES_TEMPS==1))/nrow(datos), 
                            100*length(which(datos$P19_CANVIS_RELACIONS==1))/nrow(datos), 
                            100*length(which(datos$P19_EFECTES_SOCIOPOLITICS==1))/nrow(datos)), 
                 Menores40 = c(100*length(which(datos$P19_CONFINAMENT==1 & datos$Edat3==1))/length(which(datos$Edat3==1)), 
                               100*length(which(datos$P19_FALTA_VIDA_SOCIAL==1 & datos$Edat3==1))/length(which(datos$Edat3==1)), 
                               100*length(which(datos$P19_FALTA_LLIBERTAT_RUTINES==1 & datos$Edat3==1))/length(which(datos$Edat3==1)), 
                               100*length(which(datos$P19_TREBALL==1 & datos$Edat3==1))/length(which(datos$Edat3==1)),
                               100*length(which(datos$P19_POR_MALALTIA==1 & datos$Edat3==1))/length(which(datos$Edat3==1)), 
                               100*length(which(datos$P19_EFECTES_PSICOLOGICS==1 & datos$Edat3==1))/length(which(datos$Edat3==1)), 
                               100*length(which(datos$P19_MILLORA_REL_FAMILIARS==1 & datos$Edat3==1))/length(which(datos$Edat3==1)), 
                               100*length(which(datos$P19_PROBLEMES_CONCILIAR==1 & datos$Edat3==1))/length(which(datos$Edat3==1)),
                               100*length(which(datos$P19_MES_TEMPS==1 & datos$Edat3==1))/length(which(datos$Edat3==1)), 
                               100*length(which(datos$P19_CANVIS_RELACIONS==1 & datos$Edat3==1))/length(which(datos$Edat3==1)), 
                               100*length(which(datos$P19_EFECTES_SOCIOPOLITICS==1 & datos$Edat3==1))/length(which(datos$Edat3==1))), 
                 De40a64 = c(100*length(which(datos$P19_CONFINAMENT==1 & datos$Edat3==2))/length(which(datos$Edat3==2)), 
                             100*length(which(datos$P19_FALTA_VIDA_SOCIAL==1 & datos$Edat3==2))/length(which(datos$Edat3==2)), 
                             100*length(which(datos$P19_FALTA_LLIBERTAT_RUTINES==1 & datos$Edat3==2))/length(which(datos$Edat3==2)), 
                             100*length(which(datos$P19_TREBALL==1 & datos$Edat3==2))/length(which(datos$Edat3==2)),
                             100*length(which(datos$P19_POR_MALALTIA==1 & datos$Edat3==2))/length(which(datos$Edat3==2)), 
                             100*length(which(datos$P19_EFECTES_PSICOLOGICS==1 & datos$Edat3==2))/length(which(datos$Edat3==2)), 
                             100*length(which(datos$P19_MILLORA_REL_FAMILIARS==1 & datos$Edat3==2))/length(which(datos$Edat3==2)), 
                             100*length(which(datos$P19_PROBLEMES_CONCILIAR==1 & datos$Edat3==2))/length(which(datos$Edat3==2)),
                             100*length(which(datos$P19_MES_TEMPS==1 & datos$Edat3==2))/length(which(datos$Edat3==2)), 
                             100*length(which(datos$P19_CANVIS_RELACIONS==1 & datos$Edat3==2))/length(which(datos$Edat3==2)), 
                             100*length(which(datos$P19_EFECTES_SOCIOPOLITICS==1 & datos$Edat3==2))/length(which(datos$Edat3==2))),
                 Mayores64 = c(100*length(which(datos$P19_CONFINAMENT==1 & datos$Edat3==3))/length(which(datos$Edat3==3)), 
                               100*length(which(datos$P19_FALTA_VIDA_SOCIAL==1 & datos$Edat3==3))/length(which(datos$Edat3==3)), 
                               100*length(which(datos$P19_FALTA_LLIBERTAT_RUTINES==1 & datos$Edat3==3))/length(which(datos$Edat3==3)), 
                               100*length(which(datos$P19_TREBALL==1 & datos$Edat3==3))/length(which(datos$Edat3==3)),
                               100*length(which(datos$P19_POR_MALALTIA==1 & datos$Edat3==3))/length(which(datos$Edat3==3)), 
                               100*length(which(datos$P19_EFECTES_PSICOLOGICS==1 & datos$Edat3==3))/length(which(datos$Edat3==3)), 
                               100*length(which(datos$P19_MILLORA_REL_FAMILIARS==1 & datos$Edat3==3))/length(which(datos$Edat3==3)), 
                               100*length(which(datos$P19_PROBLEMES_CONCILIAR==1 & datos$Edat3==3))/length(which(datos$Edat3==3)),
                               100*length(which(datos$P19_MES_TEMPS==1 & datos$Edat3==3))/length(which(datos$Edat3==3)), 
                               100*length(which(datos$P19_CANVIS_RELACIONS==1 & datos$Edat3==3))/length(which(datos$Edat3==3)), 
                               100*length(which(datos$P19_EFECTES_SOCIOPOLITICS==1 & datos$Edat3==3))/length(which(datos$Edat3==3))))

# Filtrar
df=df[df$Menores40>2,]

# Ordenar
df=df[c(2,1,7,3,4,6,5),]

# Nombres columnas
colnames(df)[colnames(df)=="Menores40"]="Menores de 40"
colnames(df)[colnames(df)=="De40a64"]="De 40 a 64"
colnames(df)[colnames(df)=="Mayores64"]="Mayores de 64"

# Posiciones xmin, xmax
df$xmax <- cumsum(df$segpct)
df$xmin <- df$xmax - df$segpct
df$segpct <- NULL

dfm <- melt(df, id = c("segment", "xmin", "xmax"))
dfm1 <- ddply(dfm, .(segment), transform, ymax = cumsum(value))
dfm1 <- ddply(dfm1, .(segment), transform,ymin = ymax - value)
dfm1$xtext <- with(dfm1, xmin + (xmax - xmin)/2)
dfm1$ytext <- with(dfm1, ymin + (ymax - ymin)/2)

p=ggplot(dfm1, aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = variable))+
        geom_rect(colour = I("grey")) +
        scale_fill_brewer(palette = "Dark2", name="Edad") + 
        geom_text(aes(x = xtext, y = ytext, label = paste(round(value,1),"%", sep = "")), size = 3.5) +
        # geom_text(aes(x = xtext, y = 100,label = segment), size = 3) +
        scale_x_continuous(expand=c(0,0),labels=as.character(df$segment),breaks=df$xmin+(df$xmax-df$xmin)/2)+
        scale_y_continuous(expand=c(0,0),limits = c(0,100))+
        labs(title="¿Cuál es el mayor impacto que está teniendo la crisis del COVID-19 en su vida cotidiana?",
             x="",
             y="",
             caption="Nota: Solo se muestran las respuestas más frecuentes. Cada porcentaje representa el porcentaje de personas del grupo de edad correspondiente que escogió la respuesta.\n\nFuente: Barómetro de Opinión Ciudadana de Mayo de 2020. Elaboración: Oficina d'Estadística. Ajuntament de València.")+
        theme(plot.title = element_text(size=13,hjust = 0.5,face="bold"),
              plot.caption = element_text(color = "black",face = "italic", size = 6, hjust=0.5),
              legend.position = "right",
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_text(color="black", angle=30, hjust = 1))
p
ggsave(filename = paste0("20200606 Grafico diagrama de Marimekko.png"), p,
       width = 9, height = 5, dpi = 300, units = "in", device='png')



