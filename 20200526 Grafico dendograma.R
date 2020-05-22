lista_de_paquetes <- c("curl", "dendextend ", "tidyverse", "plyr", "dplyr") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(plyr)
library(dplyr)
library(dendextend)
library(tidyverse)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/AreesVulnerables2019.csv"),sep=";",dec=",")
colnames(datos)[1]="DT"
# Agregar datos por distrito y filtramos indices vulnerabilidad
datos=aggregate(.~DT,data=datos,FUN=mean)
datos=datos[,c("Index_Equipamiento","Index_Demografia","Index_Socioeconomia")]
datos$Distrito=c("Ciutat Vella","l'Eixample","Extramurs","Campanar",
                 "la Saïdia","el Pla del Real","l'Olivereta","Patraix",
                 "Jesús","Quatre Carreres","Poblats Marítims",
                 "Camins al Grau","Algirós","Benimaclet",
                 "Rascanya","Benicalap","Pobles del Nord",
                 "Pobles de l'Oest","Pobles del Sud")
datos$Distrito=factor(datos$Distrito,levels=)

datos %>% 
  select(Index_Equipamiento, Index_Demografia, Index_Socioeconomia) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend

# Dibujamos el dendograma
png("20200526 Grafico dendograma.png", width = 9, height = 5, units = 'in', res = 300)
dend<-dend %>%set("labels_col", value = c("#1b9e77", "#7570b3", "#e7298a"), k=3) %>%
  set("branches_k_color", value = c("#1b9e77", "#7570b3", "#e7298a"), k = 3) %>% 
  set("labels", datos$Distrito[c(4,13,8,14,3,1,2,6,5,12,16,9,10,7,11,15,18,17,19)]) %>% # El orden se obtiene a simple vista al representar el grafico sin labels 
  set("labels_cex", 0.61)
plot(dend, axes=FALSE, main="Clasificación de los distritos de València según su nivel de vulnerabilidad", 
     sub="Nota: Para la obtención de la clusterización jerárquica se tienen en cuenta tres dimensiones de la vulnerabilidad estimadas a nivel de sección censal: equipamiento, demografía y socioeconomía.\n\nFuente: Àrees Vulnerables a la ciutat de València 2019 (Oficina d'Estadistica de l'Ajuntament de València, 2020). Elaboración: Oficina d'Estadística. Ajuntament de València.", 
     font.sub=1,
     cex.sub=0.4)
dev.off()





