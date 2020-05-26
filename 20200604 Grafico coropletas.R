lista_de_paquetes <- c("curl","ggplot2", "grid", "leaflet", "htmlwidgets", "htmltools", "sp", "RColorBrewer") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos

# Cargamos las librerías necesarias
library(curl)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(sp)
library(RColorBrewer)

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/AreesVulnerables2019.csv"),sep=";",dec=",")
url="https://raw.githubusercontent.com/estadisticavlc/Datos/master/secciones.rda"
# Leemos el shapefile (almacenado en github como .rda, en formato SpatialPolygonsDataFrame)
load(url(url))
# Quitar seccion El Puerto (1100)
secciones=secciones[-which(secciones$CODDISTSEC==1100),]
# Cambio de coordenadas (de UTM a grados)
proj4string(secciones)=CRS("+proj=utm +zone=30 ellps=WGS84")
secciones=spTransform(secciones,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Para el titulo del mapa
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 22px;
  }
"))
title <- tags$div(
  tag.map.title, HTML("Índice de vulnerabilidad global por sección censal en València. 2019")
)  

# Definir paleta de colores y asignar colores según valor índice global

indice=c(datos$Index_Global[1:566],
             datos$Index_Global[567],datos$Index_Global[567],datos$Index_Global[567],datos$Index_Global[567],
             datos$Index_Global[568:nrow(datos)])
paleta=rev(brewer.pal(5,"YlOrRd") )
categorias=as.numeric(cut(indice,breaks=c(-Inf,quantile(datos$Index_Global,0.2),
                                                           quantile(datos$Index_Global,0.4),
                                                           quantile(datos$Index_Global,0.6),
                                                           quantile(datos$Index_Global,0.8),+Inf)))
colores=paleta[categorias]
secciones$colores=colores
etiquetas=c(paste0("<=",round(quantile(datos$Index_Global,0.2),2)),
            paste0(round(quantile(datos$Index_Global,0.2),2),"-",round(quantile(datos$Index_Global,0.4),2)),
            paste0(round(quantile(datos$Index_Global,0.4),2),"-",round(quantile(datos$Index_Global,0.6),2)),
            paste0(round(quantile(datos$Index_Global,0.6),2),"-",round(quantile(datos$Index_Global,0.8),2)),
            paste0(">",round(quantile(datos$Index_Global,0.8),2)))

# Hacer el gráfico de coropleta
mapa=leaflet(secciones) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(-0.37739, 39.46975, zoom = 12) %>%
  addPolylines(color="black",
               weight=1)%>%
  addLegend("bottomright",
            colors = paleta,
            labels = etiquetas,
            title = "Índice de vulnerabilidad",
            labFormat = labelFormat(prefix = "$"),
            opacity = 0.75)%>%
  addPolygons(color="black", 
              fillColor = ~colores, weight = 2,
              fillOpacity = 0.7)%>%
  addControl("Nota: El índice oscila entre 1 (máxima vulnerabilidad) y 5 (mínima vulnerabilidad).", position = "bottomleft")%>%
  addControl("Elaboración: Oficina d'Estadística. Ajuntament de València.", position = "bottomleft")%>%
  addControl(title, position = "topleft", className="map-title")
print(mapa)
