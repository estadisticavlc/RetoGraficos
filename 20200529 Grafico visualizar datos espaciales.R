lista_de_paquetes <- c("curl","ggplot2", "grid", "leaflet", "htmlwidgets", "htmltools") # Definimos los paquetes que queremos cargar
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

# Leemos los datos
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/CentrosSanitarios.csv"),sep=",",dec=",")
datos$X_grad=as.numeric(as.character(datos$X_grad))
datos$Y_grad=as.numeric(as.character(datos$Y_grad))

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
    font-size: 28px;
  }
"))
title <- tags$div(
  tag.map.title, HTML("Centros sanitarios en València")
)  

mapa=leaflet() %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(-0.37739, 39.46975, zoom = 12) %>%
  addCircleMarkers(lng=datos$X_grad[datos$TIPO=="Centro de Especialidades"],lat=datos$Y_grad[datos$TIPO=="Centro de Especialidades"],radius=3,fillOpacity = 1,color="#f58ee7")%>%
  addCircleMarkers(lng=datos$X_grad[datos$TIPO=="Centro de Salud"],lat=datos$Y_grad[datos$TIPO=="Centro de Salud"],radius=3,fillOpacity = 1,color="#4287f5")%>%
  addCircleMarkers(lng=datos$X_grad[datos$TIPO=="Centro Sanitario Integrado"],lat=datos$Y_grad[datos$TIPO=="Centro Sanitario Integrado"],radius=3,fillOpacity = 1,color="#42f2f5")%>%
  addCircleMarkers(lng=datos$X_grad[datos$TIPO=="Consultorio"],lat=datos$Y_grad[datos$TIPO=="Consultorio"],radius=3,fillOpacity = 1,color="#f5b342")%>%
  addCircleMarkers(lng=datos$X_grad[datos$TIPO=="Hospital Público"],lat=datos$Y_grad[datos$TIPO=="Hospital Público"],radius=5,fillOpacity = 1,color="#32a852")%>%
  addCircleMarkers(lng=datos$X_grad[datos$TIPO=="Hospital/Clínica Privado"],lat=datos$Y_grad[datos$TIPO=="Hospital/Clínica Privado"],radius=5,fillOpacity = 1,color="#b2de71")%>%
  addLegend("bottomright",
            colors=c("#f58ee7","#4287f5","#42f2f5","#f5b342","#32a852","#b2de71"),
            labels = sort(unique(datos$TIPO)),
            title = "",
            labFormat = labelFormat(prefix = "$"),
            opacity = 0.75)%>%
  addControl("Elaboración: Oficina d'Estadística. Ajuntament de València.", position = "bottomleft")%>%
  addControl(title, position = "topleft", className="map-title")
print(mapa)
