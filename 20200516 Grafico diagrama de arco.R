lista_de_paquetes <- c("devtools", "igraph", "randomcoloR") # Definimos los paquetes que queremos cargar
paquetes_nuevos <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])] # Buscamos los paquetes que no tenemos
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos) # Instalamos los paquetes que no tenemos


# Cargamos las librerías necesarias
library(devtools)
devtools::install_github("gastonstat/arcdiagram")
library(arcdiagram)
library(igraph)
library(randomcoloR)

# Definimos el conjunto de pares de vértices adyacentes
# (Nota: el orden de los vértices será el de aparición en la siguiente matriz, 
# por lo que incluimos las parejas cbind(1:19,1:19) para determinar el orden del 1 al 19)
edgelist<-rbind(cbind(1:19,1:19),
      c(1,2),c(1,3),c(1,4),c(1,5),c(1,6),
      c(2,1),c(2,3),c(2,6),c(2,9),c(2,10),c(2,12),
      c(3,1),c(3,2),c(3,4),c(3,7),c(3,8),c(3,9),c(3,10),
      c(4,1),c(4,3),c(4,5),c(4,7),c(4,16),c(4,18),
      c(5,1),c(5,4),c(5,6),c(5,14),c(5,15),c(5,16),
      c(6,1),c(6,2),c(6,5),c(6,10),c(6,12),c(6,13),c(6,14),
      c(7,3),c(7,4),c(7,8),
      c(8,3),c(8,7),c(8,9),c(8,19),
      c(9,2),c(9,3),c(9,8),c(9,10),c(9,19),
      c(10,2),c(10,3),c(10,6),c(10,9),c(10,11),c(10,12),c(10,19),
      c(11,10),c(11,12),c(11,13),c(11,19),
      c(12,2),c(12,6),c(12,10),c(12,11),c(12,13),
      c(13,6),c(13,11),c(13,12),c(13,14),
      c(14,5),c(14,6),c(14,13),c(14,15),
      c(15,5),c(15,14),c(15,16),c(15,17),
      c(16,4),c(16,5),c(16,15),c(16,17),c(16,18),
      c(17,15),c(17,16),
      c(18,4),c(18,16),
      c(19,8),c(19,9),c(19,10),c(19,11))

# Etiquetamos los vértices
nombres_distrito=c("Ciutat Vella","l'Eixample","Extramurs","Campanar",
                   "la Saïdia","el Pla del Real","l'Olivereta","Patraix",
                   "Jesús","Quatre Carreres","Poblats Marítims",
                   "Camins al Grau","Algirós","Benimaclet",
                   "Rascanya","Benicalap","Pobles del Nord",
                   "Pobles de l'Oest","Pobles del Sud")

# Asignamos un tamaño a los vértices según el número de aristas que confluyen en él
degrees<-(degree(graph.edgelist(edgelist, directed = TRUE))-2)/2

# Dibujamos el gráfico de diagrama de arco
png("20200516 Grafico diagrama de arco.png", width = 9, height = 5, units = 'in', res = 300)
arcplot(edgelist, labels = nombres_distrito, lwd.arcs = 2, col.arcs = randomColor(length(edgelist),hue="blue"),
        main="Relaciones de vecindad entre los distritos de València", 
        cex.nodes = log(degrees)+0.5, col.nodes = "#b8e0f2", cex.labels = 0.7,
        sub="Elaboración: Oficina d'Estadística. Ajuntament de València.", font.sub=3, cex.sub=0.5)
dev.off()

