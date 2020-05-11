library(curl)
datos=read.csv(curl("https://raw.githubusercontent.com/estadisticavlc/Datos/master/MicrodatosMarzo2020.csv"),sep=";",dec=",")

p=ggplot(data=datos, aes(x=P3)) +  
   geom_bar(aes(y = 100*(..count..)/sum(..count..)),fill="#4781b3")+
   ggtitle("¿Cuántas personas viven en su casa contándose a usted?")+
   theme(title = element_text(size=10))+
   ylab("Porcentaje")+
   xlab("Personas")+
   scale_x_continuous(breaks=c(0:10))
p
ggsave(filename = paste0("20200512 Grafico barras o columnas.png"), p,
       width = 5, height = 5, dpi = 300, units = "in", device='png')


