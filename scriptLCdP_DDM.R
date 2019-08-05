
setwd("/myDirectory/DatosDeMiercoles/")
# install_packages("readr")
library(readr)
library(dplyr)
library(tidytext)
library(widyr)
library(ggpubr)
library(cowplot)

la_casa_de_papel <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv")
# obtener palabras por separado
la_casa_de_papel_subs <- la_casa_de_papel %>%
    unnest_tokens(word,texto) %>%
    anti_join(stop_words)
# definir los personajes, teniendo en cuenta los "búscados" luego de T2 (https://github.com/gamerino/DSwR-DDM/blob/master/LCDPPersonajes.png)
personajes<-c("profesor", "tokio", "rio", "denver", "helsinki", "nairobi", 
              "lisboa", "lisboa", "lisboa", "estocolmo", "estocolmo")
# teniendo en cuenta que raquel=inspectora=lisboa y que mónica=estocolmo
names(personajes)<-c("profesor", "tokio", "río", "denver", "helsinki", "nairobi", 
              "inspectora", "raquel", "lisboa", "mónica", "estocolmo")

#agrupamos los personajes según el número de veces que se los nombra por episodio por temporada
la_casa_de_papel_pers=la_casa_de_papel_subs %>% filter(word %in%names(personajes)) %>% group_by(word, episodio,temporada) %>% tally()

#agrupamos los personajes según su denominación como capital de país ( a parte del profesor :P) 
la_casa_de_papel_pers=mutate(la_casa_de_papel_pers, wordSumm = personajes[word])
# determinamos los porcentajes
la_casa_de_papel_pers_perc=la_casa_de_papel_pers %>%
    group_by(wordSumm,temporada) %>%                     
    dplyr::mutate(sumper=n/sum(n))%>%group_by(wordSumm,temporada,episodio)%>%
    dplyr::summarise(Ret=sum(sumper)*100)
# Gráficos!
graficos=list()
for( p in unique(personajes)){
    # cargamos la imágen del personaje en cuestión
    pen <- png::readPNG(paste(p, ".png", sep=""))
    # graficamos los porcentajes por temporada y por episodio
    graficos[[p]]=ggplot(la_casa_de_papel_pers_perc %>% filter(wordSumm %in% p), 
                         aes(x=temporada, y=Ret, fill=factor(episodio)))+background_image(as.raster(pen))+geom_bar(color="black",
                             stat = "identity", alpha=0.7)+theme(legend.position = "none")+guides(fill =guide_legend(nrow=1))+labs(
                                 fill="Episodios", x="Temporada", y="% de veces que son nombrados")+scale_y_continuous(limits=c(0,100), expand=c(0,0))+scale_fill_brewer(palette="Set3")
}
leyenda=get_legend(graficos[["profesor"]]+theme(legend.position = "bottom", legend.justification = "center"))
figura=plot_grid(plot_grid(plotlist = graficos, nrow=2), leyenda, ncol=1, rel_heights = c(0.9,0.1))
ggplot2::ggsave(figura, file="PorcentajeNombrePersonajes.tiff", height = 9, width=12, dpi=400)

