setwd("/Users/gabycleta/Google Drive File Stream/Mi unidad/DatosDeMiercoles/")
# install_packages("readr")
library(readr)
library(dplyr)
library(tidytext)
library(widyr)
library(ggpubr)
library(cowplot)

la_casa_de_papel <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv")
# remove words such a “a”, “an” or “the”
la_casa_de_papel_subs <- la_casa_de_papel %>%
    unnest_tokens(word,texto) %>%
    anti_join(stop_words)

personajes<-c("profesor", "tokio", "rio", "denver", "helsinki", "nairobi", 
              "lisboa", "lisboa", "lisboa", "estocolmo", "estocolmo")
names(personajes)<-c("profesor", "tokio", "río", "denver", "helsinki", "nairobi", 
              "inspectora", "raquel", "lisboa", "mónica", "estocolmo")


la_casa_de_papel_pers=la_casa_de_papel_subs %>% filter(word %in%names(personajes)) %>% group_by(word, episodio,temporada) %>% tally()
la_casa_de_papel_pers=mutate(la_casa_de_papel_pers, wordSumm = personajes[word])

la_casa_de_papel_pers_perc=la_casa_de_papel_pers %>%
    group_by(wordSumm,temporada) %>%                     
    dplyr::mutate(sumper=n/sum(n))%>%group_by(wordSumm,temporada,episodio)%>%
    dplyr::summarise(Ret=sum(sumper)*100)

graficos=list()
for( p in unique(personajes)){
    pen <- png::readPNG(paste(p, ".png", sep=""))
    graficos[[p]]=ggplot(la_casa_de_papel_pers_perc %>% filter(wordSumm %in% p), 
                         aes(x=temporada, y=Ret, fill=factor(episodio)))+background_image(as.raster(pen))+geom_bar(color="black",
                             stat = "identity", alpha=0.7)+theme(legend.position = "none")+guides(fill =guide_legend(nrow=1))+labs(
                                 fill="Episodios", x="Temporada", y="% de veces que son nombrados")+scale_y_continuous(limits=c(0,100), expand=c(0,0))+scale_fill_brewer(palette="Set3")
}
leyenda=get_legend(graficos[["profesor"]]+theme(legend.position = "bottom", legend.justification = "center"))
figura=plot_grid(plot_grid(plotlist = graficos, nrow=2), leyenda, ncol=1, rel_heights = c(0.9,0.1))
ggplot2::ggsave(figura, file="PorcentajeNombrePersonajes.tiff", height = 9, width=12, dpi=400)

