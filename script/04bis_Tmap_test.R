# Utilisation du paquest Tmap - cartes Chloropletes
library(tmap)
library(rgdal)
library(raster)
library(sf)
library(ggplot2)
library(maps)
library(ggmap)
library(dplyr)
library(RColorBrewer)

repertoire<-"d:\\OneDrive - ARVALIS - Institut du végétal\\Deudoo\\Cartes_gel"
#Data<-read.csv2(paste0(repertoire,"RMSE_departement.csv"),sep=";",dec=".",header=T)
#?Data<-read.csv2(paste0(repertoire,"Degats_gel_BTH.csv"),sep=";",dec=".",header=T)

#point<-st_as_sf(Data, coords = c("longitude", "latitude"),crs = 4326 )
#plot(st_geometry( point  ))

point<-readOGR(repertoire,"Degats_gel_OP_automne_L93",use_iconv = TRUE, encoding = "UTF-8")
proj4string(point) = "+init=epsg:2154"

#Data$CODE_DEPT<-else(nchar(Data$DPT.NUM)==1,paste0("0",Data$DPT.NUM),Data$DPT.NUM)
# Chargement de la couche département
#zone<-readOGR("d:\\OneDrive - ARVALIS - Institut du végétal\\CARTAMV2\\JEU_TEST_CARTAMV2\\vecteur","Centre_L93")
zone<-readOGR("d:\\OneDrive - ARVALIS - Institut du végétal\\CARTAMV2\\JEU_TEST_CARTAMV2\\vecteur","Grande_region_centre")
proj4string(zone) = "+init=epsg:2154"
# réalisation de la jointure
#jointure <- merge(zone, Data, by="CODE_DEPT")
## Export de la carte
#jpeg(filename=paste0(repertoire,"macarte.jpeg",sep=""),quality=75,units="px",width=2500,height=2500,res=300,type="cairo")
#print(tm_shape(jointure, projection="+init=epsg:2154") + tm_fill("RMSE_departement",title="RMSE par départements",colorNA ="white")+
#tm_borders(lwd=0.5)) 
#dev.off(2)

# Tmap point
map1<-tm_shape(zone)+ tm_fill("white")+ tm_borders() + tm_shape(point) + tm_dots(shape= "Degats",size=0.3) +
    tm_layout( "Nature des dégats (BTH)",legend.title.size = 0.0001,legend.text.size = 0.6, frame = FALSE,legend.width=-1) 
map2<-tm_shape(zone)+ tm_fill("white") + tm_borders() + tm_shape(point) +  tm_dots("Symptomes", size=0.5) + tm_layout("Nature des symptômes (BTH)",legend.title.size = 0.001, legend.text.size = 0.6, legend.position = c("left","bottom"), legend.bg.color = "white",legend.bg.alpha = 1,frame = FALSE, legend.width=-1) 

tmap_arrange(map1,map2,ncol = 2)                                                  

#tmap_save(tmap_arrange(map1,map2,ncol = 2), filename = paste0(repertoire,"//","gel_BTH.png"))

# Heat map
test_heatmap<-as.data.frame(point)
# Suppression des parcelles sans dégats de gel
#test_heatmap<- test_heatmap[!test_heatmap$Avez.vous=="Non",]
#test_heatmap<- test_heatmap[!test_heatmap$Ces.dégâ=="Non",]
                     
zone.df <- fortify(zone)

ggplot(test_heatmap, aes(x=X, y=Y)) + 
  stat_density2d(aes(fill = ..level..), alpha=0.5, geom="polygon")+
  geom_point(colour="red")+
 theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
   geom_path(data=zone.df,aes(x=long, y=lat,group=group), colour="grey50")+
  scale_fill_gradientn(colours=rev(brewer.pal(5,"Spectral")),breaks=c(1e-11,4e-11,7e-11),labels=c("Faible","Moyen","Fort"))+
 #scale_fill_gradientn(colours=rev(brewer.pal(5,"Spectral"))) +
  coord_fixed()  + ggtitle("Intensité des dégats de gel (OP)")   + labs(fill="") 



