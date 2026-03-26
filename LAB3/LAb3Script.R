library(spdep)
library(sf)
library(tidyverse)
library(tmap)
library(ggplot2)



#1
region <- sf::read_sf("./data/region_subset.shp")
glimpse(region)

tmap::tm_shape(region) + tm_polygons()

#2 

region.projected <- region %>% sf::st_transform(., "ESRI:102010")

tmap::tm_shape(region.projected) + tm_polygons()

#3
hist(region.projected$pct_blc)

 #4 
tmap:: tm_shape(region.projected) +tm_polygons("pct_blc", style = "quantile")

#5.1 
nb <- spdep::poly2nb(region.projected, queen = TRUE)

lw <- nb2listw(nb, style="W", zero.policy=TRUE)

neighbors <- attr(lw$weights,"comp")$d 
#5.2
hist(neighbors)

F.lag <- lag.listw(lw, region.projected$pct_blc)
#5.3
nbmean<- mean(neighbors)
#5.4
moran.test(region.projected$pct_blc, lw)
moran.plot(region.projected$pct_blc, lw, zero.policy=TRUE, plot=TRUE)

#6
centroid<- sf::st_coordinates(sf::st_centroid(region.projected))
knearnb<- spdep::knearneigh(centroid, k=6,  use_kd_tree=TRUE)
idw_nb<- spdep::knn2nb(knearnb, row.names = NULL, sym = TRUE)
dist<- spdep::nbdists(idw_nb, centroid, longlat = NULL)

#6.1
lw_idw<-nb2listw(idw_nb, glist=lapply(dist,function(d) 1/d), style="W", zero.policy=NULL)

#6.2
idw_neighbors<- card(idw_nb)
hist(idw_neighbors)

#6.3
meanidwnb<- mean(idw_neighbors)

#6.4

moran.test(region.projected$pct_blc, lw_idw)
moran.plot(region.projected$pct_blc, lw_idw, zero.policy=TRUE, plot=TRUE)




