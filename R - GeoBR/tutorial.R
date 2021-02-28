  
library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(leaflet)

setwd("~/ScriptR/R - GeoBR")

ac <- read_conservation_units()
class(ac)

ac1 <- subset(ac, ac$code_conservation_unit==3138)

map <- leaflet(ac1) %>% addTiles()
map%>%addPolygons()

shape <- st_write(ac1,"parque.shp")
class(shape)
kml <- st_write(ac1,"parqueKML.kml")

bioma <- read_biomes()
mt <- read_state(code_state = "MT")

biomas <- st_write(bioma,"biomas.shp")
mt <- st_write(mt,"matogrosso.shp")

biomaMT <- st_read("biomaMT.shp")
class(biomaMT)

ggplot() + geom_sf(data = biomaMT, fill="#2D3E50",color="#FEBF57",size=.15,
                   show.legend = F)

biomaMTAmazonia = subset(biomaMT, biomaMT$code_biome==1)
biomaMTCerrado = subset(biomaMT, biomaMT$code_biome==3)
biomaMTPatanal = subset(biomaMT, biomaMT$code_biome==6)

ggplot() +
  geom_sf(data = biomaMTAmazonia, fill="#5F9EA0",color="#FEBF57",size=.15,show.legend = F) +
  geom_sf(data = biomaMTCerrado, fill="#DAA520",color="#FEBF57",size=.15,show.legend = F) + geom_sf(data = biomaMTPatanal, fill="#32CD32",color="#FEBF57",size=.15,show.legend = F)




