#####################################################################################
# Introdução
# Conteúdo:
#1. Mapas Básicos
#2. Mapas com Shapefile + ggplot2
#3. Mapas com Pacote Leaflet
#4. Mapas com Google API( Dependendo do Tempo )

#####################################################################################
#1. Mapas Básicos
install.packages("maps")
library(maps) #mapas simples, eixos, escala, cidades 
install.packages("mapproj")
library(mapproj)
install.packages("rgdal", dependencies = T, force = T)
library(rgdal)

map("world")

par(mar=c(1,1,1,1))
map("world","Brazil")
map.axes()
#map.scale(ratio = F, cex = 0.5)
map(,,add = T)
map.scale(x=-47,y=-30, ratio = F, cex = .5)

map("world","Brazil",fill = T, col = "lightgray")

map.axes()
abline(h=-31.332952, lty = 2, lwd = 1)
abline(v=-54.099830, lty = 2, lwd = 1)

map("world","Brazil",fill = T, col = "lightgray",xlim = c(-58,-49),
    ylim = c(-35,-27))

par(mar=c(1,1,1,1))
m = map("world","Brazil",fill = T, col = "lightgray", plot = T)
#map.grid(m,col = "grey50", font = 1, cex=0.7, pretty = T)
map.grid(m,nx = 5, ny = 5, col = "grey50", font = 1, cex=0.7, pretty = T)

map.cities(country = "Brazil", minpop = 2000000, pch = 20,cex = 1)

#install.packages("RgoogleMaps")
library(RgoogleMaps)

center = c(-31.335785, -54.095573)
zoom = 15

mapa.bage = GetMap(center = center, zoom = zoom, maptype = "terrain",
                   destfile = "mapa_bage.png")


######################################################################
#2. Mapas com Shapefile + ggplot2

#Malha cartográfica = https://mapas.ibge.gov.br/bases-e-referenciais/bases-cartograficas/malhas-digitais

#Arquivo = http://datasus.saude.gov.br/informacoes-de-saude-tabnet/

library(ggplot2)
library(rgdal)

rs = readOGR("C:/Users/fermat/Documents/ScriptR/R - Básico Mapas","43MUE250GC_SIR")
head(rs@data)

rs$CD_GEOCMU = substr(rs$CD_GEOCMU,1,6)

populacao = read.csv2(file.choose(), header = T, sep = ",")

head(populacao)

populacao = na.omit(populacao)

names(populacao) = c("Municipio", "Populacao")

head(rs@data)

populacao$CD_GEOCMU = substr(populacao$Municipio,1,6)

head(populacao)

dim(populacao)
dim(rs@data)

head(rs@data)

populacao = populacao[order(populacao$CD_GEOCMU),]
malhaRS = rs@data[order(rs@data$CD_GEOCMU),]

head(malhaRS)
dim(populacao)
dim(malhaRS)

linhas = c(1,2)
malhaRS = malhaRS[-linhas,]
dim(populacao)
dim(malhaRS)

#Dica
#malhaRS = subset(malhaRS,CD_GEOCMU!="430000")
head(malhaRS)
head(populacao)

rs2 = merge(malhaRS,populacao)

head(rs2)

#install.packages("ggplot2", dependencies = T)
library(ggplot2)
#install.packages("rgeos",dependencies = T)
library(rgeos)
#install.packages("gpclib", type="source")
library(gpclib)
#install.packages("maptools")
library(maptools)

head(rs)
rs.rsf = fortify(rs, region = "CD_GEOCMU")

head(rs.rsf)

rs.rsf = subset(rs.rsf,id!="430000")

rs.rsf = merge(rs.rsf, rs@data, by.x = "id", by.y = "CD_GEOCMU")

rs2$PopulacaoCat = cut(rs2$Populacao, breaks = c(0,20000,40000,60000,80000,100000,2000000),
                       labels = c('0-20000',
                                  '20000-40000',
                                  '40000-60000',
                                  '60000-80000',
                                  '80000-100000',
                                  '+100000'),
                       include.lowest = T)

head(rs2)

#rm(rs2)
#rm(rs.rsf)

rs.rsf = merge(rs.rsf, rs2, by.x = "id", by.y = "CD_GEOCMU")

head(rs.rsf)

#names(rs2)[1]=c("id")

#install.packages("RColorBrewer",dependencies = T)
library(RColorBrewer)

ggplot(rs.rsf, aes(rs.rsf$long,rs.rsf$lat, group=rs.rsf$group,fill=rs.rsf$PopulacaoCat)) +
  geom_polygon(colour='green') + coord_equal() + ggtitle("População") +
  labs(x = "Longitude", y = "Latitude", fill="População") +
  scale_fill_manual(values = brewer.pal(9,'Reds')[4:9]) +
  theme(plot.title = element_text(size = rel(1), lineheight = 0.9, face = "bold",
                                  colour = 'blue'))

###########################################################################################
#2.1 Mapas com Shapefile + ggplot

library(ggplot2)
library(rgdal)

rs = readOGR("C:/Users/fermat/Documents/ScriptR/R - Básico Mapas","43MUE250GC_SIR")

head(rs@data)


rs$CD_GEOCMU = substr(rs$CD_GEOCMU,1,6)

#importar dados tabnet!
populacao = read.csv2(file.choose(),header = T, sep = ",")
nascimentos = read.csv2(file.choose(),header = T, sep = ",")
obitos = read.csv2(file.choose(),header = T, sep = ",")

head(populacao)
head(nascimentos)
head(obitos)

populacao = na.omit(populacao)
nascimentos = na.omit(nascimentos)
obitos = na.omit(obitos)

names(populacao) = c("Municipio", "Populacao")
names(nascimentos) = c("Municipio", "Nascimentos")
names(obitos) = c("Municipio", "Obitos")

head(populacao)
head(nascimentos)
head(obitos)

populacao$CD_GEOCMU = substr(populacao$Municipio,1,6)
nascimentos$CD_GEOCMU = substr(nascimentos$Municipio,1,6)
obitos$CD_GEOCMU = substr(obitos$Municipio,1,6)

head(populacao)
head(nascimentos)
head(obitos)

head(rs@data)
#rs@data

dim(populacao)
dim(nascimentos)
dim(obitos)
dim(rs@data)

#Ordenando os objetos pelo id
populacao = populacao[order(populacao$CD_GEOCMU),]
nascimentos = nascimentos[order(nascimentos$CD_GEOCMU),]
obitos = obitos[order(obitos$CD_GEOCMU),]
malhaRS = rs@data[order(rs@data$CD_GEOCMU),]

dim(malhaRS)
head(malhaRS)

linhas = c(1,2)
malhaRS = malhaRS[-linhas,]

dim(malhaRS)
head(malhaRS)

dados = populacao

dados$Nascimentos = nascimentos$Nascimentos
dados$Obitos = obitos$Obitos

rs2 = merge(malhaRS,dados)
head(rs2)

rs2$PercNascimentos = (rs2$Nascimentos*100)/rs2$Populacao
rs2$PercObitos = (rs2$Obitos*100)/rs2$Populacao

head(rs2)

rs.rsf = fortify(rs,region = "CD_GEOCMU")

head(rs.rsf)

rs.rsf = subset(rs.rsf,id!="430000")

head(rs.rsf)

rs.rsf = merge(rs.rsf, rs@data, by.x="id", by.y = "CD_GEOCMU")

head(rs.rsf)
head(rs2)

rs2$NascimentosCat = cut(rs2$Nascimentos, breaks = c(0,200,400,600,800,1000,20000),
                       labels = c('0-200',
                                  '200-400',
                                  '400-600',
                                  '600-800',
                                  '800-1000',
                                  '+1000'),
                       include.lowest = T)

rs2$ObitosCat = cut(rs2$Obitos, breaks = c(0,200,400,600,800,1000,12000),
                         labels = c('0-200',
                                    '200-400',
                                    '400-600',
                                    '600-800',
                                    '800-1000',
                                    '+1000'),
                         include.lowest = T)

rs2$PercNascCat = cut(rs2$PercNascimentos, breaks = c(0,0.3,0.6,0.9,1.2,1.5,1.8,
                                                      2.2),
                         labels = c('0-0.3',
                                    '0.3-0.6',
                                    '0.6-0.9',
                                    '1.2-1.5',
                                    '1.5-1.8',
                                    '1.8-2',
                                    '+2'),
                         include.lowest = T)

rs2$PercObitosCat = cut(rs2$PercObitos, breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,
                                                      1.6),
                      labels = c('0-0.2',
                                 '0.2-0.4',
                                 '0.4-0.6',
                                 '0.6-0.8',
                                 '0.8-1.0',
                                 '1.0-1.2',
                                 '+1.2'),
                      include.lowest = T)


head(rs2)

#rm(rs2)
#rm(rs.rsf)

rs.rsf = merge(rs.rsf, rs2, by.x = "id", by.y = "CD_GEOCMU")

head(rs.rsf)

#names(rs2)[1]=c("id")

#install.packages("RColorBrewer",dependencies = T)
library(RColorBrewer)
library(ggplot2)

ggplot(rs.rsf, aes(rs.rsf$long,rs.rsf$lat, group=rs.rsf$group,fill=rs.rsf$NascimentosCat)) +
  geom_polygon(colour='red') + coord_equal() + ggtitle("Nascimentos") +
  labs(x = "Longitude", y = "Latitude", fill="Nascimentos") +
  scale_fill_manual(values = brewer.pal(9,'Greens')[4:9]) +
  theme(plot.title = element_text(size = rel(1), lineheight = 0.9, face = "bold",
                                  colour = 'blue'))

ggplot(rs.rsf, aes(rs.rsf$long,rs.rsf$lat, group=rs.rsf$group,fill=rs.rsf$ObitosCat)) +
  geom_polygon(colour='red') + coord_equal() + ggtitle("Obitos") +
  labs(x = "Longitude", y = "Latitude", fill="Obitos") +
  scale_fill_manual(values = brewer.pal(9,'Purples')[4:9]) +
  theme(plot.title = element_text(size = rel(1), lineheight = 0.9, face = "bold",
                                  colour = 'blue'))

ggplot(rs.rsf, aes(rs.rsf$long,rs.rsf$lat, group=rs.rsf$group,fill=rs.rsf$PercNascCat)) +
  geom_polygon(colour='green') + coord_equal() + ggtitle("Percentual Nascimentos") +
  labs(x = "Longitude", y = "Latitude", fill="Perc. Nascimentos") +
  scale_fill_manual(values = brewer.pal(9,'Oranges')[3:9]) +
  theme(plot.title = element_text(size = rel(1), lineheight = 0.9, face = "bold",
                                  colour = 'blue'))

ggplot(rs.rsf, aes(rs.rsf$long,rs.rsf$lat, group=rs.rsf$group,fill=rs.rsf$PercObitosCat)) +
  geom_polygon(colour='green') + coord_equal() + ggtitle("Percentual Obitos") +
  labs(x = "Longitude", y = "Latitude", fill="Perc. Obitos") +
  scale_fill_manual(values = brewer.pal(9,'OrRd')[3:9]) +
  theme(plot.title = element_text(size = rel(1), lineheight = 0.9, face = "bold",
                                  colour = 'blue'))


###########################################################################################
#3. Mapas com Leaflet

#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rjson")
library(rjson)
#install.packages("jsonlite", dependencies = T)
library(jsonlite)
#install.packages("leaflet",dependencies = T)
library(leaflet)
#install.packages("RCurl")
library(RCurl)

# https://rstudio.github.io/leaflet/

leaflet() %>% addTiles()

leaflet() %>% addTiles() %>% addProviderTiles(providers$MtbMap) %>% 
  addProviderTiles(providers$Stamen.TonerLines, 
                   options = providerTileOptions(opacity = 0.50)) %>% 
  addProviderTiles(providers$Stamen.TonerLabels, 
                   options = providerTileOptions(opacity = 0.90))

lat = -31.333019
long = -54.100074

leaflet() %>% addTiles() %>% addMarkers(long,lat)

leaflet() %>% addTiles() %>% addCircleMarkers(long,lat)

#Diversos Pontos No Mapa

p = pontosMapa

leaflet() %>% addTiles() %>% addMarkers(p$long,p$lat)

class(p$lat)

p$lat = as.numeric(p$lat)
p$long = as.numeric(p$long)

leaflet() %>% addTiles() %>% addMarkers(p$long,p$lat)

leaflet() %>% addTiles() %>% addMarkers(p$long,p$lat, popup = p$ponto)

leaflet() %>% addTiles() %>% addCircleMarkers(p$long,p$lat)


###Mudando as cores dos marcadores

#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rjson")
library(rjson)
#install.packages("jsonlite", dependencies = T)
library(jsonlite)
#install.packages("leaflet",dependencies = T)
library(leaflet)
#install.packages("RCurl")
library(RCurl)

p = pontosMapa
class(p$long)

p$lat = as.numeric(p$lat)
p$long = as.numeric(p$long)

cor = c()
nrow(p)

for(i in 1 : nrow(p)){
  
  if(p$tipo[i] == 1){
    cor[i] = "green"
  }else if(p$tipo[i] == 2){
    cor[i] = "red"
  }else if(p$tipo[i] == 3){
    cor[i] = "pink"
  }else{
    cor[i] = "blue"
  }
}
cor

icone = awesomeIcons(icon = "pin",library = "ion", markerColor = cor)

leaflet() %>% addTiles() %>% addAwesomeMarkers(p$long,p$lat, icon = icone,
                                               popup = p$ponto, label = p$ponto)
#Clusters

icone = awesomeIcons(icon=" ",markerColor = cor)

leaflet() %>% addTiles() %>% addAwesomeMarkers(p$long,p$lat, icon = icone,
                                               popup = p$ponto, label = p$ponto,
                                               clusterOptions = markerClusterOptions())

#Alterando Marcadores Circulares

leaflet()%>% addTiles() %>% addCircleMarkers(p$long,p$lat, color = cor,
                                             label = p$ponto,
                                             stroke = T,
                                             fillOpacity = 0.5,
                                             radius = ifelse(p$tipo == 1, 10, 6)
                                              )


#Adicionando formas - Círculos
leaflet()%>% addTiles()

lat = -31.333019
long = -54.100074

leaflet()%>% addTiles() %>% addCircles(long,lat)

pf = populacaoFronteira

class(pf$lat)
class(pf$log)
class(pf$Populacao_estimada)

pf$pop = as.character(pf$Populacao_estimada)

leaflet()%>% addTiles() %>% addCircles(lng = pf$log, lat = pf$lat,
                                       radius = sqrt(pf$Populacao_estimada)*20,
                                       stroke = F,fillOpacity = 0.5,
                                       label = pf$pop)



#Adicionando formas - Retângulos
lat = -31.328593
lng = -54.101329

lat1 = -31.327218
lng1 = -54.100138
leaflet()%>% addTiles() %>% addRectangles(lng,lat,lng1,lat1,
                                          fillOpacity = .5)

###########################################################################################
#4. Mapas com Google API (NÃO FOI GRAVADO AINDA)
# Criar um novo projeto em: https://console.cloud.google.com
# Gerar Chave de Ativação, chave: KEY
# Instale o pacote ggmap R e defina a chave da API
#no R executando os comandos conforme abaixo.


install.packages("ggmap")
remove.packages("ggmap")

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

#Load the library
library("ggmap")

install.packages("devtools")


library(ggmap)
library(ggplot2)
library(dplyr)
#API Key
ggmap::register_google(key = "Key")

#Notes: If you get still have a failure then I suggest to restart R and run the library and register google commands again.
center =  c(-54.106141,-31.331287)
get_googlemap(-54.106141,-31.331287)
