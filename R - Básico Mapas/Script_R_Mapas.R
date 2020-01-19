#####################################################################################
# Introdução
# Conteúdo:
#1. Mapas Básicos
#2. Mapas com Shapefile + ggplot2
#3. Mapas com Pacote Leaflet
#4. Mapas com Google API( Dependendo do Tempo )

#####################################################################################
#1. Mapas Básicos
#install.packages("maps")
library(maps) #mapas simples, eixos, escala, cidades 
#install.packages("mapproj")
library(mapproj)
#install.packages("rgdal", dependencies = T)
library(rgdal)

map("world")

par(mar=c(1,1,1,1))
map("world","Brazil")
map.axes()
#map.scale(ratio = F, cex = 0.5)
map(,,add = T)
map.scale(x=-47,y=-30,ratio = F, cex = 0.5)

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
#2. Mapas com Shapefile

#Malha cartográfica = https://mapas.ibge.gov.br/bases-e-referenciais/bases-cartograficas/malhas-digitais

#Arquivo = http://datasus.saude.gov.br/informacoes-de-saude-tabnet/

library(ggplot2)
library(rgdal)

rs = readOGR("C:/Users/fermat/Documents/ScriptR/R - Básico Mapas","43MUE250GC_SIR")

head(rs@data)

rs$CD_GEOCMU=substr(rs$CD_GEOCMU, 1, 6)

#importar dados tabnet!
populacao

head(populacao)

populacao

populacao = na.omit(populacao)

head(populacao)

names(populacao) = c("Municipio","Populacao")

populacao$CD_GEOCMU=substr(populacao$Municipio, 1, 6)

head(populacao)

populacao

head(rs@data)

rs@data

dim(populacao)
dim(rs@data)

#Ordenando os bancos pelo id
populacao = populacao[order(populacao$CD_GEOCMU), ] 
malhaRS = rs@data[order(rs@data$CD_GEOCMU), ]

linhas = c(1,2)
malhaRS = malhaRS[-linhas,]
head(malhaRS)

#Dica !
#malhaRS = subset(malhaRS, CD_GEOCMU!="430000")

head(malhaRS)

rs2 = merge(malhaRS, populacao,by.y="CD_GEOCMU") 

head(rs2)

#install.packages("ggplot2", dependencies = T)
library(ggplot2)
#install.packages("rgeos",dependencies = T)
library(rgeos)
#install.packages("gpclib", type="source")
library(gpclib)
#install.packages("maptools")
library(maptools)

rs.df = fortify(rs, region = "CD_GEOCMU") 

head(rs.df)

#procurar na base de Malhas o nome do tal município e adicionar
rs.df = merge(rs.df, rs@data, by.x = "id", by.y = "CD_GEOCMU")

head(rs.df)

rs2$populacaoCat = cut(rs2$Populacao, breaks=c(0,10000,20000,30000,40000,50000,100000,
                                               200000),labels=c('0-10000',
                                                                '10000-20000',
                                                                '20000-30000',
                                                                '30000-40000',
                                                                '40000-50000',
                                                                '50000-100000',
                                                                '+200000'),
                       include.lowest=TRUE)

names(rs2)[1]=c("id")

rs.df = merge(rs.df, rs2, by = "id")

head(rs.df)

#install.packages("RColorBrewer",dependencies = T)
library(RColorBrewer)

ggplot(rs.df, aes(rs.df$long, rs.df$lat, group = rs.df$group,fill=rs.df$populacaoCat))+geom_polygon(colour='yellow')+coord_equal()+ggtitle("População")+theme(plot.title=element_text(size=rel(1), lineheight=0.9, face="bold", colour="blue"))+labs(x = "Longitude", y = "Latitude", fill = "População")+scale_fill_manual(values=brewer.pal(10, 'Blues')[3:10])
