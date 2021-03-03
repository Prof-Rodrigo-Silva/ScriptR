# GeoBR

#Refer?ncia : https://www.rdocumentation.org/packages/geobr/versions/1.0
#Dados IBGE: https://concla.ibge.gov.br/classificacoes/por-tema/codigo-de-areas/codigo-de-areas

#install.packages("geobr",dependencies = T)
library(geobr)
#install.packages("sf")
library(sf)
#install.packages("magrittr")
library(magrittr)
#install.packages("dplyr")
library(dplyr)
library(ggplot2)

conj_dados = list_geobr()
#
# Leia todas as ?reas geogr?ficas do pa?s

brasil = read_state(code_state = "all",year = 2018)
plot(brasil)

ggplot() + geom_sf(data = brasil, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

micro_reg = read_micro_region(code_micro = "all",year = 2015)
ggplot() + geom_sf(data = micro_reg, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

meso_reg = read_meso_region(code_meso = "all",year = 2010)
ggplot() + geom_sf(data = meso_reg, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

municipios = read_municipality(code_muni = 42, year = 2018)
ggplot() + geom_sf(data = municipios, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

plot(municipios)

# Leia todas as ?reas geogr?ficas de um estado com um determinado c?digo
rs = read_state(code_state = 43)
plot(rs)
ggplot() + geom_sf(data = rs, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

meso_reg = read_meso_region(code_meso = 4306)
meso_reg1 = read_meso_region(code_meso = 4301)
ggplot() + geom_sf(data = meso_reg, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F) +
  geom_sf(data = meso_reg1, fill="#6F4E50", color="#FEBF57", size=.15,
            show.legend = F)

micro_reg = read_micro_region(code_micro = 43)
ggplot() + geom_sf(data = micro_reg, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

micro_reg$name_micro

municipios = read_municipality(code_muni = 4301636)
ggplot() + geom_sf(data = municipios, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)


# Biomas no GeoBR

library(geobr)
library(sf)
library(magrittr)
library(dplyr)
library(ggplot2)

conj_dados = list_geobr()

bioma = read_biomes()
plot(bioma)

bioma$name_biome
bioma$code_biome
bioma$geom

pampa = bioma[5,]

plot(pampa)

ggplot() + geom_sf(data = pampa, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

mata_atlat = bioma[4,]

ggplot() + geom_sf(data = mata_atlat, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)


ggplot() + geom_sf(data = pampa, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F) + 
  geom_sf(data = mata_atlat, fill="#5F3E50", color="#FEBF57", size=.15,
                                              show.legend = F) +
  ggtitle("Biomas Pampa e Mata Atl?ntica")


ggplot()+geom_sf(data = bioma, fill="#810d0a", color="#FEBF57", size=.15,
                  show.legend = F)+geom_sf(data = mt, fill="#2D3E50", color="#FEBF57", size=.15,
                show.legend = F) 
##########################################################################################
#Regi?es, ?reas de Pondera??o e Amaz?nia
library(geobr)
library(sf)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

r1 = read_intermediate_region(code_intermediate = 4302)
plot(r1)
ggplot() + geom_sf(data = r1, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

r2 = read_immediate_region(code_immediate = 430010)
plot(r2)
ggplot() + geom_sf(data = r2, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

r3 = read_weighting_area(code_weighting = 4300604)
plot(r3)

ggplot() + geom_sf(data = r3, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

r4 = r3[5:8,]

ggplot() + geom_sf(data = r4, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

r5 = read_amazon()
ggplot() + geom_sf(data = r5, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

ap = read_weighting_area()
ap = subset(ap,ap$abbrev_state=="RS")
library(leaflet)
map = leaflet(ap)%>%addTiles()
map%>%addPolygons()

#Semi?rido
library(geobr)
library(sf)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

rs = read_semiarid()
plot(rs)
rs1 = rs[3:187,]
plot(rs1)
ggplot() + geom_sf(data = rs, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

#Setores Censit?rios
library(geobr)
library(sf)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

r = read_census_tract(code_tract = 4301602)
plot(r)
ggplot() + geom_sf(data = r, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)
map = leaflet(r)%>%addTiles()
map%>%addPolygons()
rr = read_census_tract(code_tract = 4301602, year = 2000, zone = "rural")
ggplot() + geom_sf(data = rr, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

ru = read_census_tract(code_tract = 4301602, year = 2000, zone = "urban")
plot(ru)
ggplot() + geom_sf(data = ru1, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

ru1 = ru[1:2,]
#Sedes municipais e grade de informa??es
library(geobr)
library(sf)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

sm = read_municipal_seat()
#sm1 = sm[1:30,]
plot(sm)

sm1 = subset(sm, code_state == 43)
plot(sm1)

ggplot() + geom_sf(data = sm1, fill="#2D3E50", color="#FF0000", size=.15,
                   show.legend = F)

sm2 = subset(sm, name_region == "Sul")
ggplot() + geom_sf(data = sm2, fill="#2D3E50", color="#FF0000", size=.15,
                   show.legend = F)

bg = subset(sm, name_muni == "Bag?")
bg = subset(sm,code_muni == 4301636)

ggplot() + geom_sf(data = bg, fill="#2D3E50", color="#FF0000", size=.15,
                   show.legend = F)

e = read_statistical_grid(code_grid = 43)

e1 = e[1:3,]

plot(e1)

ggplot() + geom_sf(data = e1, fill="#F03E50", color="#FF0000", size=.15,
                   show.legend = F) + geom_sf(data = sm2, fill="#2D3E50", color="#FF0000", size=.15,
                                              show.legend = F) 

en = NULL
en$k1 = e$nome_1KM
en = as.data.frame(en)
en$geom = e$geom


############################
# ?reas metropolitanas
# A fun??o retorna as formas dos munic?pios agrupados por suas
# respectivas ?reas metropolitanas.
library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

ma = read_metro_area()

plot(ma)

rs = subset(ma, ma$code_state==43)

plot(rs)

maPoA = subset(rs, rs$name_metro=="RM Porto Alegre")

plot(maPoA)

ggplot() + geom_sf(data = maPoA, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

############################
# ?reas urbanas
# Esta fun??o l? os dados oficiais sobre a pegada urbana das cidades
# brasileiras nos anos de 2005 e 2015.

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

au = read_urban_area()

auBG = subset(au, au$code_muni==4301602)

ggplot() + geom_sf(data = auBG, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

auBG1 = subset(auBG, auBG$area_geo > 25)

ggplot() + geom_sf(data = auBG1, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)


##############################################3
# ?reas de conserva??o
# Seu conjunto de dados abrange todo o Brasil e inclui os pol?gonos de todas
# as unidades de conserva??o presentes no territ?rio brasileiro.
# A ?ltima atualiza??o dos dados foi 09-2019

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(leaflet)
c = list_geobr()

ac <- read_conservation_units()

plot(ac)
ac = subset(ac, ac$government_level=="municipal")
ac1 = subset(ac, ac$code_conservation_unit==3138)
shape <- st_write(ac1,"parque.shp")
kml <- st_write(ac1,"parque.kml")
class(ac1)
ac2 = ac[250:260,]
map = leaflet(ac1)%>%addTiles()
map%>%addPolygons()
ggplot() + geom_sf(data = ac, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

ac1 = subset(ac, ac$category=="Parque")

ggplot() + geom_sf(data = ac1, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

ac2 = subset(ac, ac$name_conservation_unit=="FLORESTA ESTADUAL DO ARAGUAIA")

ggplot() + geom_sf(data = ac2, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

###############################################
# ?reas de risco

# Esta fun??o l? os dados oficiais das ?reas de risco de desastre no Brasil.
# Ele se concentra especificamente em desastres geodin?micos e hidrometeorol?gicos
# capazes de provocar deslizamentos de terra e inunda??es. O conjunto de dados
# abrange todo o pa?s. Cada pol?gono da ?rea de risco (conhecido como 'BATER') possui
# um ID de c?digo exclusivo (coluna 'geo_bater'). 
# O conjunto de dados traz informa??es sobre a extens?o em que os pol?gonos da ?rea
# de risco se sobrep?em aos setores censit?rios e faces do bloco (coluna "acuracia") 
# e n?mero de ?reas de risco dentro de cada ?rea de risco (coluna 'num').

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

ar = read_disaster_risk_area()

ggplot() + geom_sf(data = ar, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.lgeend = F)

arRS = subset(ar, ar$code_state==43)

ggplot() + geom_sf(data = arRS, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

arPelotas = subset(ar, ar$code_muni==4314407)

ggplot() + geom_sf(data = arPelotas, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

pelotas = read_municipality(code_muni = 4314407)

ggplot() + geom_sf(data = pelotas, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F) +
  geom_sf(data = arPelotas, fill="#3E4F60", color="#FFF000", size=.15,
          show.legend = F)

##########################################
# ?reas ind?genas
# O conjunto de dados abrange todo o Brasil e inclui terras ind?genas
# de todas as etnias e em diferentes est?gios de demarca??o.

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

ai = read_indigenous_land()

ggplot() + geom_sf(data= ai, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

aiRS = subset(ai, ai$abbrev_state == 'RS')

ggplot() + geom_sf(data = aiRS, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

library(leaflet)
map <- leaflet(aiRS)%>%addTiles()
map%>%addPolygons()

aiC = subset(aiRS, aiRS$name_muni == 'Ca?apava do Sul')

ggplot() + geom_sf(data = aiC, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

aiE = subset(aiRS, aiRS$etnia_nome=='Guaran?')

ggplot() + geom_sf(data = aiE, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

##########################################
# Unidade de sa?de

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

us = read_health_facilities()

usRS = subset(us, us$code_state == 43)
rs = read_municipality(code_muni = 43)

ggplot() + geom_sf(data = rs, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F) + 
  geom_sf(data = usRS, fill="#2D3E50", color="#FF0000", size=.25,
                     show.legend = F)

#Obs: C?digo munic?pio com 1 d?gito a menos
usB = subset(usRS, usRS$code_muni == 430160)

bg = subset(rs, rs$code_muni == 4301602)

ggplot() + geom_sf(data = bg, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F) + 
  geom_sf(data = usB, fill="#2D3E50", color="#FF0000", size=.25,
          show.legend = F)


##########################################
# Limites de Vizinhan?a
# conjunto de dados inclui os limites de vizinhan?a de 720 munic?pios brasileiros.
# ? baseado em agrega??es dos setores censit?rios do censo brasileiro.

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

lv = read_neighborhood()

lvRS = subset(lv, lv$code_state==43)

ggplot() + geom_sf(data = lvRS, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)


lvC = subset(lvRS, lvRS$name_muni=='Cangu?u')

ggplot() + geom_sf(data = lvC, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)


################################################################################
# Pr?tica com leaflet e geobr

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(colorspace)
library(ggplot2)
library(gganimate)
library(gifski)
library(leaflet)

rs = read_municipality(code_muni = 'RS', year=2018)
bg = subset(rs, rs$name_muni=='Bag?')

ggplot()+geom_sf(data = bg, fill="#810d0a", color="#810d0a", size=.15,
                 show.legend = F)


map = leaflet(rs) %>% addTiles()

map %>% addPolygons()

map %>% addPolygons(
  weight = 1,
  opacity = 0.5,
  color = "red",
  dashArray = "1",
  fillOpacity = 0)

sRS = read_health_facilities()
bgS = subset(sRS, sRS$code_muni==430160)

map = leaflet(bgS) %>% addTiles()
map %>% addMarkers()

ar = read_disaster_risk_area()
arRS = subset(ar, ar$abbrev_state=='RS')
map = leaflet(arRS) %>% addTiles()

map %>% addPolygons(
  weight = 1,
  opacity = 0.5,
  color = "red",
  dashArray = "1",
  fillOpacity = 0.5)
#
##########################################
# Fun??es complementares

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)


municipio = lookup_muni(name_muni = 'Bag?')

cIBGE = grid_state_correspondence_table

ba <- read_municipality(code_muni = "BA", year = 2018)

plot(ba)

##########################################
# Escolas
# Data comes from the School Census collected by INEP, the National 
# Institute for Educational Studies and Research "Anisio Teixeira".
# The date of the last data update is registered in the database in the 
# column 'date_update'. These data uses Geodetic reference system "SIRGAS2000"
# and CRS(4674). The coordinates of each school if collected by INEP. 
# Periodically the coordinates are revised with the objective of improving 
# the quality of the data. More information available at 
# http://portal.inep.gov.br/web/guest/dados/catalogo-de-escolas

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c <- list_geobr()

escolas <- read_schools()

bg <- subset(escolas, escolas$name_muni=="Bag?")

ggplot()+geom_sf(data = bg, fill="#810d0a", color="#810d0a", size=.15,
                 show.legend = F)

rs <- read_municipality(code_muni = 'RS', year = 2018)

bage <- subset(rs, rs$name_muni=="Bag?")

ggplot()+geom_sf(data = bage, fill="#2D3E50", color="#FEBF57", size=.15,
                 show.legend = F)+geom_sf(data = bg, fill="#810d0a", color="#810d0a", size=.15,
                 show.legend = F)

library(leaflet)

map <- leaflet(bg) %>% addTiles()

map %>% addMarkers()

##########################################
# CEP
# Zips codes in Brazil are known as CEP, the abbreviation for 
# postal code address. CEPs in Brazil are 8 digits long, with the 
# format 'xxxxx-xxx'.

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c <- list_geobr()

uf <- cep_to_state(cep = 69900000)

###########################################################################
# Inserindo Legendas nos Mapas

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)
#install.packages("ggrepel")
library(ggrepel)

rs <- read_municipality(code_muni = 'RS')

bage <- subset(rs, rs$name_muni=="Bagé")

ggplot(bage)+
  geom_sf(fill="#2D3E50", color="#FEBF57", size=.15,
                 show.legend = F)+
 geom_sf_label(aes(label=bage$name_muni), label.padding = unit(0.5,"mm"), size = 5)

estados <- read_state()

ggplot(estados)+
  geom_sf(fill="#2D3E50", color="#FEBF57", size=.15,
          show.legend = F)+
  geom_sf_label(aes(label=estados$abbrev_state), label.padding = unit(0.5,"mm"), size = 3)


head(estados)

ggplot(estados,aes(geometry = geom))+
  geom_sf(fill="#2D3E50", color="#FEBF57", size=.15,show.legend = F)+
  geom_point(stat = "sf_coordinates", color = "#FEBF57" )+
  geom_text_repel(aes(label = estados$abbrev_state),
                  stat = "sf_coordinates",
                  segment.curvature = 2e-10,
                  force = 2e3)

ggplot(estados,aes(geometry = geom))+
  geom_sf()+
  geom_point(stat = "sf_coordinates", color = "red" )+
  geom_text_repel(aes(label = estados$abbrev_state),
                  stat = "sf_coordinates",
                  segment.curvature = 1e-20,
                  force = 1e4)

