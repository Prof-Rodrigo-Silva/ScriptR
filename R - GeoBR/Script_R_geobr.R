# GeoBR

#Referência : https://www.rdocumentation.org/packages/geobr/versions/1.0
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
# Leia todas as áreas geográficas do país

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

# Leia todas as áreas geográficas de um estado com um determinado código
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
  ggtitle("Biomas Pampa e Mata Atlântica")

#Regiões, Áreas de Ponderação e Amazônia
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
                   show.legend = )F

r4 = r3[5:8,]

ggplot() + geom_sf(data = r4, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

r5 = read_amazon()
ggplot() + geom_sf(data = r5, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)


#Semiárido
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

#Setores Censitários
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

rr = read_census_tract(code_tract = 4301602, year = 2000, zone = "rural")
ggplot() + geom_sf(data = rr, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

ru = read_census_tract(code_tract = 4301602, year = 2000, zone = "urban")
plot(ru)
ggplot() + geom_sf(data = ru1, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

ru1 = ru[1:2,]
#Sedes municipais e grade de informações
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

bg = subset(sm, name_muni == "Bagé")

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
# Áreas metropolitanas
# A função retorna as formas dos municípios agrupados por suas
# respectivas áreas metropolitanas.
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
# Áreas urbanas
# Esta função lê os dados oficiais sobre a pegada urbana das cidades
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
# Áreas de conservação
# Seu conjunto de dados abrange todo o Brasil e inclui os polígonos de todas
# as unidades de conservação presentes no território brasileiro.
# A última atualização dos dados foi 09-2019

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

ac = read_conservation_units()

plot(ac)

ggplot() + geom_sf(data = ac, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

ac1 = subset(ac, ac$category=="Parque")

ggplot() + geom_sf(data = ac1, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

ac2 = subset(ac, ac$name_conservation_unit=="FLORESTA ESTADUAL DO ARAGUAIA")

ggplot() + geom_sf(data = ac2, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

###############################################
# Áreas de risco

# Esta função lê os dados oficiais das áreas de risco de desastre no Brasil.
# Ele se concentra especificamente em desastres geodinâmicos e hidrometeorológicos
# capazes de provocar deslizamentos de terra e inundações. O conjunto de dados
# abrange todo o país. Cada polígono da área de risco (conhecido como 'BATER') possui
# um ID de código exclusivo (coluna 'geo_bater'). 
# O conjunto de dados traz informações sobre a extensão em que os polígonos da área
# de risco se sobrepõem aos setores censitários e faces do bloco (coluna "acuracia") 
# e número de áreas de risco dentro de cada área de risco (coluna 'num').

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
# Áreas indígenas
# O conjunto de dados abrange todo o Brasil e inclui terras indígenas
# de todas as etnias e em diferentes estágios de demarcação.

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(ggplot2)

c = list_geobr()

ai = read_indigenous_land()

ggplot() + geom_sf(dat a= ai, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

aiRS = subset(ai, ai$abbrev_state == 'RS')

ggplot() + geom_sf(data = aiRS, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

aiC = subset(aiRS, aiRS$name_muni == 'Caçapava do Sul')

ggplot() + geom_sf(data = aiC, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

aiE = subset(aiRS, aiRS$etnia_nome=='Guaraní')

ggplot() + geom_sf(data = aiE, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)

##########################################
# Unidade de saúde

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

#Obs: Código município com 1 dígito a menos
usB = subset(usRS, usRS$code_muni == 430160)

bg = subset(rs, rs$code_muni == 4301602)

ggplot() + geom_sf(data = bg, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F) + 
  geom_sf(data = usB, fill="#2D3E50", color="#FF0000", size=.25,
          show.legend = F)


##########################################
# Limites de Vizinhança
# conjunto de dados inclui os limites de vizinhança de 720 municípios brasileiros.
# É baseado em agregações dos setores censitários do censo brasileiro.

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


lvC = subset(lvRS, lvRS$name_muni=='Canguçu')

ggplot() + geom_sf(data = lvC, fill="#2D3E50", color="#FEBF57", size=.15,
                   show.legend = F)


################################################################################
# Prática com leaflet e geobr

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



