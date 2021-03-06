###

library(sf)
library(geobr)
library(magrittr)
library(dplyr)
library(colorspace)
library(ggplot2)
library(gganimate)
library(gifski)
library(leaflet)

rs <- read_municipality(code_muni = 'RS', year=2018)

ggplot(rs) + geom_sf(fill="#2D3E50", color = "#FEBF57", size=.15, show.legend = FALSE)

rs

#Retirar linhas
linhas <- c(1,2)

rs <- rs[-linhas,]

ggplot(rs) + geom_sf(fill="#2D3E50", color = "#FEBF57", size=.15, show.legend = FALSE)


casosRS <- covid19_367887fae3de4815b3414e3e9a3bff34

linhas <- c(499)

casosRS <- casosRS[-linhas,]

#Remover colunas
colunas<- c(3,4)

rs <- rs[,-colunas]

colunas<- c(1,2,4,7)

casosRS <- casosRS[,-colunas]

rsCasosCovid <- merge(rs, casosRS, by.x="code_muni", by.y="city_ibge_code")

map <- leaflet(rsCasosCovid) %>% addTiles()

map %>% addPolygons()

map %>% addPolygons(
  weight = 1,
  opacity = 0.5,
  color = "blue",
  dashArray = "1",
  fillOpacity = 0
)
#Bins e Colors
bins <- c(0,10,20,50,100,200,500,1000,Inf)
pal <- colorBin("YlOrRd", domain = rsCasosCovid$deaths, bins = bins)

map %>% addPolygons(
  fillColor = ~pal(deaths),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "1",
  fillOpacity = 0.7
)
#Add Interatividade
map %>% addPolygons(
  fillColor = ~pal(deaths),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "1",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray ="",
    fillOpacity = 0.7,
    bringToFront = TRUE))

#Customizando Informações

label <- sprintf(
  "<strong>%s</strong></br>%g Confirmados</br>%g Óbitos",
  rsCasosCovid$name_muni, rsCasosCovid$confirmed,rsCasosCovid$deaths
) %>% lapply(htmltools::HTML)

map %>% addPolygons(
  fillColor = ~pal(deaths),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "1",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray ="",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = label,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"
  ))
#Legenda

map %>% addPolygons(
  fillColor = ~pal(deaths),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "1",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray ="",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = label,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"
  )) %>% addLegend(pal = pal,
                   values = ~deaths,
                   opacity = 0.7,
                   title = "Casos de óbitos",
                   position = "bottomright")

  

