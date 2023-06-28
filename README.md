# Tarea-3-PDG
---
title: "Tarea 3 PDG Riqueza de mamiferos por regiones socioeconómicas"
author: "Daniela Hidalgo y Ferdy Salazar"
format: 
 html:
  theme: sketchy
  toc: true
  toc_floot: true
editor: visual
lang: es
---

# 1.Introducción:

En esta tarea para el curso *GF-0604 Procesamiento de datos Geograficoss*, se usaran dos archivos de datos. El primero es un archivo geojson de [regiones Socioeconomicas de Costa Rica](https://repositoriotec.tec.ac.cr/handle/2238/6749?show=full), cuya fuente es el **Atlas Digital de Costa Rica de 2014**. El segundo archivo, es un csv de [Presencia y observación de mamiferos en Costa Rica](https://www.gbif.org/occurrence/download/0031158-230530130749713), estos se obtuvieron en el portal de datos de observaciones de **GBIF**.

Con los archivos de datos mencionados anteriormente será posible una mejor compresión de la distribucción y presencia de mamiferos en las regiones socioeconómicas del país: Region Central, Brunca, Chorotega, Pacífico Central, Huetar Norte, Huetar Atlántica.


# 2.Carga de paquetes:

```{r}
#| label: carga-paquetes
#| warning: false
#| code-fold: true 
#| message: false
 
library(tidyverse)
library(DT)
library(sf)
library(rgdal)
library(raster)
library(terra)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(viridis)
library(devtools)
```

# 3.Carga de datos:

```{r}
#| label: carga-datos
#| warning: false
#| code-fold: true 
#| message: false

###Regiones

regiones <- 
  st_read("datos/regiones.geojson", quiet = TRUE)

###Mamiferos

mamiferos <-
  st_read(
    "datos/mamiferos.csv.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude", # columna de longitud decimal
      "Y_POSSIBLE_NAMES=decimalLatitude"   # columna de latitud decimal
    ),
    quiet = TRUE
  )

# Cambio de sistema de coordenadas
regiones <-
  regiones |>
  st_transform(4326)

st_crs(mamiferos) <- 4326

```

# 4.Uniones

```{r}
#| label: carga-uniones-uno
#| warning: false
#| code-fold: true 
#| message: false


mamiferos_union_region <-
  st_join(
    x = mamiferos,
    y = dplyr::select(regiones, region),
    join = st_within
  )

```

```{r}
#| label: carga-uniones-dos
#| warning: false
#| code-fold: true 
#| message: false

riqueza_especies_mamiferos <-
  mamiferos_union_region |>
  st_drop_geometry() |>
  group_by(region) |>
  summarise(riqueza_especies_mamiferos = n_distinct(species, na.rm = TRUE))

```

```{r}
#| label: carga-uniones-tres
#| warning: false
#| code-fold: true 
#| message: false

region_union_riqueza <-
  left_join(
    x = regiones,
    y = riqueza_especies_mamiferos,
    by = "region"
  ) |>
  replace_na(list(riqueza_especies_mamiferos = 0))
```

# 5.Mapa de riqueza de especies de mamíferos en regiones socioeconómicas

```{r}
#| label: carga-mapa
#| warning: false
#| code-fold: true 
#| message: false

# Paleta de colores de riqueza de mamiferos
colores_riqueza_especies <-
  colorNumeric(
    palette = "blue",
    domain = region_union_riqueza$riqueza_especies_mamiferos,
    na.color = "transparent"
  )

# Paleta colores de especies

colores_especies <- colorFactor(
  palette = viridis(length(unique(mamiferos$species))), 
  domain = mamiferos$species
)

# Mapa leaflet

leaflet() |>
  setView(
    lng = -84.19452,
    lat = 9.572735,
    zoom = 7) |>
  addTiles(group = "Mapa general (OpenStreetMap)") |>
  addProviderTiles(
    providers$Esri.WorldImagery, 
    group = "Imágenes satelitales (ESRI World Imagery)"
  ) |> 
  addPolygons(
    data = region_union_riqueza,
    fillColor = ~ colores_riqueza_especies(region_union_riqueza$riqueza_especies_mamiferos),
    fillOpacity = 0.8,
    color = "black",
    stroke = TRUE,
    weight = 1.0,
    popup = paste(
      paste("<strong>Riqueza de mamiferos:</strong>", region_union_riqueza$riqueza_especies_mamiferos),
      sep = '<br/>'
    ),
    group = "Riqueza de mamiferos"
  ) |>
  addScaleBar(
    position = "bottomleft", 
    options = scaleBarOptions(imperial = FALSE)
  ) |>    
  addLegend(
    position = "bottomleft",
    pal = colores_riqueza_especies,
    values = region_union_riqueza$riqueza_especies_mamiferos,
    group = "Riqueza de mamiferos",
    title = "Riqueza de mamiferos"
  ) |>
  addCircleMarkers(
    data = mamiferos,
    stroke = F,
    radius = 4,
    fillColor = ~colores_especies(mamiferos$species),
    fillOpacity = 1.0,
    popup = paste(
      paste0("<strong>Especie: </strong>", mamiferos$species),
      paste0("<strong>Localidad: </strong>", mamiferos$locality),
      paste0("<strong>Fecha: </strong>", mamiferos$eventDate),
      paste0("<strong>Fuente: </strong>", mamiferos$institutionCode),
      paste0("<a href='", mamiferos$occurrenceID, "'>Más información</a>"),
      sep = '<br/>'
    ),    
    group = "Registros de presencia"
  ) |>  
  addLayersControl(
    baseGroups = c(
      "Mapa general (OpenStreetMap)", 
      "Imágenes satelitales (ESRI World Imagery)"
    ),
    overlayGroups = c(
      "Riqueza de mamiferos",
      "Registros de presencia"
    )
  ) |>
  addResetMapButton() |>
  addSearchOSM() |>
  addMouseCoordinates() |>
  addFullscreenControl() |>
  hideGroup("Registros de presencia") 
```
# 6.Tabla de riqueza de especies de mamíferos en regiones socioeconómicas

```{r}
#| label: carga-tabla
#| warning: false
#| code-fold: true 
#| message: false

riqueza_especies_mamiferos |>
  dplyr::select(region, riqueza_especies_mamiferos) |>
  datatable(
    colnames = c("Nombre de la región socioeconómica", "Riqueza de especies de mamíferos"),
    options = list(
      pageLength = 5,
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
    ))

```
# 7.Gráficos estadísticos:

# 7.1.Gráfico de barras de riqueza de especies de mamíferos en regiones socioeconómicas

```{r}
#| label: carga-grafico-riqueza
#| warning: false
#| code-fold: true 
#| message: false
grafico_mamiferos_region <-
riqueza_especies_mamiferos |>
  ggplot(aes(x = reorder(region,-riqueza_especies_mamiferos), y = riqueza_especies_mamiferos)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Riqueza de mamíferos en regiones socioeconómicas") +
  xlab("Regiones socioeconómicas") +
  ylab("Riqueza de mamíferos")+
  labs(caption = "Fuente: Ministerio de Planificación (MIDELAN)") +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(grafico_mamiferos_region)
```
# 7.2.Gráfico de barras de cantidad de registros de presencia de Bradypus variegatus (perezoso de tres dedos) por año, desde 2000 hasta 2023.

```{r}
#| label: carga-grafico-cantidad
#| warning: false
#| code-fold: true 
#| message: false

perezosos_3dedos <-
mamiferos_union_region |>
  filter(year >= 2000) |>
  filter(species == "Bradypus variegatus") |>
  ggplot(aes(x = year)) +
  geom_bar() +
  ggtitle("Registro de presencia del Bradypus variegatus (perezoso de tres dedos) 
desde el año 2000 hasta el 2023.") +
  xlab("Año") +
  ylab("Cantidad de perezosos de tres dedos") +
  theme_gray()

ggplotly(perezosos_3dedos)


```
