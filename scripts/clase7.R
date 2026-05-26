# 0 Seteamos ####

## librerias ###
library(tidyverse)
library(sf)
library(leaflet)

options(scipen = 999)

# 1 Cargamos datos ####

# DELITOS CABA
df_delitos <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/delitos/delitos_2023.csv",
                       encoding = "UTF-8")

# COMISARÍAS CABA
df_comisarias <- read.csv("https://data.buenosaires.gob.ar/dataset/comisarias-policia-ciudad/resource/juqdkmgo-591-resource/download",
                          encoding = "UTF-8")
# https://data.buenosaires.gob.ar/dataset/comisarias-policia-ciudad

# BARRIOS DE CABA (polígonos)
# nuevo tipo de archivo: .geojson
df_barrios <- st_read("https://data.buenosaires.gob.ar/dataset/barrios/resource/1c3d185b-fdc9-474b-b41b-9bd960a3806e/download")

# LÍNEAS DE SUBTE
# nuevo tipo de archivo: .shp
df_subte <- read_sf("./data/clase7/subterraneo-lineas/Red_de_subterraneo.shp")

# 2 Datos Georreferenciados: un nuevo tipo de dato ####

## Proyecciones ####

# los datos georreferenciados tienen el atributo de la proyección, si bien son numéricos, requieren de un sistema de coordenadas
# que nos permita realizar diferentes operaciones algebraicas

# https://rpubs.com/HAVB/geoinfo

# https://truesize.net/

# https://fronterasblog.com/2019/06/25/visualizando-la-distorsion-de-la-proyeccion-de-mercator-con-una-naranja/


# sistema de referencia de coordenadas
st_crs(df_barrios)
st_crs(df_subte)

# los objetos sf tienen geometría + proyección,
# un csv es una tabla, sin info de proyección no es dato georref 

# los csv son tablas normales
class(df_delitos)
class(df_comisarias)

# no tienen geometría
st_crs(df_delitos)
st_crs(df_comisarias)

# aunque tengan latitud y longitud,
# siguen siendo tablas hasta convertirlas en objetos espaciales


## Convertimos a sf ####

# delitos
df_delitos_sf <- df_delitos %>%
  st_as_sf(
    coords = c("longitud", "latitud"), # columnas con coordenadas
    crs = 4326, # WGS84
    na.fail = F
  )

# comisarías
df_comisarias_sf <- st_as_sf(
  df_comisarias,
  wkt = "geometry", # columna con geometría en texto
  crs = 4326 # sistema de coordenadas
)

# chequeamos
st_crs(df_delitos_sf)
st_crs(df_comisarias_sf)

## 2.1 Tipos de geometrías ####

## 2.1.1 Puntos ####

# delitos y comisarías son puntos
st_geometry_type(df_delitos_sf)
st_geometry_type(df_comisarias_sf)

## 2.1.2 Líneas ####

# líneas de subte
st_geometry_type(df_subte)

## 2.1.3 Polígonos ####

# barrios
st_geometry_type(df_barrios)

## 2.1.4 Multipoligonos ####

# 3 Visualizaciones ####

## 3.1 Primer mapa  ####

p1_barrios <- ggplot(df_barrios)+
  geom_sf()
p1_barrios

## 3.2 Coropletas ####
df1_cantidad_delitos <- df_delitos %>%
  mutate(
    barrio = case_when(barrio == "BOCA" ~ "LA BOCA",
                       TRUE ~ barrio)
  )%>%
  group_by(barrio)%>%
  summarise(cantidad_delitos = n())

df1_cantidad_delitos <- df1_cantidad_delitos%>%
  left_join(df_barrios%>%mutate(nombre = str_to_upper(nombre)),
            by = c("barrio" = "nombre"))%>%
  st_as_sf()

p1_coropleta <- ggplot(df1_cantidad_delitos) +
  geom_sf(
    aes(fill = cantidad_delitos),
    color = "white",
    linewidth = 0.2
  ) +
  # scale_fill_binned(type = "viridis")+ # escala segmentada
  # scale_fill_distiller(
  #   palette = "Reds",
  #   direction = 1,
  #   name = "Cantidad\nde delitos"
  # ) +
  # scale_fill_gradientn(#colours = c("yellow", "orange", "red", "darkred"),
  #                      colours = c("gray", "blue", "orange", "darkgreen"),
  #                      values = c(0, 0.25, 0.40, 0.95, 1)) + # escala manual
  scale_fill_viridis_c(option = "magma", direction = -1)+
  labs(
    title = "Cantidad de delitos registrados por barrio",
    subtitle = "Ciudad Autónoma de Buenos Aires - 2023",
    caption = "Fuente: Datos Abiertos CABA"
  ) +
  theme_void()
p1_coropleta

# podemos jugar con la forma de colorear aplicando otras medidas o cálculos
df1_cantidad_delitos <- df1_cantidad_delitos%>%
  mutate(
    decil = ntile(x = cantidad_delitos, n = 10)
  )

p1_coropleta_b <- ggplot(df1_cantidad_delitos) +
  geom_sf(
    aes(fill = decil),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_distiller(
    palette = "Reds",
    direction = 1,
    name = "Decil cantidad de delitos"
  ) +
  labs(
    title = "Cantidad de delitos registrados por barrio",
    subtitle = "Ciudad Autónoma de Buenos Aires - 2023",
    caption = "Fuente: Datos Abiertos CABA"
  ) +
  theme_void()
p1_coropleta_b

## 3.3 Superposición de capas ####

p2_capas <- ggplot() +
  geom_sf(
    data = df1_cantidad_delitos,
    aes(fill = cantidad_delitos),
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = df_subte,
    color = "blue",
    linewidth = 1,
    alpha = 0.8
  ) +
  # geom_sf(
  #   data = df_comisarias_sf,
  #   shape = 24,
  #   fill = "red",
  #   color = "white",
  #   size = 2
  # ) +
  geom_sf_text(
    data = df_comisarias_sf,
    label = "\U0001F46E", # emojis en UNICODE
    size = 4
  )+
  scale_fill_viridis_c(
    option = "magma",
    name = "Cantidad de delitos",
    direction =  -1
  ) +
  labs(
    title = "Delitos, comisarías y red de subterráneos",
    subtitle = "Ejemplo de superposición de capas espaciales"
  ) +
  theme_void() 
p2_capas

## 3.4 Densidad espacial ####

df_delitos_coords <- df_delitos_sf %>%
  cbind(st_coordinates(.)) %>% # separamos la variable geométrica pero sin perder sistema de coordenadas
  filter(
    X != 0,
    Y != 0,
    barrio != "NULL" # eliminamos puntos que están fuera de CABA
  )

# https://ggplot2.tidyverse.org/reference/geom_density_2d.html # documentacion

p3_densidad <- ggplot()+
  geom_sf(data = df_barrios)+
  geom_density_2d_filled(data = df_delitos_coords,
                         aes(x = X, y = Y),
                         alpha = 0.5,
                         n = 25)+
  geom_density_2d(data = df_delitos_coords, 
                  aes(x = X, y = Y), 
                  col = "white",
                  alpha = 0.8)+
  labs(
    title = "Densidad espacial de delitos",
    subtitle = "Ciudad Autónoma de Buenos Aires - 2023",
    fill = "Densidad"
  ) +
  theme_minimal()
p3_densidad

# 4 Operaciones espaciales ####

## 4.1 Calcular area ####

# área de los barrios
df_barrios <- df_barrios %>% 
  mutate(area = st_area(df_barrios))

# tengo un error en el df, chequeo cuáles polígonos dan error
df_barrios <- df_barrios %>%
  mutate(poligono_valido = st_is_valid(df_barrios))

# corrijo y vuelvo a chequear
df_barrios <- st_make_valid(df_barrios)%>%
  mutate(poligono_valido = st_is_valid(df_barrios))

# ahora sí, calculo área
df_barrios <- df_barrios %>% 
  mutate(area = st_area(df_barrios))

## 4.2 Calcular perimetro ####

# obtenemos bordes
bordes <- st_boundary(df_barrios)

# calculamos longitud del borde
df_barrios <- df_barrios %>%
  mutate(perimetro = st_length(bordes))

## 4.3 Calcular centroide ####
# centro geométrico de cada barrio
centroides <- st_centroid(df_barrios)

p4_centroides <- ggplot() + 
  geom_sf(data = df_barrios) +
  geom_sf(data = centroides) +
  theme_minimal() + 
  labs(
    title = "Centro geométrico de cada barrio"
  )
p4_centroides

## 4.4 Buffers: área de influencia ####

# IMPORTANTE:
# para trabajar con distancias reales
# transformamos a una proyección en metros

df_comisarias_metros <- st_transform(df_comisarias_sf, 5347)

# buffer de 1200 metros (algo así como 12 cuadras)
buffer_comisarias <- st_buffer(
  df_comisarias_metros,
  dist = 900 # algo así como 12 cuadras
)

p5_buffer <- ggplot() +
  geom_sf(data = df_barrios) +
  # geom_sf(data = df_comisarias_metros)+
  geom_sf(data = buffer_comisarias, alpha = 0.5)
p5_buffer

## 4.5 Join espacial ####

# transformamos delitos a misma proyección
df_delitos_metros <- st_transform(df_delitos_sf, 5347)

# delitos dentro del buffer de cada comisaría
df_delitos_buffer <- st_join(
  df_delitos_metros,
  buffer_comisarias,
  join = st_within, 
  suffix = c("", "_buffer"))

df_delitos_buffer

## cantidad de delitos por comisaría 

delitos_comisaria <- df_delitos_buffer %>%
  st_drop_geometry() %>% # volvemos a df 
  group_by(nombre) %>%
  summarise(cantidad_delitos = n())


# 5 Visualizaciones interactivas ####

leaflet()%>%
  addTiles()

leaflet()%>%
  addProviderTiles("Esri.WorldImagery")%>%
  setView(lng = -58.445531, lat = -34.606653, zoom = 11)

#https://leaflet-extras.github.io/leaflet-providers/preview/

# providers$NASAGIBS.ViirsEarthAtNight2012

## 5.1 Coropletas ####
library(htmltools)

# creamos función para colorear
pal <- colorBin(
  palette = "Purples",
  domain = df1_cantidad_delitos$cantidad_delitos,
  n = 5
)

# armamos el mapa
m1_coropleta <- leaflet(df1_cantidad_delitos)%>%
  addProviderTiles("CartoDB.Positron")%>%
  addPolygons()
m1_coropleta

# coloreamos los polígonos
m1_coropleta <- leaflet(df1_cantidad_delitos)%>%
  addProviderTiles("CartoDB.Positron") %>% # fondo del mapa
  addPolygons(
    fillColor = ~pal(cantidad_delitos),
    fillOpacity = 0.8,
    color = "white",
    weight = 1)
m1_coropleta

# agregamos popup
m1_coropleta <- leaflet(df1_cantidad_delitos)%>%
  addProviderTiles("CartoDB.Positron") %>% # fondo del mapa
  addPolygons(
    fillColor = ~pal(cantidad_delitos),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    popup = ~paste0(
      "<b>Barrio:</b> ", barrio,
      "<br><b>Delitos:</b> ", cantidad_delitos
    )
  ) 
m1_coropleta

# cambiamos popup y agregamos label

df1_cantidad_delitos <- df1_cantidad_delitos%>%
  mutate(popup_mapa = lapply(
    paste0(
      "<b>Barrio:</b>", barrio,
      "<br>",
      "<b>Comuna:</b>", comuna,
      "<br>",
      "<b>Cantidad de delitos:</b>", cantidad_delitos),
    HTML
    )
  )

m1_coropleta <- leaflet(df1_cantidad_delitos)%>%
  addProviderTiles("CartoDB.Positron") %>% # fondo del mapa
  addPolygons(
    fillColor = ~pal(cantidad_delitos),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    
    popup = ~popup_mapa,
    
    label = ~paste0(barrio)
    )
m1_coropleta

# último toque 

m1_coropleta <- leaflet(df1_cantidad_delitos)%>%
  addProviderTiles("CartoDB.Positron") %>% # fondo del mapa
  addPolygons(
    
    fillColor = ~pal(cantidad_delitos),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    
    popup = ~popup_mapa,
    
    label = ~paste0(barrio),
    
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      fillOpacity = 1,
      bringToFront = TRUE)
  )
m1_coropleta

# agregamos referencia
m1_coropleta <- m1_coropleta%>%
  addLegend(
    pal = pal,
    values = ~cantidad_delitos,
    title = "Cantidad de delitos"
  )
m1_coropleta


## 5.2 Íconos ####

icono_comisaria <- makeIcon(
  iconUrl = "./varios/police-station.png",
  iconWidth = 15,
  iconHeight = 15,
  iconAnchorX = 15,
  iconAnchorY = 15
)

m2_iconos <- leaflet(df_comisarias_sf) %>%
  addTiles() %>%
  addMarkers(
    icon = icono_comisaria,
    popup = ~nombre
  )
m2_iconos

## 5.3 Capas ####

# vamos a juntar ambos mapas: la coropleta y las comisarias

m3_capas <- leaflet() %>%
  # mapa base
  addProviderTiles("CartoDB.Positron",
                   group = "CartoDB") %>%
  # coropleta
  addPolygons(
    data = df1_cantidad_delitos,
    
    fillColor = ~pal(cantidad_delitos),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    
    popup = ~popup_mapa,
    label = ~barrio,
    
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      fillOpacity = 1,
      bringToFront = TRUE
    ),
    
    group = "Delitos por barrio" # para poder seleccionar el mapa
  ) %>%
  
  # comisarías
  addMarkers(
    data = df_comisarias_sf,
    icon = icono_comisaria,
    popup = ~paste0(
      "<b>", nombre, "</b><br>",
      direccion
    ),
    group = "Comisarías" # para poder seleccionar el mapa
  ) %>%
  
  # leyenda
  addLegend(
    pal = pal,
    values = ~cantidad_delitos,
    data = df1_cantidad_delitos,
    title = "Cantidad de delitos"
  ) %>%
  
  # selector de capas
  addLayersControl(
    overlayGroups = c(
      "Delitos por barrio",
      "Comisarías"
    ),
    
    options = layersControlOptions(
      collapsed = FALSE
    )
  )
m3_capas

## 5.4 Mapa de calor ####
library(leaflet.extras)

m4_heatmap <- leaflet(df_delitos_coords) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~X,
    lat = ~Y,
    blur = 25,
    radius = 10
  )
m4_heatmap

## 5.5 Guardar mapa interactivo ####
#install.packages("htmlwidgets")
htmlwidgets::saveWidget(mapa,
                        "./varios/mapa.html",
                        selfcontained = T)
