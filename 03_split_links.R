library(rgdal)
library(lubridate)
library(tidyverse)
library(reshape2)
library(lwgeom)
library(polylineSplitter)
library(sf)
library(mapview)
library(birk)
library(stplanr)
library(raster)
library(data.table)
library(leaflet)
library(htmlwidgets)
library(sp)

select <- dplyr::select

##set the code for ukgrid and lat lon conversions
rdnew = "+init=epsg:28992"
latlong <- "+init=epsg:4326"

select <- dplyr::select

##Enter the total width to scan in metres
Width2Scope <- 20
##Enter the start point in metres from the road centre line
StartPoint <- 0.5
#Enter the interval in metres to scan the road
Interval <- 0.5

# #VGT create -------------------------------------------------------------

#Load in model A version of the network

speed_links <- readRDS("out/road_network.RDS") %>% 
  select(osm_id, geometry) %>% 
  mutate(lengt = st_length(geometry))



all_routes <- readRDS("out/all_routes_10.RDS")
all_routes_buff <- all_routes %>% 
  st_transform(rdnew) %>% 
  st_buffer(2) %>% 
  st_transform(latlong)

sp_links <- speed_links[all_routes_buff,]



links_routes <- st_intersects(speed_links, all_routes)

lr <- melt(do.call(rbind, links_routes))
lr <- unique(lr$value)

sp_links <- speed_links[lr,]
mapview(all_routes, color = "red")+mapview(sp_links, color = "green")
##SPLIT INTO SMALLER LINKS FOR ADVANCED CANYON
##advanced canyon links

##MAX LINK LENGTH
MAX_AC <- 50
v <- "273144482"
v <- "7166925"
v <- "7166832"
v <- vs[1]
vs <- unique(sp_links$osm_id)
ac_links <- list()
for (v in vs) {
  sl <- filter(speed_links, osm_id == v)
  sl <- st_transform(sl, rdnew)
  len <- as.numeric(st_length(sl))
  
  if(len < MAX_AC){
    sl <- sl %>% as("Spatial") %>% 
      spTransform(latlong)
    sl <- st_as_sf(sl, "latlong")
    sl <- data.frame(st_coordinates(sl))
    sl$L2 <- sl$L1
    sl$XY <- paste0(sl$L2,"_", sl$X, "_", sl$Y)
  } 
  
  if(len >= MAX_AC & len <= MAX_AC*2){
    n_pts <- ceiling(len/5)
    df1 <- st_line_sample(sl, sample = 0)
    df2 <- st_line_sample(sl, n = n_pts)
    df3 <- st_line_sample(sl, sample = 1)
    df1 <- data.frame(st_coordinates(df1))
    df2 <- data.frame(st_coordinates(df2))
    df3 <- data.frame(st_coordinates(df3))
    df_out <- rbind(df1, df2, df3)
    
    sl <- st_as_sf(df_out, coords = c("X", "Y"), crs = rdnew)
    sl <- sl %>% group_by(L1) %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
    sl <- sl %>% as("Spatial") %>% 
      polylineSplitter::splitLines(dist = len/2) ##default distance in 200m, change to split for different lengths
    proj4string(sl) = CRS(rdnew)
    sl <- spTransform(sl, latlong)
    sl <- st_as_sf(sl, "latlong")
    sl <- data.frame(st_coordinates(sl))
    #sl$L2 <- sl$L1
    sl$XY <- paste0(sl$L2,"_", sl$X, "_", sl$Y)
  }
  
  if(len > MAX_AC*2){
    n_pts <- ceiling(len/5)
    df1 <- st_line_sample(sl, sample = 0)
    df2 <- st_line_sample(sl, n = n_pts)
    df3 <- st_line_sample(sl, sample = 1)
    df1 <- data.frame(st_coordinates(df1))
    df2 <- data.frame(st_coordinates(df2))
    df3 <- data.frame(st_coordinates(df3))
    df_out <- rbind(df1, df2, df3)
    
    sl <- st_as_sf(df_out, coords = c("X", "Y"), crs = rdnew)
    sl <- sl %>% group_by(L1) %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
    len_20 <- ceiling(len/MAX_AC)
    #if(len_20<4){len_20 <- 4}
    sl <- sl %>% as("Spatial") %>% 
      polylineSplitter::splitLines(dist = len/len_20) ##default distance in 200m, change to split for different lengths
    proj4string(sl) = CRS(rdnew)
    sl <- spTransform(sl, latlong)
    sl <- st_as_sf(sl, "latlong")
    sl <- data.frame(st_coordinates(sl))
    #sl$L2 <- sl$L1
    sl$XY <- paste0(sl$L2,"_", sl$X, "_", sl$Y)
  }
  
  sl <- distinct(sl, XY, .keep_all = TRUE)
  
  ##convert back to sf
  sl <- st_as_sf(sl, coords = c("X", "Y"), crs = latlong)
  sl <- sl %>% group_by(L2) %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
  sl$Source.name <- v
  ##generate an Advanced Canyon Name
  sl$ACN <- paste0(sl$Source.name, "_", LETTERS[seq( from = 1, to = NROW(sl) )])
  ac_links[[v]] <- sl
  print(v)
}

AC_links <- do.call(rbind, ac_links)
AC_links <- select(AC_links, Source.name, ACN, geometry)

##MAX LINK LENGTH
MAX_R <- 20
v <- "273144482"
v <- "7166925"
v <- "7166832"
v <- "716683"
vs <- unique(all_routes$routeID)
r_links <- list()
for (v in vs) {
  sl <- filter(all_routes, routeID == v)
  sl <- st_transform(sl, rdnew)
  len <- as.numeric(st_length(sl))
  
  if(len < MAX_AC){
    sl <- sl %>% as("Spatial") %>% 
      spTransform(latlong)
    sl <- st_as_sf(sl, "latlong")
    sl <- data.frame(st_coordinates(sl))
    sl$L2 <- sl$L1
    sl$XY <- paste0(sl$L2,"_", sl$X, "_", sl$Y)
  } 
  
  if(len >= MAX_AC & len <= MAX_AC*2){
    n_pts <- ceiling(len/5)
    df1 <- st_line_sample(sl, sample = 0)
    df2 <- st_line_sample(sl, n = n_pts)
    df3 <- st_line_sample(sl, sample = 1)
    df1 <- data.frame(st_coordinates(df1))
    df2 <- data.frame(st_coordinates(df2))
    df3 <- data.frame(st_coordinates(df3))
    df_out <- rbind(df1, df2, df3)
    
    sl <- st_as_sf(df_out, coords = c("X", "Y"), crs = rdnew)
    sl <- sl %>% group_by(L1) %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
    sl <- sl %>% as("Spatial") %>% 
      polylineSplitter::splitLines(dist = len/2) ##default distance in 200m, change to split for different lengths
    proj4string(sl) = CRS(rdnew)
    sl <- spTransform(sl, latlong)
    sl <- st_as_sf(sl, "latlong")
    sl <- data.frame(st_coordinates(sl))
    #sl$L2 <- sl$L1
    sl$XY <- paste0(sl$L2,"_", sl$X, "_", sl$Y)
  }
  
  if(len > MAX_AC*2){
    n_pts <- ceiling(len/5)
    df1 <- st_line_sample(sl, sample = 0)
    df2 <- st_line_sample(sl, n = n_pts)
    df3 <- st_line_sample(sl, sample = 1)
    df1 <- data.frame(st_coordinates(df1))
    df2 <- data.frame(st_coordinates(df2))
    df3 <- data.frame(st_coordinates(df3))
    df_out <- rbind(df1, df2, df3)
    
    sl <- st_as_sf(df_out, coords = c("X", "Y"), crs = rdnew)
    sl <- sl %>% group_by(L1) %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
    len_20 <- ceiling(len/MAX_AC)
    #if(len_20<4){len_20 <- 4}
    sl <- sl %>% as("Spatial") %>% 
      polylineSplitter::splitLines(dist = len/len_20) ##default distance in 200m, change to split for different lengths
    proj4string(sl) = CRS(rdnew)
    sl <- spTransform(sl, latlong)
    sl <- st_as_sf(sl, "latlong")
    sl <- data.frame(st_coordinates(sl))
    #sl$L2 <- sl$L1
    sl$XY <- paste0(sl$L2,"_", sl$X, "_", sl$Y)
  }
  
  sl <- distinct(sl, XY, .keep_all = TRUE)
  
  ##convert back to sf
  sl <- st_as_sf(sl, coords = c("X", "Y"), crs = latlong)
  sl <- sl %>% group_by(L2) %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
  sl$Source.name <- v
  ##generate an Advanced Canyon Name
  sl$ACN <- paste0(sl$Source.name, "_", LETTERS[seq( from = 1, to = NROW(sl) )])
  r_links[[v]] <- sl
}

route_links <- do.call(rbind, r_links)
route_links <- select(AC_links, ACN, geometry)

saveRDS(AC_links, "out/split_links.RDS")

mapview(AC_links)
saveRDS(AC_links, "01_Model/OUT/advanced_canyon_links.RDS")

lat <- 51.387383
lon <- -2.822237


write.csv(gradients, "Gradients.csv", row.names = FALSE)

pal_L <- colorNumeric("viridis", domain = g_sf$gradient)

g_sf <- mutate(g_sf, cntnt=paste0('<strong>Link #: </strong>',L1,
                                        '<br><strong>Gradient: </strong> ', gradient,
                                        '<br><strong>Length: </strong> ', length))

m <- leaflet() %>% 
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri Satellite") %>%
    addProviderTiles("OpenStreetMap", group = "Open Street Map") %>%
    setView(lon, lat, zoom = 13)

ugs <- as.character(unique(g_sf$L1))

for (g in ugs){
df <- filter(g_sf, L1 == g)
  m <- m %>% addPolylines(data = df, color = ~pal_L(gradient),
                         opacity = 1.0, fillOpacity = 0.3, popup = ~as.character(cntnt),
                         highlightOptions = highlightOptions(color = "white", weight = 0.1,
                                                             bringToFront = FALSE), group = g)
}
  m <- m %>% addLegend(position = c("bottomleft"), labels = "Gradient %",
    pal = pal_L, values = g_sf$gradient)


  m <- m %>% addLayersControl(overlayGroups = ugs,
                              baseGroups = c("CartoDB", "Esri Satellite", "Open Street Map"),
                              options = layersControlOptions(collapsed = FALSE), position = "topright")

  #m <- m %>% hideGroup(c("Raster grid"))

  withr::with_dir('./', saveWidget(m, file = paste0("roads.html")))

