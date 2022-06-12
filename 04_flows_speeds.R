library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(osmdata)
library(osrm)
library(lwgeom)
library(NISTunits)
library(stringr)
library(reshape2)
library(stplanr)
library(knitr)
library(tmap)
library(pals)

select <- dplyr::select

##define coordinate systems
latlong = "+init=epsg:4326"
rdnew = "+init=epsg:28992"

snapped_pts <- readRDS("out/snapped_pts.RDS")
snapped_rds <- readRDS("out/snapped_rds.RDS")
df_sf <- readRDS("out/original_pts.RDS")
load("out/all_routes.RData")
load("out/moves.RData")
rd_network <- readRDS("out/road_network.RDS") %>% 
  select(osm_id, geometry)

ar <- all_routes
st_geometry(ar) <- NULL
uroute <- unique(ar$routeID)
all_rh <- list()
for (u in uroute){
  
  
  df <- data.frame(hour = 0:23)
  df$routeID <- u
  all_rh[[u]] <- df
}

all_routes_hours <- do.call(rbind, all_rh)
ar_hours <- left_join(all_routes_hours, ar, by = "routeID")

moves_dat <- ar_hours %>% 
  left_join(moves_hr, by = c("routeID" = "ID", "hour"))

moves_dat$total[is.na(moves_dat$total)] <- 0

moves_dat <- transmute(moves_dat, routeID, hour, total)
tots <- moves_dat$total
IDs <- moves_dat$routeID
hrz <- moves_dat$hour

movez <- data.frame('vehicles' = tots, IDs, hrz)
movez <- left_join(movez, all_routes, by = c("IDs" = "routeID"))
st_geometry(movez) <- movez$geometry
#movez <- filter(movez, tots > 1)

movez_day <- left_join(moves_day, all_routes, by = c('ID' = 'routeID'))
st_geometry(movez_day) <- movez_day$geometry
rnet_tot <- overline(movez_day, attrib = "total", ncores = 4)

ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

max_tot <- ceiling_dec(max(rnet_tot$total), -3)

me_pal <- c("#0000b3", "#0000eb", "#1d00ff", "#4a00ff", "#7600ff", "#a211ee", "#cf2ed1", "#fb4ab5", 
            "#ff6798", "#ff837c", "#ff9f60", "#ffbc43", "#ffd827", "#fff50a")

bks_day <- seq(from = 0, to = max_tot, by = max_tot/NROW(me_pal))

bks_hr <- seq(from = 0, to = 3000, by = 3000/NROW(me_pal))

## trim road network
route_coords <- st_coordinates(movez_day)

##create min x and y of camera area
x_min <- min(route_coords[,1])
x_max <- max(route_coords[,1])
y_min <- min(route_coords[,2])
y_max <- max(route_coords[,2])

##create a data frame to setup polygon generation
df <- data.frame(X = c(x_min, x_max, x_max, x_min), 
                 Y = c(y_max, y_max, y_min, y_min))

##generate a polygon of the area
route_area <- df %>%
  st_as_sf(coords = c("X", "Y"), crs = latlong) %>%
  dplyr::summarise(data = st_combine(geometry)) %>%
  st_cast("POLYGON")

rd_network <- rd_network[route_area,]

tm1 <- tm_shape(rnet_tot) +
  tm_lines(palette = me_pal, breaks = bks_day,
           lwd = 4,
           alpha = 0.6,
           col = "total")+
  tm_layout("total daily trips", legend.position = c("right", "bottom"),frame = FALSE)+
  tm_scale_bar(position = c("left", "bottom"))

tm1

tmap_save(tm1, "out/plots/tm1.png")

saveRDS(tm1, "out/mkdn/tm1.RDS")
h <- 8
hrz <- 0:23
plotz <- list()
for (h in hrz){

dm <- movez %>% 
  filter(hrz == h) %>% 
  select(IDs, vehicles, geometry)


rnet <- overline(dm, attrib = "vehicles", ncores = 4)

png(filename= paste0("out/plots/", h, "_rnet.png"), width=200,height=200,units="mm",res=100)
plot(rnet["vehicles"], lwd =sqrt(rnet$vehicles)/4, main = paste0("hour ", h), breaks = bks_hr)
dev.off()

}

#snapped_pts <- select(snapped_pts, -L1)

moves_od <-  colsplit(moves_day$ID, "_", c("o", "d"))

moves_od <- filter(moves_od, o %in% snapped_pts$id)
moves_od <- filter(moves_od, d %in% snapped_pts$id)

desire_lines = od2line(flow = moves_od, zones = snapped_pts)
desire_lines_day <- desire_lines %>% 
  mutate(ID = paste0(o, "_", d)) %>% 
  left_join(moves_day, by = "ID")

max_tot <- ceiling_dec(max(desire_lines_day$avg_time), -2)

me_pal <- c("#0000b3", "#0000eb", "#1d00ff", "#4a00ff", "#7600ff", "#a211ee", "#cf2ed1", "#fb4ab5", 
            "#ff6798", "#ff837c", "#ff9f60", "#ffbc43", "#ffd827", "#fff50a")

bks_day <- seq(from = 0, to = max_tot, by = max_tot/NROW(me_pal))

jet_pal <- pals::jet(NROW(bks_day))

tmap_mode("plot")

# tmaptools::palette_explorer()

tm2 <- tm_shape(desire_lines_day) +
  tm_lines(palette = me_pal, breaks = bks_day,
           lwd = "total",
           scale = 20,
           title.lwd = "Number of trips",
           alpha = 0.6,
           col = "avg_time",
           title = "time taken (seconds)"
  ) +
  tm_layout(frame = FALSE, legend.outside = TRUE)+
  tm_scale_bar(position = c("left", "bottom"))
tm2
tmap_save(tm2, "out/plots/tm2.png")

tm3 <- tm_shape(rd_network) +
  tm_lines(col = "grey")+
  tm_shape(desire_lines_day) +
  tm_lines(palette = me_pal, breaks = bks_day,
           lwd = "total",
           scale = 20,
           title.lwd = "Number of trips",
           alpha = 0.6,
           col = "avg_time",
           title = "time taken (seconds)"
  ) +
  tm_layout(frame = FALSE, legend.outside = TRUE)+
  tm_scale_bar(position = c("left", "bottom"))

tmap_save(tm3, "out/plots/tm3.png")

moves_od_hr <-  colsplit(moves_hr$ID, "_", c("o", "d"))

moves_od_hr <- filter(moves_od_hr, o %in% snapped_pts$id)
moves_od_hr <- filter(moves_od_hr, d %in% snapped_pts$id)

desire_lines_hr = od2line(flow = moves_od_hr, zones = snapped_pts)
desire_lines_hr <- desire_lines_hr %>% 
  mutate(ID = paste0(o, "_", d)) %>% 
  left_join(moves_hr, by = "ID")

max_hr <- ceiling_dec(max(desire_lines_hr$avg_time), -2)

bks_hr <- seq(from = 0, to = max_hr, by = max_hr/NROW(me_pal))

# tmaptools::palette_explorer()
tm4 <- tm_shape(desire_lines_hr) +
  tm_lines(palette = me_pal, breaks = bks_hr,
           lwd = "total",
           scale = 20,
           title.lwd = "Number of trips",
           alpha = 0.6,
           col = "avg_time",
           title = "time taken (seconds)"
  ) +
  tm_facets(along = "hour", free.coords = FALSE)+
  tm_layout(frame = FALSE, legend.outside = TRUE)+
  tm_scale_bar(position = c("left", "bottom"))

tmap_animation(tm4, filename = "out/plots/OD_day.gif", delay = 100)

moves_dat <- select(moves_dat, -total)
hrz <- unique(moves_dat)




all_routes_1 <- filter(all_routes, routeID %in% moves_dat$ID)


route1 <- filter(all_routes, routeID == "29002_3754001")
route2 <- filter(all_routes, routeID == "3772001_3407001")

library(leaflet.extras)
library(leaflegend)
library(htmlwidgets)

lat <- mean(st_coordinates(all_routes)[,2])
lon <- mean(st_coordinates(all_routes)[,1])

binz <- data.frame(width = c(1:10), trips = c(0,600, 1200, 1800, 2400, 3000, 3600, 4200, 4800, 5400))

pal_me <- colorNumeric(me_pal, domain = desire_lines_day$avg_time)

desire_lines_day$width <- cut(desire_lines_day$total, breaks = 20, labels = 1:20)
#desire_lines_hr$width <- cut(desire_lines_hr$total, breaks = 20, labels = 1:20)

m <- leaflet() %>% 
  setView(lon, lat, zoom = 12) %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = TRUE, updateWhenIdle = TRUE))

m <- m %>% 
  addPolylines(data = desire_lines_day, color = ~pal_me(avg_time), weight = desire_lines_day$width,
               opacity = 0.7, fillOpacity = 0.2, popup = paste("ID:", desire_lines_day$ID, "<br>",
                                                         "Origin:", desire_lines_day$o, "<br>",
                                                         "Destination:", desire_lines_day$d, "<br>",
                                                         "Time:", desire_lines_day$avg_time, "<br>",
                                                         "Total trips:", desire_lines_day$total, "<br>"),
               highlightOptions = highlightOptions(color = "white", weight = 2,
                                                   bringToFront = FALSE), group = "full day")

m = m %>% addLegend("bottomleft", pal=pal_me, values=desire_lines_day$avg_time,
                    title = "Average trip time",
                    opacity = 1)

m <- m %>%  addLegendSize(values = desire_lines_day$total,color = 'black',baseSize = 1,title = 'Total trips',
  labelStyle = 'margin: auto;',shape = 'rect',orientation = 'horizontal',opacity = .7,breaks = 10, group = "full day",
  position = 'bottomright')

m <- m %>% addLayersControl(baseGroups = c("full day"),
                            options = layersControlOptions(collapsed = FALSE), position = "topright") 


withr::with_dir('./', saveWidget(m, file="desire_lines.html"))

pal_tot <- colorNumeric(me_pal, domain = rnet_tot$total)

m <- leaflet() %>% 
  setView(lon, lat, zoom = 12) %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = TRUE, updateWhenIdle = TRUE))

m <- m %>% 
  addPolylines(data = rnet_tot, color = ~pal_tot(total), weight = 2,
               opacity = 0.7, fillOpacity = 0.2, popup = paste("total flow:", rnet_tot$total, "<br>"),
               highlightOptions = highlightOptions(color = "white", weight = 3,
                                                   bringToFront = FALSE), group = "combined routes")

m = m %>% addLegend("bottomleft", pal=pal_tot, values=rnet_tot$total,
                    title = "Total flow per segment",
                    opacity = 1)

m <- m %>% addLayersControl(baseGroups = c("combined routes"),
                            options = layersControlOptions(collapsed = FALSE), position = "topright") 


withr::with_dir('./', saveWidget(m, file="combined_routes.html"))


snapped_rds <- mutate(snapped_rds, cntnt=paste0('<strong>OSM ID: </strong>',osm_id,
                                                '<br><Direction: </strong> ', direction)) 

snapped_pts <- mutate(snapped_pts, cntnt=paste0('<strong>ID: </strong>',id,
                                                '<br><Direction: </strong> ', Richting.van.het.verkeer,
                                                '<br><Street: </strong> ', Standplaatsomschrijving))

df_sf <- st_transform(df_sf, latlong)
                      

m <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri Satellite") %>%
  addProviderTiles("OpenStreetMap", group = "Open Street Map") %>%
  setView(lon, lat, zoom = 15)

m <- m %>% addCircleMarkers(data = df_sf, color = "black", weight = 1,
                            opacity = 1.0, fillOpacity = 0.3, radius = 8,
                            fillColor = "#4FF54C",
                            popup = paste("ID:", df_sf$id, "<br>",
                                          "Street:", df_sf$Standplaatsomschrijving, "<br>",
                                          "Direction:", df_sf$Richting.van.het.verkeer, "<br>"), group = "Original Camera Points")

m <- m %>% addCircleMarkers(data = snapped_pts, color = "black", weight = 1,
                            opacity = 1.0, fillOpacity = 0.6, radius = 8,
                            fillColor = "orange",
                            popup = ~as.character(cntnt), group = "Snapped Camera Points")

m <- m %>% addPolylines(data = snapped_rds, color = "orange", weight = 4,
                        opacity = 1.0, fillOpacity = 0.2,
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = FALSE),popup = ~as.character(cntnt),
                        group = "Snapped Road Linestring")

m = m %>% addLegend("bottomleft", colors = c('#4FF54C', 'orange'),labels = c("Original", "Snapped"),
                    title = "Original vs Snapped",
                    opacity = 1)

m <- m %>% addLayersControl(overlayGroups = c("Original Camera Points", "Snapped Camera Points", "Snapped Camera Roads"),
                            baseGroups = c("CartoDB", "Esri Satellite", "Open Street Map"),
                            options = layersControlOptions(collapsed = FALSE), position = "topright")

withr::with_dir('./', saveWidget(m, file="camera_points.html"))


## top 500 routes
moves_500 <- movez_day %>% 
  arrange(desc(total)) %>% 
  slice(1:1000)

lat <- mean(st_coordinates(moves_500)[,2])
lon <- mean(st_coordinates(moves_500)[,1])

## plot each route individually

pal_r <- colorNumeric(me_pal, domain = moves_500$duration)

m <- leaflet() %>% 
  setView(lon, lat, zoom = 12) %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = TRUE, updateWhenIdle = TRUE))

route_ids <- unique(moves_500$ID)

r_ids <- list()
for (r in route_ids){
  
  dl <- filter(moves_500, ID == r)
  start_pts <- filter(snapped_pts, id == dl$src)
  end_pts <- filter(snapped_pts, id == dl$dst)
  #new <- filter(all_new_routes, link_id == r)
  
  m <- m %>% 
    addPolylines(data = dl, color = ~pal_r(duration), weight = 2,
                 opacity = 0.7, fillOpacity = 0.2, popup = paste("ID:", dl$ID, "<br>",
                                                                 "Origin:", dl$src, "<br>",
                                                                 "Destination:", dl$dst, "<br>",
                                                                 "Time:", dl$duration, "<br>",
                                                                 "Distance:", dl$distance, "<br>"),
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = FALSE), group = r)
  
  m <- m %>% addCircleMarkers(data = start_pts, color = "black", weight = 1,
                              opacity = 1.0, fillOpacity = 0.4, radius = 8,
                              fillColor = "green",
                              popup = paste("ID:", start_pts$id, "<br>"), group = r)
  
  m <- m %>% addCircleMarkers(data = end_pts, color = "black", weight = 1,
                              opacity = 1.0, fillOpacity = 0.4, radius = 8,
                              fillColor = "red",
                              popup = paste("ID:", end_pts$id, "<br>"), group = r)
  
  r_ids[[r]] <- r
  
}

r_ids_plotted <- do.call(rbind, r_ids)

m = m %>% addLegend("bottomleft", pal=pal_r, values=moves_500$duration,
                    title = "Average trip time (minutes)",
                    opacity = 1)

m <- m %>%  addLegendSize(values = desire_lines_day$total,color = 'black',baseSize = 1,title = 'Total trips',
                          labelStyle = 'margin: auto;',shape = 'rect',orientation = 'horizontal',opacity = .7,breaks = 10, group = "full day",
                          position = 'bottomright')

m <- m %>% addLayersControl(baseGroups = r_ids_plotted,
                            options = layersControlOptions(collapsed = FALSE), position = "topright")


withr::with_dir('./', saveWidget(m, file="top_1000_routes.html"))

