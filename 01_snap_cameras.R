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
library(gsheet)

##define coordinate systems
latlong = "+init=epsg:4326"
ukgrid = "+init=epsg:27700"

## observation is large ~165MB
observations <- readRDS("dat/ANPR/observations.RDS")

siteIDs <- unique(observations$site_id)

## import cam data
cam_dat <- read.csv("dat/ANPR/sites.csv")

## create spatial data frame
df_sf <- cam_dat %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = latlong) %>% 
  st_transform(ukgrid)

## find all the direction definitions
u_dirs <- unique(df_sf$direction)

## extract the camera coordinates
cam_coords <- data.frame(st_coordinates(df_sf))

##create min x and y of camera area
x_min <- min(cam_coords$X)
x_max <- max(cam_coords$X)
y_min <- min(cam_coords$Y)
y_max <- max(cam_coords$Y)

##create a data frame to setup polygon generation
df <- data.frame(X = c(x_min, x_max, x_max, x_min), 
                 Y = c(y_max, y_max, y_min, y_min))

##generate a polygon of the area
vgt_area <- df %>%
  st_as_sf(coords = c("X", "Y"), crs = ukgrid) %>%
  dplyr::summarise(data = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_buffer(5000) %>% 
  st_transform(latlong)

##get osm geometry for camera area
x <- opq(bbox = vgt_area) %>% 
  add_osm_feature(key = c('highway')) %>% osmdata_sf()
## extract line geometry
rdz <- x$osm_lines %>% 
  filter(highway %in% c("tertiary", "trunk", "secondary", "service", "primary", "residential"))

#mapview(sites_sf)+rdz

## save road network
saveRDS(rdz, "out/road_network.RDS")

## get unique IDs of camera locations
cam_nums <- unique(df_sf$id)

## snap the cameras to nearest road in the correct direction
rds_near <- list()
pt_near <- list()
for (c in cam_nums){
  
  dc <- df_sf %>% 
    filter(id == c) %>% 
    st_transform(latlong)
  
  osm_near <- rdz[st_nearest_feature(dc, rdz),] %>% 
    select(osm_id, geometry)
  
  dt <- st_transform(osm_near, ukgrid)
  
  start <- st_line_sample(dt, sample = 0)
  start <- st_cast(start, "POINT")
  start <- st_transform(start, latlong)
  #start <- data.frame(start)
  end <- st_line_sample(dt, sample = 1)
  end <- st_cast(end, "POINT")
  end <- st_transform(end, latlong)
  # end <- data.frame(end)
  p <- c(start, end)
  a <- st_geod_azimuth(p)
  a <- as.numeric(NISTradianTOdeg(a))
  
  direction <- ifelse(a>-11.25 & a<11.25, "N",
                      ifelse(a>11.25 & a<33.75, "NNO",
                             ifelse(a>=33.75 & a<56.25, "NO",
                                    ifelse(a>=56.25 & a<78.75, "ONO",
                                           ifelse(a>=78.75 & a<101.25, "O",
                                                  ifelse(a>=78.75 & a<101.25, "OZO",
                                                         ifelse(a>=123.75 & a<146.25, "ZO",
                                                                ifelse(a>=146.25 & a<168.75, "ZZO",
                                                                       ifelse(a>=168.75 & a<180, "Z",
                                                                              ifelse(a<=-11.25 & a>-33.75, "NNW",
                                                                                     ifelse(a<=-33.75 & a>-56.25, "NW",
                                                                                            ifelse(a<=-56.25 & a>-78.75, "WNW",
                                                                                                   ifelse(a<=-78.75 & a>-101.25, "W",
                                                                                                          ifelse(a<=-78.75 & a>-101.25, "WZW",
                                                                                                                 ifelse(a<=-123.75 & a>-146.25, "ZW",
                                                                                                                        ifelse(a<=-146.25 & a>-168.75, "ZZW",
                                                                                                                               ifelse(a<=-168.75 & a>-180, "Z", "none")))))))))))))))))
  
           
  
  dt$direction <- direction
  
  ## determine if the orientation of the road geometry matches the camera information
  ds <- str_count(dc$direction, direction)
  ## if it does
  if(ds>0){
  
  nam <- as.character(c)
  osm_near$direction <- direction
  rds_near[[nam]] <- osm_near
  ## increase number of coordinate points on the linestring to improve snapping
  osm_near <- osm_near %>% 
    st_transform(ukgrid) %>% 
    st_line_sample(n = 40) %>% 
    st_cast("POINT") %>% 
    st_transform(latlong)
  osm_coords <- data.frame(st_coordinates(osm_near))
  osm_pts <- st_as_sf(osm_coords, coords = c("X", "Y"), crs = latlong)
  nr_pt <- osm_pts[st_nearest_feature(dc, osm_pts),]
  nr_pt$id <- dc$id
  pt_near[[nam]] <- nr_pt
  ## if not
  } else {
    rdz_minus <- filter(rdz, !osm_id == osm_near$osm_id)
    osm_near2 <- rdz[st_nearest_feature(dc, rdz),] %>% 
      select(osm_id, geometry)
    
    
    dt <- st_transform(osm_near2, ukgrid)
    
    start <- st_line_sample(dt, sample = 0)
    start <- st_cast(start, "POINT")
    start <- st_transform(start, latlong)
    #start <- data.frame(start)
    end <- st_line_sample(dt, sample = 1)
    end <- st_cast(end, "POINT")
    end <- st_transform(end, latlong)
    # end <- data.frame(end)
    p <- c(start, end)
    a <- st_geod_azimuth(p)
    a <- as.numeric(NISTradianTOdeg(a))
    
    direction2 <- ifelse(a>-11.25 & a<11.25, "N",
                        ifelse(a>11.25 & a<33.75, "NNO",
                               ifelse(a>=33.75 & a<56.25, "NO",
                                      ifelse(a>=56.25 & a<78.75, "ONO",
                                             ifelse(a>=78.75 & a<101.25, "O",
                                                    ifelse(a>=78.75 & a<101.25, "OZO",
                                                           ifelse(a>=123.75 & a<146.25, "ZO",
                                                                  ifelse(a>=146.25 & a<168.75, "ZZO",
                                                                         ifelse(a>=168.75 & a<180, "Z",
                                                                                ifelse(a<=-11.25 & a>-33.75, "NNW",
                                                                                       ifelse(a<=-33.75 & a>-56.25, "NW",
                                                                                              ifelse(a<=-56.25 & a>-78.75, "WNW",
                                                                                                     ifelse(a<=-78.75 & a>-101.25, "W",
                                                                                                            ifelse(a<=-78.75 & a>-101.25, "WZW",
                                                                                                                   ifelse(a<=-123.75 & a>-146.25, "ZW",
                                                                                                                          ifelse(a<=-146.25 & a>-168.75, "ZZW",
                                                                                                                                 ifelse(a<=-168.75 & a>-180, "Z", "none")))))))))))))))))
    
    
    
    dt$direction <- direction2
    
    ds <- str_count(dc$direction, direction2)
    
    if(ds>0){
      
      nam <- as.character(c)
      osm_near2$direction <- direction2
      rds_near[[nam]] <- osm_near2
      ## increase number of coordinate points on the linestring to improve snapping
      osm_near2 <- osm_near2 %>% 
        st_transform(ukgrid) %>% 
        st_line_sample(n = 40) %>% 
        st_cast("POINT") %>% 
        st_transform(latlong)
      osm_coords <- data.frame(st_coordinates(osm_near2))
      osm_pts <- st_as_sf(osm_coords, coords = c("X", "Y"), crs = latlong)
      nr_pt <- osm_pts[st_nearest_feature(dc, osm_pts),]
      nr_pt$id <- dc$id
      pt_near[[nam]] <- nr_pt
      
    } else {
      nam <- as.character(c)
      osm_near$direction <- direction
      rds_near[[nam]] <- osm_near
      ## increase number of coordinate points on the linestring to improve snapping
      osm_near <- osm_near %>% 
        st_transform(ukgrid) %>% 
        st_line_sample(n = 40) %>% 
        st_cast("POINT") %>% 
        st_transform(latlong)
      osm_coords <- data.frame(st_coordinates(osm_near))
      osm_pts <- st_as_sf(osm_coords, coords = c("X", "Y"), crs = latlong)
      nr_pt <- osm_pts[st_nearest_feature(dc, osm_pts),]
      nr_pt$id <- dc$id
      pt_near[[nam]] <- nr_pt
  }
  
  }
  print(c)
}

## combine road lines that match cameras
snapped_rds <- do.call(rbind, rds_near)
##combine snapped camera locations
snapped_pts <- do.call(rbind, pt_near)

## View output
mapview(df_sf, col.regions = "red")+ mapview(snapped_pts, col.regions = "green")+mapview(snapped_rds)+rdz
## add site id to snapped points
snapped_pts <- left_join(snapped_pts, cam_dat, by = "id")
## add camera data to original sf points
df_sf <- left_join(df_sf, cam_dat, by = "id")
## save points and road strings
saveRDS(df_sf, "out/original_pts.RDS")
saveRDS(snapped_pts, "out/snapped_pts.RDS")
saveRDS(snapped_rds, "out/snapped_rds.RDS")
