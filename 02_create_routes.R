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

select <- dplyr::select

##define coordinate systems
latlong = "+init=epsg:4326"
rdnew = "+init=epsg:28992"

## read in snapped points
snapped_pts <- readRDS("out/snapped_pts.RDS")
snapped_rds <- readRDS("out/snapped_rds.RDS")
o_pts <- readRDS("out/original_pts.RDS")

##define time limit in minutes for two sightings that represent a journey
timelim <- 15
## read in vehicle hashed number plate data
observations <- readRDS("dat/ANPR/observations.RDS")

## process obs
obs_df <- observations %>% 
  #mutate(doy = yday(date)) %>% 
  #filter(doy == "304") %>% 
  select(date, Cameranummer = site_id, id) %>% 
  arrange(id, date) %>% 
  mutate(seconds = as.numeric(seconds(date)),
         Cameranummer = as.character(Cameranummer)) %>% 
  filter(!id == "")

## chop data frame into a list for each vehicle
spl <- split(obs_df, obs_df$id)

## for each vehicle determine the journeys and filter those that are above time limit
## code taken from https://stackoverflow.com/questions/40240663/origin-and-destination-with-r
move.spl <- lapply(spl, function(x) {
  if(NROW(x)>1){
   
  df <- data.frame(from=head(x$Cameranummer, -1), to=tail(x$Cameranummer, -1))
  df$diff <- tail(x$seconds, -1) - head(x$seconds, -1)
  df$hour <- hour(x$date[1:(NROW(x)-1)])
  df$doy <- yday(x$date[1:(NROW(x)-1)])
  ret <- filter(df, diff < (timelim*60))
  return(ret)
  print(x$id)
  }
})

## combine those that we under time limit
moves <- do.call(rbind, move.spl)

## convert to factor
moves$from <- factor(moves$from)
moves$to <- factor(moves$to)

## group trips by hour
moves_hr <- moves %>% 
  mutate(ID = paste0(from, "_", to),
         cnt = 1) %>% 
  group_by(ID, hour) %>% 
  summarise(avg_time = mean(diff),
            total = sum(cnt))

## all data with more than one movement
moves_hr_1 <- moves %>% 
  mutate(ID = paste0(from, "_", to),
         cnt = 1) %>% 
  group_by(ID, hour) %>% 
  summarise(avg_time = mean(diff),
            total = sum(cnt)) %>% 
  filter(total > 1)

## all moves for one day
moves_day <- moves %>% 
  mutate(ID = paste0(from, "_", to),
         cnt = 1) %>% 
  group_by(ID) %>% 
  summarise(avg_time = mean(diff),
            total = sum(cnt)) %>% 
  filter(total > 10)

save(moves_hr, moves_day, file = "out/moves.RData")

all_combs <- unique(moves_hr$ID)

routes <- list()
missing_sp <- list()
missing_ep <- list()

for(rowt in all_combs){
  
  r_df <- colsplit(rowt, "_", c("o", "d"))
  
  if(r_df$o == r_df$d){} else {
  
  sp <- filter(snapped_pts, id == r_df$o)
  ep <- filter(snapped_pts, id == r_df$d)
  
  if(NROW(sp)>0 & NROW(ep)>0){

  
  success <- FALSE
  while(!success){
    tryCatch({
      route <- osrmRoute(src = sp, dst = ep,
                         overview = "full", returnclass = "sf")
      
      success <- NROW(route) > 0
      
      
      #show progress
      print(rowt)
      # update GUI console
      flush.console()
    },error=function(e){
      Sys.sleep(5)
    },finally={})
    
  }
  
  nam <- as.character(rowt)
  route$routeID <- paste0(r_df$o, '_', r_df$d)
  route$count <- r_df$count
  
  routes[[nam]] <- route
  
  } else {
    nam <- as.character(rowt)
    if(NROW(sp)<1){
     
      missing_sp[[nam]] <- r_df$o
    } else {
      
      missing_ep[[nam]] <- r_df$d
    }
  }
  
  }
  
}

## combine all routes
all_routes <- do.call(rbind, routes)
all_m_sp <- do.call(rbind, missing_sp)
all_m_ep <- do.call(rbind, missing_ep)
 ## save for next script
save(all_routes, all_m_sp, all_m_ep, file = "out/all_routes.RData")
