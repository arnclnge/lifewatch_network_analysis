library(etn)
library(tidyverse)
con <- connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

setwd("~/lifewatch_network_analysis/")

#---EXTRACT DATA

projs <- c("bpns", "ws1", "ws2","ws3","cpodnetwork")

#get active deployments
recv <- get_receivers(status = c("Available"))

deploy <- get_deployments(network_project_code = projs)
deploy_active <- deploy %>% filter(deploy_date_time > as.POSIXct("2021-12-31 00:00:00", tz="UTC"))
stn_active <- deploy_active %>% summarise(network_project_code, station_name, deploy_longitude,deploy_latitude) %>% unique()
stn_cpod <- get_deployments(network_project_code = "cpodnetwork") %>%  select(station_name, deploy_longitude,deploy_latitude)%>% unique()

#get detections
detect_activeStn <- get_detections(network_project_code = projs,station_name = stn_active$station_name)


#add year column
detect_activeStn$year = as.numeric(format(detect_activeStn$date_time, "%Y", tz="UTC"))

#summarise
detect_sum <- detect_activeStn %>% group_by(network_project_code, scientific_name) %>% 
                                   filter(scientific_name!="Built-in")%>% 
                                   summarise(no_individuals = length(unique(animal_id)),detections = n()) %>% 
                                   mutate(total_individuals = sum(no_individuals))
write_csv(detect_sum, "~/etn_analysis/detect_activeNetwork2_14-11-2022.csv")


#---EXPLORE DATA

#how many detected individuals per station?
x <- detect_activeStn %>% group_by(network_project_code,scientific_name,year) %>% summarise(no_individuals = length(unique(animal_id))) %>% 
                                              filter(scientific_name!="Built-in")
ggplot(x,aes(as.factor(year),no_individuals)) + geom_bar(aes(fill = scientific_name), position = "dodge",stat='identity')+theme(axis.text.x=element_text(size = 10,angle=15))
write_csv(x, "~/etn_analysis/network_species.csv")

#---DATA VISUALIZATION

# 1. heat map of species detected
x <- detect_activeStn %>% group_by(scientific_name,year) %>% summarise(no_individuals = length(unique(animal_id)), detections =n()) %>% 
  filter(scientific_name!="Built-in", year!=2043)

#organise labels
x$scientific_name <- recode(x$scientific_name," Salmo trutta"="Salmo trutta")
x$scientific_name <- fct_relevel(x$scientific_name, rev)

ggplot(x, aes(as.factor(year), scientific_name, fill= detections)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="blue", trans="log1p", breaks = c(1000000, 100000,10000,1000,100,10)) +
  geom_text(aes(label = no_individuals))+
  theme_linedraw() + theme(axis.text.x=element_text(size = 7),axis.title = element_blank())
ggsave("plots/species_yearly_detections_heatmap.png", device='png', dpi = 300)

# 2. map 
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(rgdal)
library(broom)
library(maptools)
library(ggspatial)

world <- ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

bpns <- readOGR( 
  dsn= "~/lifewatch_network_analysis/shp/belgium_eez/", 
  layer="eez",
  verbose=FALSE)
bpns_fortified <- tidy(bpns, region = "geoname")

#active stations

ggplot(data=world) + geom_sf()+
  geom_polygon(data = bpns_fortified, aes(x = long, y = lat, group = group), fill="lightgrey", alpha=0.75)+
  geom_point(data=stn_active, aes(x=deploy_longitude, y=deploy_latitude), size = 1)+
  geom_text_repel(data=stn_active, aes(x=deploy_longitude, y=deploy_latitude, label=station_name), size=1.5)+
  coord_sf(xlim = c(2, 4.4), ylim = c(51,51.9), expand = FALSE)+theme_bw()+theme(axis.title = element_blank())+
  #geom_text(data= world_points,aes(x=X, y=Y, label=name),color = "darkblue", fontface = "bold", check_overlap = FALSE)+
  annotate(geom = "text", x = c(3.25, 4.3, 2.46), y = c(51.15, 51.75, 51.03), label = c("BE", "NL","FR"), size = 3) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave("plots/activeStn_map.png", device='png', dpi =300)


# 3. Timeline of when species were tagged and station detections

projs <- c("bpns", "ws1", "ws2","ws3","cpodnetwork")      #  "bpns", "ws1", "ws2","ws3","cpodnetwork"
sp <- c("Dicentrarchus labrax") #"Alosa fallax", "Anguilla anguilla", "Gadus morhua", "Dicentrarchus labrax","Raja clavata"

#get active deployments
deploy <- get_acoustic_deployments(acoustic_project_code = projs, open_only = FALSE)
deploy_active <- get_acoustic_deployments(acoustic_project_code = projs, open_only = TRUE)
deploy_active <- deploy_active %>% filter(deploy_date_time > as.POSIXct("2021-12-31 00:00:00", tz="UTC") | battery_estimated_end_date > Sys.Date())
stn_active <- deploy_active %>% summarise(acoustic_project_code, station_name, deploy_longitude,deploy_latitude) %>% unique() 

#get detections of stations with active deployments
detect <- get_acoustic_detections(acoustic_project_code = projs,station_name = stn_active$station_name, start_date = 2014, scientific_name =sp) %>%  #scientific_name =sp for specific species
  mutate(date = as.Date(date_time)) %>% as.data.frame() 

#get animals of detections
tags_detect <- detect %>% summarise(tag_serial =unique(tag_serial_number))
tag_df <- get_tags(tag_serial_number = tags_detect$tag_serial)
an_df <- get_animals(tag_serial_number = tags_detect$tag_serial)
an_df <- an_df %>% select(release_date_time, release_location, release_longitude, release_latitude) %>% mutate(type = "animal release")
names(an_df) <- c("date_time", "station_name","deploy_longitude", "deploy_latitude","type")

#merge detect & tag data
detect <- detect %>% select(date_time, station_name, deploy_longitude, deploy_latitude) %>% mutate(type = "detection")
detect_and_tags <- rbind(detect,an_df)

detect_and_tags %>% group_by(type) %>% arrange(station_name, .by_group=TRUE) %>% 
  mutate(station_name=factor(station_name, levels=unique(station_name))) %>% 
  ggplot(aes(date_time, station_name, colour = type, shape = type)) + geom_point(size = 1) +
    scale_color_manual(values = c("animal release" = "red", "detection" = "black")) + scale_shape_manual(values=c(17,16))+
    theme_linedraw()+ theme(axis.title = element_blank())+ ggtitle("Thornback ray: timeline of animal release and detections")+
    scale_y_discrete(limits = levels(detect_and_tags$type))

ggsave("plots/ray_release_detections.png", device='png', dpi = 300, width=13, height=7)

# 4. Explore detections and tag locations

library(RColorBrewer)
library(leaflet)

#how many detections per station?
x <- detect_and_tags %>% group_by(station_name,deploy_latitude,deploy_longitude,type) %>% summarise(num=n())

pal <- colorNumeric(palette = "magma",domain = x$num)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = x[x$type=='detection',],
    lng = ~deploy_longitude,
    lat = ~deploy_latitude,
    radius = ~num/2000,
    color = ~pal(num),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~paste(
      sep = "<br/>",
      paste0("Station: ", station_name),
      paste0("# Detections: ", num)
    )
  ) %>% 
  addLegend(data = x[x$type=='detection',],
    title = "Detections",
    pal = pal,
    values = ~num) %>%
  addMarkers(data = x[x$type=='animal release',],
    lng = ~deploy_longitude,
    lat = ~deploy_latitude,
    popup = ~paste0("Release location: ", station_name))
  

#--where were the animals tagged?
#get unique animal IDs from detect_bpns
an_bpns_id <- detect_bpns %>% group_by(animal_id, tag_serial_number,scientific_name) %>% summarise(n= n())
an_bpns_id$release_location <- an$release_location[match(an_bpns_id$animal_id,an$animal_id)]
an_bpns_id$release_latitude <- an$release_latitude[match(an_bpns_id$animal_id,an$animal_id)]
an_bpns_id$release_longitude <- an$release_longitude[match(an_bpns_id$animal_id,an$animal_id)]
write_csv(an_bpns_id, "outputs/telemetry/animals_ids_bpns.csv")
#17 animals with mismatched lat long fixed
an_bpns_id <- read_csv("outputs/telemetry/animals_ids_bpns_correct.csv")

pal <- colorNumeric(palette = "magma",domain = an_bpns_id$n)

leaflet(an_bpns_id) %>%
  addTiles() %>%
  addMarkers(
    lng = ~release_longitude,
    lat = ~release_latitude,
    popup = ~paste0("Release location: ", release_location))