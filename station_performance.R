# Assessment of station performance based on how much time a receiver was lost/broken/replaced, and length of data gaps 

library(tidyverse)
library(leaflet)
library(etn)

con <- connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

setwd("~/lifewatch_network_analysis/")

#---Extract data

projs <- c("bpns", "ws1", "ws2","ws3","cpodnetwork")      #  "bpns", "ws1", "ws2","ws3","cpodnetwork"
sp <- c("Alosa fallax", "Anguilla anguilla", "Gadus morhua", "Dicentrarchus labrax","Raja clavata") #"Alosa fallax", "Anguilla anguilla", "Gadus morhua", "Dicentrarchus labrax","Raja clavata"

#SETTINGS

# assess only currently active stations
deploy <- get_acoustic_deployments(acoustic_project_code = projs, open_only = FALSE)
deploy_active <- get_acoustic_deployments(acoustic_project_code = projs, open_only = TRUE)
deploy_active <- deploy_active %>% filter(deploy_date_time > as.POSIXct("2021-12-31 00:00:00", tz="UTC") | battery_estimated_end_date > Sys.Date())

first_deploy <- deploy %>% group_by(station_name) %>% summarise(first_deploy = min(deploy_date_time)) 

# run mutate & filter if you want to remove new stations = deployments which only started in 2021
stn_active <- deploy_active %>% summarise(acoustic_project_code, station_name, deploy_longitude,deploy_latitude) %>% unique() 
  #mutate(first_deploy = first_deploy$first_deploy[match(station_name, first_deploy$station_name)]) %>% 
  #filter(first_deploy < as.POSIXct("2021-01-01 00:00:00", tz="UTC"))

# get detections from active stations
detect <- get_acoustic_detections(acoustic_project_code = projs,station_name = stn_active$station_name, start_date = 2014) %>%  #scientific_name =sp for specific species
  mutate(date = as.Date(date_time))
# get deployments from active stations
deploy <- deploy %>% filter(station_name %in% stn_active$station_name)

###########################--- Detect receivers with issues (modified scripts from https://github.com/cfmuniz/etn_analysis/blob/main/scripts/receivers_lost.R)

## 1. Status 'broken' or 'lost' -------------------------------------------

# get all receivers lost/broken from active stations
recv <- deploy %>% filter(station_name %in% stn_active$station_name) %>% distinct(receiver_id) %>% pull()
recv_issue <- get_acoustic_receivers() %>% filter(receiver_id %in% recv & status %in% c("lost", "broken"))

# get last deployment info of lost/broken receivers
recv_issue_deploy_info <- deploy %>% filter(receiver_id %in% recv_issue$receiver_id) %>% 
  group_by(receiver_id) %>% slice(which.max(deploy_date_time)) %>% 
  mutate(status = recv_issue$status[match(receiver_id, recv_issue$receiver_id)])

# 2. Comments from deployments

# A. get comments from deployments of active stations, use key words to assign status as lost/broken
deploy <- deploy %>% filter(receiver_id %in% recv) %>% select(comments) %>% distinct()

key_lost <- c('lost', 'Lost', 'los','LOST','miss', 'Miss', 'not found', 'Not found','verdwenen','afwezig') 
key_broken <- c('broke', 'Broke','BROKEN','broken','vervangen','replaced','Replaced','sterk uitgesleten') 
key_shore <- c('brought','shore','oever') 

deploy <- deploy %>% mutate(issue = case_when(str_detect(comments,paste(key_lost, collapse="|"))~"lost",
                                              str_detect(comments,paste(key_broken, collapse="|"))~"broken",
                                              str_detect(comments,paste(key_shore, collapse="|"))~"brought ashore"))

recv_commented <- deploy[!is.na(deploy$issue),] %>% filter(station_name %in% unique(detect$station_name))

# B. status of comments assigned by Jan Reubens manually
comment_status_JR <- read_csv("csv/comments_JR.csv")

#assign status of comments to deploy info of receivers
deploy <- deploy %>% filter(receiver_id %in% recv) %>% 
              mutate(status = comment_status_JR$status[match(comments, comment_status_JR$Comment)])

# Receivers missing (according to comment) but not marked in the status
# Cross-reference with lost/broken
#recv_needs_status_update <- deploy[!is.na(deploy$issue),] %>% filter(!receiver_id %in% recv_issue$receiver_id) #run this if comments status generated from section A
recv_needs_status_update <- deploy[!is.na(deploy$status),] %>% filter(!receiver_id %in% recv_issue$receiver_id) #run this if comments status generated from section B
write_csv(recv_needs_status_update, "csv/recv_needs_status_update.csv") #send this to Carlota, update on ETN

# 3. Merge receivers with comments and marked in status as lost/broken

#last deployment info of all lost/broken receivers
recv_all_issues <- rbind(as.data.frame(recv_issue_deploy_info),as.data.frame(recv_needs_status_update)) %>% 
                      filter(station_name %in% unique(detect$station_name))

######################################--- Plot data gaps per receiver of each station and lost/broken receivers

#---timeline of detections & lost/broken receivers
ggplot(detect) + geom_point(aes(date, station_name, color=acoustic_project_code)) +
  geom_point(data=deploy, aes(as.Date(deploy_date_time),station_name), color = "black")+
  geom_point(data=recv_all_issues, aes(as.Date(deploy_date_time),station_name), color ="black", shape = 4)+
  theme_linedraw()
ggsave("plots/detections_lost_deploy_timeline.png", device='png', dpi = 300, width=13, height=7)

#---data gaps: plot deployments and if download_file_name is present

    #there are different cases:
        # 1. status of receiver is lost/broken but last deployment has a download_file_name = most likely receiver was recovered and data was downloaded
        # 2. status of receiver is not lost/broken, no comments, no download_file_name and deployment already closed = data gap, but is this considered lost/broken?
deploy_no_downloadfile_no_status <- deploy[is.na(deploy$download_file_name) & is.na(deploy$status) & !is.na(deploy$recover_date_time),]
deploy_no_downloadfile <- deploy[is.na(deploy$download_file_name) & !is.na(deploy$recover_date_time),]
deploy_with_download_file <- deploy[!is.na(deploy$download_file_name) & !is.na(deploy$recover_date_time),]

# no download_file_name = data gap

#exclude open deployments when plotting no download files 
deploy %>% filter(!deployment_id %in% deploy_active$deployment_id) %>% 
            ggplot(aes(x = deploy_date_time, y = station_name)) + geom_point(size = 1) + 
                  geom_linerange(data = deploy_with_download_file, aes(xmin = deploy_date_time, xmax = recover_date_time, color ="deployment to recovery")) +
                  geom_point(data = deploy_no_downloadfile, aes(deploy_date_time,station_name, color ="no download file"),shape = 18, size = 3)+
                  scale_color_manual(values = c("black","red"))+
                  theme_linedraw()+ggtitle("Overview of deployments & data availability")+theme(legend.title=element_blank())

ggsave("plots/deployments_no_download_file.png", device='png', dpi = 300, width=14, height=9)
              
######################################--- Overview

#---how many lost /broken receivers per station and per project
recv_issue_count <- recv_all_issues %>% filter(deploy_date_time > as.Date("2013-12-31")) %>% 
                                        group_by(acoustic_project_code, station_name) %>% 
                                        summarise(deploy_with_issues = n()) 

#---how many days of data gaps? since not all deployments have recover_date_time, we rely on the next deployment date 

#get deployments from active stations
deploy_all_in_active_stn <- get_acoustic_deployments(acoustic_project_code = projs, open_only = FALSE) %>% 
                              filter(station_name %in% stn_active$station_name) %>% 
                              mutate(next_deploy = as.Date(NA), day_count_to_next_deploy=NA) %>% 
                              group_by(station_name) %>% arrange(deploy_date_time, .by_group=TRUE) %>% as.data.frame()

#convert deploy times to dates
deploy_all_in_active_stn$deploy_date_time <- as.Date(deploy_all_in_active_stn$deploy_date_time)

#fills in next_deploy with the next deployment date in the station
for (n in 1:nrow(deploy_all_in_active_stn)){
      print(n)
      if (deploy_all_in_active_stn[n,"station_name"] == deploy_all_in_active_stn[n+1,"station_name"]) {
            deploy_all_in_active_stn[n,"next_deploy"] <- deploy_all_in_active_stn$deploy_date_time[n+1]
          
      } else {
          deploy_all_in_active_stn[n,"next_deploy"] <- NA
    }}

#count the days of deployments with no download file from 2014! 
deploy_2014_in_active_stn <- deploy_all_in_active_stn %>% filter(deploy_date_time > as.Date("2013-12-31"))
for (i in 1:nrow(deploy_2014_in_active_stn)){
  print(i)
  if(!is.na(deploy_2014_in_active_stn$next_deploy[i])){
    deploy_2014_in_active_stn[i,"day_count_to_next_deploy"] <- length(seq(deploy_2014_in_active_stn$deploy_date_time[i], deploy_2014_in_active_stn$next_deploy[i], by = 1))
  }
}

datagap_station <- deploy_2014_in_active_stn %>% filter(is.na(download_file_name)) %>% group_by(station_name) %>% 
                          summarise(days_no_data = sum(day_count_to_next_deploy, na.rm=TRUE))

#add first day of deployment, number of days, number of deployments
#calculate percentage of total deployment days with no download file
datagap_station <- deploy_2014_in_active_stn %>% group_by(acoustic_project_code, station_name) %>% 
                              summarise(first_deploy = min(deploy_date_time), 
                                        total_days_active = length(seq(first_deploy, Sys.Date(), by = 1)),
                                        no_deployments = n()) %>% 
                              merge(datagap_station,by=c("station_name"), all=TRUE) %>% 
                              mutate(datagap_percent = days_no_data/total_days_active*100)


#merge info on lost/broken receivers & data gap
deploy_no_downloadfile_summary <- deploy_no_downloadfile %>% group_by(station_name) %>% summarise(deploy_no_downloadfile = n())
station_performance_summary <- merge(datagap_station, recv_issue_count, by=c("station_name"), all=TRUE)
station_performance_summary <- merge(station_performance_summary,deploy_no_downloadfile_summary, by= c("station_name"), all =TRUE) %>% 
                                select(-c("acoustic_project_code.x","acoustic_project_code.y"))
write_csv(station_performance_summary, "csv/station_performance_summary.csv")

#---- plot data gap percent

station_performance_summary %>% ggplot(aes(datagap_percent, station_name))+geom_bar(stat = 'identity')+theme_linedraw()
ggsave("plots/datagap_station.png", device='png', dpi = 300, width=12, height=8)

# how many days of no detections per station
data_gaps_station <- detect %>% group_by(acoustic_project_code, station_name) %>% 
  summarise(first_detect = min(date), last_detect = max(date)) %>% 
  mutate(missing_dates= NA, data_gap_percent = NA)

for (i in 1:nrow(data_gaps_station)){
  date_range <- seq(data_gaps_station$first_detect[i], data_gaps_station$last_detect[i], by = 1) 
  dates_missing <- date_range[!date_range %in% detect$date]
  data_gaps_station$missing_dates[i] <- length(dates_missing)
  data_gaps_station$data_gap_percent[i] <- data_gaps_station$missing_dates[i]/length(date_range)*100
}

#####################################---venn diagram with REI
library(ggvenn)

venn_stn <- as.list(read_csv("csv/venn_station.csv"))

ggvenn(venn_stn, show_elements = T, label_sep = "\n", text_size =2.5,
       fill_color = c("#0073C2FF", "#EFC000FF","#CD534CFF"),
       stroke_size = 0.25, stroke_alpha = 0.5, set_name_size = 3)

#####################################---map of stations with data gaps
stn_coords <- deploy_2014_in_active_stn %>% group_by(station_name) %>% summarise(lat = mean(deploy_latitude),
                                                                                 lon = mean(deploy_longitude))
data_gap_map <- station_performance_summary %>% filter(datagap_percent >0) %>% 
                                                merge(stn_coords,by = "station_name")
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(rgdal)
library(broom)
library(maptools)
library(ggspatial)

bpns <- readOGR( 
  dsn= "~/lifewatch_network_analysis/shp/belgium_eez/", 
  layer="eez",
  verbose=FALSE)
bpns_fortified <- tidy(bpns, region = "geoname")

europe <- readOGR( 
  dsn= "~/lifewatch_network_analysis/shp/europe/", 
  layer="Europe",
  verbose=FALSE)
eur_fortified <- tidy(europe, region = "NAME")

#active stations

ggplot()+
  geom_polygon(data = bpns_fortified, aes(x = long, y = lat, group = group), fill="lightgrey", alpha=0.75)+
  geom_polygon(data=eur_fortified, aes(x = long, y = lat, group = group), fill="white", colour="black")+
  coord_cartesian(xlim = c(2, 4.4), ylim = c(51,51.9))+
  geom_point(data=data_gap_map, aes(x=lon, y=lat, color = datagap_percent))+
  scale_color_gradient(low="blue", high="red")+
  geom_text_repel(data=data_gap_map, aes(x=lon, y=lat, label=station_name), size=3)+
  theme_classic()+theme(axis.title = element_blank())+
  annotate(geom = "text", x = c(3.25, 4.4, 2.46), y = c(51.15, 51.5, 51.03), label = c("BE", "NL","FR"), size = 3) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave("plots/datagap_map.png", device='png', dpi =300, width=14, height=7)

# review literature on assessment of data gaps in acoustic telemetry
