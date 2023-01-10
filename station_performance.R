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

#remove new stations = deployments which only started in 2021
first_deploy <- deploy %>% group_by(station_name) %>% summarise(first_deploy = min(deploy_date_time)) 

stn_active <- deploy_active %>% summarise(acoustic_project_code, station_name, deploy_longitude,deploy_latitude) %>% unique() %>% 
  mutate(first_deploy = first_deploy$first_deploy[match(station_name, first_deploy$station_name)]) %>% 
  filter(first_deploy < as.POSIXct("2021-01-01 00:00:00", tz="UTC"))

# get detections from active stations
detect <- get_acoustic_detections(acoustic_project_code = projs,station_name = stn_active$station_name, start_date = 2014) %>%  #scientific_name =sp for specific species
  mutate(date = as.Date(date_time))

#---Overview

# 1. Plot data gaps per receiver of each station and lost/broken receivers

detect %>% ggplot(aes(date, station_name, color=acoustic_project_code)) + geom_point() +
  geom_point()

# detect receivers with issues (modified scripts from https://github.com/cfmuniz/etn_analysis/blob/main/scripts/receivers_lost.R)

## 1. Status 'broken' or 'lost' -------------------------------------------

# get all receivers lost/broken from active stations
recv <- detect %>% distinct(receiver_id) %>% pull()
recv_issue <- get_acoustic_receivers() %>% filter(receiver_id %in% recv & status %in% c("lost", "broken"))


# 2. Comments from deployments

# get comments from deployments of active stations
deploy <- get_acoustic_deployments() %>% filter(receiver_id %in% recv) %>% select(comments) %>% distinct()


key_lost <- c('lost', 'Lost','los', 'miss', 'Miss', 'not found', 'Not found','verdwenen') 
key_broken <- c('broke', 'Broke','vervangen','needs redeployment','replaced','sterk uitgesleten') 
key_shore <- c('brought','shore') 

#continue searching for keywords from comments row#58



# assess only currently active stations
deploy <- get_acoustic_deployments(acoustic_project_code = projs, open_only = FALSE)
deploy_active <- get_acoustic_deployments(acoustic_project_code = projs, open_only = TRUE)
deploy_active <- deploy_active %>% filter(deploy_date_time > as.POSIXct("2021-12-31 00:00:00", tz="UTC") | battery_estimated_end_date > Sys.Date())

#remove new stations = deployments which only started in 2021
first_deploy <- deploy %>% group_by(station_name) %>% summarise(first_deploy = min(deploy_date_time)) 

stn_active <- deploy_active %>% summarise(acoustic_project_code, station_name, deploy_longitude,deploy_latitude) %>% unique() %>% 
  mutate(first_deploy = first_deploy$first_deploy[match(station_name, first_deploy$station_name)]) %>% 
  filter(first_deploy < as.POSIXct("2021-01-01 00:00:00", tz="UTC"))

# get detections from active stations
detect <- get_acoustic_detections(acoustic_project_code = projs,station_name = stn_active$station_name, start_date = 2014) %>%  #scientific_name =sp for specific species
  mutate(date = as.Date(date_time))

#---Overview

# 1. Plot data gaps per receiver of each station and lost/broken receivers

detect %>% ggplot(aes(date, station_name, color=acoustic_project_code)) + geom_point() +
  geom_point()

# detect receivers with issues (modified scripts from https://github.com/cfmuniz/etn_analysis/blob/main/scripts/receivers_lost.R)

## 1. Status 'broken' or 'lost' -------------------------------------------

# get all receivers lost/broken from active stations
recv <- detect %>% distinct(receiver_id) %>% pull()
recv_issue <- get_acoustic_receivers() %>% filter(receiver_id %in% recv & status %in% c("lost", "broken"))


# 2. Comments from deployments

# get comments from deployments of active stations
deploy <- get_acoustic_deployments() %>% filter(receiver_id %in% recv) #%>% select(comments) %>% distinct()

key_lost <- c('lost', 'Lost', 'los','LOST','miss', 'Miss', 'not found', 'Not found','verdwenen','afwezig') 
key_broken <- c('broke', 'Broke','BROKEN','broken','vervangen','replaced','Replaced','sterk uitgesleten') 
key_shore <- c('brought','shore') 

deploy_issue <- deploy %>% filter(str_detect(paste(key_lost, collapse="|")))


