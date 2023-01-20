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
recv_issue_count <- recv_all_issues %>% group_by(acoustic_project_code, station_name) %>% summarise(deploy_with_issues = n()) 

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

#count the days of deployments with no download file
for (i in 1:nrow(deploy_all_in_active_stn)){
  print(i)
  if(!is.na(deploy_all_in_active_stn$next_deploy[i])){
  deploy_all_in_active_stn[i,"day_count_to_next_deploy"] <- length(seq(deploy_all_in_active_stn$deploy_date_time[i], deploy_all_in_active_stn$next_deploy[i], by = 1))
  }
}
datagap_station <- deploy_all_in_active_stn %>% filter(is.na(download_file_name)) %>% group_by(station_name) %>% 
                          summarise(days_no_data = sum(day_count_to_next_deploy, na.rm=TRUE))


#merge info on lost/broken receivers & data gap
deploy_no_downloadfile_summary <- deploy_no_downloadfile %>% group_by(station_name) %>% summarise(deploy_no_downloadfile = n())
station_performance_summary <- merge(datagap_station, recv_issue_count, by=c("station_name"), all=TRUE)
station_performance_summary <- merge(station_performance_summary,deploy_no_downloadfile_summary, by= c("station_name"))
write_csv(station_performance_summary, "csv/station_performance_summary.csv")

# to do: calculate percentage of total deployment days with no download file
# what to do with deployments of two receivers on the same day (VEMCO & THELMA), and one is lost.. is that data gap?


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


# map of lost receivers

# review literature on assessment of data gaps in acoustic telemetry
