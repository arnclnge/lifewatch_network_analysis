library(tidyverse)
library(leaflet)

setwd("D:/VLIZ_Network_Analysis")
mycols <- colors()[c(7,8,26,31,32,34,37,43,47,86,142,228,93,509,656,261)]

#---input files
an <- read_csv("fish_data/animals_download_date_20220822.csv")
deploy <- read_csv("fish_data/deployments_all_download_date_20220822.csv")
detect <- read_csv("fish_data/detections_download_date_20220819.csv")
tags <- read_csv("fish_data/tags_download_date_20220822.csv")

#check detections not within deploy period
detect = as.data.frame(detect)
detect$deploy_date_time = as.Date(deploy$deploy_date_time[match(detect$deployment_id,deploy$deployment_id)])
detect$recover_date_time = as.Date(deploy$recover_date_time[match(detect$deployment_id,deploy$deployment_id)])
detect_deploy_issues <- detect %>% mutate(within_deploy_recover_period = case_when(as.Date(date_time,"UTC") >= deploy_date_time  & 
                                                                                     as.Date(date_time,"UTC") <= recover_date_time &
                                                                                     as.Date(date_time,"UTC") <= deploy_date_time + 456.25 ~ "YES", TRUE ~ "NO")) %>% 
                            filter(within_deploy_recover_period=="NO")

detect_deploy_issues <- detect %>% mutate(within_deploy_recover_period = case_when(as.Date(date_time,"UTC") <= deploy_date_time + 456.25 ~ "YES", TRUE ~ "NO")) %>% 
                                     filter(within_deploy_recover_period=="NO")

detect_clean<- detect %>% mutate(within_deploy_recover_period = case_when(as.Date(date_time,"UTC") >= deploy_date_time  & 
                                                                            as.Date(date_time,"UTC") <= recover_date_time &
                                                                            as.Date(date_time,"UTC") <= deploy_date_time + 456.25 ~ "YES", TRUE ~ "NO")) %>% 
                     filter(within_deploy_recover_period=="YES") %>% as.data.frame()
write_csv (detect_deploy_issues,"fish_data/detections_download_date_20220819_deploydateissues.csv")


#--filter detections
#remove detections if a given tag was detected fewer than 5 times per day
detect_rm <- detect_clean %>% group_by(station_name,deploy_longitude,deploy_latitude,as.Date(date_time, "UTC"), tag_serial_number) %>% summarise(tag_n = n()) %>% filter(tag_n<5) 
detect_rm$date <- detect_rm$`as.Date(date_time, "UTC")`
detect_clean$date <- as.Date(detect_clean$date_time, "UTC")
detect_clean <- anti_join(detect_clean,detect_rm, by=c('station_name','deploy_longitude','deploy_latitude','date','tag_serial_number')) 

#if scientific_name = NA, fill out from animal dataframe using tag_serial_number 
#detect_bpns$scientific_name <- if_else(is.na(detect_bpns$scientific_name), an$scientific_name[match(detect_bpns$tag_serial_number,an$tag_serial_number)],detect_bpns$scientific_name)
#an_list2 <- detect_bpns %>% group_by(scientific_name) %>% summarise(no_tags = length(unique(tag_serial_number)),detections = n(),no_stations=length(unique(station_name)))
#write_csv(an_list,"outputs/telemetry/animals_tags_detections.csv")

#remove NA, built-in and sync tags
detect_clean <- detect_clean %>% filter(scientific_name !="Built-in",scientific_name!="Sync tag" )
detect_clean <- detect_clean %>% filter(!is.na(scientific_name) & !is.na(animal_id))

#check if there are duplicates of detection_id
dup0 <- detect_clean[duplicated(detect_clean[,c("detection_id","date_time")]),c("detection_id","date_time")]
dup0[,2] <- as.POSIXct(dup0[,2], format = "%Y-%m-%d %H:%M:%S", tz="UTC")
dup <- merge(dup0,detect)
write_csv(dup,"fish_data/detections_download_date_20220819_duplicates.csv")

#---DATA EXPLORATION
projs <- detect_clean %>% group_by(acoustic_project_code) %>% summarise(detections = n(), stations_n = length(unique(station_name)),min_date = min(date_time,na.rm=TRUE), max_date=max(date_time,na.rm=TRUE))
detect_bpns <- detect_clean %>% filter(acoustic_project_code=="bpns") 

#animals
an_list <- detect_bpns %>% group_by(scientific_name) %>% summarise(individuals = length(unique(animal_id)), no_tags=length(unique(tag_serial_number)),detections = n())
write_csv(an_list,"outputs/telemetry/animals_list.csv")

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

#release location by the numbers
ggplot(an_bpns_id,aes(y=release_location, fill=scientific_name)) + geom_bar(stat='count')+scale_fill_manual(values = mycols)

#---MAP DETECTIONS 

#list stations
stn_list <- detect_bpns %>% group_by(station_name, deploy_latitude,deploy_longitude) %>% count()
write_csv(stn_list, "mapping/fish/receivers_list.csv")

#visualize stations
leaflet(stn_list) %>%
  addTiles() %>%
  addMarkers(
    lng = ~deploy_longitude,
    lat = ~deploy_latitude,
    popup = ~paste0("Station: ", station_name))

#visualise tags through time
library(scales)
library(gridExtra)
library(grid)

x <- detect_bpns %>% filter(scientific_name=="Spondyliosoma cantharus")
plotList <- list()

for (i in 1:length(unique(x$tag_serial_number))){
  y <- x %>% filter(tag_serial_number == unique(x$tag_serial_number)[i])
  min_date = min(y$date_time)
  max_date = max(y$date_time)
  plot <- y %>% ggplot(aes(date_time, station_name)) +
    geom_point(size=0.3)+
    theme(axis.title=element_blank(),plot.title = element_text(size = 8))+
    ggtitle(paste("tag serial:",as.character(unique(x$tag_serial_number)))[i])+
    scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"),limit=c(min(min_date),max(max_date)))
  plotList[[i]] <- plot
}
do.call("grid.arrange", c(plotList, ncol = 1, top ="Spondyliosoma cantharus"))
ggsave(paste0("outputs/dets_cpower.png"), device='png', dpi=150, width=5, height=2)


#detected animals per station
library(RColorBrewer)
x <- detect_bpns %>% group_by(station_name,deploy_latitude,deploy_longitude) %>% summarise(n_animals = length(unique(scientific_name)))

pal <- colorNumeric(palette = "magma",domain = x$n_animals)

x %>% leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~deploy_longitude,
    lat = ~deploy_latitude,
    radius = ~n_animals,
    color = ~pal(n_animals),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~paste(
      sep = "<br/>",
      paste0("Station: ", station_name),
      paste0("# detected animals: ", n_animals)
    )
  ) %>% 
  addLegend(
    title = "Detected animals",
    pal = pal,
    values = ~n_animals)

#how many detected animals per station?
x <- detect_bpns %>% group_by(station_name,deploy_latitude,deploy_longitude,scientific_name) %>% summarise(n=length(unique(scientific_name)))

dev.off()
ggplot(x,aes(n,station_name, fill=scientific_name)) + geom_bar(stat='identity')+scale_fill_manual(values = mycols)

#how many detections per station?
x <- detect_bpns %>% group_by(station_name,deploy_latitude,deploy_longitude,scientific_name) %>% summarise(detections=n())

ggplot(x,aes(detections,station_name)) + geom_bar(aes(fill = scientific_name),stat='identity')+scale_fill_manual(values = mycols)

pal <- colorNumeric(palette = "magma",domain = x$detections)
x %>% leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~deploy_longitude,
    lat = ~deploy_latitude,
    radius = ~detections/30000,
    color = ~pal(detections),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~paste(
      sep = "<br/>",
      paste0("Station: ", station_name),
      paste0("# Detections: ", detections)
    )
  ) %>% 
  addLegend(
    title = "Detections",
    pal = pal,
    values = ~detections)

#which receivers have detections of an animal that the other receivers did not pick up?
theme_linedraw(
  base_size = 11,
  base_family = "",
  base_line_size = 11/22,
  base_rect_size = 11/22
)

unique_an <- detect_bpns %>% group_by(station_name,scientific_name) %>% summarise(detections=n())
ggplot(unique_an,aes(scientific_name,station_name))+geom_point(size=3, colour='red')+theme_linedraw()+ theme(axis.text.x=element_text(size = 10,angle=20, hjust=1),axis.title.x=element_blank()) 

#which network has detections of an animal that the other networks did not pick up?
unique_an_netw <- detect_clean %>% group_by(acoustic_project_code,station_name,scientific_name) %>% summarise(detections=n())
ggplot(unique_an_netw,aes(scientific_name,acoustic_project_code))+geom_point(size=4, colour='red')+theme_linedraw()+ theme(axis.text.x=element_text(size = 10,angle=20, hjust=1),axis.text.y=element_text(size = 12),axis.title.x=element_blank()) 

#---CPOD NETWORK
#what fish do we see in the c-pod stations
theme_linedraw(
  base_size = 11,
  base_family = "",
  base_line_size = 11/22,
  base_rect_size = 11/22
)

unique_an_cpod <- detect_clean %>% filter(acoustic_project_code=="cpodnetwork") %>% group_by(station_name,scientific_name)  %>% summarise(detections=n()) 
ggplot(unique_an_cpod,aes(scientific_name,station_name))+geom_point(size=4, colour='red')+theme_linedraw()+ theme(axis.text.x=element_text(size = 12),axis.title.x=element_blank(),axis.text.y=element_text(size = 12)) 

##################################################################

#Receiver activity days
library(lubridate)

#### Quality filter of deploy from Jolien ####
deploy = deploy %>% 
  filter(as.Date(deploy_date_time, "UTC") != as.Date(recover_date_time,"UTC")) %>% 
  filter(deploy_latitude >= 50 & deploy_latitude <= 52) %>% 
  filter(!(station_name == "bpns-Buitenratel" & as.Date(deploy_date_time,"UTC") == as.Date("2020-09-07")))

# If battery estimated end dates are before deploy date, we assume someone forgot to fill them out correctly
deploy = deploy %>% 
  mutate(battery_estimated_end_date = ifelse(battery_estimated_end_date < deploy_date_time,
                                             as.character(deploy_date_time + months(15)),
                                             as.character(battery_estimated_end_date))) %>% 
  mutate(battery_estimated_end_date = parse_date_time(battery_estimated_end_date, orders = "ymd HMS"))

# Filter for deployments that were recovered and that resulted in a download
deploy = deploy %>% filter(!is.na(recover_date_time) & !is.na(download_file_name))

#### Add column end date ####
# Needs to be changed: database issues with battery end dates
deploy = deploy %>%
  mutate(timedif = difftime(recover_date_time, battery_estimated_end_date, units = "days")) %>%
  mutate(end_date = case_when(
    is.na(timedif) & difftime(recover_date_time, deploy_date_time, units = "weeks") < 78 ~ as.character(recover_date_time),
    is.na(timedif) & difftime(recover_date_time, deploy_date_time, units = "weeks") >= 78 ~ as.character(deploy_date_time + months(15)),
    as.numeric(timedif) < 90 ~ as.character(recover_date_time),
    as.numeric(timedif) >= 90 ~ as.character(battery_estimated_end_date)
  ))

################## COMPUTATION OF REI ###########################
total_tags <- length(unique(detect_bpns$tag_serial_number))
total_species <- length(unique(detect_bpns$scientific_name))
total_detection_days <- detect_bpns %>% group_by(station_name) %>% summarise(N = length(unique(date))) 
total_detection_days <- sum(total_detection_days$N)
total_network_days <- (as.numeric(difftime( "2022-08-07","2014-07-11", units = "days")))+1

# deploy days should be maximum 15 months or 456.25 days
deploy_bpns <- deploy %>% filter(acoustic_project_code=="bpns") %>% group_by(station_name,deployment_id, deploy_date_time,recover_date_time) %>% filter(!is.na(recover_date_time))%>% 
                          summarise(deploy_days = (as.numeric(difftime(recover_date_time, deploy_date_time, units = "days")))+1) %>% 
                          mutate(deploy_days = if_else(deploy_days > 456.25, 456.25, deploy_days)) %>% 
                          group_by(station_name) %>% summarise(deploy_days = sum(deploy_days,na.rm=TRUE)) 
write_csv(deploy_bpns2,"outputs/deployment_period_download_date_20220822.csv")

REI <- detect_bpns %>% group_by(station_name) %>% summarise(no_tags = length(unique(tag_serial_number)), 
                                                     no_species = length (unique(scientific_name)),
                                                     detection_days = length(unique(date))) %>% 
        mutate(deploy_days= deploy_bpns$deploy_days[match(station_name,deploy_bpns$station_name)],
                rei = (no_tags/total_tags)*(no_species/total_species)*(detection_days/total_detection_days)*(total_network_days/deploy_days)*1000,
               Percent_REI = rei / sum(rei)*100, Rank = rank(-Percent_REI))
write_csv(REI, "outputs/telemetry/REI.csv")

#summary stats
summary_tags <- detect_bpns %>% group_by(station_name) %>% summarise(N = length(unique(tag_serial_number))) %>% 
                    summarise(Total=total_tags, mean = mean(N), SD = sd(N), Max =max(N))
summary_dets <- detect_bpns %>% group_by(station_name) %>% summarise(N = n()) %>% 
  summarise(Total=sum(N), mean = mean(N), SD = sd(N), Max =max(N))
summary_det_days <- detect_bpns %>% group_by(station_name) %>% summarise(N = length(unique(date))) %>% 
  summarise(Total=sum(N), mean = mean(N), SD = sd(N), Max =max(N))
summary_species <- detect_bpns %>% group_by(station_name) %>% summarise(N = length(unique(scientific_name))) %>% 
  summarise(Total=total_species, mean = mean(N), SD = sd(N), Max =max(N))
summary_stats<- dplyr::bind_rows(summary_tags,summary_dets,summary_det_days,summary_species)
write_csv(summary_stats, "outputs/telemetry/summary_stats.csv")

#----CUMULATIVE CURVES

#add REI ranking to detect_bpns
detect_bpns$receiver_rank <- REI$Rank[match(detect_bpns$station_name,REI$station_name)]
#order by REI rank
detect_bpns <- detect_bpns[order(detect_bpns$receiver_rank),]

#TAGS
tags_cumsum <- detect_bpns %>%
                    mutate(cum_unique_entries = cumsum(!duplicated(tag_serial_number))) %>%
                    group_by(receiver_rank) %>%
                    summarise(cum_unique_entries = last(cum_unique_entries))

breaks = seq(50, 322, by=50)
labels = as.character(breaks)

ggplot(tags_cumsum, aes(x = receiver_rank, y = cum_unique_entries)) + geom_line() + geom_point() +theme_bw()+
        geom_hline(aes(yintercept=total_tags*0.75,color = "75% benchmark"), linetype="dashed")+
        scale_y_continuous(limits = c(50, 355), breaks = breaks, labels = labels,name = "No. of tags")+
        geom_vline(aes(xintercept =42, color = "REI > 0.16%"),linetype="dotted", size=1.5)+
        scale_color_manual(values = c("red","blue"))+
        theme(legend.title=element_blank(), legend.position="bottom")

#SPECIES
sp_cumsum <- detect_bpns %>%
  mutate(cum_unique_entries = cumsum(!duplicated(scientific_name))) %>%
  group_by(receiver_rank) %>%
  summarise(cum_unique_entries = last(cum_unique_entries))


ggplot(sp_cumsum, aes(x = receiver_rank, y = cum_unique_entries)) + geom_line() + geom_point() +theme_bw()+
  geom_hline(aes(yintercept=total_species,color = "100% benchmark"), linetype="dashed")+
  scale_y_continuous(name = "No. of species")+
  geom_vline(aes(xintercept =42, color = "REI > 0.16%"),linetype="dotted", size=1.5)+
  scale_color_manual(values = c("red","blue"))+
  theme(legend.title=element_blank(), legend.position="bottom")


#DETECTIONS
det_cumsum <- detect_bpns %>%
  mutate(cum_unique_entries = cumsum(!duplicated(detection_id))) %>%
  group_by(receiver_rank) %>%
  summarise(cum_unique_entries = last(cum_unique_entries))

ggplot(det_cumsum, aes(x = receiver_rank, y = cum_unique_entries)) + geom_line() + geom_point() +theme_bw()+
  geom_hline(aes(yintercept=last(cum_unique_entries)*0.75,color = "75% benchmark"), linetype="dashed")+
  scale_y_continuous(name = "No. of detections")+
  geom_vline(aes(xintercept =42, color = "REI > 0.16%"),linetype="dotted", size=1.5)+
  scale_color_manual(values = c("red","blue"))+
  theme(legend.title=element_blank(), legend.position="bottom")

################### INQUIRIES

#high detections from Cpowerreefballs - real? from when?

detect_bpns %>% filter (station_name=="bpns-CPowerReefballs") %>% 
  ggplot(aes(date_time, station_name)) +
  geom_point(size=0.2)
ggsave(paste0("outputs/dets_cpower.png"), device='png', dpi=150, width=5, height=2)

#fish specific detections per station ###change scientific_name
x <- detect_bpns %>% filter(scientific_name == "Spondyliosoma cantharus") %>% group_by(station_name,deploy_latitude,deploy_longitude) %>% summarise(detections=n())

x %>% ggplot(aes(detections,station_name)) + geom_bar(stat='identity')
ggsave(paste0("outputs/dets_thinlipmullet.png"), device='png', dpi=150, width=5, height=3)

pal <- colorNumeric(palette = "magma",domain = x$detections)
x %>% leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~deploy_longitude,
    lat = ~deploy_latitude,
    radius = ~detections/100,
    color = ~pal(detections),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~paste(
      sep = "<br/>",
      paste0("Station: ", station_name),
      paste0("# Detections: ", detections)
     ))

############### EXPLORE STATIONS THAT CAN BE DISCARDED BASED ON REI
library(rgdal)
library(broom)
library(ggrepel)
library(viridis)

#map these stations
stn_discard <- detect_bpns %>% filter(receiver_rank %in% (43:59))
stn_discard_list <- stn_discard %>% group_by(station_name) %>% summarise(long=mean(deploy_longitude), lat=mean(deploy_latitude), detections = n(), species =length(unique(scientific_name)))
stn_retain_list <- detect_bpns %>% filter(receiver_rank %in% (1:42)) %>% group_by(station_name) %>% summarise(long=mean(deploy_longitude), lat=mean(deploy_latitude), detections = n(), species =length(unique(scientific_name)))
REI$long <- detect_bpns$deploy_longitude[match(REI$station_name,detect_bpns$station_name)]
REI$lat <- detect_bpns$deploy_latitude[match(REI$station_name,detect_bpns$station_name)]

bpns <- readOGR( 
  dsn= "C:\\Workspace_2MA1\\Thesis\\Data\\belgium_eez", 
  layer="eez",
  verbose=FALSE)

bpns_fortified <- tidy(bpns, region = "geoname")

REI_filter <- REI %>% filter(between(lat,51.1,51.4), between(long,2.75,2.79)) 

#change variable to REI/REI_filter/stn_discard_list/stn_retain_list
stn_retain_list %>% 
  ggplot(aes(x=long, y=lat)) +
  geom_polygon(data = bpns_fortified, aes(x = long, y = lat, group = group), fill="grey", alpha=0.75) +
  geom_text_repel( data=stn_retain_list, aes(x=long, y=lat, label=station_name), size=3) +
  geom_point(aes(x=long, y=lat, size=species, color=detections)) +
  scale_size_continuous(range=c(0.5,5)) +
  scale_color_viridis() +
  coord_map()+
  ggtitle("Most efficient receivers (Rank 1-42 based of Receiver Efficiency Index)")+
  theme(plot.title = element_text(size= 10, colour="blue"))
  #coord_cartesian(xlim= c(2.75,2.79),ylim = c(51.1, 51.4))
dev.off()

#summary stats
total_tags <- length(unique(stn_discard$tag_serial_number))
total_species <- length(unique(stn_discard$scientific_name))
total_detection_days <- stn_discard %>% group_by(station_name) %>% summarise(N = length(unique(date))) 
total_detection_days <- sum(total_detection_days$N)
total_network_days <- (as.numeric(difftime( "2022-07-24","2014-07-11", units = "days")))+1

summary_tags <- stn_discard %>% group_by(station_name) %>% summarise(N = length(unique(tag_serial_number))) %>% 
  summarise(Total=total_tags, mean = mean(N), SD = sd(N), Max =max(N))
summary_dets <- stn_discard %>% group_by(station_name) %>% summarise(N = n()) %>% 
  summarise(Total=sum(N), mean = mean(N), SD = sd(N), Max =max(N))
summary_det_days <- stn_discard %>% group_by(station_name) %>% summarise(N = length(unique(date))) %>% 
  summarise(Total=sum(N), mean = mean(N), SD = sd(N), Max =max(N))
summary_species <- stn_discard %>% group_by(station_name) %>% summarise(N = length(unique(scientific_name))) %>% 
  summarise(Total=total_species, mean = mean(N), SD = sd(N), Max =max(N))
summary_stats<- dplyr::bind_rows(summary_tags,summary_dets,summary_det_days,summary_species)
write_csv(summary_stats, "outputs/telemetry/summary_stats_stn_discard.csv")


#---how many detections per station?
x <- stn_discard %>% group_by(station_name,deploy_latitude,deploy_longitude,scientific_name) %>% summarise(detections=n())

ggplot(x,aes(detections,station_name)) + geom_bar(aes(fill = scientific_name),stat='identity')+scale_fill_manual(values = mycols)

#order by REI rank
stn_discard <- stn_discard[order(stn_discard$receiver_rank),]

thm <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size =10), axis.text=element_text(size=12), legend.title=element_blank(),strip.text = element_text(size=10, color="darkblue")) 
m <- ggplot(stn_discard,aes(x = date,y = (..count..),fill = scientific_name)) + geom_bar() + 
  expand_limits(y = 0) + scale_y_continuous(expand = c(0, 0))+ facet_wrap(receiver_rank~station_name, nrow=5, scales="free") + thm + 
  xlab("time") + ylab("detections")+scale_fill_manual(values = mycols)#+scale_x_date(date_labels="%b %Y")
m
ggsave(paste0("outputs/telemetry/rank52-56.png"), device='png')

#---VISUALIZE TAGS
#visualise tags of a particular station
dets_Cpower <- detect_bpns %>% filter(station_name=="bpns-CPowerReefballs")
m <- ggplot(dets_Cpower,aes(x = date,y = (..count..),fill = scientific_name)) + geom_bar() + 
  expand_limits(y = 0) + scale_y_continuous(expand = c(0, 0))+ thm + 
  xlab("time") + ylab("detections")+scale_fill_manual(values = mycols)#+scale_x_date(date_labels="%b %Y")
m

#visualise tags through time for a specific station
library(scales)
library(gridExtra)
library(grid)

x <- detect_bpns %>% filter(station_name=="bpns-CPowerReefballs")
plotList <- list()

for (i in 1:length(unique(x$scientific_name))){
  y <- x %>% filter(scientific_name == unique(x$scientific_name)[i])
  min_date = min(y$date_time)
  max_date = max(y$date_time)
  plot <- y %>% ggplot(aes(x=date_time, y=as.factor(tag_serial_number))) +
    geom_point(size=0.3)+
    theme(axis.title=element_blank(),plot.title = element_text(size = 8))+
    ggtitle(paste("species:",as.character(unique(x$scientific_name)[i])))+
    scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"),limit=c(min(min_date),max(max_date)))
  plotList[[i]] <- plot
}
do.call("grid.arrange", c(plotList, ncol = 1, top ="bpns-CPowerReefballs"))
ggsave(paste0("outputs/dets_cpower.png"), device='png', dpi=150, width=5, height=2)

#---order species plot by REI rank
unique_an <- detect_bpns %>% group_by(station_name,receiver_rank,scientific_name) %>% summarise(detections=n())
unique_an$station_name <- factor(unique_an$station_name, levels = unique(unique_an$station_name[order(unique_an$receiver_rank,decreasing = TRUE)]))
stn_sp <- unique_an %>% group_by(scientific_name) %>% summarise(stn=n()) 
stn_sp$scientific_name <- factor(stn_sp$scientific_name, levels = stn_sp$scientific_name[order(stn_sp$stn,decreasing = TRUE)])
unique_an$scientific_name <- factor(unique_an$scientific_name, levels =  stn_sp$scientific_name[order(stn_sp$stn,decreasing = TRUE)])
ggplot(unique_an,aes(scientific_name,station_name))+geom_point(size=3, colour='red')+theme_linedraw()+ 
  theme(axis.text.x=element_text(size = 10,angle=20, hjust=1),axis.title.x=element_blank())


