library(tidyverse)
library(leaflet)
library(etn)

con <- connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

setwd("~/lifewatch_network_analysis/")

#---Extract data

projs <- c("bpns", "ws1", "ws2","ws3","cpodnetwork")      #  "bpns", "ws1", "ws2","ws3","cpodnetwork"
sp <- c("Alosa fallax", "Anguilla anguilla", "Gadus morhua", "Dicentrarchus labrax","Raja clavata") #"Alosa fallax", "Anguilla anguilla", "Gadus morhua", "Dicentrarchus labrax","Raja clavata"

#SETTINGS

#get active deployments
recv <- get_acoustic_receivers(status = c("available"))
deploy <- get_acoustic_deployments(acoustic_project_code = projs, open_only = FALSE)
deploy_active <- get_acoustic_deployments(acoustic_project_code = projs, open_only = TRUE)
deploy_active <- deploy_active %>% filter(deploy_date_time > as.POSIXct("2021-12-31 00:00:00", tz="UTC") | battery_estimated_end_date > Sys.Date())

#remove new stations = deployments which only started in 2021
#first_deploy <- deploy %>% group_by(station_name) %>% summarise(first_deploy = min(deploy_date_time)) 

stn_active <- deploy_active %>% summarise(acoustic_project_code, station_name, deploy_longitude,deploy_latitude) %>% unique() 
#                  %>% mutate(first_deploy = first_deploy$first_deploy[match(station_name, first_deploy$station_name)]) %>% 
#                  filter(first_deploy < as.POSIXct("2021-01-01 00:00:00", tz="UTC")) #retain only stations whose first deployments were prior to 2021

deploy <- deploy %>% filter(station_name %in% stn_active$station_name)

#get detections of stations with active deployments
detect <- get_acoustic_detections(acoustic_project_code = projs,station_name = stn_active$station_name, start_date = 2014) %>%  #scientific_name =sp for specific species
  mutate(date = as.Date(date_time))

####################################################################################################################################

#---Clean data

#remove NA, built-in and sync tags
detect <- detect %>% filter(scientific_name !="Built-in",scientific_name!="Sync tag" )
detect <- detect %>% filter(!is.na(scientific_name) & !is.na(animal_id))

#check detections not within deploy period
detect = as.data.frame(detect)
detect$deploy_date_time = as.Date(deploy$deploy_date_time[match(detect$deployment_id,deploy$deployment_id)])
detect$recover_date_time = as.Date(deploy$recover_date_time[match(detect$deployment_id,deploy$deployment_id)])
detect_deploy_issues <- detect %>% mutate(within_deploy_recover_period = case_when(as.Date(date_time,"UTC") >= deploy_date_time  & 
                                                                                     as.Date(date_time,"UTC") <= recover_date_time | is.na(recover_date_time)~ "YES", 
                                                                                   TRUE ~ "NO")) %>% 
  filter(within_deploy_recover_period=="NO")

#write_csv(detect_deploy_issues, "csv/detect_deploy_issues.csv")

#remove detections outside of deployment period
detect <- detect %>% mutate(within_deploy_recover_period = case_when(as.Date(date_time,"UTC") >= deploy_date_time  & 
                                                                       as.Date(date_time,"UTC") <= recover_date_time | is.na(recover_date_time)~ "YES", 
                                                                     TRUE ~ "NO")) %>% 
  filter(within_deploy_recover_period=="YES")

#remove detections if a given tag was detected fewer than 5 times per day
detect_rm <- detect %>% group_by(station_name,deploy_longitude,deploy_latitude,as.Date(date_time, "UTC"), tag_serial_number) %>% summarise(tag_n = n()) %>% filter(tag_n<5) %>% as.data.frame()
detect_rm$date <- detect_rm$`as.Date(date_time, "UTC")`
detect <- anti_join(detect,detect_rm, by=c('station_name','deploy_longitude','deploy_latitude','date','tag_serial_number')) 

#if scientific_name = NA, fill out from animal dataframe using tag_serial_number 
#detect_bpns$scientific_name <- if_else(is.na(detect_bpns$scientific_name), an$scientific_name[match(detect_bpns$tag_serial_number,an$tag_serial_number)],detect_bpns$scientific_name)
#an_list2 <- detect_bpns %>% group_by(scientific_name) %>% summarise(no_tags = length(unique(tag_serial_number)),detections = n(),no_stations=length(unique(station_name)))
#write_csv(an_list,"outputs/telemetry/animals_tags_detections.csv")

#check if there are duplicates of detection_id
dup0 <- detect[duplicated(detect[,c("detection_id","date_time")]),c("detection_id","date_time")]
dup0[,2] <- as.POSIXct(dup0[,2], format = "%Y-%m-%d %H:%M:%S", tz="UTC")
dup <- merge(dup0,detect)
write_csv(dup,"csv/detections_20230102_duplicates.csv")

#################################################################

#---Summary stats
network_summary <- detect %>% group_by(acoustic_project_code) %>% summarise(no_stations_active = length(unique(station_name)),
                                                                            total_tags = length(unique(acoustic_tag_id)),
                                                                            total_species = length(unique(scientific_name)),
                                                                            total_dets_days = length(unique(date)),
                                                                            total_detections = n())

#write_csv(network_summary, "csv/network_summary.csv")

total_tags <- length(unique(detect$acoustic_tag_id))
total_species <- length(unique(detect$scientific_name))
total_detection_days <- detect %>% summarise(length(unique(date))) 
total_detection_days <- total_detection_days$`length(unique(date))`
total_network_days <- (as.numeric(difftime(max(Sys.Date()),as.Date(min(deploy$deploy_date_time)), units = "days")))+1


################## COMPUTATION OF REI ###########################

# deploy days should be maximum 15 months or 456.25 days
deploy_summary <- deploy %>% as.data.frame() %>% filter(station_name %in% stn_active$station_name) %>% 
  group_by(station_name,acoustic_project_code,deployment_id, deploy_date_time,recover_date_time) %>% filter(!is.na(recover_date_time))%>% 
  summarise(deploy_days = (as.numeric(difftime(recover_date_time, deploy_date_time, units = "days")))+1) %>% 
  mutate(deploy_days = if_else(deploy_days > 456.25, 456.25, deploy_days)) %>% 
  group_by(acoustic_project_code, station_name) %>% summarise(no_deploy = length(unique(deployment_id)),deploy_days = sum(deploy_days,na.rm=TRUE)) 

REI <- detect %>% group_by(acoustic_project_code,station_name) %>% summarise(no_tags = length(unique(acoustic_tag_id)), 
                                                                             no_species = length (unique(scientific_name)),
                                                                             detection_days = length(unique(date))) %>%
  mutate(deploy_days= deploy_summary$deploy_days[match(station_name,deploy_summary$station_name)],
         rei = (no_tags/total_tags)*(no_species/total_species)*(detection_days/total_detection_days)*(total_network_days/deploy_days)*1000)

sumREI <- sum(REI$rei)
REI$Percent_REI <- REI$rei/sumREI*100
REI$Rank <- rank(-REI$Percent_REI)

REI <- REI %>% mutate(tags_percent = no_tags/total_tags*100, species_percent = no_species/total_species*100, dd_percent = detection_days/1964*100, deploy_percent = deploy_days/total_network_days*100)

#write_csv(REI, paste0("csv/REI_bpns_",sp,".csv"))
write_csv(REI, paste0("csv/REI_bpns_5sp.csv"))

################### CUMULATIVE CURVES #########################

#add REI ranking to detect
detect$receiver_rank <- REI$Rank[match(detect$station_name,REI$station_name)]
#order by REI rank
detect <- detect[order(detect$receiver_rank),]

#TAGS
tags_cumsum <- detect %>%
  mutate(cum_unique_entries = cumsum(!duplicated(acoustic_tag_id))) %>%
  group_by(receiver_rank,acoustic_project_code) %>%
  summarise(cum_unique_entries = last(cum_unique_entries))

#SPECIES
sp_cumsum <- detect %>%
  mutate(cum_unique_entries = cumsum(!duplicated(scientific_name))) %>%
  group_by(receiver_rank,acoustic_project_code) %>%
  summarise(cum_unique_entries = last(cum_unique_entries))

#DETECTIONS
det_cumsum <- detect %>%
  mutate(cum_unique_entries = cumsum(!duplicated(detection_id))) %>%
  group_by(receiver_rank,acoustic_project_code) %>%
  summarise(cum_unique_entries = last(cum_unique_entries))

################### GET BENCHMARKS ###########################

tag_benchmark <- total_tags*0.75
sp_benchmark <- total_species
det_benchmark <- nrow(detect)*0.75

#get minimum receiver ranks to meet performance benchmarks
tag_min_rank <- tags_cumsum %>% filter(cum_unique_entries >= tag_benchmark) %>% as.data.frame() %>% slice_min(receiver_rank) %>% pull(receiver_rank)
sp_min_rank <- sp_cumsum %>% filter(cum_unique_entries >= sp_benchmark) %>% as.data.frame() %>% slice_min(receiver_rank) %>% pull(receiver_rank)
det_min_rank <- det_cumsum %>% filter(cum_unique_entries >= det_benchmark) %>% as.data.frame() %>% slice_min(receiver_rank) %>% pull(receiver_rank)

#decide overall minimum rank of receiver needed to meet ALL 3 performance benchmarks
receiver_overall_rank <- max(tag_min_rank,sp_min_rank,det_min_rank)
REI_overall_rank <- REI$Percent_REI[REI$Rank == receiver_overall_rank]
station_benchmark <- REI$station_name[REI$Rank == receiver_overall_rank]

###################### PLOT CUMULATIVE CURVES ########################
breaks = seq(5, total_tags, by=20)
labels = as.character(breaks)

#tags
ggplot(tags_cumsum, aes(x = receiver_rank, y = cum_unique_entries,color=acoustic_project_code)) + geom_point(size=2) +
  scale_y_continuous(limits = c(0, total_tags), breaks = breaks, labels = labels,name = "No. of tags")+
  geom_vline(aes(xintercept =receiver_overall_rank, color = paste0("REI > ", round(REI_overall_rank,3),"%")),linetype="dotted")+
  geom_hline(aes(yintercept=tag_benchmark,color = "75% benchmark"), linetype="dashed")+
  scale_color_manual(values = c("darkgrey", "black","green", "red","purple","orange","blue"))+
  theme_linedraw()+theme(legend.title=element_blank(), legend.position="bottom")

ggsave("plots/tags_cumsum_BPNS.png", device='png', dpi = 300, width= 7, height = 5)

#species
ggplot(sp_cumsum, aes(x = receiver_rank, y = cum_unique_entries,color=acoustic_project_code)) + geom_point()+
  geom_hline(aes(yintercept=sp_benchmark,color = "100% benchmark"), linetype="dashed")+
  scale_y_continuous(name = "No. of species")+
  geom_vline(aes(xintercept =receiver_overall_rank, color = paste0("REI > ", round(REI_overall_rank,3),"%")),linetype="dotted", size=1.5)+
  scale_color_manual(values = c("darkgrey", "black","green", "red","purple","orange","blue"))+
  theme_linedraw()+theme(legend.title=element_blank(), legend.position="bottom")

ggsave("plots/sp_cumsum_WS.png", device='png', dpi = 300, width= 7, height = 5)

#detections
ggplot(det_cumsum, aes(x = receiver_rank, y = cum_unique_entries,color=acoustic_project_code)) + geom_point(data=det_cumsum,) +
  geom_hline(aes(yintercept=det_benchmark,color = "75% benchmark"), linetype="dashed")+
  scale_y_continuous(name = "No. of detections")+
  geom_vline(aes(xintercept =receiver_overall_rank, color = paste0("REI > ", round(REI_overall_rank,3),"%")),linetype="dotted", size=1.5)+
  scale_color_manual(values = c("darkgrey", "black","green", "red","purple","orange","blue"))+
  theme_linedraw()+theme(legend.title=element_blank(), legend.position="bottom")

ggsave("plots/dets_cumsum_WS.png", device='png', dpi = 300, width= 7, height = 5)

#---HEAT MAP REI
detect$date_hour <- format(detect$date_time,format='%Y-%m-%d %H')

unique_fish <- detect %>% group_by(station_name,receiver_rank,scientific_name) %>% summarise(no_individuals = length(unique(animal_id)), Detection_days = length(unique(date))) %>% 
  as.data.frame()

#add other stations to the plot, those with no detections of the species of interest. Run this section only if running per species
stn_no_detections_sp <- stn_active %>% filter(!station_name %in% unique_fish$station_name)%>% mutate(scientific_name = sp, no_individuals=0) 
unique_fish <- unique_fish %>% bind_rows(stn_no_detections_sp[,c("station_name", "scientific_name", "no_individuals")]) 

#rank stations 
unique_fish$station_name <- factor(unique_fish$station_name, levels = unique(unique_fish$station_name[order(unique_fish$receiver_rank,decreasing = FALSE)]))
#rank by most detected species
stn_sp <- unique_fish %>% group_by(scientific_name) %>% summarise(stn=n()) 
stn_sp$scientific_name <- factor(stn_sp$scientific_name, levels = stn_sp$scientific_name[order(stn_sp$stn,decreasing = FALSE)])
unique_fish$scientific_name <- factor(unique_fish$scientific_name, levels =  stn_sp$scientific_name[order(stn_sp$stn,decreasing = FALSE)])

ggplot(unique_fish, aes(station_name, scientific_name, fill= Detection_days)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="blue") +  #for large detections, log transform the values: + scale_fill_gradient(low="yellow", high="blue", trans="log1p", breaks = c(1000000, 100000,10000,1000,100,10)) 
  geom_text(aes(label = no_individuals), size=3.5)+
  geom_vline(aes(xintercept =station_benchmark,color = "Performance benchmark cut-off"),linetype="dotted", size = 1)+
  theme_bw() + theme(axis.text.x=element_text(size = 9, angle = 90, hjust=1),axis.text.y = element_text(face="italic"),axis.title = element_blank(),
                          legend.position="bottom", legend.text=element_text(size=9),legend.title=element_text(size=9),panel.grid = element_blank()) 

#change file name: WS or BPNS
#ggsave(paste0("plots/",sp," BPNS_REI_heatmap.png"), device='png', dpi = 500, width=13, height=4.8)
ggsave(paste0("plots/WS_REI_heatmap.png"), device='png', dpi = 500, width=13, height=7)

##############################################

#---VISUALIZE species plot by ranking
unique_an <- detect %>% group_by(station_name,acoustic_project_code,receiver_rank,scientific_name) %>% summarise(detections=n())
unique_an$station_name <- factor(unique_an$station_name, levels = unique(unique_an$station_name[order(unique_an$receiver_rank,decreasing = TRUE)]))
stn_sp <- unique_an %>% group_by(scientific_name) %>% summarise(stn=n()) 
stn_sp$scientific_name <- factor(stn_sp$scientific_name, levels = stn_sp$scientific_name[order(stn_sp$stn,decreasing = TRUE)])
unique_an$scientific_name <- factor(unique_an$scientific_name, levels =  stn_sp$scientific_name[order(stn_sp$stn,decreasing = TRUE)])

REI <- REI[order(-REI$Rank),]
labs = REI$acoustic_project_code
redlabs <- "cpodnetwork"
colorlist = c("black","darkorange")
# one of many ways to generate the color labels
axiscolor = colorlist[labs %in% redlabs+1]

ggplot(unique_an,aes(scientific_name,station_name))+geom_point(size=3, colour='red')+theme_linedraw()+ 
  theme(axis.text.x=element_text(size = 10,angle=20, hjust=1),axis.text.y = element_text(color=axiscolor),axis.title.x=element_blank())

#############################################

#---MAP REI + add pins for release locations: prepare data frame for mapping in QGIS

#read REI outputs, then combine
REI_bpns <- read_csv("csv/REI_bpns_5sp.csv")%>% select(station_name, Percent_REI) 
REI_ws <- read_csv("csv/REI_ws_5sp.csv")%>% select(station_name, Percent_REI)
REI_all <- rbind(REI_bpns,REI_ws)

REI_Alosa_bpns <- read_csv("csv/REI_bpns_Alosa fallax.csv") %>% select(station_name, Percent_REI)
REI_Alosa_ws <- read_csv("csv/REI_ws_Alosa fallax.csv") %>% select(station_name, Percent_REI)
REI_Alosa <- rbind(REI_Alosa_bpns,REI_Alosa_ws) %>% rename(REI_Alosa=Percent_REI)

REI_Anguilla_bpns <- read_csv("csv/REI_bpns_Anguilla anguilla.csv") %>% select(station_name, Percent_REI)
REI_Anguilla_ws <- read_csv("csv/REI_ws_Anguilla anguilla.csv") %>% select(station_name, Percent_REI)
REI_Anguilla <- rbind(REI_Anguilla_bpns,REI_Anguilla_ws)%>% rename(REI_Anguilla=Percent_REI)

REI_Dicentrarchus_bpns <- read_csv("csv/REI_bpns_Dicentrarchus labrax.csv") %>% select(station_name, Percent_REI)
REI_Dicentrarchus_ws <- read_csv("csv/REI_ws_Dicentrarchus labrax.csv") %>% select(station_name, Percent_REI)
REI_Dicentrarchus <- rbind(REI_Dicentrarchus_bpns,REI_Dicentrarchus_ws)%>% rename(REI_Dicentrarchus=Percent_REI)

REI_Gadus_bpns <- read_csv("csv/REI_bpns_Gadus morhua.csv") %>% select(station_name, Percent_REI)
REI_Gadus_ws <- read_csv("csv/REI_ws_Gadus morhua.csv") %>% select(station_name, Percent_REI)
REI_Gadus <- rbind(REI_Gadus_bpns,REI_Gadus_ws)%>% rename(REI_Gadus=Percent_REI)

REI_Raja_bpns <- read_csv("csv/REI_bpns_Raja clavata.csv") %>% select(station_name, Percent_REI)
REI_Raja_ws <- read_csv("csv/REI_ws_Raja clavata.csv") %>% select(station_name, Percent_REI)
REI_Raja <- rbind(REI_Raja_bpns,REI_Raja_ws)%>% rename(REI_Raja=Percent_REI)

#merge dataframes
REI_list <- list(REI_Alosa, REI_Anguilla, REI_Dicentrarchus, REI_Gadus, REI_Raja, REI_all)

#check which stations are spelled differently between the data frames, then change if necessary
REI_list %>% reduce(full_join, by='station_name') %>% anti_join(stn_active, by="station_name") 
stn_active$station_name <- recode(stn_active$station_name,"bpns-cpowerreefballs"="bpns-Cpowerreefballs-CPOD")

#merge                                   
REI_map <- REI_list %>% reduce(full_join, by='station_name') %>% merge(stn_active, by="station_name")

write_csv(REI_map, "csv/REI_map.csv")

#---outputs animal release locations csv (for input to interpolation.py)
an = get_animals(scientific_name = sp)
an_release = an %>% group_by(scientific_name,release_longitude,release_latitude) %>% tally()

write_csv(an_release, "csv/an_release.csv")

for (n in 1:n_distinct(an_release$scientific_name)){
  sp_rel = unique(an_release$scientific_name)
  sp = sp_rel[n]
  an_release %>% filter(scientific_name==sp) %>% write_csv(paste0("csv/",sp,"_release.csv"))
}





