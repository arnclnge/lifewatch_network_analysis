library(tidyverse)
library(leaflet)

setwd("D:/VLIZ_Network_Analysis")

#Set inputs
an <- read_csv("fish_data/animals_download_date_20220822.csv")
deploy <- read_csv("fish_data/deployments_all_download_date_20220822.csv")
tags <- read_csv("fish_data/tags_download_date_20220822.csv")

#input detect_clean from fish_explore.R

detect_bpns_cpod <- detect_clean %>% filter(acoustic_project_code=="bpns" | acoustic_project_code=="cpodnetwork") 
detect_cpod <- detect_clean %>% filter(acoustic_project_code=="cpodnetwork") 

#summary stats of cpod
total_tags <- length(unique(detect_cpod$tag_serial_number))
total_species <- length(unique(detect_cpod$scientific_name))
total_detection_days <- detect_cpod %>% group_by(station_name) %>% summarise(N = length(unique(date))) 
total_detection_days <- sum(total_detection_days$N)
total_network_days <- (as.numeric(difftime( "2022-07-29","2018-10-09", units = "days")))+1

summary_tags <- detect_cpod %>% group_by(station_name) %>% summarise(N = length(unique(tag_serial_number))) %>% 
  summarise(Total=total_tags, mean = mean(N), SD = sd(N), Max =max(N))
summary_dets <- detect_cpod %>% group_by(station_name) %>% summarise(N = n()) %>% 
  summarise(Total=sum(N), mean = mean(N), SD = sd(N), Max =max(N))
summary_det_days <- detect_cpod %>% group_by(station_name) %>% summarise(N = length(unique(date))) %>% 
  summarise(Total=sum(N), mean = mean(N), SD = sd(N), Max =max(N))
summary_species <- detect_cpod %>% group_by(station_name) %>% summarise(N = length(unique(scientific_name))) %>% 
  summarise(Total=total_species, mean = mean(N), SD = sd(N), Max =max(N))
summary_stats<- dplyr::bind_rows(summary_tags,summary_dets,summary_det_days,summary_species)
write_csv(summary_stats, "outputs/telemetry/summary_stats_cpod.csv")

################## COMPUTATION OF REI ###########################
total_tags <- length(unique(detect_bpns_cpod$tag_serial_number))
total_species <- length(unique(detect_bpns_cpod$scientific_name))
total_detection_days <- detect_bpns_cpod %>% group_by(station_name) %>% summarise(N = length(unique(date))) 
total_detection_days <- sum(total_detection_days$N)
total_network_days <- (as.numeric(difftime( "2022-08-07","2014-07-11", units = "days")))+1

# deploy days should be maximum 15 months or 456.25 days
deploy_bpns_cpod <- deploy %>% as.data.frame() %>% filter(acoustic_project_code=="bpns"| acoustic_project_code=="cpodnetwork") %>% group_by(station_name,acoustic_project_code,deployment_id, deploy_date_time,recover_date_time) %>% filter(!is.na(recover_date_time))%>% 
  summarise(deploy_days = (as.numeric(difftime(recover_date_time, deploy_date_time, units = "days")))+1) %>% 
  mutate(deploy_days = if_else(deploy_days > 456.25, 456.25, deploy_days)) %>% 
  group_by(station_name,acoustic_project_code) %>% summarise(deploy_days = sum(deploy_days,na.rm=TRUE)) 

REI <- detect_bpns_cpod %>% group_by(station_name, acoustic_project_code) %>% summarise(no_tags = length(unique(tag_serial_number)), 
                                                            no_species = length (unique(scientific_name)),
                                                            detection_days = length(unique(date))) %>% 
  mutate(deploy_days= deploy_bpns_cpod$deploy_days[match(station_name,deploy_bpns_cpod$station_name)],
         rei = (no_tags/total_tags)*(no_species/total_species)*(detection_days/total_detection_days)*(total_network_days/deploy_days)*1000)

sumREI <- sum(REI$rei)
REI$Percent_REI <- REI$rei/sumREI*100
REI$Rank <- rank(-REI$Percent_REI)

write_csv(REI, "outputs/telemetry/REI_withcpod.csv")

#----CUMULATIVE CURVES

#add REI ranking to detect_bpns_cpod
detect_bpns_cpod$receiver_rank <- REI$Rank[match(detect_bpns_cpod$station_name,REI$station_name)]
#order by REI rank
detect_bpns_cpod <- detect_bpns_cpod[order(detect_bpns_cpod$receiver_rank),]

#TAGS
tags_cumsum <- detect_bpns_cpod %>%
  mutate(cum_unique_entries = cumsum(!duplicated(tag_serial_number))) %>%
  group_by(receiver_rank,acoustic_project_code) %>%
  summarise(cum_unique_entries = last(cum_unique_entries))

breaks = seq(50, total_tags, by=50)
labels = as.character(breaks)

ggplot(tags_cumsum, aes(x = receiver_rank, y = cum_unique_entries,color=acoustic_project_code)) + geom_line() + geom_point() +theme_bw()+
  scale_y_continuous(limits = c(50, total_tags), breaks = breaks, labels = labels,name = "No. of tags")+
  geom_vline(aes(xintercept =45, color = "REI > 0.16%"),linetype="dotted", size=1.5)+
  geom_hline(aes(yintercept=total_tags*0.75,color = "75% benchmark"), linetype="dashed")+
  scale_color_manual(values = c("red", "black","orange","blue"))+
  theme(legend.title=element_blank(), legend.position="bottom")

#SPECIES
sp_cumsum <- detect_bpns_cpod %>%
  mutate(cum_unique_entries = cumsum(!duplicated(scientific_name))) %>%
  group_by(receiver_rank,acoustic_project_code) %>%
  summarise(cum_unique_entries = last(cum_unique_entries))


ggplot(sp_cumsum, aes(x = receiver_rank, y = cum_unique_entries,color=acoustic_project_code)) + geom_line() + geom_point() +theme_bw()+
  geom_hline(aes(yintercept=total_species,color = "100% benchmark"), linetype="dashed")+
  scale_y_continuous(name = "No. of species")+
  geom_vline(aes(xintercept =45, color = "REI > 0.16%"),linetype="dotted", size=1.5)+
  scale_color_manual(values = c("red", "black","orange","blue"))+
  theme(legend.title=element_blank(), legend.position="bottom")


#DETECTIONS
det_cumsum <- detect_bpns_cpod %>%
  mutate(cum_unique_entries = cumsum(!duplicated(detection_id))) %>%
  group_by(receiver_rank,acoustic_project_code) %>%
  summarise(cum_unique_entries = last(cum_unique_entries))

ggplot(det_cumsum, aes(x = receiver_rank, y = cum_unique_entries,color=acoustic_project_code)) + geom_line() + geom_point() +theme_bw()+
  geom_hline(aes(yintercept=last(cum_unique_entries)*0.75,color = "75% benchmark"), linetype="dashed")+
  scale_y_continuous(name = "No. of detections")+
  geom_vline(aes(xintercept =45, color = "REI > 0.16%"),linetype="dotted", size=1.5)+
  scale_color_manual(values = c("red", "black","orange","blue"))+
  theme(legend.title=element_blank(), legend.position="bottom")

#species plot by ranking
unique_an <- detect_bpns_cpod %>% group_by(station_name,acoustic_project_code,receiver_rank,scientific_name) %>% summarise(detections=n())
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

