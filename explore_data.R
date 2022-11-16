library(etn)
library(tidyverse)
con <- connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

projs <- c("bpns", "ws1", "ws2","ws3","cpodnetwork")

#get active deployments
recv <- get_receivers(status = c("Available"))

deploy <- get_deployments(network_project_code = projs)
deploy_active <- deploy %>% filter(deploy_date_time > as.POSIXct("2021-12-31 00:00:00", tz="UTC"))
stn_active <- deploy_active %>% distinct(station_name)
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



#how many detected individuals per station?
x <- detect_activeStn %>% group_by(network_project_code,scientific_name,year) %>% summarise(no_individuals = length(unique(animal_id))) %>% 
                                              filter(scientific_name!="Built-in")
ggplot(x,aes(as.factor(year),no_individuals)) + geom_bar(aes(fill = scientific_name), position = "dodge",stat='identity')+theme(axis.text.x=element_text(size = 10,angle=15))
write_csv(x, "~/etn_analysis/network_species.csv")

