
#---HEATMAP

library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) 
library(tidyr)

setwd("~/lifewatch_network_analysis/cpod/")

#input file
df <- read_csv("csv/df.csv")
stn <- "Buitenratel"

df_heat <- df %>% filter(species =="NBHF", station == stn) %>% 
                   mutate(year = as.numeric(format(time, "%Y", tz="UTC")), month = as.numeric(format(time, "%m", tz="UTC")), day = as.numeric(format(time, "%d", tz="UTC")), hour = as.numeric(format(time, "%H", tz="UTC")),
                          noise = number_clicks_total - number_clicks_filtered,)

p <-ggplot(df_heat,aes(day,hour,fill=noise))+ #change fill = dpm or noise
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Clicks",option ="C")
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse",breaks = unique(df$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste0("Noise - ",stn), x="Day", y="Hour")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p

ggsave(paste0("plots/heatmap_noise_",stn,".png"), dpi =300, width = 14, height=9)


#plot tide data
tide_data <- read_csv("csv/tidal_output.csv")
stn_geometry <- cpod1_df %>% group_by(station, latitude,longitude) %>% summarise()
tide_data$station <- stn_geometry$station[match(tide_data$Longitude,stn_geometry$longitude)]

stn <- "Grafton"

tide_heat <- tide_data %>% filter(station == stn) %>% 
  mutate(year = as.numeric(format(datetime, "%Y", tz="UTC")), month = as.numeric(format(datetime, "%m", tz="UTC")), day = as.numeric(format(datetime, "%d", tz="UTC")), hour = as.numeric(format(datetime, "%H", tz="UTC")))

p <-ggplot(tide_heat,aes(day,hour,fill=sea_surface_height_above_sea_level))+ #change fill = dpm or noise
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="m",option ="C", limits=c(-3, 3.4))
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse",breaks = unique(df$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste0("Sea surface height above sea level - ",stn), x="Day", y="Hour")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p

ggsave(paste0("plots/heatmap_tide_",stn,".png"), dpi =300, width = 14, height=9)
