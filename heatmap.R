
#---HEATMAP

library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) 
library(tidyr)

#input file
df <- read_csv(df, "outputs/df.csv")

df_heat <- df %>% filter(species =="NBHF", station == "bpns-Nautica Ena") %>% 
                   mutate(year = as.numeric(format(time, "%Y", tz="UTC")), month = as.numeric(format(time, "%m", tz="UTC")), day = as.numeric(format(time, "%d", tz="UTC")), hour = as.numeric(format(time, "%H", tz="UTC")),
                          noise = number_clicks_total - number_clicks_filtered,)

p <-ggplot(df_heat,aes(day,hour,fill=noise))+ #change fill = dpm or noise
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Clicks",option ="C")
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse",breaks = unique(df$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Noise -  bpns-Nautica Ena"), x="Day", y="Hour")
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
