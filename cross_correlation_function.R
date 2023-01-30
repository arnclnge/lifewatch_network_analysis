library(ggExtra) 
library(tidyr)
library(ggpubr)

setwd("~/lifewatch_network_analysis/cpod/")

#input file
df <- read_csv("csv/df.csv")
stn <- "Buitenratel"

df_heat <- df %>% filter(species =="NBHF", station == stn) %>% 
  mutate(year = as.numeric(format(time, "%Y", tz="UTC")), month = as.numeric(format(time, "%m", tz="UTC")), day = as.numeric(format(time, "%d", tz="UTC")), hour = as.numeric(format(time, "%H", tz="UTC")),
         noise = number_clicks_total - number_clicks_filtered,)

tide_df <- read_csv("csv/tide_df.csv") %>% filter(station==stn) %>% select(-c("year","month","day","hour"))

# combine dfs

df_ccf <- merge(df_heat,tide_df,by.x=c("time","station"),by.y=c("datetime", "station"))
df_ccf <- df_ccf %>% filter(time > as.POSIXct("2021-07-01 01:00:00", tz="UTC"))

# passing to a long format
df_ccf_long <- df_ccf %>% pivot_longer(cols = c(noise,sea_surface_height_above_sea_level), names_to = "names", values_to = "value") 
df_ccf_long <- df_ccf_long[!duplicated(df_ccf_long), ]

# plot time series
coeff <- 100000
ggplot(df_ccf, aes(x=time)) +
  geom_line( aes(y=sea_surface_height_above_sea_level, color = "Sea surface height above sea level (m)"), size=0.25) + 
  geom_line( aes(y=noise/coeff, color = "Noise clicks"), size=0.25) +
  scale_y_continuous(name = "Sea surface height above sea level (m)", sec.axis = sec_axis(~.*coeff,name="Noise clicks"))+
  scale_color_manual(values=c("blue", "black"))+theme_classic()+theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "bottom") #+scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 

# ccf time series
par(mfrow=c(1,1))
ccf_plot <- ccf(df_ccf$noise, df_ccf$sea_surface_height_above_sea_level,
    main = "Cross-Correlation Plot", 
    ylab = "CCF")


