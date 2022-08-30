library(ggpmisc)
library(ggplot2)
library(ggpubr)

theme_set(
  theme_minimal() +
    theme(legend.position = "top")
)

#analysis per deployment

#PER SEASON
df_deploy <- df %>% group_by(deployment_fk, station,Season, species) %>% summarise(days = length(unique(date)), hrs = length(unique(hour)),number_clicks_filtered=sum(number_clicks_filtered), number_clicks_total=sum(number_clicks_total), noise=sum(noise), dph=sum(dph), dpm=sum(dpm), lost_minutes=sum(lost_minutes)) %>% 
  mutate(noise_percent = noise/number_clicks_total)
df_deploy$station <- as.factor(df_deploy$station)

#--- Days of activity vs DPM per deployment
b <- df_deploy %>% filter(Season =="December Solstice",species=="NBHF" |species=="Dolphins") %>% group_by(deployment_fk,station) %>% summarise(days = mean(days), dpm=sum(dpm)) %>% ggplot(aes(x = days, y = dpm)) 

b1 <- b + geom_point(aes(color = station, shape = station)) +
  geom_rug(aes(color =station)) +
  geom_smooth(aes(color = station), method = lm, 
              se = FALSE, fullrange = FALSE)+
  ggpubr::stat_cor(aes(color = station), label.x = 0.5)+thm

ggarrange(b1,b2,b3,b4,ncol = 2, nrow=2, labels = c("December Solstice", "March Equinox", "June Solstice", "September Equinox"),
          font.label = list(size = 10.5), common.legend = TRUE, legend="bottom", label.y = 1.015)

ggsave("outputs/corr_deploydays_dpm_season.png")

#--- Lost minutes vs DPM per deployment
b <- df_deploy %>% filter(Season =="June Solstice",species=="NBHF") %>% group_by(deployment_fk,station) %>% summarise(days = mean(days), lost_minutes=sum(lost_minutes)) %>% ggplot(aes(x = days, y = lost_minutes)) 

b3 <- b + geom_point(aes(color = station, shape = station)) +
  geom_rug(aes(color =station)) +
  geom_smooth(aes(color = station), method = lm, 
              se = FALSE, fullrange = FALSE)+
  ggpubr::stat_cor(aes(color = station), label.x = 0.5)

ggarrange(b1,b2,b3,b4,ncol = 2, nrow=2, labels = c("December Solstice", "March Equinox", "June Solstice", "September Equinox"),
          font.label = list(size = 10), common.legend = TRUE, legend="bottom", label.y = 1.02)

ggsave("outputs/corr_deploydays_dpm_season.png")



#TOTAL

df_deploy <- df %>% group_by(deployment_fk, station, species) %>% summarise(days = length(unique(date)), hrs = length(unique(hour)),number_clicks_filtered=sum(number_clicks_filtered), number_clicks_total=sum(number_clicks_total), noise=sum(noise), dph=sum(dph), dpm=sum(dpm), lost_minutes=sum(lost_minutes)) %>% 
  mutate(noise_percent = noise/number_clicks_total)

#--- Days of activity vs DPM per deployment
b <- df_deploy %>% filter(species=="NBHF" |species=="Dolphins") %>% group_by(deployment_fk,station) %>% summarise(days = mean(days), noise=mean(noise),dpm=sum(dpm)) %>% ggplot(aes(x = days, y = dpm)) 

#linear model
model <- df_deploy %>% group_by(station) %>% do(model = lm(dpm~days, data =.))

b + geom_point(aes(color = station, shape = station)) +
  geom_rug(aes(color =station)) +
  geom_smooth(aes(color = station), method = lm, 
              se = FALSE, fullrange = FALSE)+
  ggpubr::stat_cor(aes(color = station), label.x = 0.5)+
  ggpubr::stat_regline_equation(aes(color=station),label.x = 15)+thm

#--- Lost minutes vs DPM per deployment
b <- df_deploy %>% filter(species=="NBHF") %>% group_by(deployment_fk,station) %>% summarise(days = mean(days), lost_minutes=sum(lost_minutes)) %>% ggplot(aes(x = days, y = lost_minutes)) 

b + geom_point(aes(color = station, shape = station)) +
  geom_rug(aes(color =station)) +
  geom_smooth(aes(color = station), method = lm, 
              se = FALSE, fullrange = FALSE)+
  ggpubr::stat_cor(aes(color = station), label.x = 0.5)

#--- Noise vs DPM 
b <- df_day %>% filter(species=="NBHF" |species=="Dolphins") %>% ggplot(aes(x = noise, y = dpm)) 
b + geom_point(aes(color = station, shape = station)) +
  geom_rug(aes(color =station)) +
  geom_smooth(aes(color = station), method = lm, 
              se = FALSE, fullrange = FALSE)+
  ggpubr::stat_cor(aes(color = station), label.x = 3500000)+
  ggpubr::stat_regline_equation(aes(color=station),label.x = 5000000)+thm

