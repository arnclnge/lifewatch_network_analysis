library(tidyverse)
library(lessR)
library(tidyr)

setwd("D:/VLIZ_Network_Analysis")

#input data
load("D:/VLIZ_Network_Analysis/R/cpod_df_20180701_20220801_week_hourly.Rdata")
cpod1_df <- as.data.frame(cpod1_df)
season <- as.data.frame(read_csv("R/seasons.csv"))

#check station names for consistency
cpod1_df$station[cpod1_df$station=="bpns-Reefballs belwind"] <- "bpns-Reefballs Belwind"

#convert time to POSIXct class
cpod1_df[,2] <- as.POSIXct(cpod1_df[,2], format = "%Y-%m-%d %H:%M:%S", tz="UTC")
season[,6] <- as.POSIXct(season[,6], format = "%Y-%m-%d %H:%M:%S", tz="UTC")
#add season
df <- cpod1_df %>% mutate(date=format(time,"%Y-%m-%d",tz="UTC"),year = as.numeric(format(time, "%Y", tz="UTC")), month = as.numeric(format(time, "%m", tz="UTC")), day = as.numeric(format(time, "%d", tz="UTC")), hour = as.numeric(format(time, "%H", tz="UTC")),
                   noise = number_clicks_total - number_clicks_filtered) %>%
                   mutate(Season = case_when (time >= season[1,6]  & time < season[2,6] ~ season[1,1],
                             time >= season[2,6]  & time < season[3,6] ~ season[2,1],
                             time >= season[3,6]  & time < season[4,6] ~ season[3,1],
                             time >= season[4,6]  & time < season[5,6] ~ season[4,1],
                             time >= season[5,6]  & time < season[6,6] ~ season[5,1],
                             time >= season[6,6]  & time < season[7,6] ~ season[6,1],
                             time >= season[7,6]  & time < season[8,6] ~ season[7,1],
                             time >= season[8,6]  & time < season[9,6] ~ season[8,1],
                             time >= season[9,6]  & time < season[10,6] ~ season[9,1],
                             time >= season[10,6]  & time < season[11,6] ~ season[10,1],
                             time >= season[11,6]  & time < season[12,6] ~ season[11,1],
                             time >= season[12,6]  & time < season[13,6] ~ season[12,1],
                             time >= season[13,6]  & time < season[14,6] ~ season[13,1],
                             time >= season[14,6]  & time < season[15,6] ~ season[14,1],
                             time >= season[15,6]  & time < season[16,6] ~ season[15,1], 
                             time >= season[16,6]  & time < season[17,6] ~ season[16,1],
                             time >= season[17,6]  & time < season[18,6] ~ season[17,1],
                             time >= season[18,6]  & time < season[19,6] ~ season[18,1],
                             time >= season[19,6]  & time < season[20,6] ~ season[19,1]))

df$station[df$station=="AP_bpns-Grafton"] <- "bpns-Grafton"
df$station[df$station=="AP-bpns-Birkenfels"] <- "bpns-Birkenfels"
df$station[df$station=="AP-bpns-Belwind"] <- "bpns-Reefballs Belwind"
df$station[df$station=="AP-bpns-Cpower"] <- "bpns-Reefballs-cpower"

df <- df %>% filter(station != "bpns-G88")

###CHECKING FOR DUPLICATES 

#check if there are duplicates
duplicates0 <- cpod1_df[duplicated(cpod1_df[,c(1,2,3,13,17)]),c(1,2,3,13,17)]
duplicates0[,2] <- as.POSIXct(duplicates0[,2], format = "%Y-%m-%d %H:%M:%S", tz="UTC")
duplicates <- merge(duplicates0,cpod1_df)
save(duplicates,file="outputs/cpod1_df_duplicates.Rdata")

#remove duplicated rows where recorded = 10 mins, retain only 60 mins.
duplicates_10mins <- duplicates %>% filter(recorded==10)
df <- anti_join(df,duplicates_10mins)
write_csv(df, "outputs/df.csv")

#double check if there are duplicates
duplicates0 <- df[duplicated(df[,c(1,2,3,13,17)]),c(1,2,3,13,17)]
duplicates0[,2] <- as.POSIXct(duplicates0[,2], format = "%Y-%m-%d %H:%M:%S", tz="UTC")
duplicates <- merge(duplicates0,df)
save(duplicates,file="outputs/cpod1_df_duplicates_60mins.Rdata")
#---still there are duplicates both with 60 mins of recordings

###DATA EXPLORATION

df %>% ggplot(aes(time, dpm, colour = station)) + geom_point(size = 0.1) 

#analysis by day
df_day <- df %>% group_by(station,longitude, latitude,deployment_fk,date,time,hour,Season,species) %>% summarise(number_clicks_filtered=sum(number_clicks_filtered), number_clicks_total=sum(number_clicks_total), noise=sum(noise), dph=if_else(dpm>0,1,0), dpm=sum(dpm), lost_minutes=sum(lost_minutes)) %>% 
  group_by(station,longitude, latitude,deployment_fk,date,Season, species) %>% summarise(hrs = length(unique(hour)),number_clicks_filtered=sum(number_clicks_filtered), number_clicks_total=sum(number_clicks_total), noise=sum(noise), dph=sum(dph), dpm=sum(dpm), lost_minutes=sum(lost_minutes)) %>% 
  mutate(noise_percent = noise/number_clicks_total, dpd = if_else(dpm>0,1,0), lost_hrs = if_else(lost_minutes>9,1,0))
write_csv(df_day, "outputs/df_day.csv")

#---compute station activity
cpod1_df %>% mutate(date=format(time,"%Y-%m-%d",tz="UTC")) %>% group_by(station, deployment_fk, time,date) %>% summarise(hrs_activity = length(unique(time))) %>% 
              group_by(station) %>% summarise(hrs_activity = sum(hrs_activity), no_deployments = length(unique(deployment_fk)), mean_hrs_deploy = hrs_activity/no_deployments, no_days=length(unique(date))) %>% 
              write_csv("outputs/station_activity.csv")

#---visualize station activity
cpod1_df %>% 
  ggplot(aes(time, station)) +
  geom_point(size = 0.1)  +
  ggtitle("C-POD Network Station Activity") +  theme(axis.text.y=element_text(size=12), axis.text.x = element_text(size=12))

#NBHF clicks and noise per station
df_clicks <- cpod1_df %>% filter(species == "NBHF" | species =="Dolphins") %>% select(station, time, number_clicks_filtered,number_clicks_total) %>% 
  group_by(station,time) %>% summarise(number_clicks_total=mean(number_clicks_total), number_clicks_filtered=sum(number_clicks_filtered)) %>% mutate(noise = number_clicks_total - number_clicks_filtered) 
df_long <- gather(df_clicks, type, number, c(number_clicks_filtered,noise), factor_key=TRUE) 
df_long$type <- relevel(df_long$type,"noise")
thm <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size =10), axis.text=element_text(size=12), legend.title=element_blank()) 
m <- ggplot(df_long) + geom_line(aes(x = time, y = number,color = type), size = 0.8) + facet_wrap(~station, nrow=2) + thm + xlab("time") + ylab("number of clicks")+scale_colour_manual(values = c("steelblue", "orange")) 

#Dolphin clicks per station
df_clicks <- df %>% filter(species =="Dolphins") %>% select(station, time, number_clicks_filtered,number_clicks_total) %>% 
  group_by(station,time) %>% summarise(number_clicks_total=mean(number_clicks_total), number_clicks_filtered=sum(number_clicks_filtered)) %>% mutate(noise = number_clicks_total - number_clicks_filtered) 
thm <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size =10), axis.text=element_text(size=12), legend.title=element_blank()) 
m <- ggplot(df_clicks) + geom_line(aes(x = time, y = number_clicks_filtered), size = 0.8) + facet_wrap(~station, nrow=2) + thm + xlab("time") + ylab("number_clicks_filtered - Dolphins")+scale_colour_manual(values = "orange") 

#deployment activity
cpod1_df$deployment_fk <- as.factor(cpod1_df$deployment_fk)
cpod1_df %>% 
  ggplot(aes(time, deployment_fk)) +
  geom_point(size = 0.1, color="orange")  +
  ggtitle("Deployment Activity") +  theme(axis.text.x = element_text(size=12),axis.ticks.y=element_blank(),axis.text.y=element_blank())+facet_wrap(~station,nrow=2)
  
#hours of activity per season
df %>% group_by(station,Season) %>% summarise(hrs=length(unique(time))) %>% group_by(Season) %>% summarise(hrs=sum(hrs))

###---------ANALYSIS

#boxplots of daily clicks
p_meds <- df_day %>% filter(species == "NBHF") %>% ddply(.(station), summarise, med = median(number_clicks_filtered))
p <- df_day %>% filter(species == "NBHF") %>% ggplot(aes(x=station, y=number_clicks_filtered)) + 
  geom_boxplot() +labs(y= "Daily clicks - NBHF")+ geom_text(data = p_meds, aes(x = station, y = med, label = med), size = 3, vjust = 2)
ggsave("outputs/daily_clicks_nbhf.png")

p <- df_day %>% filter(species == "NBHF") %>% ggplot(aes(x=station, y=number_clicks_filtered, fill=Season)) + 
  geom_boxplot() +labs(y= "Daily clicks - NBHF")
ggsave("outputs/daily_clicks_nbhf_season.png")

p_meds <- df_day %>% filter(species == "Dolphins") %>% ddply(.(station), summarise, med = median(number_clicks_filtered))
p <- df_day %>% filter(species == "Dolphins") %>% ggplot(aes(x=station, y=number_clicks_filtered)) + 
  geom_boxplot() +labs(y= "Daily clicks - Dolphins")
ggsave("outputs/daily_clicks_dol.png")

p <- df_day %>% filter(species == "Dolphins") %>% ggplot(aes(x=station, y=number_clicks_filtered, fill=Season)) + 
  geom_boxplot() +labs(y= "Daily clicks - Dolphins")
ggsave("outputs/daily_clicks_dol_season.png")

p <- df_day %>% filter(species == "NBHF") %>% ggplot(aes(x=station, y=noise, fill=Season)) + 
  geom_boxplot() +labs(y= "Daily clicks - noise") 
ggsave("outputs/daily_clicks_noise.png")

#---------noise percent over time
p <- ggplot(df_day, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()


#---waffle plot: Proportions
library(waffle)

#computation of proportion of noise clicks
p <- df %>% filter(species=="NBHF") %>% group_by(station) %>% 
            summarise(noise_percent = sum(noise)/sum(number_clicks_total)*100, porp_percent = sum(number_clicks_filtered)/sum(number_clicks_total)*100) %>% 
            gather(key = "Clicks", value = "Percent", 2:3, factor_key=TRUE) %>% 
            mutate(Clicks = case_when(Clicks == 'noise_percent' ~ 'noise',TRUE ~ 'NBHF'))

#computation of proportion of minutes with an NBHF train
p <- df_day %>% filter(species=="NBHF") %>% group_by(station) %>% 
  summarise(dpm=sum(dpm), hrs =sum(hrs)) %>% 
  mutate(mins_active = hrs*60, dpm_percent = (dpm/mins_active)*100, mins_inactive_percent = 100-dpm_percent) %>% 
  gather(key = "Clicks", value = "Percent", 5:6, factor_key=TRUE) %>% 
  mutate(Clicks = case_when(Clicks == 'dpm_percent' ~ 'DPM - NBHF',TRUE ~ 'No detection minutes'))

#computation of proportion of minutes with an NBHF train per season
p<- df_day %>% filter(species=="NBHF", station != "bpns-G88", Season =="June Solstice") %>% group_by(station, Season) %>% 
  summarise(dpm=sum(dpm), hrs =sum(hrs)) %>% 
  mutate(mins_active = hrs*60, dpm_percent = (dpm/mins_active)*100) %>% arrange(desc(dpm_percent)) %>% mutate(mins_inactive_percent = 100-dpm_percent) %>% 
  gather(key = "Clicks", value = "Percent", 6:7, factor_key=TRUE) %>% 
  mutate(Clicks = case_when(Clicks == 'dpm_percent' ~ 'DPM',TRUE ~ 'No detection minutes')) 

#computation of proportion of lost minutes over total active minutes
p <- df_day %>% filter(species=="NBHF") %>% group_by(station) %>% 
  summarise(lost_minutes=sum(lost_minutes), hrs =sum(hrs)) %>% 
  mutate(mins_active = hrs*60, lost_mins_percent = (lost_minutes/mins_active)*100, mins_not_lost = 100-lost_mins_percent) %>% 
  gather(key = "Minutes", value = "Percent", 5:6, factor_key=TRUE) %>% 
  mutate(Minutes = case_when(Minutes == 'lost_mins_percent' ~ 'lost minutes',TRUE ~ 'total active minutes'))

##computation of proportion of minutes with a Dolphin train
p <- df_day %>% filter(species=="Dolphins") %>% group_by(station) %>% 
  summarise(dpm=sum(dpm), hrs =sum(hrs)) %>% 
  mutate(mins_active = hrs*60, dpm_percent = (dpm/mins_active)*100, mins_inactive_percent = 100-dpm_percent) %>% 
  gather(key = "Clicks", value = "Percent", 5:6, factor_key=TRUE) %>% 
  mutate(Clicks = case_when(Clicks == 'dpm_percent' ~ 'DPM - Dolphins',TRUE ~ 'No detection minutes'))

#plot! change fill according to field name of p (Clicks/Minutes)
p1 <- ggplot(p, aes(fill = Clicks, values = Percent)) +
geom_waffle(n_rows = 10, size = 0.5, colour = "#ffffff",  flip = TRUE) +
scale_fill_manual(values = c("grey","skyblue")) +
coord_equal() +
theme_minimal() +
theme_enhance_waffle() +
facet_wrap(~reorder(station, -Percent), nrow = 2)+
theme(strip.text.x = element_text(size = 15))
#+ggtitle("March Equinox")

plot_grid(p1, p2,p3, p4, ncol = 2, nrow=2, labels = c("December Solstice", "March Equinox", "June Solstice", "September Equinox"), label_size = 11,label_x=0.3)

#---Receiver Efficiency Index
#OVERVIEW 
REI_overview <- df_day %>% filter (species == "Dolphins") %>% group_by(station) %>% summarise(dph=sum(dph), active_hrs=sum(hrs), lost_hrs = sum(lost_hrs)) %>% 
  mutate(percent_dph = dph/active_hrs*100, percent_activity = active_hrs/total_hrs_network*100, percent_losthrs = lost_hrs/active_hrs*100)
write_csv(REI_overview,"outputs/REI/REI_overview_Dol.csv")

#days
total_days_network = (as.numeric(difftime("2018-07-01", "2022-06-23", units = "days"))*-1)+1
REI <- df_day %>% filter (species == "NBHF") %>% group_by(station, date) %>% summarise(dph=sum(dph)) %>% mutate(dpd = if_else(dph>0,1,0)) %>% 
                  group_by(station) %>% summarise(dpd=sum(dpd), active_days = length(date)) %>% 
                  mutate(percent_dpd = dpd/active_days, percent_activity = total_days_network/active_days, REI = percent_dpd*percent_activity)
write_csv(REI,"outputs/REI_days.csv")

#hour (some days have hrs > 24 because of two deployments within the same period)
total_hrs_network = ((as.numeric(difftime("2018-07-01", "2022-06-23", units = "days"))*-1)+1)*24
REI <- df_day %>% filter (species == "NBHF") %>% group_by(station, Season) %>% summarise(dph=sum(dph), active_hrs=sum(hrs)) %>% 
  mutate(percent_dph = dph/active_hrs, activity_index = total_hrs_network/active_hrs, REI = percent_dph*activity_index)

write_csv(REI,"outputs/REI_hrs_per_season.csv")

#with lost mins
REI <- df_day %>% filter (species == "NBHF") %>% group_by(station, Season) %>% summarise(dph=sum(dph), active_hrs=sum(hrs), lost_mins = sum(lost_minutes)) %>% 
  mutate(percent_dph = dph/active_hrs, activity_index = total_hrs_network/active_hrs, active_mins = active_hrs*60, lost_index = (active_mins/lost_mins)/1000, REI = percent_dph*activity_index*lost_index)

write_csv(REI,"outputs/REI_hrs_w_lost_per_season.csv")

#with dolphins
REI <- df_day %>% filter (species == "NBHF"|species=="Dolphins") %>% group_by(station, Season,species) %>% summarise(dph=sum(dph), active_hrs=sum(hrs), lost_mins = sum(lost_minutes)) %>% 
                  spread(species,dph) %>% group_by(station,Season) %>% summarise(active_hrs=mean(active_hrs),Dol=sum(Dolphins,na.rm=TRUE),NBHF=sum(NBHF,na.rm=TRUE),lost_mins=lost_mins) %>% 
                  mutate(percent_dph_nbhf = NBHF/active_hrs,percent_dph_dol=Dol/active_hrs, active_mins = active_hrs*60,activity_index = total_hrs_network/active_hrs, lost_index = (active_mins/lost_mins)/1000, REI = percent_dph_nbhf*percent_dph_dol*activity_index*lost_index*100)

write_csv(REI,"outputs/REI_DOLPHIN_hrs_per_season.csv")

#REI based on NBHF/Dolphins DPM, changed total hrs for Nautica Ena, lost hours and activity index, to view seasonal REI: group_by(station,Season)
total_hrs_NauticaEna = (((as.numeric(difftime("2018-07-01", "2021-06-29", units = "days"))*-1)+1)*24)+6+8070 #add the diff of total hrs of network to Nautica's total hrs

REI <- df_day %>% filter (species == "Dolphins") %>% group_by(station,Season) %>% summarise(dph=sum(dph), active_hrs=sum(hrs), lost_hrs = sum(lost_hrs)) %>% 
  mutate(percent_dph = dph/active_hrs, activity_index = total_hrs_network/active_hrs, lost_index = (active_hrs/lost_hrs)/100, REI = percent_dph*activity_index*lost_index) %>% 
  mutate(activity_index = if_else(station=="bpns-Nautica Ena", total_hrs_NauticaEna/active_hrs,activity_index),
         REI = percent_dph*activity_index*lost_index)
write_csv(REI,"outputs/REI/REI_seasonal_losthrs10min_Dol.csv")

#PLOT REI
p <- REI %>% ggplot(aes(x=station, y=REI, colour=Season, size=Season)) + 
  geom_point(size=5) +labs(y= "REI")+theme(axis.text.x=element_text(size=10.25))+ggtitle("Receiver Efficiency Index (REI) - Dolphins")
                                                  
#issue - double entry
x <- df_day %>% filter(date=="2018-11-30", station == "bpns-Reefballs Belwind", species=="NBHF" )

#---STATISTICAL TESTS

#normality of daily clicks
tapply(X=df_day$number_clicks_filtered, INDEX=df_day$station, FUN=shapiro.test) #--> not normal

#Kruskal-Wallis Test
kruskal.test(number_clicks_filtered ~ station, data = df_day) # significant station effect
kruskal.test(number_clicks_filtered ~ Season, data = df_day) # significant seasonal effect
kruskal.test(number_clicks_filtered ~ interaction(Season,station), data = df_day) #significant interaction effect

pairwise.wilcox.test(df_day$number_clicks_filtered, df_day$station,
                     p.adjust.method = "BH")

pairwise.wilcox.test(df_day$number_clicks_filtered, df_day$Season,
                     p.adjust.method = "BH")

pairwise.wilcox.test(df_day$number_clicks_filtered, interaction(df_day$station,df_day$Season),
                     p.adjust.method = "BH")


#noise
kruskal.test(noise ~ station, data = df_day) # significant station effect
pairwise.wilcox.test(df_day$noise, df_day$station,
                     p.adjust.method = "BH")

kruskal.test(noise ~ Season, data = df_day) # significant seasonal effect
kruskal.test(noise ~ interaction(Season,station), data = df_day) #significant interaction effect

#dpm
kruskal.test(dpm ~ station, data = df_day) # significant station effect
kruskal.test(dpm ~ Season, data = df_day) # significant seasonal effect
kruskal.test(dpm ~ interaction(Season,station), data = df_day) #significant interaction effect

#lost_mins
kruskal.test(lost_minutes ~ station, data = df_day) # significant station effect
kruskal.test(lost_minutes ~ Season, data = df_day) # significant seasonal effect
kruskal.test(lost_minutes ~ interaction(Season,station), data = df_day) #significant interaction effect

#---MAPPING DF

interp_csv <- df_day %>% filter(species=="Dolphins") %>% group_by(station,Season) %>% summarise(sum(number_clicks_filtered), sum(number_clicks_total), sum(noise), sum(dph), sum(dpm), sum(lost_minutes))

#add lat long
stn_list <- read_csv("R/stn_list.csv")

stn_list$location_col[stn_list$location_col=="bpns-Belwindreefballs-CPOD"] <- "bpns-Reefballs Belwind"
stn_list$location_col[stn_list$location_col=="bpns-Cpowerreefballs-CPOD"] <- "bpns-Reefballs-cpower"
stn_list$location_col[stn_list$location_col=="bpns-Nauticaena"] <- "bpns-Nautica Ena"
interp_csv$latitude <- stn_list$latitude[match(interp_csv$station,stn_list$location_col)]
interp_csv$longitude <- stn_list$longitude[match(interp_csv$station,stn_list$location_col)]

write_csv(interp_csv, "outputs/interp_dol_season.csv")



