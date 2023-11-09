#check for yearly exports
setwd("~/cpod_paper/datasets/")

#DOWNLOADING CPOD DATA FOR YEARLY EXPORTS

start_date = as.Date("2022-01-01")
stop_date = as.Date("2023-01-01")
date_interval = seq(start_date, length.out = 38, by="10 days")

cpod_dataset = data.frame()

for (i in 1:(length(date_interval)-1)){
  cpodData = getCpodData(startdate= as.character(date_interval[i]), stopdate= as.character(date_interval[i+1]), processing='Raw', quality=c("Hi","Mod"), by="1 min", usr = "userid", pwd = "pwd")
  cpodData = cpodData %>% filter(SPECIES == "NBHF")
  cpod_dataset = dplyr::bind_rows(cpod_dataset,cpodData)
  save(cpod_dataset, file = "datasets/cpod_dataset_2022.RData")
  print(i)
}

#check duplicates
any(duplicated(cpod_dataset))
cpod_dataset <- cpod_dataset %>% distinct() %>% filter(as.Date(TIME) < stop_date)

#CHECK DOWNLOADED DATA FROM LIFEWATCH RSTUDIO

load("~/cpod_paper/datasets/cpod_dataset_2022.RData")
#adjustments of data explorer / rstudio download of the data
#settings:
#Quality High+moderate
#raw data
#per minute export

DP <- cpod_dataset

#Subset on project LifeWatch (delete other projects)
DP<-subset(DP, DP$PROJECTNAME=="Lifewatch")
DP <- DP %>% filter(SPECIES == "NBHF") 
DP$SPECIES <- recode_factor(DP$SPECIES,"NBHF"  = "Harbour porpoise")

#Rename Quality "3" to Hi+mod
# in data explorer quality Hi = 3, Mod=2 and Low = 1, if you choose combination, than the number if from the highest quality chosen
DP <- DP %>% mutate(QUALITY = replace(QUALITY, QUALITY==3, "Hi+mod"))

#add standard names for stations = Zones (since we don't move stations anymore, zones = stations )
DP$ZONE<-as.factor(DP$STATION)

colnames(DP)[2] = "Datetime (UTC)"

#rename columns
colnames(DP) = c("Deployment_fk", "Datetime (UTC)", "Species", "Milliseconds", "Number_clicks_filtered", "Number_clicks_total", "Lost_minutes", "Recorded", "Dpm", "Angle", "Temperature_min", "Temperature_max",
                     "Quality", "Projectname", "Station", "Latitude", "Longitude", "Mooring_type", "Receiver", "Deploy_date_time", "Recover_date_time", "Valid_data_until_datetime", "Zone")

#reorder columns
DP = DP %>% select(Projectname, Station, Zone, Latitude, Longitude, 
                            Mooring_type,Species, Receiver, Quality, "Datetime (UTC)", Milliseconds, 
                            Number_clicks_filtered, Number_clicks_total, Lost_minutes,
                            Recorded, Dpm, Angle, Temperature_min, Deployment_fk, Deploy_date_time, Recover_date_time, Valid_data_until_datetime)

colnames(DP)[18] = "Temperature"

DP$`Datetime (UTC)` = as.POSIXct(DP$`Datetime (UTC)`, tz="UTC", format = "%Y-%m-%d %H:%M:%S")

#--------------------ADD FPOD DATA TO 2022 DATASET

#add Belwind FPOD txt file to 2022 dataset
stop_date = as.Date("2023-01-01")
fpod = read.delim("export_bpns-Belwindreefballs 2022 10 17 FPOD_7103  NB HiandMod.txt", header = TRUE, sep = "\t", dec = ".")

#recode column names
fpod = subset(fpod, select = -c(NBHFclxPerMin, Nall.m))
fpod$Temperature_min = fpod$Temp
fpod$Temperature_max = fpod$Temp
colnames(fpod)[16] = "Deployment_fk"

#format datetime
fpod$Datetime = as.POSIXct(fpod$Datetime, tz="UTC", format=  "%d/%m/%Y %H:%M")
#add recover and valid data until datetime
fpod$Recover_date_time = "2023-02-23 09:10:00"
fpod$Valid_data_until_datetime = "2023-02-23 08:00:00"
fpod$Deploy_date_time = "2022-10-17 11:03:00"
fpod$Projectname = "Lifewatch"
fpod$Receiver = "POD-7103"

#remove Temp
fpod = fpod[,-17]

fpod$Station = fpod$Zone
colnames(fpod)[8] = "Datetime (UTC)"
colnames(fpod)[9] = "Milliseconds"
colnames(fpod)[10] = "Number_clicks_filtered"
colnames(fpod)[11] = "Number_clicks_total"
colnames(fpod)[14] = "Dpm"

fpod = fpod %>% select(Projectname, Station, Zone, Latitude, Longitude, 
                           Mooring_type,Species, Receiver, Quality, "Datetime (UTC)", Milliseconds, 
                           Number_clicks_filtered, Number_clicks_total, Lost_minutes,
                           Recorded, Dpm, Angle, Temperature_min, Temperature_max, Deployment_fk, Deploy_date_time, Recover_date_time, Valid_data_until_datetime)

fpod = fpod %>% distinct() %>% filter(`Datetime (UTC)` < stop_date)

DP = dplyr::bind_rows(DP,fpod)


#####################

#temperature  = 0 --> CPOD WAS NOT RECORDING
#delete rows with recorded = 0
#check temperature again

write_csv(DP, "Cetacean passive acoustic network 2022.csv")

#####################

#check for yearly exports
d0=read.csv("Cetacean passive acoustic network 2021.csv", header=TRUE, sep = ",")
#Quality ok?
str(d0)
levels(d0$Quality)#no
#make dataset with quality 2
d0Mod<-subset(d0, d0$Quality=="2")
d0Mod$Deployment_fk<-as.factor(d0Mod$Deployment_fk)
levels(d0Mod$Deployment_fk)
#exclude these and paste the updated deployments of that year
library(dplyr)
D01<-d0 %>%
  filter(!((d0$Deployment_fk == '9682') |
             (d0$Deployment_fk == '9940') |
             (d0$Deployment_fk == '12740') ))
#drop the levels you filtered out
D01<-droplevels(D01)
#check
levels(D01$Quality)
#subset the newly uploaded deployment_fk where the quality Hi was lacking
DP<-read.table(  "LW_CPOD_data_2021-01-01_2021-12-31_downloaded_on_2023-10-23-08-53.tab",  sep="\t", header=TRUE)
str(DP)
#need to subset for species nbhf and lw project, and the deployments which you want to add to the general 2021 dataset
DPnbhf<- subset(DP, DP$Species=="NBHF")
DPnbhf<-subset(DPnbhf, DPnbhf$Projectname =="Lifewatch")
str(DPnbhf)
DP1<-DPnbhf %>%
  filter (((DPnbhf$Deployment_fk == '9682') |
             (DPnbhf$Deployment_fk == '9940') |
             (DPnbhf$Deployment_fk == '12740') ))
DP1$Zone<-DP1$Station
DP1<-droplevels(DP1)
str(DP1)
#rename to harbour porpoise
levels(DP1$Species)[levels(DP1$Species)=="NBHF"] <- "Harbour porpoise"
#Rename Time to Datetime (UTC)
names(DP1)[names(DP1) == "Time"] <- "Datetime..UTC."
str(DP1)
#reorder columns to match dataset 2021
DP1 = DP1%>% select(Projectname, Station, Zone, Latitude, Longitude, 
                    Mooring_type,Species, Receiver, Quality, Datetime..UTC., Milliseconds, 
                    Number_clicks_filtered, Number_clicks_total, Lost_minutes,
                    Recorded, Dpm, Angle, Temperature_min, Temperature_max, Deployment_fk, Deploy_date_time, Recover_date_time, Valid_data_until_datetime  )
DP1$Quality<-as.factor(DP1$Quality)
#rename quality to Hi+mod
levels(DP1$Quality)[levels(DP1$Quality)=="3"] <- "Hi+mod"
#there is 1 hour of data with quality 2 of reefballs Belwind, so we will delete this hour
DP2<-DP1 %>%
  filter(!((DP1$Quality =="2")))
##merge the two dataframe
DF<-rbind(D01,DP2)

#
detach("package:dplyr", unload = TRUE)
#need to set columns with data time to the correct format
library(lubridate)
str(DF)
#DF$Datetime..UTC.<-as.POSIXct(as.character(DF$Datetime..UTC.), tz="UTC", tryFormats=c("%Y-%m-%d %H:%M:%S"))
DF$Datetime..UTC. <- ymd_hms(DF$Datetime..UTC.)
DF$Recover_date_time <- ymd_hms(DF$Recover_date_time)
DF$Valid_data_until_datetime <- ymd_hms(DF$Valid_data_until_datetime)
DF$Deploy_date_time <- ymd_hms(DF$Deploy_date_time)


str(DF)
#station AP-bpns-Cpower remove from dataset, this is not open data
library(dplyr)
DF<-DF %>%
  filter(!((DF$Station =="AP-bpns-Cpower")))
DF<-DF %>%
  filter(!((DF$Station =="AP-bpns-Belwind")))
#rename some stations to general name
#rename AP-bpns-Birkenfels and AP-bpns-Grafton to standardized station names
levels(DF$Station)[levels(DF$Station)=="AP-bpns-Birkenfels"] <- "bpns-Birkenfels"
levels(DF$Station)[levels(DF$Station)=="AP-bpns-Grafton"] <- "bpns-Grafton"
#check quality ==2?
nrow(DF[DF$Quality =="2",])
#remove 1 temperature column and rename the other one
cols.dont.want <- "Temperature_min"
DF<- DF[, ! names(DF) %in% cols.dont.want, drop = F]
str(DF)
DF<-DF %>% 
  rename(
    Temperature = Temperature_max,
  )
#zone should be same as station for unique stations
DF$Zone<-DF$Station
write.csv(DF, "Cetacean passive acoustic network 2021bis.csv",row.names=FALSE)
str(DF)
summary(DF)

