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

#need to set columns with data time to the correct format
library(lubridate)
str(DF)
#DF$Datetime..UTC.<-as.POSIXct(as.character(DF$Datetime..UTC.), tz="UTC", tryFormats=c("%Y-%m-%d %H:%M:%S"))
DF$Datetime..UTC. <- ymd_hms(DF$Datetime..UTC.)
DF$Recover_date_time <- ymd_hms(DF$Recover_date_time)
DF$Valid_data_until_datetime <- ymd_hms(DF$Valid_data_until_datetime)
DF$Deploy_date_time <- ymd_hms(DF$Deploy_date_time)

str(DF)

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

