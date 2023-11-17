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

#remove no recorded minutes
DP<-DP %>%
  filter(!((DP$Recorded == '0')  ))
DP<-droplevels(DP)

#Check for missing data 
colSums(is.na(DP))
#a lot of missing data for the lat and longitude
DP$Latitude[DP$Station == "bpns-Reefballs-cpower"]<-"51.580667"
DP$Longitude[DP$Station == "bpns-Reefballs-cpower"]<-"2.996"
colSums(is.na(DP))

#Check levels of quality, so you know if there is some data missing
levels(DP$Quality)

#temperature  = 0 --> CPOD WAS NOT RECORDING

#check temperature again

#####################

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

#################################

#Script to aggregate minute-resolution exports to hourly-resolution

#check for yearly exports
rm(list=ls())
d0=read.csv("Cetacean passive acoustic network 2022.csv", header=TRUE, sep = ",")
library( lubridate)
#per hour
#extra parameter DPH= detection positive hours
#dataset per hour from dataset per min
#Step 1: set datetime utc per hour
d0$min<-as.POSIXct(d0$Datetime..UTC. , format = "%Y-%m-%d  %H:%M" , tz="UTC")
d0$min60 <-as.POSIXlt(floor(as.numeric(d0$min)/(60*60))*(60*60),origin=(as.POSIXlt('1970-01-01 00:00')), tz="UTC")

d0$min60<-as.POSIXct(d0$min60 , format = "%Y-%m-%d  %H:%M" , tz="UTC")

#Step 2: dataset aggregation per hour, sum most of the variables
DFh=aggregate(cbind(Recorded=d0$Recorded, Lost_minutes= d0$Lost_minutes, Number_clicks_total=d0$Number_clicks_total, sumDPM=d0$Dpm, Milliseconds=d0$Milliseconds, Number_clicks_filtered=d0$Number_clicks_filtered)~ min60+Projectname+Station + Zone + Latitude+Longitude+Mooring_type+Species+Receiver+Quality+Deployment_fk+Deploy_date_time+Recover_date_time+Valid_data_until_datetime, data=d0, FUN=sum)#DPH = dph = detections positive hour
### dpm wordt naar dph vertaald (vanaf dat je 1 positive dpm hebt is het uur ook positief), hier wordt géén rekening gehouden of we een volledig uur gemeten hebben 
DFh$Dph<- ifelse(is.na(DFh$sumDPM)| DFh$sumDPM== 0, "0", "1")
DFh$Dph<-as.integer(DFh$Dph)

#Rename min60 to Datetime (UTC)
names(DFh)[names(DFh) == "min60"] <- "Datetime..UTC."
names(DFh)[names(DFh) == "sumDPM"] <- "sumDpm"
#reorder columns to match dataset 2021
library(dplyr)
DF = DFh%>% select(Projectname, Station, Zone, Latitude, Longitude, 
                   Mooring_type,Species, Receiver, Quality, Datetime..UTC., Milliseconds, 
                   Number_clicks_filtered, Number_clicks_total, Lost_minutes,
                   Recorded, sumDpm, Dph, Deployment_fk, Deploy_date_time, Recover_date_time, Valid_data_until_datetime  )
write.csv(DF, "Cetacean passive acoustic network 2022 hourly.csv",row.names=FALSE)


