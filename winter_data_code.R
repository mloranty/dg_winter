library(dplyr)
library(ggplot2)
library(lubridate)


#### Data Read In ####

# Read in Air Temperature data
dgAirTemp <- read.csv("C:\\Users\\brian\\OneDrive\\Documents\\GEOG331\\Project Data\\data\\dg_air_temperature.csv",
                      stringsAsFactors = T)
# Adds dataF and Month Columns to assist in Sorting by Season
dgAirTemp$dateF <- as.Date(dgAirTemp$timestamp, "%Y-%m-%d")
dgAirTemp$month <- as.numeric(format(dgAirTemp$dateF,"%m"))


# Read in Relative Humidity data
dgRelativeHumid <- read.csv("C:\\Users\\brian\\OneDrive\\Documents\\GEOG331\\Project Data\\data\\dg_relative_humidity.csv",
                      stringsAsFactors = T)
# Adds dataF and Month Columns to assist in Sorting by Season
dgRelativeHumid$dateF <- as.Date(dgRelativeHumid$timestamp, "%Y-%m-%d")
dgRelativeHumid$month <- as.numeric(format(dgRelativeHumid$dateF,"%m"))


# Read in PAR data
dgPAR <- read.csv("C:\\Users\\brian\\OneDrive\\Documents\\GEOG331\\Project Data\\data\\dg_par.csv",
                            stringsAsFactors = T)
# Adds dataF and Month Columns to assist in Sorting by Season
dgPAR$dateF <- as.Date(dgPAR$timestamp, "%Y-%m-%d")
dgPAR$month <- as.numeric(format(dgPAR$dateF,"%m"))


# Read in Soil Temperature data
dgSoilTemp <- read.csv("C:\\Users\\brian\\OneDrive\\Documents\\GEOG331\\Project Data\\data\\dg_soil_temperature.csv",
                  stringsAsFactors = T)
# Adds dataF and Month Columns to assist in Sorting by Season
dgSoilTemp$dateF <- as.Date(dgSoilTemp$timestamp, "%Y-%m-%d")
dgSoilTemp$month <- as.numeric(format(dgSoilTemp$dateF,"%m"))

# filter sensorZ data at sites for appropriate sensor depth
filter(dgSoilTemp, site == "DAVY" & sensorZ == -10 |
         site == "HDF1" & sensorZ == -9 |
         site == "LBR" & sensorZ == -18 |
         site == "LDF2" & sensorZ == -8 |
         site == "MDF2" & sensorZ == -9 |
         site == "MDF1" & sensorZ == -6)


# Read in Soil Moisture data
dgSoilMoist <- read.csv("C:\\Users\\brian\\OneDrive\\Documents\\GEOG331\\Project Data\\data\\dg_soil_moisture.csv",
                  stringsAsFactors = T)
# Adds dataF and Month Columns to assist in Sorting by Season
dgSoilMoist$dateF <- as.Date(dgSoilMoist$timestamp, "%Y-%m-%d")
dgSoilMoist$month <- as.numeric(format(dgSoilMoist$dateF,"%m"))


# Adds Canopy Cover variable for each site to dgAirTemp
dgAirTemp <- mutate(dgAirTemp, Cover = case_when(
  site == "DAVY" ~ 71,
  site == "HDF1" ~ 54,
  site == "LBR" ~ 29,
  site == "LDF2" ~ 7,
  site == "MDF2" ~ 33,
  site == "MDF1" ~ 28
))

# Adds Canopy Cover variable for each site to dgRelativeHumid
dgRelativeHumid<- mutate(dgRelativeHumid, Cover = case_when(
  site == "DAVY" ~ 71,
  site == "HDF1" ~ 54,
  site == "LBR" ~ 29,
  site == "LDF2" ~ 7,
  site == "MDF2" ~ 33,
  site == "MDF1" ~ 28
))

# Adds Canopy Cover variable for each site to dgPAR
dgPAR <- mutate(dgPAR, Cover = case_when(
  site == "DAVY" ~ 71,
  site == "HDF1" ~ 54,
  site == "LBR" ~ 29,
  site == "LDF2" ~ 7,
  site == "MDF2" ~ 33,
  site == "MDF1" ~ 28
))

# Adds Canopy Cover variable for each site to dgSoilTemp
dgSoilTemp <- mutate(dgSoilTemp, Cover = case_when(
  site == "DAVY" ~ 71,
  site == "HDF1" ~ 54,
  site == "LBR" ~ 29,
  site == "LDF2" ~ 7,
  site == "MDF2" ~ 33,
  site == "MDF1" ~ 28
))

# Adds Canopy Cover variable for each site to dgSoilMoist
dgSoilMoist <- mutate(dgSoilMoist, Cover = case_when(
  site == "DAVY" ~ 71,
  site == "HDF1" ~ 54,
  site == "LBR" ~ 29,
  site == "LDF2" ~ 7,
  site == "MDF2" ~ 33,
  site == "MDF1" ~ 28
))



#### End Data Read in ####


#### Create Winter Data Tables ####

# Stores all Air Temperature data from Oct 1 to April 30
winterAirTemp <- rbind(subset(dgAirTemp, dgAirTemp$month > "9"), subset(dgAirTemp, dgAirTemp$month < "5"))
# Sorts winterAirTemp based on year and doy
winterAirTemp <- winterAirTemp %>% arrange(year, doy)

# Stores all Relative Humidity data from Oct 1 to April 30
winterRelativeHumid <- rbind(subset(dgRelativeHumid, dgRelativeHumid$month > "9"), 
                             subset(dgRelativeHumid, dgRelativeHumid$month < "5"))
# Sorts winterRelativeHumid based on year and doy
winterRelativeHumid <- winterRelativeHumid %>% arrange(year, doy)

# Stores all PAR data from Oct 1 to April 30
winterPAR <- rbind(subset(dgPAR, dgPAR$month > "9"), subset(dgPAR, dgPAR$month < "5"))
# Sorts winterPAR based on year and doy
winterPAR <- winterPAR %>% arrange(year, doy)

# Stores all Soil Temperature data from Oct 1 to April 30
winterSoilTemp <- rbind(subset(dgSoilTemp, dgSoilTemp$month > "9"), subset(dgSoilTemp, dgSoilTemp$month < "5"))
# Sorts winterSoilTemp based on year and doy
winterSoilTemp <- winterSoilTemp %>% arrange(year, doy)

# Stores all Soil Moisture data from Oct 1 to April 30
winterSoilMoist <- rbind(subset(dgSoilMoist, dgSoilMoist$month > "9"), subset(dgSoilMoist, dgSoilMoist$month < "5"))
# Sorts winterSoilMoist based on year and doy
winterSoilMoist <- winterSoilMoist %>% arrange(year, doy)

#### End Create Winter Data Tables ####

# Create dataframe containing average WindTemp for every day in winter, separated by site
winterDailyAirTempAvg <- aggregate(t_air ~ site + dateF, data = winterAirTemp, FUN = mean, na.rm = TRUE)


# Create dataframe containing average SoilTemp for every day in winter, separated by site
winterDailySoilTempAvg <- aggregate(t_soil ~ site + dateF, 
                                    data = filter(dgSoilTemp, site == "DAVY" & sensorZ == -10 |
                                                              site == "HDF1" & sensorZ == -9 |
                                                              site == "LBR" & sensorZ == -18 |
                                                              site == "LDF2" & sensorZ == -8 |
                                                              site == "MDF2" & sensorZ == -9 |
                                                              site == "MDF1" & sensorZ == -6),
                                    FUN = mean, na.rm = TRUE)

# combine winter air and soil temperatures based on site, doy, and year
winterDailyTemperatureAvg <- merge(winterDailyAirTempAvg, winterDailySoilTempAvg, by = c("site", "dateF"))
winterDailyTemperatureAvg$year <- format(winterDailyTemperatureAvg$dateF, format = "%Y")
winterDailyTemperatureAvg$doy <- yday(winterDailyTemperatureAvg$dateF)

# Create leap year to later help calculate water year
leap.year<-data.frame(year=seq(2013,2020), leapID=rep(c(0,0,0,1), times=2))
colnames(leap.year)<-c("year", "leapID")

winterDailyTemperatureAvg$year <- as.integer(winterDailyTemperatureAvg$year)

# add leap year to winterDailyTemperatureAvg
winterDailyTemperatureAvg <- left_join(winterDailyTemperatureAvg,leap.year,by=c("year"))

# Add water year to help categorize data by water year
winterDailyTemperatureAvg$wyear<-
  ifelse(winterDailyTemperatureAvg$leapID == 1 & winterDailyTemperatureAvg$doy <  275,
         winterDailyTemperatureAvg$year,
         ifelse(winterDailyTemperatureAvg$leapID == 0 & winterDailyTemperatureAvg$doy < 274,
                winterDailyTemperatureAvg$year,winterDailyTemperatureAvg$year+1))



winterDailyTemperatureAvg$wdoy<-ifelse(winterDailyTemperatureAvg$leapID==1&winterDailyTemperatureAvg$doy<=274, winterDailyTemperatureAvg$doy+92,
                  ifelse(winterDailyTemperatureAvg$leapID==1&winterDailyTemperatureAvg$doy>274, winterDailyTemperatureAvg$doy-274,
                         ifelse(winterDailyTemperatureAvg$leapID==0&winterDailyTemperatureAvg$doy<=273,winterDailyTemperatureAvg$doy+92,
                                ifelse(winterDailyTemperatureAvg$leapID==0&winterDailyTemperatureAvg$doy>273,winterDailyTemperatureAvg$doy-273,NA))))

#the .01 is added because day 273 needs to be included in the water year, but if it 
#is exactly one that bumps it to the first day of the water year.
winterDailyTemperatureAvg$wdoyP<-ifelse(leap_year(winterDailyTemperatureAvg$wyear)==TRUE, 
                                        (winterDailyTemperatureAvg$wdoy-1)/366,(winterDailyTemperatureAvg$wdoy-1)/365 )

#now add to the year		

winterDailyTemperatureAvg$decdate<-winterDailyTemperatureAvg$wyear+winterDailyTemperatureAvg$wdoyP





# Create and sort subset of winterDailyTemperatureAvg for DAVY site
DAVYWinterAvg <- winterDailyTemperatureAvg[winterDailyTemperatureAvg$site == "DAVY",]
DAVYWinterAvg <- DAVYWinterAvg %>% arrange(dateF)


colors <- c("Air Temp (2015)" = "firebrick1", "Soil Temp (2015)" = "steelblue1",
            #"Air Temp (2016)" = "firebrick3", "Soil Temp (2016)" = "steelblue3")
            "Air Temp (2017)" = "firebrick4", "Soil Temp (2017)" = "steelblue4")

# create plot of Winter Air and Soil temp at DAVY site over all measured years
ggplot(data = DAVYWinterAvg[DAVYWinterAvg$wyear == "2015",], aes(x=wdoy)) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2015",]$t_air, color = "Air Temp (2015)")) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2015",]$t_soil, color = "Soil Temp (2015)"), linetype = "solid") +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2017",]$t_air, color = "Air Temp (2017)")) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2017",]$t_soil, color = "Soil Temp (2017)")) +
  labs(x = "Water Year by DOY (DOY 0 = October 1st)",
  y = "Temperature (C°)",
  color = "Legend") +
  scale_color_manual(values = colors) +
  ggtitle("Average Temperature at DAVY Site")
  








# add legend to clarify which line means what

# Create new variable for day of the season, send an email to lorenty



