library(dplyr)


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
WinterDailyAirTempAvg <- aggregate(t_air ~ site + doy + year, data = winterAirTemp, FUN = mean, na.rm = TRUE)


# Create dataframe containing average SoilTemp for every day in winter, separated by site
WinterDailySoilTempAvg <- aggregate(t_soil ~ site + doy + year, 
                                    data = filter(dgSoilTemp, site == "DAVY" & sensorZ == -10 |
                                                              site == "HDF1" & sensorZ == -9 |
                                                              site == "LBR" & sensorZ == -18 |
                                                              site == "LDF2" & sensorZ == -8 |
                                                              site == "MDF2" & sensorZ == -9 |
                                                              site == "MDF1" & sensorZ == -6),
                                    FUN = mean, na.rm = TRUE)






