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


# Read in Soil Moisture data
dgSoilMoist <- read.csv("C:\\Users\\brian\\OneDrive\\Documents\\GEOG331\\Project Data\\data\\dg_soil_moisture.csv",
                  stringsAsFactors = T)
# Adds dataF and Month Columns to assist in Sorting by Season
dgSoilMoist$dateF <- as.Date(dgSoilMoist$timestamp, "%Y-%m-%d")
dgSoilMoist$month <- as.numeric(format(dgSoilMoist$dateF,"%m"))


#### End Data Read in ####

# Stores all Air Temperature data from Oct 1 to April 30
winterAirTemp <- rbind(subset(dgAirTemp, dgAirTemp$month > "9"), subset(dgAirTemp, dgAirTemp$month < "5"))

# Stores all Relative Humidity data from Oct 1 to April 30
winterRelativeHumid <- rbind(subset(dgRelativeHumid, dgRelativeHumid$month > "9"), 
                             subset(dgRelativeHumid, dgRelativeHumid$month < "5"))

# Stores all PAR data from Oct 1 to April 30
winterPAR <- rbind(subset(dgPAR, dgPAR$month > "9"), subset(dgPAR, dgPAR$month < "5"))

# Stores all Soil Temperature data from Oct 1 to April 30
winterSoilTemp <- rbind(subset(dgSoilTemp, dgSoilTemp$month > "9"), subset(dgSoilTemp, dgSoilTemp$month < "5"))

# Stores all Soil Moisture data from Oct 1 to April 30
winterSoilMoist <- rbind(subset(dgSoilMoist, dgSoilMoist$month > "9"), subset(dgSoilMoist, dgSoilMoist$month < "5"))
