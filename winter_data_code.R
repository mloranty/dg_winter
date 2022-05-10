library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)



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


# Create dataframe containing average WindTemp for every day in winter, separated by site
winterDailyAirTempAvg <- aggregate(t_air ~ site + dateF, data = winterAirTemp, FUN = mean, na.rm = TRUE)


# Create dataframe containing average SoilTemp for every day in winter, separated by site
# Also filters sensorZ data at sites for appropriate sensor depth
winterDailySoilTempAvg <- aggregate(t_soil ~ site + dateF, 
                                    data = filter(dgSoilTemp, site == "DAVY" & sensorZ == -10 |
                                                              site == "HDF1" & sensorZ == -9 |
                                                              site == "LBR" & sensorZ == -18 |
                                                              site == "LDF2" & sensorZ == -8 |
                                                              site == "MDF2" & sensorZ == -9 |
                                                              site == "MDF1" & sensorZ == -6),
                                    FUN = mean, na.rm = TRUE)

winterDailyPARAvg <- aggregate(par ~ site + dateF, data = winterPAR, FUN = mean, na.rm = TRUE)

#### Create WinterDailyTemperatureAvg Data Table ####

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

# Calculate Freezing Degree Days for Air Temp for each water year by site
winterDailyTemperatureAvg <- merge(winterDailyTemperatureAvg, aggregate(t_air ~ wyear + site, data = winterDailyTemperatureAvg, FUN = sum, subset = t_air <= 0.0, na.rm = TRUE),
                                  by = c("wyear", "site"))

# Calculate Freezing Degree Days for Soil Temp for each water year by site
winterDailyTemperatureAvg <- merge(winterDailyTemperatureAvg, aggregate(t_soil ~ wyear + site, data = winterDailyTemperatureAvg, FUN = sum, subset = t_soil <= 0.0, na.rm = TRUE),
                                  by = c("wyear", "site"))

names(winterDailyTemperatureAvg)[12] <- 'AirFreezeDegreeDaysAvg'
names(winterDailyTemperatureAvg)[13] <- 'SoilFreezeDegreeDaysAvg'

winterDailyTemperatureAvg$FreezingNFactor <- winterDailyTemperatureAvg$SoilFreezeDegreeDaysAvg/winterDailyTemperatureAvg$AirFreezeDegreeDaysAvg


#### Create winterPARAvg Data table ####
winterDailyPARAvg$year <- format(winterDailyPARAvg$dateF, format = "%Y")
winterDailyPARAvg$doy <- yday(winterDailyPARAvg$dateF)

winterDailyPARAvg$year <- as.integer(winterDailyPARAvg$year)

# add leap year to winterDailyTemperatureAvg
winterDailyPARAvg <- left_join(winterDailyPARAvg,leap.year,by=c("year"))

# Add water year to help categorize data by water year
winterDailyPARAvg$wyear<-
  ifelse(winterDailyPARAvg$leapID == 1 & winterDailyPARAvg$doy <  275,
         winterDailyPARAvg$year,
         ifelse(winterDailyPARAvg$leapID == 0 & winterDailyPARAvg$doy < 274,
                winterDailyPARAvg$year,winterDailyPARAvg$year+1))


winterDailyPARAvg$wdoy<-ifelse(winterDailyPARAvg$leapID==1&winterDailyPARAvg$doy<=274, winterDailyPARAvg$doy+92,
                                       ifelse(winterDailyPARAvg$leapID==1&winterDailyPARAvg$doy>274, winterDailyPARAvg$doy-274,
                                              ifelse(winterDailyPARAvg$leapID==0&winterDailyPARAvg$doy<=273,winterDailyPARAvg$doy+92,
                                                     ifelse(winterDailyPARAvg$leapID==0&winterDailyPARAvg$doy>273,winterDailyPARAvg$doy-273,NA))))

#the .01 is added because day 273 needs to be included in the water year, but if it 
#is exactly one that bumps it to the first day of the water year.
winterDailyPARAvg$wdoyP<-ifelse(leap_year(winterDailyPARAvg$wyear)==TRUE, 
                                        (winterDailyPARAvg$wdoy-1)/366,(winterDailyPARAvg$wdoy-1)/365 )

#now add to the year		
winterDailyPARAvg$decdate<-winterDailyPARAvg$wyear+winterDailyPARAvg$wdoyP

# Calculate Avg PAR for each water year by site
winterDailyPARAvg <- merge(winterDailyPARAvg, aggregate(par ~ wyear + site, data = winterDailyPARAvg, FUN = mean, na.rm = TRUE),
                                   by = c("wyear", "site"))

names(winterDailyPARAvg)[11] <- 'PARWaterYearAvg'

winterDailyPARAvg <- subset(winterDailyPARAvg, wyear <= "2019")

#### Create N Factor Graph ####

# Freezing Degree Days N_Factor Comparison
# Closer to 1, the colder the soil. Closer to 0, the warmer the soil
nFactors = unique(winterDailyTemperatureAvg[, c("wyear", "site", "AirFreezeDegreeDaysAvg", "SoilFreezeDegreeDaysAvg", "FreezingNFactor")])
nFactors[nrow(nFactors) + 1,] <- c("2015", "LBR", NA, NA, NA)
nFactors$AirFreezeDegreeDaysAvg = as.numeric(nFactors$AirFreezeDegreeDaysAvg)
nFactors$SoilFreezeDegreeDaysAvg = as.numeric(nFactors$SoilFreezeDegreeDaysAvg)
nFactors$FreezingNFactor = as.numeric(nFactors$FreezingNFactor)

siteColors = c("DAVY" = "darkgreen", "HDF1" = "chartreuse3",
               "MDF1" = "indianred1", "MDF2" = "indianred3",
               "LBR" = "blue", "LDF2" = "cyan4")


#need to fix LBR placement
# Shades of colors to mark vegetation level (Green for high density, orange/red for medium, blue for low)

ggplot(data = nFactors[nFactors$site == "DAVY",], aes(x=wyear, y=FreezingNFactor, group = 1, color = siteColors)) + 
  geom_line(aes(color = "DAVY")) + 
  geom_point(aes(color = "DAVY")) +
  geom_line(aes(y = nFactors[nFactors$site == "HDF1",]$FreezingNFactor, color = "HDF1")) + 
  geom_point(aes(y = nFactors[nFactors$site == "HDF1",]$FreezingNFactor, color = "HDF1")) +
  geom_line(aes(y = nFactors[nFactors$site == "LBR",]$FreezingNFactor, color = "LBR"), na.rm = TRUE) +
  geom_point(aes(y = nFactors[nFactors$site == "LBR",]$FreezingNFactor, color = "LBR"), na.rm = TRUE) +
  geom_line(aes(y = nFactors[nFactors$site == "LDF2",]$FreezingNFactor, color = "LDF2")) +
  geom_point(aes(y = nFactors[nFactors$site == "LDF2",]$FreezingNFactor, color = "LDF2")) +
  geom_line(aes(y = nFactors[nFactors$site == "MDF1",]$FreezingNFactor, color = "MDF1")) +
  geom_point(aes(y = nFactors[nFactors$site == "MDF1",]$FreezingNFactor, color = "MDF1")) +
  geom_line(aes(y = nFactors[nFactors$site == "MDF2",]$FreezingNFactor, color = "MDF2")) +
  geom_point(aes(y = nFactors[nFactors$site == "MDF2",]$FreezingNFactor, color = "MDF2")) +
  labs(x = "Water Year (October 1st to April 30th)",
       y = "N Factor (Out of 1)",
       color = "Legend") +
  scale_color_manual(values = siteColors) +
  ggtitle("Freezing Degree Days N Factor by Water Year from 2015 to 2019") +
  ylim(0.0, 0.4)


#### Create Avg PAR by water year graph ####

# Create graph of water year avg PAR at all sites by water year

PARToGraph = unique(winterDailyPARAvg[, c("wyear", "site", "PARWaterYearAvg")])
PARToGraph[nrow(PARToGraph) + 1,] <- c("2015", "DAVY", NA)
PARToGraph$PARWaterYearAvg = as.numeric(PARToGraph$PARWaterYearAvg)

ggplot(data = PARToGraph[PARToGraph$site == "HDF1",], aes(x=wyear, y=PARWaterYearAvg, group = 1, color = siteColors)) + 
  geom_line(aes(color = "HDF1")) + 
  geom_point(aes(color = "HDF1")) +
  geom_line(aes(y = PARToGraph[PARToGraph$site == "DAVY",]$PARWaterYearAvg, color = "DAVY"), na.rm = TRUE) + 
  geom_point(aes(y = PARToGraph[PARToGraph$site == "DAVY",]$PARWaterYearAvg, color = "DAVY"), na.rm = TRUE) +
  geom_line(aes(y = PARToGraph[PARToGraph$site == "LBR",]$PARWaterYearAvg, color = "LBR")) +
  geom_point(aes(y = PARToGraph[PARToGraph$site == "LBR",]$PARWaterYearAvg, color = "LBR")) +
  geom_line(aes(y = PARToGraph[PARToGraph$site == "LDF2",]$PARWaterYearAvg, color = "LDF2")) +
  geom_point(aes(y = PARToGraph[PARToGraph$site == "LDF2",]$PARWaterYearAvg, color = "LDF2")) +
  geom_line(aes(y = PARToGraph[PARToGraph$site == "MDF1",]$PARWaterYearAvg, color = "MDF1")) +
  geom_point(aes(y = PARToGraph[PARToGraph$site == "MDF1",]$PARWaterYearAvg, color = "MDF1")) +
  geom_line(aes(y = PARToGraph[PARToGraph$site == "MDF2",]$PARWaterYearAvg, color = "MDF2")) +
  geom_point(aes(y = PARToGraph[PARToGraph$site == "MDF2",]$PARWaterYearAvg, color = "MDF2")) +
  labs(x = "Water Year (October 1st to April 30th)",
       y = "N Factor (Out of 1)",
       color = "Legend") +
  scale_color_manual(values = siteColors) +
  ggtitle("Avg PAR by Water Year from 2015 to 2019") +
  ylim(0.0, 80.0)







#### Create HDF1 Graphs ####
# Create and sort subset of winterDailyTemperatureAvg for HDF1 site
HDF1WinterAvg <- winterDailyTemperatureAvg[winterDailyTemperatureAvg$site == "HDF1",]
HDF1WinterAvg <- HDF1WinterAvg %>% arrange(dateF)


TempColors <- c("Air Temp" = "firebrick1", "Soil Temp" = "steelblue1")
            

# create plot of Winter Air and Soil temp at HDF1 site over 2015 Water Year
HDF12015 <- ggplot(data = HDF1WinterAvg[HDF1WinterAvg$wyear == "2015",], aes(x=wdoy)) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2015",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2015",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
  y = "Temperature (C°)",
  color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at HDF1 Site During 2015 Water Year")
  

# create plot of Winter Air and Soil temp at HDF1 site over 2016 Water Year
HDF12016 <- ggplot(data = HDF1WinterAvg[HDF1WinterAvg$wyear == "2016",], aes(x=wdoy)) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2016",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2016",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at HDF1 Site During 2016 Water Year")


# create plot of Winter Air and Soil temp at HDF1 site over 2017 Water Year
HDF12017 <- ggplot(data = HDF1WinterAvg[HDF1WinterAvg$wyear == "2017",], aes(x=wdoy)) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2017",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2017",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at HDF1 Site During 2017 Water Year")


# create plot of Winter Air and Soil temp at HDF1 site over 2018 Water Year
HDF12018 <- ggplot(data = HDF1WinterAvg[HDF1WinterAvg$wyear == "2018",], aes(x=wdoy)) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2018",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2018",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at HDF1 Site During 2018 Water Year")


# create plot of Winter Air and Soil temp at HDF1 site over 2019 Water Year
HDF12019 <- ggplot(data = HDF1WinterAvg[HDF1WinterAvg$wyear == "2019",], aes(x=wdoy)) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2019",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = HDF1WinterAvg[HDF1WinterAvg$wyear == "2019",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at HDF1 Site During 2019 Water Year")


# Create and sort subset of winterDailyPARAvg for HDF1 site
HDF1WinterPARAvg <- winterDailyPARAvg[winterDailyPARAvg$site == "HDF1",]
HDF1WinterPARAvg <- HDF1WinterPARAvg %>% arrange(dateF)
names(HDF1WinterPARAvg)[4] <- 'PARDailyAvg'

# removes final day of 2016 so it can fit on graph
HDF1WinterPARAvg <- filter(HDF1WinterPARAvg, wdoy != 213)

PARColors = c("2015" = "blue", "2016" = "red",
               "2017" = "green", "2018" = "purple",
               "2019" = "goldenrod")


# create plot of all PAR Data for HDF1 site by year
HDF1PARGraph <- ggplot(data = HDF1WinterPARAvg[HDF1WinterPARAvg$wyear == "2015",], aes(x=wdoy, y = PARDailyAvg, group = 1, color = siteColors)) +
  geom_line(aes(y = HDF1WinterPARAvg[HDF1WinterPARAvg$wyear == "2015",]$PARDailyAvg, color = "2015")) + 
  geom_line(aes(y = HDF1WinterPARAvg[HDF1WinterPARAvg$wyear == "2016",]$PARDailyAvg, color = "2016")) +
  geom_line(aes(y = HDF1WinterPARAvg[HDF1WinterPARAvg$wyear == "2017",]$PARDailyAvg, color = "2017")) +
  geom_line(aes(y = HDF1WinterPARAvg[HDF1WinterPARAvg$wyear == "2018",]$PARDailyAvg, color = "2018")) +
  geom_line(aes(y = HDF1WinterPARAvg[HDF1WinterPARAvg$wyear == "2019",]$PARDailyAvg, color = "2019")) +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "PAR (µmol/(m²·s))",
       color = "Legend") +
  scale_color_manual(values = PARColors) +
  ggtitle("Average Daily PAR at HDF1 Site Over the Water Year")

grid.arrange(HDF12015, HDF12016, HDF12017, HDF12018, HDF12019, HDF1PARGraph)

#### Create LDF2 Graphs ####
LDF2WinterAvg <- winterDailyTemperatureAvg[winterDailyTemperatureAvg$site == "LDF2",]
LDF2WinterAvg <- LDF2WinterAvg %>% arrange(dateF)


# create plot of Winter Air and Soil temp at LDF2 site over 2015 Water Year
LDF22015 <- ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2015",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2015",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2015",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2015 Water Year")


# create plot of Winter Air and Soil temp at LDF2 site over 2016 Water Year
LDF22016 <- ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2016",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2016",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2016",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2016 Water Year")


# create plot of Winter Air and Soil temp at LDF2 site over 2017 Water Year
LDF22017 <- ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2017",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2017",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2017",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2017 Water Year")


# create plot of Winter Air and Soil temp at LDF2 site over 2018 Water Year
LDF22018 <- ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2018",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2018",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2018",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2018 Water Year")+
  xlim(0, 212)


# create plot of Winter Air and Soil temp at LDF2 site over 2019 Water Year
LDF22019 <- ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2019",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2019",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2019",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2019 Water Year")


# Create and sort subset of winterDailyPARAvg for LDF2 site
LDF2WinterPARAvg <- winterDailyPARAvg[winterDailyPARAvg$site == "LDF2",]
LDF2WinterPARAvg <- LDF2WinterPARAvg %>% arrange(dateF)
names(LDF2WinterPARAvg)[4] <- 'PARDailyAvg'


LDF2WinterPARAvg$wyear <- as.numeric(LDF2WinterPARAvg$wyear)
LDF2WinterPARAvg$wdoy <- as.numeric(LDF2WinterPARAvg$wdoy)
LDF2WinterPARAvg$PARDailyAvg <- as.numeric(LDF2WinterPARAvg$PARDailyAvg)

# removes final day of 2016 so it can fit on graph
LDF2WinterPARAvg <- filter(LDF2WinterPARAvg, wdoy != 213)

# Add NAs to dataframe so I can graph 2018 data
for (x in 158:217) {
  LDF2WinterPARAvg[nrow(LDF2WinterPARAvg) + 1,] <- c("2018", "LDF2", NA, NA, NA, NA, NA, x, NA, NA, NA)
}



# create plot of all PAR Data for LDF2 site by year
LDF2PARGraph <- ggplot(data = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2015",], aes(x=wdoy, y = PARDailyAvg, group = 1, color = PARColors)) +
  geom_line(aes(y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2015",]$PARDailyAvg, color = "2015")) + 
  geom_line(aes(y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2016",]$PARDailyAvg, color = "2016")) +
  geom_line(aes(y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2017",]$PARDailyAvg, color = "2017")) +
  geom_line(aes(y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2018",]$PARDailyAvg, color = "2018"), na.rm = TRUE) +
  geom_line(aes(y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2019",]$PARDailyAvg, color = "2019")) +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "PAR (µmol/(m²·s))",
       color = "Legend") +
  scale_color_manual(values = PARColors) +
  ggtitle("Average Daily PAR at LDF2 Site Over the Water Year")

grid.arrange(LDF22015, LDF22016, LDF22017, LDF22018, LDF22019, LDF2PARGraph)

