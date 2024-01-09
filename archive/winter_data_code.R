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



#### Create PAR boxplot ####

winterDailyPARAvg$density <- ifelse(grepl("L",winterDailyPARAvg$site),paste("LOW"),
                                   ifelse(grepl("M",winterDailyPARAvg$site), paste("MED"),
                                   paste("HIGH")))

PAR.aov <- aov(par.x ~ density, data=winterDailyPARAvg)
summary(PAR.aov)
TukeyHSD(PAR.aov)

winterDailyPARAvg$density <- as.character(winterDailyPARAvg$density)
winterDailyPARAvg$density <- factor(winterDailyPARAvg$density,levels=c("HIGH","MED","LOW"))

# Create box plot for PAR, exclude par values below 10 due to shorter days in the winter
ggplot(data = subset(winterDailyPARAvg, par.x > 10), aes(x=density,y=par.x,fill=density)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  xlab("Stand Density") +
  ylab("Photosynthetically Active Radiation (µmol/(m²·s))") +
  theme(legend.position = "none")



#### Create Soil Temp boxplot ####

# remove data following 2019 water year
winterDailyTempAvgModify <- subset(winterDailyTemperatureAvg, wyear <= "2019")

winterDailyTempAvgModify$density <- ifelse(grepl("L",winterDailyTempAvgModify$site),paste("LOW"),
                                    ifelse(grepl("M",winterDailyTempAvgModify$site), paste("MED"),
                                           paste("HIGH")))

SoilTemp.aov <- aov(t_soil.x ~ density, data=winterDailyTempAvgModify)
summary(SoilTemp.aov)
TukeyHSD(SoilTemp.aov)

winterDailyTempAvgModify$density <- as.character(winterDailyTempAvgModify$density)
winterDailyTempAvgModify$density <- factor(winterDailyTempAvgModify$density,levels=c("HIGH","MED","LOW"))

# Create box plot for soil temperature
ggplot(data = winterDailyTempAvgModify, aes(x=density,y=t_soil.x,fill=density)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  xlab("Stand Density") +
  ylab("Temperature (C°)") +
  theme(legend.position = "none")



#### Table Data Work ####

DensitySoilTemp <- aggregate(t_soil.x ~ density, data = winterDailyTempAvgModify, FUN = mean, na.rm = TRUE)
DensityPAR <- aggregate(par.x ~ density, data = winterDailyPARAvg, FUN = mean, na.rm = TRUE)

NFactorsToAverage <- unique(winterDailyTempAvgModify[, c("FreezingNFactor", "density")])
DensityNFactor <- aggregate(FreezingNFactor ~ density, data = NFactorsToAverage, FUN = mean, na.rm = TRUE)

#### Create N Factor Graph ####

# Freezing Degree Days N_Factor Comparison
# Closer to 1, the colder the soil. Closer to 0, the warmer the soil
nFactors = unique(winterDailyTemperatureAvg[, c("wyear", "site", "AirFreezeDegreeDaysAvg", "SoilFreezeDegreeDaysAvg", "FreezingNFactor")])
nFactors[nrow(nFactors) + 1,] <- c("2015", "LBR", NA, NA, NA)
nFactors$AirFreezeDegreeDaysAvg = as.numeric(nFactors$AirFreezeDegreeDaysAvg)
nFactors$SoilFreezeDegreeDaysAvg = as.numeric(nFactors$SoilFreezeDegreeDaysAvg)
nFactors$FreezingNFactor = as.numeric(nFactors$FreezingNFactor)
row.names(nFactors) <- 1:nrow(nFactors) 


nFactors[c(18,21, 22),"FreezingNFactor"] <- NA

siteColors = c("DAVY" = "darkgreen", "HDF1" = "chartreuse3",
               "MDF1" = "indianred1", "MDF2" = "indianred3",
               "LBR" = "blue", "LDF2" = "cyan4")


plot(x = nFactors[nFactors$site == "HDF1",]$wyear, y = nFactors[nFactors$site == "HDF1",]$FreezingNFactor,type = "b",col = "indianred1",
     xlab = "Water Year (October 1st to April 30th)", ylab = "N Factor (Out of 1)", main = "Freezing Degree Days N Factor by Water Year from 2015 to 2019", ylim=c(0.0,0.45), lwd = 3)
grid(nx = NA, ny = NULL, lty = 2, lwd = 1, col = "black")

lines(x = nFactors[nFactors$site == "DAVY",]$wyear, y = nFactors[nFactors$site == "DAVY",]$FreezingNFactor, type = "b", col = "indianred4", lwd = 3)
lines(x = nFactors[nFactors$site == "MDF2",]$wyear, y = nFactors[nFactors$site == "MDF2",]$FreezingNFactor, type = "b", col = "darkgreen", lwd = 3)
lines(x = nFactors[nFactors$site == "LBR",]$wyear, y = nFactors[nFactors$site == "LBR",]$FreezingNFactor, type = "b", col = "blue", lwd = 3)
lines(x = nFactors[nFactors$site == "MDF1",]$wyear, y = nFactors[nFactors$site == "MDF1",]$FreezingNFactor, type = "b", col = "chartreuse3", lwd = 3)
lines(x = nFactors[nFactors$site == "LDF2",]$wyear, y = nFactors[nFactors$site == "LDF2",]$FreezingNFactor, type = "b", col = "cyan4", lwd = 3)

legend("topright", c("DAVY", "HDF1", "MDF1", "MDF2", "LBR", "LDF2"), 
       lty = c(1,1), lwd = 3, cex=0.6, 
       col = c("indianred4", "indianred1", "chartreuse3", "darkgreen", "blue", "cyan4"))


#### Create Avg PAR by water year graph ####

# Create graph of water year avg PAR at all sites by water year
# Find a way to drop DAVY, LDF2, and MDF1 for 2018

PARToGraph = unique(winterDailyPARAvg[, c("wyear", "site", "PARWaterYearAvg")])
PARToGraph[nrow(PARToGraph) + 1,] <- c("2015", "DAVY", NA)
PARToGraph$PARWaterYearAvg = as.numeric(PARToGraph$PARWaterYearAvg)
row.names(PARToGraph) <- 1:nrow(PARToGraph) 

PARToGraph[c(18,21,22),"PARWaterYearAvg"] <- NA

plot(x = PARToGraph[PARToGraph$site == "HDF1",]$wyear, y = PARToGraph[PARToGraph$site == "HDF1",]$PARWaterYearAvg,type = "b",col = "indianred1",
     xlab = "Water Year (October 1st to April 30th)", ylab = "PAR (µmol/(m²·s))", main = "Average PAR by Water Year from 2015 to 2019", ylim=c(0,80), lwd = 3) 

lines(x = PARToGraph[PARToGraph$site == "DAVY",]$wyear, y = PARToGraph[PARToGraph$site == "DAVY",]$PARWaterYearAvg, type = "b", col = "indianred4", lwd = 3)
lines(x = PARToGraph[PARToGraph$site == "MDF1",]$wyear, y = PARToGraph[PARToGraph$site == "MDF1",]$PARWaterYearAvg, type = "b", col = "chartreuse3", lwd = 3)
lines(x = PARToGraph[PARToGraph$site == "MDF2",]$wyear, y = PARToGraph[PARToGraph$site == "MDF2",]$PARWaterYearAvg, type = "b", col = "darkgreen", lwd = 3)
lines(x = PARToGraph[PARToGraph$site == "LBR",]$wyear, y = PARToGraph[PARToGraph$site == "LBR",]$PARWaterYearAvg, type = "b", col = "blue", lwd = 3)
lines(x = PARToGraph[PARToGraph$site == "LDF2",]$wyear, y = PARToGraph[PARToGraph$site == "LDF2",]$PARWaterYearAvg, type = "b", col = "cyan4", lwd = 3)

legend("topright", c("DAVY", "HDF1", "MDF1", "MDF2", "LBR", "LDF2"), 
       lty = c(1,1), lwd = 3, cex=0.8, 
       col = c("indianred4", "indianred1", "chartreuse3", "darkgreen", "blue", "cyan4"))


#### Create DAVY Graphs ####
# Create and sort subset of winterDailyTemperatureAvg for DAVY site
DAVYWinterAvg <- winterDailyTemperatureAvg[winterDailyTemperatureAvg$site == "DAVY",]
DAVYWinterAvg <- DAVYWinterAvg %>% arrange(dateF)

# Create colors to use for air and soil temp
TempColors <- c("Air Temp" = "firebrick1", "Soil Temp" = "steelblue1")
            

# create plot of Winter Air and Soil temp at DAVY site over 2015 Water Year
ggplot(data = DAVYWinterAvg[DAVYWinterAvg$wyear == "2015",], aes(x=wdoy)) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2015",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2015",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
  y = "Temperature (C°)",
  color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at DAVY Site During 2015 Water Year") + ylim(-42.0, 5.0)
  

# create plot of Winter Air and Soil temp at DAVY site over 2016 Water Year
ggplot(data = DAVYWinterAvg[DAVYWinterAvg$wyear == "2016",], aes(x=wdoy)) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2016",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2016",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at DAVY Site During 2016 Water Year") + ylim(-42.0, 5.0)


# create plot of Winter Air and Soil temp at DAVY site over 2017 Water Year
ggplot(data = DAVYWinterAvg[DAVYWinterAvg$wyear == "2017",], aes(x=wdoy)) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2017",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2017",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at DAVY Site During 2017 Water Year") + ylim(-42.0, 5.0)


# create plot of Winter Air and Soil temp at DAVY site over 2018 Water Year
ggplot(data = DAVYWinterAvg[DAVYWinterAvg$wyear == "2018",], aes(x=wdoy)) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2018",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2018",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at DAVY Site During 2018 Water Year") +
  xlim(0, 212) + ylim(-42.0, 5.0)


# create plot of Winter Air and Soil temp at DAVY site over 2019 Water Year
ggplot(data = DAVYWinterAvg[DAVYWinterAvg$wyear == "2019",], aes(x=wdoy)) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2019",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = DAVYWinterAvg[DAVYWinterAvg$wyear == "2019",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at DAVY Site During 2019 Water Year") + ylim(-42.0, 5.0)



# Create and sort subset of winterDailyPARAvg for DAVY site
DAVYWinterPARAvg <- winterDailyPARAvg[winterDailyPARAvg$site == "DAVY",]
DAVYWinterPARAvg <- DAVYWinterPARAvg %>% arrange(dateF)
names(DAVYWinterPARAvg)[4] <- 'PARDailyAvg'


DAVYWinterPARAvg$wyear <- as.numeric(DAVYWinterPARAvg$wyear)
DAVYWinterPARAvg$wdoy <- as.numeric(DAVYWinterPARAvg$wdoy)
DAVYWinterPARAvg$PARDailyAvg <- as.numeric(DAVYWinterPARAvg$PARDailyAvg)

# removes final day of 2016 so it can fit on graph
DAVYWinterPARAvg <- filter(DAVYWinterPARAvg, wdoy != 213)



plot(x = DAVYWinterPARAvg[DAVYWinterPARAvg$wyear == "2016",]$wdoy, y = DAVYWinterPARAvg[DAVYWinterPARAvg$wyear == "2016",]$PARDailyAvg,type = "b",col = "chartreuse3",
     xlab = "Water Year by DOY (October 1st to April 30th)", ylab = "PAR (µmol/(m²·s))", main = "Average Daily PAR at DAVY Site Over the Water Year", lwd = 2, ylim=c(0,475)) 

lines(x = DAVYWinterPARAvg[DAVYWinterPARAvg$wyear == "2017",]$wdoy, y = DAVYWinterPARAvg[DAVYWinterPARAvg$wyear == "2017",]$PARDailyAvg, type = "b", col = "indianred1", lwd = 2)
lines(x = DAVYWinterPARAvg[DAVYWinterPARAvg$wyear == "2018",]$wdoy, y = DAVYWinterPARAvg[DAVYWinterPARAvg$wyear == "2018",]$PARDailyAvg, type = "b", col = "indianred4", lwd = 2)
lines(x = DAVYWinterPARAvg[DAVYWinterPARAvg$wyear == "2019",]$wdoy, y = DAVYWinterPARAvg[DAVYWinterPARAvg$wyear == "2019",]$PARDailyAvg, type = "b", col = "blue", lwd = 2)


legend("topleft", c("2016", "2017", "2018", "2019"), 
       lty = c(1,1), lwd = 3, cex=0.8, 
       col = c("chartreuse3", "indianred1", "indianred4", "blue"))




#### Create LDF2 Graphs ####
LDF2WinterAvg <- winterDailyTemperatureAvg[winterDailyTemperatureAvg$site == "LDF2",]
LDF2WinterAvg <- LDF2WinterAvg %>% arrange(dateF)


# create plot of Winter Air and Soil temp at LDF2 site over 2015 Water Year
ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2015",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2015",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2015",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2015 Water Year") + ylim(-42.0, 5.0)


# create plot of Winter Air and Soil temp at LDF2 site over 2016 Water Year
ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2016",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2016",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2016",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2016 Water Year") + ylim(-42.0, 5.0)


# create plot of Winter Air and Soil temp at LDF2 site over 2017 Water Year
ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2017",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2017",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2017",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2017 Water Year") + ylim(-42.0, 5.0)


# create plot of Winter Air and Soil temp at LDF2 site over 2018 Water Year
ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2018",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2018",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2018",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2018 Water Year")+
  xlim(0, 212) + ylim(-42.0, 5.0)


# create plot of Winter Air and Soil temp at LDF2 site over 2019 Water Year
ggplot(data = LDF2WinterAvg[LDF2WinterAvg$wyear == "2019",], aes(x=wdoy)) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2019",]$t_air, color = "Air Temp")) +
  geom_line(aes(y = LDF2WinterAvg[LDF2WinterAvg$wyear == "2019",]$t_soil, color = "Soil Temp"), linetype = "solid") +
  labs(x = "Water Year by DOY (October 1st to April 30th)",
       y = "Temperature (C°)",
       color = "Legend") +
  scale_color_manual(values = TempColors) +
  ggtitle("Average Daily Temperature at LDF2 Site During 2019 Water Year") + ylim(-42.0, 5.0)


# Create and sort subset of winterDailyPARAvg for LDF2 site
LDF2WinterPARAvg <- winterDailyPARAvg[winterDailyPARAvg$site == "LDF2",]
LDF2WinterPARAvg <- LDF2WinterPARAvg %>% arrange(dateF)
names(LDF2WinterPARAvg)[4] <- 'PARDailyAvg'


LDF2WinterPARAvg$wyear <- as.numeric(LDF2WinterPARAvg$wyear)
LDF2WinterPARAvg$wdoy <- as.numeric(LDF2WinterPARAvg$wdoy)
LDF2WinterPARAvg$PARDailyAvg <- as.numeric(LDF2WinterPARAvg$PARDailyAvg)

# removes final day of 2016 so it can fit on graph
LDF2WinterPARAvg <- filter(LDF2WinterPARAvg, wdoy != 213)



plot(x = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2015",]$wdoy, y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2015",]$PARDailyAvg,type = "b",col = "chartreuse3",
     xlab = "Water Year by DOY (October 1st to April 30th)", ylab = "PAR (µmol/(m²·s))", main = "Average Daily PAR at LDF2 Site Over the Water Year", lwd = 2, ylim=c(0,475)) 

lines(x = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2016",]$wdoy, y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2016",]$PARDailyAvg, type = "b", col = "darkgreen", lwd = 2)
lines(x = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2017",]$wdoy, y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2017",]$PARDailyAvg, type = "b", col = "indianred4", lwd = 2)
lines(x = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2018",]$wdoy, y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2018",]$PARDailyAvg, type = "b", col = "blue", lwd = 2)
lines(x = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2019",]$wdoy, y = LDF2WinterPARAvg[LDF2WinterPARAvg$wyear == "2019",]$PARDailyAvg, type = "b", col = "indianred1", lwd = 2)


legend("topleft", c("2015", "2016", "2017", "2018", "2019"), 
       lty = c(1,1), lwd = 3, cex=0.8, 
       col = c("darkgreen", "chartreuse3", "indianred1", "indianred4", "blue"))

