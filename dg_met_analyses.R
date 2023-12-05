#-------------------------------------------#
# analyses of micrometeorological data
# across a forest density gradient in   
# northeastern Siberia
# MML 08/22/23
#-------------------------------------------#

# lorad packages and clean work space
library(tidyverse)
library(ggplot2)
library(patchwork)
library(lubridate)
library(scales)

rm(list = ls())

# define functions
# determine water year from timestamp
wy <- function(x)
{
  #ifelse(is.POSIXct(x),,"Input is not Date/Time class")
  ifelse(month(x)<10,year(x),year(x)+1)
}

# calculate water day from timestamp
wd <- function(x)
{
  ifelse(leap_year(x),
         ifelse(month(x)<10,yday(x)+92,yday(x)+91),
         ifelse(month(x)<10,yday(x)+91,yday(x)-92))
}

# freezing degree day
fdd <- function(x)
{
  z <- which(x <0)
  sum(x[z], na.rm = T)
}

# thawing degree day
tdd <- function(x)
{
  z <- which(x >0)
  sum(x[z], na.rm = T)
}
# read met data from density gradient from Arctic Data Center
######
# first is a ~7 year data set from 6 different sites https://doi.org/10.18739/A2H12V877

# create a vector of canopy cover values
cc <- as.data.frame(c(71.24, 54.31, 15.44, 13.66, 28.14, 33.19))
names(cc) <- ("cc")
cc$site <- c("DAVY", "HDF1", "LBR","LDF2","MDF1","MDF2")
# air temperature
ta <- read.csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:2008bcf8-f7aa-423b-85df-abec4c15584f", sep = ",", header = T)
ta$timestamp <- as.POSIXct(ta$timestamp, format = "%Y-%m-%d %H:%M:%S")

# soil temperature
ts <- read.csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:685c49d2-1758-4ad8-bced-c24b02533ccf", sep = ",", header = T)
ts$timestamp <- as.POSIXct(ts$timestamp, format = "%Y-%m-%d %H:%M:%S")

# filter to include only sensors at Organic-Mineral interface
tso <- filter(ts, site == "DAVY" & sensorZ == -10 | 
                    site == "HDF1" & sensorZ ==-9 |
                    site =="LBR" & sensorZ == -18 |
                    site =="LDF2" & sensorZ == -8 |
                    site == "MDF1" & sensorZ == -6|
                    site == "MDF2" & sensorZ == -9)

# filter to include only sensors in mineral soil
tsm <- filter(ts, site == "DAVY" & sensorZ == -20 | 
                site == "HDF1" & sensorZ ==-19 |
                site =="LBR" & sensorZ == -28 |
                site =="LDF2" & sensorZ == -18 |
                site == "MDF1" & sensorZ == -16|
                site == "MDF2" & sensorZ == -19)
# soil moisture
sm <- read.csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:80cf437a-661b-460e-bc16-187b93c495db", sep = ",", header = T)

# par
par <- read.csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:0c9373e9-4b09-435e-987b-b3d2e6d2b33a", sep = ",", header = T)


# next get energy balance data from the Viper project
# https://doi.org/10.18739/A2M32NB2V

# ground heat flux
gh <- read.csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:28727af6-8444-4fc5-b735-812139448cd9", sep = ",", header = T)

# net radiation
nr <- read.csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:229ad93b-ddfe-4e3f-a32d-d8e8e17eca4d", sep = ",", header = T)

# viper soil temp
tsv <- read.csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:753917ef-7505-42c0-baa4-a1863cc1077d", sep = ",", header = T)

# last read snowdepth data from Cherskiy met station(s) 
snw <- read.csv("L:/data_repo/field_data/siberia_climate_data/cherskiy_met_1940_2021.csv", sep=",",header = T)

# add timestamp
snw$timestamp = as.POSIXct(snw$DATE, format = "%Y-%m-%d")

# add water year
snw$wy <- wy(snw$timestamp)

# fix erroneous high values (mostly typos - determined by comparing to surrounding days)
snw$SNWD[which(snw$DATE == "2006-12-09")] <- 311
snw$SNWD[which(snw$DATE == "2009-11-26")] <- 220
snw$SNWD[which(snw$DATE == "2019-09-30")] <- 31
snw$SNWD[which(snw$DATE == "2021-09-25")] <- NA

#subset to include only data since 2000
snw2 <- snw[which(snw$wy > 2013 & snw$wy < 2022),]

#-------------------------------------------------------#
# create consecutive sequence of days over the instrument record
# missing dates are omitted from published data add these back in for plotting convenience
d <- as.data.frame(rep(seq(as.Date("2014/1/1"), as.Date("2021/8/31"), "days"),6))
names(d) <- "timestamp"
d$site <- rep(unique(tsd$site), each = nrow(d)/6)

# aggregate soil temperature to daily values
tsd <- tso %>%
  group_by(year, doy, site, sensorZ) %>%
  summarise(t_soil = mean(t_soil)) %>%
  select(-sensorZ)

# aggregate air temperature to daily values
tad <- ta %>%
  group_by(year, doy, site) %>%
  summarise(t_air = mean(t_air))

# add timestamp
tsd$timestamp = as.POSIXct(paste(tsd$year,tsd$doy,sep="-"), format = "%Y-%j")
tad$timestamp = as.POSIXct(paste(tad$year,tad$doy,sep="-"), format = "%Y-%j")

# add missing dates back in
tsd <- full_join(tsd,d)
tad <- full_join(tad,d)

#add water year
tsd$wy <- wy(tsd$timestamp)
tad$wy <- wy(tad$timestamp)

#add daily snow data to soil temp
tsd <- left_join(tsd,snw2[,c(9,17)])

# add canopy cover data
tsd <- left_join(tsd,cc)

#calculate fdd/tdd and related temp vars
tsa <- tsd %>%
  group_by(wy,site) %>%
  summarise(ts.fdd = fdd(t_soil),
            ts.tdd = tdd(t_soil),
            ts.mean = mean(t_soil, na.rm = T),
            ts.obs = length(which(is.na(t_soil)) ==T))

taa <- tad %>%
  group_by(wy,site) %>%
  summarise(ta.fdd = fdd(t_air),
            ta.tdd = tdd(t_air),
            ta.mean = mean(t_air),
            ta.obs = length(which(is.na(t_soil)) ==T))


# summarise annual snow depth by water year
snwy <- snw2 %>%
  group_by(wy) %>%
  summarise(t5 = quantile(SNWD,na.rm=T, prob = 0.95), 
            max = max(SNWD, na.rm = T), 
            mean = mean(SNWD, na.rm = T),
            med = median(SNWD, na.rm = T))

# join the  temp summaries
tann <- full_join(tsa,taa)

#calculate freezing n-factor
tann$nf <- tann$ts.fdd/tann$ta.fdd

# add TDD from previous growing season as a var
t1 <- select(tann, c(wy,site, ta.tdd, ts.tdd))
t1$wy <- t1$wy+1

t1 <- rename(t1, ta.tdd1 = ta.tdd, ts.tdd1 = ts.tdd)


tann <- left_join(tann,t1)
#join snow depth data
tann <- left_join(tann,snwy)
        

# tann.om <- filter(tann, site == "DAVY" & sensorZ == -10 | 
#                         site == "HDF1" & sensorZ ==-9 |
#                         site =="LBR" & sensorZ == -18 |
#                         site =="LDF2" & sensorZ == -8 |
#                         site == "MDF1" & sensorZ == -6|
#                         site == "MDF2" & sensorZ == -9)

# get rid of years with data gaps - doing this manually b/c some gaps still allow analyses of FDD/TDD respectively
 r <-  which(tann$wy ==2014 |
              tann$wy ==2015 & tann$site == "LBR" |
               tann$wy == 2018 & tann$site == "DAVY" |
               tann$wy == 2018 & tann$site == "LDF2" |
               tann$wy == 2018 & tann$site == "MDF1" )

tann <- tann[-r,]


# group data by site
site <- tann %>%
  group_by(site) %>%
  summarise(ts.fdd = mean(ts.fdd, na.rm = T),
            ts.tdd = mean(ts.tdd, na.rm = T),
            ts.mean = mean(ts.mean, na.rm = T),
            ta.mean = mean(ta.mean, na.rm = T),
            ta.fdd = mean(ta.fdd, na.rm = T),
            ta.tdd = mean(ta.tdd, na.rm = T),
            nf = mean(nf, na.rm = T))
# make a few plots to summarize the existing data (and associated gaps)
#####


# time series of all air temperature data
ggplot(tad, aes(x = timestamp, y = t_air, group = site, color = site)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_datetime(labels = label_date("%Y-%m-%d"))

# time series of all soil temperature data
p1 <- ggplot(tsd, aes(x = timestamp, y = t_soil, group = site, color = site)) +
  geom_line(data = tsd, aes(y = t_soil)) +
#geom_col(snw2, aes(y = SNWD)) +
  geom_col(data = snw2, aes(x = timestamp, y = SNWD))
  scale_y_continuous(name = "Soil Temperature") +
  scale_colour_discrete(breaks = c("DAVY", "HDF1", "MDF1", "MDF2", "LBR", "LDF2"),
                        labels = c("H1", "H2", "M1", "M2", "L1", "L2")) +
  theme_bw() +
  theme(axis.title.x = element_blank())


# plot soil temp and snow depth, colored by site
ggplot() +
  geom_line(data = tsd, aes(x = timestamp, y = t_soil, group = site, color = site)) + 
  geom_line(data = snw2, aes(x = timestamp, y = SNWD/75)) +
  scale_y_continuous(sec.axis = sec_axis(~.*75, name = "Snow Depth (mm)", breaks = seq(0,1000,200),)) +
  labs(y = expression(paste("Soil Temperature (", degree,"C)",sep=""))) +
  labs(color = "Site")  +
  theme_bw() +
  scale_colour_discrete(breaks = c("DAVY", "HDF1", "MDF1", "MDF2", "LBR", "LDF2"),
                      labels = c("H1", "H2", "M1", "M2", "L1", "L2")) +
  theme(axis.title.x = element_blank(), 
        axis.title.y.right = element_text(hjust=0.15),
        legend.position = c(0.05,0.2)) 


# plot soil temp and snow depth, colored by canopy cover
p1 <- ggplot() +
  geom_line(data = tsd, aes(x = timestamp, y = t_soil, group = site, color = cc)) + 
#  scale_fill_viridis_d() +
  scale_color_gradient(low = "tan", high = "darkgreen")  +
  geom_line(data = snw2, aes(x = timestamp, y = SNWD/75),color = "blue", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~.*75, name = "Snow Depth (mm)", breaks = seq(0,1000,200))) +
  labs(y = expression(paste("Soil Temperature (", degree,"C)",sep=""))) +
  labs(color = "Canopy\nCover")  +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        axis.title.y.right = element_text(hjust=0.15, color = "blue"),
        axis.text.y.right=element_text(color = "blue"),
        axis.ticks.y.right=element_line(color = "blue"),
        legend.position = c(0.075,0.2))  
  
  
  
  
  
  
  
# timeseries of snow depth data
ggplot(snw2, aes(x = timestamp, y = SNWD)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_datetime(labels = label_date("%Y")) 
  
# plots of cumulative freezing degree days each winter? 
# plots of cumulative heat flux each winter? 


