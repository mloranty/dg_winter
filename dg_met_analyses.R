#-------------------------------------------#
# analyses of micrometeorological data
# across a forest density gradient in   
# northeastern Siberia
# MML 08/22/23
#-------------------------------------------#

# load packages and clean work space
library(tidyverse)
library(ggplot2)
library(patchwork)
library(lubridate)
library(scales)
library(MuMIn)

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

se <- function(x)
{
  sd(x, na.rm = T)/length(which(is.na(x)==F))
}
# read met data from density gradient from Arctic Data Center
######
# first is a ~7 year data set from 6 different sites https://doi.org/10.18739/A2H12V877

# create a vector of canopy cover values
cc <- as.data.frame(c(71.24, 54.31, 28.8, 13.66, 28.14, 33.19))
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
# select only sensors at 50cm depth for DAVY and LDF2 sites
tsv <- tsv[which(tsv$sensorZ==50),]

tsvd <- tsv %>%
  group_by(year, doy, site) %>%
  summarise(t_soil = mean(tempS)) 

tsvd$timestamp <- as.POSIXct(paste(tsvd$year,tsvd$doy,sep = "-"),
                            format = "%Y-%j")
    
tsvd$wy <- wy(tsvd$timestamp)

tsv.fdd <- tsvd %>%
  group_by(site, wy) %>%
  summarise(fdd = fdd(t_soil))
# last read snowdepth data from Cherskiy met station(s) 
snw <- read.csv("data/cherskiy_met_1940_2022.csv", sep=",",header = T)

# add timestamp
snw$timestamp = as.POSIXct(snw$DATE, format = "%Y-%m-%d")

# add water year
snw$wy <- wy(snw$timestamp)

# fix erroneous high values (mostly typos - determined by comparing to surrounding days)
snw$SNWD[which(snw$DATE == "2006-12-09")] <- 311
snw$SNWD[which(snw$DATE == "2009-11-26")] <- 220
snw$SNWD[which(snw$DATE == "2019-09-30")] <- 31
snw$SNWD[which(snw$DATE == "2021-09-25")] <- NA

#subset to include only data since 2013 - easier for analysis and plotting
snw <- snw[which(snw$wy > 2013 & snw$wy < 2022),]

#-------------------------------------------------------#
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

# create consecutive sequence of days over the instrument record
# missing dates are omitted from published data add these back in for plotting convenience
d <- as.data.frame(rep(seq(as.Date("2014/1/1"), as.Date("2021/8/31"), "days"),6))
names(d) <- "timestamp"
d$site <- rep(unique(tsd$site), each = nrow(d)/6)

# add missing dates back in
tsd <- full_join(tsd,d)
tad <- full_join(tad,d)

#add water year
tsd$wy <- wy(tsd$timestamp)
tad$wy <- wy(tad$timestamp)

#add daily snow data to soil temp
tsd <- left_join(tsd,snw[,c(9,17)])

# add canopy cover data
tsd <- left_join(tsd,cc)

#calculate fdd/tdd and related temp vars
tsa <- tsd %>%
  group_by(wy,site) %>%
  summarise(ts.fdd = fdd(t_soil),
            ts.tdd = tdd(t_soil),
            ts.mean = mean(t_soil, na.rm = T),
            ts.obs = length(which(is.na(t_soil)) ==F))

taa <- tad %>%
  group_by(wy,site) %>%
  summarise(ta.fdd = fdd(t_air),
            ta.tdd = tdd(t_air),
            ta.mean = mean(t_air,na.rm = T),
            ta.obs = length(which(is.na(t_air)) ==F))

taa <- na.omit(taa)
# summarise annual snow depth by water year
snwy <- snw %>%
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
        
# write the full table to csv file for a supplemental table
write.csv(tann, file = "tables/table_s1_site_years.csv",
          row.names = F)

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
               tann$wy == 2018 & tann$site == "MDF1" |
               tann$wy == 2020 & tann$site == "HDF1" |
               tann$wy == 2021 & tann$site == "HDF1" )

tann <- tann[-r,]

# add canopy cover
tann <- left_join(tann,cc)
# group data by site
site <- tann %>%
  group_by(site) %>%
  summarise(ts.fdd = mean(ts.fdd, na.rm = T),
            ts.fddse = sd(ts.fdd,na.rm = T), # this is not working, unsure why
            ts.tdd = mean(ts.tdd, na.rm = T),
            ts.tddse = sd(ts.tdd, na.rm = T),
            ts.mean = mean(ts.mean, na.rm = T),
            ta.mean = mean(ta.mean, na.rm = T),
            ta.fdd = mean(ta.fdd, na.rm = T),
            ta.tdd = mean(ta.tdd, na.rm = T),
            t5 = mean(t5, na.rm = T),
            max = mean(max, na.rm = T),
            nf = mean(nf, na.rm = T))

site <- left_join(site,cc)


#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
#
# DATA ANALYSES AND PLOTS FOR ERL MANUSCRIPT
#
#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#

#-------------------------------------------------#
## FIGURE 2 - FOR ERL MANUSCRIPT ##
## timeseries of OM soil temp and snow depth
## (9)note Figure 1 is a site map created in ArcGIS Pro)
#-------------------------------------------------#
# plot soil temp and snow depth, colored by canopy cover
p1 <- ggplot() +
  geom_line(data = tsd, aes(x = timestamp, y = t_soil, group = site, color = cc)) + 
#  scale_fill_viridis_d() +
  scale_color_gradient(low = "tan", high = "darkgreen")  +
  geom_line(data = snw, aes(x = timestamp, y = SNWD/75),color = "blue", linewidth = 1) +
  scale_y_continuous(sec.axis = sec_axis(~.*75, name = "Snow Depth (mm)", breaks = seq(0,1000,200))) +
  labs(y = expression(paste("Soil Temperature (", degree,"C)",sep=""))) +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  theme(axis.title.x = element_blank(), 
        axis.title.y.right = element_text(hjust=0.15, color = "blue"),
        axis.text.y.right=element_text(color = "blue"),
        axis.ticks.y.right=element_line(color = "blue"),
        legend.position = c(0.075,0.2))  
  
ggsave("figures/dg_tsoil_snow_timeseries.png", plot = p1,
       width = 10, height = 6, units = "in")  
  
#--------------------------------------------------#
## FIGURE 3 - FDD SCATTERPLOTS AND REGRESSION TABLE
#-------------------------------------------------#
####################################################
# Soil FDD vs. MAST

# regression summary to look at relationship between vars
p2r <- lm(tann$ts.mean~abs(tann$ts.fdd), na.action = na.omit)
p2rs <- summary(p2r)
int <- p2rs$coefficients[1,1]
slp <- p2rs$coefficients[2,1]

# create scatter plot
p2 <- ggplot() +
  geom_point(data = tann, aes(x=-ts.fdd, y = ts.mean, color = cc, size = 1.25)) +
  scale_color_gradient(low = "tan", high = "darkgreen") +
  labs(y = expression(paste("MAST (", degree,"C)",sep="")),
       x = expression(FDD[soil])) +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  guides(color = "colorbar", size = "none") +
  geom_abline(intercept = int, slope = slp, color="black",  
              linetype="dashed", size=1.25) +
  theme(legend.position = c(0.15,0.25)) 

ggsave("figures/fdd_mast.png", plot = p2,
       width = 6, height = 6, units = "in")  
#--------------------------------------------------#
## FIGURE 3 - FDD SCATTERPLOTS AND REGRESSION TABLE
#-------------------------------------------------#
####################################################

# Soil FDD vs. previous season Soil TDD

# regression summary to look at relationship between vars
p3r <- lm(-tann$ts.fdd~tann$ts.tdd1, na.action = na.omit)
p3rs <- summary(p3r)

# create scatter plot
p3 <- ggplot() +
  geom_point(data = tann, aes(x=ts.tdd1, y = -ts.fdd, color = cc, size = 1.25)) +
  scale_color_gradient(low = "tan", high = "darkgreen") +
  labs(y = expression(FDD[soil]),
       x = expression(Prev~TDD[soil])) +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  guides(color = "colorbar", size = "none") +
  ggtitle("a") +
  theme(legend.position = c(0.9,0.8)) 


# Soil FDD vs. previous season Air TDD
p4r <- lm(-tann$ts.fdd~tann$ta.tdd1, na.action = na.omit)
p4rs <- summary(p4r)
int <- p4rs$coefficients[1,1]
slp <- p4rs$coefficients[2,1]

# create scatter plot
p4 <- ggplot() +
  geom_point(data = tann, aes(x=ta.tdd1, y = -ts.fdd, color = cc, size = 1.25)) +
  scale_color_gradient(low = "tan", high = "darkgreen") +
  labs(y = expression(FDD[soil]),
       x = expression(Prev~TDD[air])) +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  guides(color = "colorbar", size = "none") +
  ggtitle("b") +
  geom_abline(intercept = int, slope = slp, color="black",  
                  linetype="dashed", size=1.25)
  theme(legend.position = c(0.9,0.8)) 
  
  
# Soil FDD vs. Air FDD

# get rid of sites years with missing Air FDD due to sensor outages

p5r <- lm(-tann$ts.fdd~abs(tann$ta.fdd), na.action = na.omit)
p5rs <- summary(p5r)
int <- p5rs$coefficients[1,1]
slp <- p5rs$coefficients[2,1]

# create scatter plot  
p5 <- ggplot() +
  geom_point(data = tann, aes(x=abs(ta.fdd), y = abs(ts.fdd), color = cc, size = 1.25)) +
  scale_color_gradient(low = "tan", high = "darkgreen") +
 #   xlim(0,-max(tann$ta.fdd)) +
  labs(y = expression(FDD[soil]),
       x = expression(FDD[air])) +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  guides(color = "colorbar", size = "none") +
  ggtitle("c") +
  geom_abline(intercept = int, slope = slp, color="black",  
               linetype="dashed", size=1.25)
  theme(legend.position = c(0.9,0.8)) 
  
  
# Soil FDD vs. Snow Depth

p6r <- lm(-tann$ts.fdd~tann$t5, na.action = na.omit)
p6rs <- summary(p6r)
int <- p6rs$coefficients[1,1]
slp <- p6rs$coefficients[2,1] 
  
p6 <- ggplot() +
  geom_point(data = tann, aes(x=t5, y = -ts.fdd, color = cc,size=1.25)) +
  scale_color_gradient(low = "tan", high = "darkgreen") +
    #   xlim(0,-max(tann$ta.fdd)) +
  labs(y = expression(FDD[soil]),
       x = "Snow Depth (mm)") +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  guides(color = "colorbar", size = "none") +
  ggtitle("d") +
  geom_abline(intercept = int, slope = slp, color="black",  
              linetype="dashed", size=1.25)
  
  
  # Soil FDD vs. Canopy Cover
p7r <- lm(-tann$ts.fdd~tann$cc, na.action = na.omit)
p7rs <-  summary(p7r)
int <- p7rs$coefficients[1,1]
slp <- p7rs$coefficients[2,1] 
  
p7 <- ggplot() +
  geom_point(data = tann, aes(x=cc, y = -ts.fdd, color = cc,size=1.25)) +
  scale_color_gradient(low = "tan", high = "darkgreen") +
    #   xlim(0,-max(tann$ta.fdd)) +
  labs(y = expression(FDD[soil]),
       x = "Canopy Cover (%)") +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  guides(color = "colorbar", size = "none") +
  ggtitle("e") +
  geom_abline(intercept = int, slope = slp, color="black",  
              linetype="dashed", size=1.25)
  
# CREATE A MULTI-PANEL FIGURE  
# add all these to make a plot and remove legends
f3 <- p3+theme(legend.position = "none")+
      p4+theme(legend.position = "none")+
      p5+theme(legend.position = "none")+
      p6+theme(legend.position = "none")+
      p7+theme(legend.position = c(1.5,0.5))
  
#np + theme(base_size = 14)

ggsave("figures/fdd_scatterplots.png", plot = f3,
         width = 15, height = 9, units = "in")  

# CREATE A TABLE SUMMARIZING THE REGRESSIONS#
  
rtab <- as.data.frame(cbind(c("TDDsoil", "TDDair","FDDair","Snow Depth", "CC"),
                            c(p3rs$coefficients[1,1],p4rs$coefficients[1,1],p5rs$coefficients[1,1],p6rs$coefficients[1,1],p7rs$coefficients[1,1]),
                            c(p3rs$coefficients[2,1],p4rs$coefficients[2,1],p5rs$coefficients[2,1],p6rs$coefficients[2,1],p7rs$coefficients[2,1]),
                            c(p3rs$adj.r.squared,p4rs$adj.r.squared,p5rs$adj.r.squared,p6rs$adj.r.squared,p7rs$adj.r.squared),
                            c(p3rs$coefficients[2,4],p4rs$coefficients[2,4],p5rs$coefficients[2,4],p6rs$coefficients[2,4],p7rs$coefficients[2,4])
                            ))

# add column names
colnames(rtab) <- c("Variable","intercept","Slope", "Adj R2", "p value")

# write a csv file
write.csv(rtab, file = "tables/table2_fdd_regression_table.csv", row.names = F)

# multiple regression to look at different variable effects on FDD  and n-factor
fm1 <- lm(-ts.fdd~abs(ta.fdd)+ta.tdd1+t5+cc, data = tann)
fm2 <- lm(-ts.fdd~abs(ta.fdd)+t5+cc, data = tann)
fm3 <- lm(-ts.fdd~t5+cc, data = tann)

nm1 <- lm(nf~cc+t5+ta.tdd1, data = tann)
nm2 <- lm(nf~cc+t5, data = tann)

mr <- list(fm1,fm2,fm3,nm1,nm2)
mrs <- lapply(mr, summary)

mrtab <- as.data.frame(cbind(c("fdd~FDDair, TDDair, Snow Depth, Canopy Cover", "fdd~FDDair,Snow Depth, Canopy Cover", "fdd~Snow Depth, Canopy Cover", "nf~TDDair, Snow Depth, Canopy Cover","nf~Snow Depth, Canopy Cover" ),
                              sapply(mrs,"[[",7)[2,],
                              sapply(mrs,"[[",9),
                              sapply(mr, AICc)))

colnames(mrtab) <- c("Model","DF","Adj R2", "AICc")

write.csv(mrtab, file = "tables/table3_multiple_regression_table.csv")
#------  ------#
# END Figure 3
################
summary(lm(tann$ta.tdd1~tann$cc, na.action = na.omit))

#--------------------------------------------------#
## FIGURE 4 - N-Factor SCATTERPLOTS AND REGRESSION TABLE
####################################################
  
#-------------------------------------------------#    

# calculate correlation coefficient between nf and soil FDD
cor(tann$nf, -tann$ts.fdd,  method = "pearson", use = "complete.obs")


### have a look at n-factors
p8 <- ggplot() +
  geom_point(data = tann, aes(x=cc, y = nf, color = cc,size=1.25)) +
  scale_color_gradient(low = "tan", high = "darkgreen") +
  #   xlim(0,-max(tann$ta.fdd)) +
  labs(y = " Freezing n-factor",
       x = "Canopy Cover (%)") +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  ggtitle("a") +
  guides(color = "colorbar", size = "none") 

p8b <- ggplot() +
  geom_point(data = tann, aes(x=ts.tdd1, y = nf, color = cc,size=1.25)) +
  scale_color_gradient(low = "tan", high = "darkgreen") +
  #   xlim(0,-max(tann$ta.fdd)) +
  labs(y = " Freezing n-factor",
       x = expression(Prev~TDD[air])) +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  ggtitle("a") +
  guides(color = "colorbar", size = "none") 

p9 <- ggplot() +
  geom_point(data = tann, aes(x=t5, y = nf, color = cc,size=1.25)) +
  scale_color_gradient(low = "tan", high = "darkgreen") +
  #   xlim(0,-max(tann$ta.fdd)) +
  labs(y = " Freezing n-factor",
       x = "Snow Depth (mm)") +
  labs(color = "Canopy\nCover")  +
  theme_bw(base_size = 18) +
  ggtitle("b") +
  guides(color = "colorbar", size = "none")

np <- p8+theme(legend.position = "none")+ p9
#np + theme(base_size = 14)

#np + theme(base_size = 14)
ggsave("figures/nf_scatterplots.png", plot = np,
       width = 13, height = 6, units = "in")  

ggsave("figures/nf_scatterplot.png", plot = p9,
       width = 7, height = 6, units = "in") 

nm2 <- lm(nf~cc+t5, data = tann)
nm1 <- lm(nf~cc+t5+ta.tdd1, data = tann)

summary(lm(nf~cc+t5+ta.tdd1-1, data = tann))
summary(lm(nf~cc+t5+ta.tdd1, data = tann))

summary(lm(nf~cc+t5-1, data = tann))
summary(lm(nf~cc+t5, data = tann))
# junk code I can't part with yet  
summary(m2)



### look at deeper temps and soil heat flux from energy balance sites
p10 <- ggplot() +
  geom_line(data = tsvd, aes(x = timestamp, y = t_soil, group = site, color = site)) + 
  #  scale_fill_viridis_d() +
  scale_color_gradient(low = "tan", high = "darkgreen") 
#################################################################

#exploratory analyses of mixed effect model
tann.m <- tann
tann.m$sf <- as.factor(tann$site)
tann.m$yf <- as.factor(tann$wy)
m0 <- lmer(nf~cc+t5+(1|yf)+(1|site), data = tann.m)
m1 <- lmer(nf~cc+(cc|site), data = tann.m)  
m2 <- lmer(nf~t5+(t5|yf), data = tann.m) 
m3 <- lmer(nf~t5+cc+(1|site), data = tann.m) 

# timeseries of snow depth data
ggplot(snw2, aes(x = timestamp, y = SNWD)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_datetime(labels = label_date("%Y")) 
  
# plots of cumulative freezing degree days each winter? 
# plots of cumulative heat flux each winter? 


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


yr <- tann %>%
  group_by(wy) %>%
  summarise(nfr = max(nf, na.rm = T)-min(nf,na.rm=T),
            t5 = mean(t5, na.rm = T),
            max = mean(max, na.rm = T))