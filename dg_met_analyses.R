#-------------------------------------------#
# analyses of micrometeorological data
# across a forest density gradient in   
# northeastern Siberia
# MML 08/22/23
#-------------------------------------------#

library(tidyverse)
library(ggplot2)
library(patchwork)
library(lubridate)
library(scales)

rm(list = ls())


# read met data from density gradient from Arctic Data Center
######
# first is a ~7 year data set from 6 different sites https://doi.org/10.18739/A2H12V877

# 

# air temperature
ta <- read.csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:2008bcf8-f7aa-423b-85df-abec4c15584f", sep = ",", header = T)
ta$timestamp <- as.POSIXct(ta$timestamp, format = "%Y-%m-%d %H:%M:%S")

# soil temperature
ts <- read.csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:685c49d2-1758-4ad8-bced-c24b02533ccf", sep = ",", header = T)
as.POSIXct(ts$timestamp, format = "%Y-%m-%d %H:%M:%S")


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




tsd <- ts %>%
  group_by(year, doy, site, sensorZ) %>%
  summarise(t_soil = mean(t_soil))


# make a few plots to summarize the existing data (and associated gaps)
#####


ggplot(ta, aes(x = timestamp, y = t_air, group = site, color = site)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_datetime(labels = label_date("%Y-%m-%d"))




