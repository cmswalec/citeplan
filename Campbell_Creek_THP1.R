# CAMPBELL CREEK THP ANALYSIS - V1 
# CAITLIN SWALEC
############################################
library(tidyverse)
library(sf)
library(sp)
library(gstat)
library(rgdal)
library(tmap)
library(rgdal)
library(raster)
library(RArcInfo)
library(gganimate)
library(ggplot2)
library(gganimate)
library(gridExtra)

#Note: When you create a binary grid, the files named dblbnd.adf, hdr.adf, sta.adf, vat.adf, and w001001x.adf provide metadata about the raster grid, while the file w001001.adf 

# Read in as a raster layer
#CC_THP_2001_gdal <- readGDAL("\\Campbell_Creek_THP1\\camp1_2001\\w001001.adf")
CC_THP_2001 <- raster("Campbell_Creek_THP1\\camp1_2001\\w001001.adf") 
CC_THP_2008 <- raster("Campbell_Creek_THP1\\camp1_2008\\w001001.adf") 
CC_THP_2010 <- raster("Campbell_Creek_THP1\\camp1_2010\\w001001.adf")
CC_THP_2012 <- raster("Campbell_Creek_THP1\\camp1_2012\\w001001.adf")
CC_THP_2014 <- raster("Campbell_Creek_THP1\\camp1_2014\\w001001.adf")

#Create plot for each raster
plot_CC_THP_2001 <- plot(CC_THP_2001)
plot_CC_THP_2008 <- plot(CC_THP_2008)
plot_CC_THP_2010 <- plot(CC_THP_2010)
plot_CC_THP_2012 <- plot(CC_THP_2012)
plot_CC_THP_2014 <- plot(CC_THP_2014)

#Create frequency tables (matrices) for each raster layer
freq_CC_THP_2001 <- as_data_frame(freq(CC_THP_2001, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value)
freq_CC_THP_2001$year <- rep(2001,nrow(freq_CC_THP_2001))

hist_CC_THP_2001 <- as.vector(rep(freq_CC_THP_2001$flame_length_feet, freq_CC_THP_2001$count))

freq_CC_THP_2008 <- as_data_frame(freq(CC_THP_2008, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value)
freq_CC_THP_2008$year <- rep(2008,nrow(freq_CC_THP_2008))

hist_CC_THP_2008 <- as.vector(rep(freq_CC_THP_2008$flame_length_feet, freq_CC_THP_2008$count))

freq_CC_THP_2010 <- as_data_frame(freq(CC_THP_2010, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value)
freq_CC_THP_2010$year <- rep(2010,nrow(freq_CC_THP_2010))

hist_CC_THP_2010 <- as.vector(rep(freq_CC_THP_2010$flame_length_feet, freq_CC_THP_2010$count))

freq_CC_THP_2012 <- as_data_frame(freq(CC_THP_2012, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value)
freq_CC_THP_2012$year <- rep(2012,nrow(freq_CC_THP_2012))

hist_CC_THP_2012 <- as.vector(rep(freq_CC_THP_2012$flame_length_feet, freq_CC_THP_2012$count))

freq_CC_THP_2014 <- as_data_frame(freq(CC_THP_2014, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value)
freq_CC_THP_2014$year <- rep(2014,nrow(freq_CC_THP_2014))

hist_CC_THP_2014 <- as.vector(rep(freq_CC_THP_2014$flame_length_feet, freq_CC_THP_2014$count))

##note: would love to find a better way to merge more than two data frames in one function (tried do.call(rbind()) and merge(as.list()))
freq_CC_THP_thru2008 <- merge(freq_CC_THP_2001, freq_CC_THP_2008, by = c("flame_length_feet", "year", "count"), all=TRUE)

freq_CC_THP_thru2010 <- merge(freq_CC_THP_thru2008, freq_CC_THP_2010, by = c("flame_length_feet", "year", "count"), all=TRUE)

freq_CC_THP_thru2012 <- merge(freq_CC_THP_thru2010, freq_CC_THP_2012, by = c("flame_length_feet", "year", "count"), all=TRUE)

freq_CC_THP_all <- merge(freq_CC_THP_thru2012, freq_CC_THP_2014, by = c("flame_length_feet", "year", "count"), all=TRUE) %>% replace_na(list(count_2001 = 0, count_2008 = 0, count_2010 = 0, count_2012 = 0, count_2014 = 0)) %>% 
  filter(flame_length_feet != "NA")

hist_all <- ggplot(freq_CC_THP_all, aes(flame_length_feet, count, fill = year)) +
  geom_bar(stat="identity",position='dodge')
print(hist_all)




count_2001 = freq_CC_THP_2001$count
m_2001 <- mean(count_2001)
std <- sqrt(var(count_2001))

bar_2001 <- ggplot(freq_CC_THP_2001, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2001",x = "Flame Length (Feet)") +
  xlim(0,90)
print(hist_2008)

bar_2008 <- ggplot(freq_CC_THP_2008, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2008",x = "Flame Length (Feet)") +
  xlim(0,90)
print(hist_2008)

bar_2010 <- ggplot(freq_CC_THP_2010, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2010",x = "Flame Length (Feet)") +
  xlim(0,90)
print(hist_2010)

bar_2012 <- ggplot(freq_CC_THP_2012, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2012",x = "Flame Length (Feet)") +
  xlim(0,90)
print(hist_2012)

bar_2014 <- ggplot(freq_CC_THP_2014, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2014",x = "Flame Length (Feet)") +
  xlim(0,90)
print(hist_2014)

animation <- ggplot(freq_CC_THP_all, aes(x = flame_length_feet, y = count))+
  geom_bar(stat = "identity") +
  xlim(0,90) +
  theme_minimal() +
  labs(title = "Year: {closest_state}", x = "Flame Length (Feet)") +
  transition_states(year, transition_length = 0)

#animate(animation)

hist_2001 <- ggplot(hist_CC_THP_2001,aes(x = flame_length_feet)) +
  geom_histogram(binwidgth = 1) +
  geom_density(aes(y = 1*..count..))
  #hist(hist_CC_THP_2001)
hist_2008 <- hist(hist_CC_THP_2008)
hist_2010 <- hist(hist_CC_THP_2010)
hist_2012 <- hist(hist_CC_THP_2012)
hist_2014 <- hist(hist_CC_THP_2014)

#grid.arrange(hist_2001, hist_2008, hist_2010, hist_2012, hist_2014, nrow = 2)

