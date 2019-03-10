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
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")
freq_CC_THP_2001$year <- rep(2001,nrow(freq_CC_THP_2001))

freq_CC_THP_2008 <- as_data_frame(freq(CC_THP_2008, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")
freq_CC_THP_2008$year <- rep(2008,nrow(freq_CC_THP_2008))

freq_CC_THP_2010 <- as_data_frame(freq(CC_THP_2010, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")
freq_CC_THP_2010$year <- rep(2010,nrow(freq_CC_THP_2010))

freq_CC_THP_2012 <- as_data_frame(freq(CC_THP_2012, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")
freq_CC_THP_2012$year <- rep(2012,nrow(freq_CC_THP_2012))

freq_CC_THP_2014 <- as_data_frame(freq(CC_THP_2014, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")
freq_CC_THP_2014$year <- rep(2014,nrow(freq_CC_THP_2014))

#Create histogram formatted tables
hist_CC_THP_2001 <- as.data.frame(rep(freq_CC_THP_2001$flame_length_feet, freq_CC_THP_2001$count)) 
colnames(hist_CC_THP_2001) <- "lengths"

hist_CC_THP_2008 <- as.data.frame(rep(freq_CC_THP_2008$flame_length_feet, freq_CC_THP_2008$count))
colnames(hist_CC_THP_2008) <- "lengths"

hist_CC_THP_2010 <- as.data.frame(rep(freq_CC_THP_2010$flame_length_feet, freq_CC_THP_2010$count))
colnames(hist_CC_THP_2010) <- "lengths"

hist_CC_THP_2012 <- as.data.frame(rep(freq_CC_THP_2012$flame_length_feet, freq_CC_THP_2012$count))
colnames(hist_CC_THP_2012) <- "lengths"

hist_CC_THP_2014 <- as.data.frame(rep(freq_CC_THP_2014$flame_length_feet, freq_CC_THP_2014$count))
colnames(hist_CC_THP_2014) <- "lengths"

##note: would love to find a better way to merge more than two data frames in one function (tried do.call(rbind()) and merge(as.list()))
freq_CC_THP_thru2008 <- merge(freq_CC_THP_2001, freq_CC_THP_2008, by = c("flame_length_feet", "year", "count"), all=TRUE)

freq_CC_THP_thru2010 <- merge(freq_CC_THP_thru2008, freq_CC_THP_2010, by = c("flame_length_feet", "year", "count"), all=TRUE)

freq_CC_THP_thru2012 <- merge(freq_CC_THP_thru2010, freq_CC_THP_2012, by = c("flame_length_feet", "year", "count"), all=TRUE)

freq_CC_THP_all <- merge(freq_CC_THP_thru2012, freq_CC_THP_2014, by = c("flame_length_feet", "year", "count"), all=TRUE) %>% replace_na(list(count_2001 = 0, count_2008 = 0, count_2010 = 0, count_2012 = 0, count_2014 = 0)) %>% 
  filter(flame_length_feet != "NA")

freq_all <- ggplot(freq_CC_THP_all, aes(flame_length_feet, count, fill = year)) +
  geom_bar(stat="identity",position='dodge')
print(freq_all)

#Create bar graphs and animation of bar graphs from frequency tables
bar_2001 <- ggplot(freq_CC_THP_2001, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2001",x = "Flame Length (Feet)") +
  xlim(0,90)
print(bar_2001)

bar_2008 <- ggplot(freq_CC_THP_2008, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2008",x = "Flame Length (Feet)") +
  xlim(0,90)
print(bar_2008)

bar_2010 <- ggplot(freq_CC_THP_2010, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2010",x = "Flame Length (Feet)") +
  xlim(0,90)
print(bar_2010)

bar_2012 <- ggplot(freq_CC_THP_2012, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2012",x = "Flame Length (Feet)") +
  xlim(0,90)
print(bar_2012)

bar_2014 <- ggplot(freq_CC_THP_2014, aes(x = flame_length_feet, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2014",x = "Flame Length (Feet)") +
  xlim(0,90)
print(bar_2014)

animation <- ggplot(freq_CC_THP_all, aes(x = flame_length_feet, y = count))+
  geom_bar(stat = "identity") +
  xlim(0,90) +
  theme_minimal() +
  labs(title = "Year: {closest_state}", x = "Flame Length (Feet)") +
  transition_states(year, transition_length = 0)

#animate(animation)

#Create histograms for each year from histogram formatted tables **NOTE: UNCOMPLETE**
hist_2001 <- ggplot(hist_CC_THP_2001, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_CC_THP_2001$lengths, sd = sd(hist_CC_THP_2001$lengths))))
print(hist_2001)

hist_2008 <- ggplot(hist_CC_THP_2008, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_CC_THP_2008$lengths, sd = sd(hist_CC_THP_2008$lengths))))
print(hist_2008)
  
hist_2010 <- ggplot(hist_CC_THP_2010, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_CC_THP_2010$lengths, sd = sd(hist_CC_THP_2010$lengths))))
print(hist_2010)

hist_2012 <- ggplot(hist_CC_THP_2012, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_CC_THP_2012$lengths, sd = sd(hist_CC_THP_2012$lengths))))
print(hist_2012)

hist_2014 <- ggplot(hist_CC_THP_2014, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_CC_THP_2014$lengths, sd = sd(hist_CC_THP_2014$lengths))))
print(hist_2014)

#grid.arrange(hist_2001, hist_2008, hist_2010, hist_2012, hist_2014, nrow = 2)

#Calculate bin percents for high and low flame length where high flame length is >8 feet and low is = or < 8 feet
low_freq_CC_THP_2001 <- filter(freq_CC_THP_2001, flame_length_feet <= 8)
high_freq_CC_THP_2001 <- filter(freq_CC_THP_2001, flame_length_feet > 8)

lo01 <- round(sum(low_freq_CC_THP_2001$count)/sum(freq_CC_THP_2001$count), 2)
hi01 <- round(sum(high_freq_CC_THP_2001$count)/sum(freq_CC_THP_2001$count), 2)

low_freq_CC_THP_2008 <- filter(freq_CC_THP_2008, flame_length_feet <= 8)
high_freq_CC_THP_2008 <- filter(freq_CC_THP_2008, flame_length_feet > 8)

lo08 <- round(sum(low_freq_CC_THP_2008$count)/sum(freq_CC_THP_2001$count), 2)
hi08 <- round(sum(high_freq_CC_THP_2008$count)/sum(freq_CC_THP_2001$count), 2)

low_freq_CC_THP_2010 <- filter(freq_CC_THP_2010, flame_length_feet <= 8)
high_freq_CC_THP_2010 <- filter(freq_CC_THP_2010, flame_length_feet > 8)

lo10 <- round(sum(low_freq_CC_THP_2010$count)/sum(freq_CC_THP_2001$count), 2)
hi10 <- round(sum(high_freq_CC_THP_2010$count)/sum(freq_CC_THP_2001$count), 2)

low_freq_CC_THP_2012 <- filter(freq_CC_THP_2012, flame_length_feet <= 8)
high_freq_CC_THP_2012 <- filter(freq_CC_THP_2012, flame_length_feet > 8)

lo12 <- round(sum(low_freq_CC_THP_2012$count)/sum(freq_CC_THP_2001$count), 2)
hi12 <- round(sum(high_freq_CC_THP_2012$count)/sum(freq_CC_THP_2001$count), 2)

low_freq_CC_THP_2014 <- filter(freq_CC_THP_2014, flame_length_feet <= 8)
high_freq_CC_THP_2014 <- filter(freq_CC_THP_2014, flame_length_feet > 8)

lo14 <- round(sum(low_freq_CC_THP_2014$count)/sum(freq_CC_THP_2001$count), 2)
hi14 <- round(sum(high_freq_CC_THP_2014$count)/sum(freq_CC_THP_2001$count), 2)

#Create table of high and low flame lengths for each year

dif_table <- data.frame("year" = c(2001, 2008, 2010, 2012, 2014), "low_count" = c(lo01, lo08, lo10, lo12, lo14), "high_count" = c(hi01, hi08, hi10, hi12, hi14), "low_count_change" = c("", lo08-lo01, lo10-lo08, lo12-lo10, lo14-lo12), "high_count_change" = c("", hi08-hi01, hi10-hi08, hi12-hi10, hi14-hi12))

#Because each raster has the same number of cells (1131), I can use a pooled-variance t-test to test for a significant difference in mean/median

#independent 2-group 2-tailed (and then 1-tailed) t-tests with equal variances

t.test(freq_CC_THP_2001$count, freq_CC_THP_2008$count, var.equal = TRUE)
#p < 0.05 so there is a significant difference

t.test(freq_CC_THP_2008$count, freq_CC_THP_2010$count, var.equal = TRUE)
#p> 0.05 so there is not a significant difference

t.test(freq_CC_THP_2010$count, freq_CC_THP_2012$count, var.equal = TRUE)
#p<0.05 so there is a significant difference

t.test(freq_CC_THP_2012$count, freq_CC_THP_2014$count, var.equal = TRUE)
#p<0.05 so there is a significant difference

t.test(freq_CC_THP_2001$count, freq_CC_THP_2008$count, var.equal = TRUE, alternative="less")
#p<0.05 so the difference between mean in 2001 and 2008 is significantly less than 0

t.test(freq_CC_THP_2010$count, freq_CC_THP_2012$count, var.equal = TRUE, alternative="greater")
#p<0.05 so the difference between mean in 2010 and 20012 is significantly greater than 0

t.test(freq_CC_THP_2012$count, freq_CC_THP_2014$count, var.equal = TRUE, alternative="less")
#p<0.05 so the difference between means in 2012 and 2014 is significantly less than 0

###Summary:
#There is significant difference in means between 2001-2008, 2010-2012, and 2012-2014, but not 2008-2010
#The mean flame length in 2001 is significantly less than the mean flame length in 2008
#The mean flame length in 2010 is significantly greater than the mean flame length in 2012
#The mean flame length in 2012 is significantly greater than the mean flame length in 2014

