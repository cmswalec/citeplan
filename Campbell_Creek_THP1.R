# CAMPBELL CREEK THP ANALYSIS - V1_cleaned up
# CAITLIN SWALEC
############################################
library(tidyverse)
library(raster)
library(ggplot2)
library(gridExtra)
library(broom)
library(purrr)

#Note: When you create a binary grid, the files named dblbnd.adf, hdr.adf, sta.adf, vat.adf, and w001001x.adf provide metadata about the raster grid, while the file w001001.adf 


##3 questions: 1) relative to baseline is there ever a point where hazard increases? if yes, (2) which years have a significant change and (3) is the change an increase of cells to high or low?
#How many cells over the whole time period --> classify these as "often" and "rarely"
#Make a matrix where you consider if the THP is in an area that is already high hazard
#Use all four buckets (not just the two groupings we did)
#Break it out by the different hazard buckets and the location in the harvest cycle. Then consider the total percent conversion 

# Read in as a raster layer and provide year of each THP
#### NOTE: This is the ONLY section where input is required ####
THP_1 <- raster("v2 adf Campbell Creek THP 1\\01thp\\w001001.adf") 
THP_2 <- raster("v2 adf Campbell Creek THP 1\\08thp\\w001001.adf") 
THP_3 <- raster("v2 adf Campbell Creek THP 1\\10thp\\w001001.adf")
THP_4 <- raster("v2 adf Campbell Creek THP 1\\12thp\\w001001.adf")
THP_5 <- raster("v2 adf Campbell Creek THP 1\\14thp\\w001001.adf")

THP_1_year <- 2001 #pre-harvest
THP_2_year <- 2008 #mid-harvest
THP_3_year <- 2010 #post-harvest - 1 year
THP_4_year <- 2012 #post-harvest - 3 year
THP_5_year <- 2014 #post-harvest - 5 year

########

#Creates plot for each raster
plot_THP_1 <- plot(THP_1)
plot_THP_2 <- plot(THP_2)
plot_THP_3 <- plot(THP_3)
plot_THP_4 <- plot(THP_4)
plot_THP_5 <- plot(THP_5)

#Creates frequency tables (matrices) for each raster layer
freq_THP_1 <- as_data_frame(freq(THP_1, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")

freq_THP_2 <- as_data_frame(freq(THP_2, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")

freq_THP_3 <- as_data_frame(freq(THP_3, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")

freq_THP_4 <- as_data_frame(freq(THP_4, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")

freq_THP_5 <- as_data_frame(freq(THP_5, digits = 0, value = NULL, useNA='ifany', progress='')) %>% 
  rename(flame_length_feet = value) %>% 
  filter(flame_length_feet != "NA")

#Create histogram formatted tables
hist_THP_1 <- as.data.frame(rep(freq_THP_1$flame_length_feet, freq_THP_1$count)) 
colnames(hist_THP_1) <- "lengths"

hist_THP_2 <- as.data.frame(rep(freq_THP_2$flame_length_feet, freq_THP_2$count))
colnames(hist_THP_2) <- "lengths"

hist_THP_3 <- as.data.frame(rep(freq_THP_3$flame_length_feet, freq_THP_3$count))
colnames(hist_THP_3) <- "lengths"

hist_THP_4 <- as.data.frame(rep(freq_THP_4$flame_length_feet, freq_THP_4$count))
colnames(hist_THP_4) <- "lengths"

hist_THP_5 <- as.data.frame(rep(freq_THP_5$flame_length_feet, freq_THP_5$count))
colnames(hist_THP_5) <- "lengths"

#Creates histograms for each year from histogram formatted tables
hist_1 <- ggplot(hist_THP_1, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_THP_1$lengths, sd = sd(hist_THP_1$lengths)))) +
  ggtitle(THP_1_year)
print(hist_1)

hist_2 <- ggplot(hist_THP_2, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_THP_2$lengths, sd = sd(hist_THP_2$lengths)))) +
  ggtitle(THP_2_year)
print(hist_2)
  
hist_3 <- ggplot(hist_THP_3, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_THP_3$lengths, sd = sd(hist_THP_3$lengths)))) +
  ggtitle(THP_3_year)
print(hist_3)

hist_4 <- ggplot(hist_THP_4, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_THP_4$lengths, sd = sd(hist_THP_4$lengths)))) +
  ggtitle(THP_4_year)
print(hist_4)

hist_5 <- ggplot(hist_THP_5, aes(x = lengths)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  stat_function(geom="line", 
                fun = dnorm, 
                colour = "red",
                args = list(mean = mean(hist_THP_5$lengths, sd = sd(hist_THP_5$lengths)))) +
  ggtitle(THP_5_year)
print(hist_5)

grid.arrange(hist_1, hist_2, hist_3, hist_4, hist_5, nrow = 2)

#Calculates bin percents for no (0- <4 ft), low (4- <8 ft), med (8- <11 ft), and high (11+ ft) flamelength

no_freq_THP_1 <- filter(freq_THP_1, flame_length_feet < 4)
low_freq_THP_1 <- filter(freq_THP_1, flame_length_feet >=4 & flame_length_feet <8)
med_freq_THP_1 <- filter(freq_THP_1, flame_length_feet >=8 & flame_length_feet <11)
high_freq_THP_1 <- filter(freq_THP_1, flame_length_feet >= 11)

no1 <- round(sum(no_freq_THP_1$count)/sum(freq_THP_1$count), 2)
lo1 <- round(sum(low_freq_THP_1$count)/sum(freq_THP_1$count), 2)
me1 <- round(sum(med_freq_THP_1$count)/sum(freq_THP_1$count), 2)
hi1 <- round(sum(high_freq_THP_1$count)/sum(freq_THP_1$count), 2)

no_freq_THP_2 <- filter(freq_THP_2, flame_length_feet < 4)
low_freq_THP_2 <- filter(freq_THP_2, flame_length_feet >=4 & flame_length_feet <8)
med_freq_THP_2 <- filter(freq_THP_2, flame_length_feet >=8 & flame_length_feet <11)
high_freq_THP_2 <- filter(freq_THP_2, flame_length_feet >= 11)

no2 <- round(sum(no_freq_THP_2$count)/sum(freq_THP_2$count), 2)
lo2 <- round(sum(low_freq_THP_2$count)/sum(freq_THP_2$count), 2)
me2 <- round(sum(med_freq_THP_2$count)/sum(freq_THP_2$count), 2)
hi2 <- round(sum(high_freq_THP_2$count)/sum(freq_THP_2$count), 2)

no_freq_THP_3 <- filter(freq_THP_3, flame_length_feet < 4)
low_freq_THP_3 <- filter(freq_THP_3, flame_length_feet >=4 & flame_length_feet <8)
med_freq_THP_3 <- filter(freq_THP_3, flame_length_feet >=8 & flame_length_feet <11)
high_freq_THP_3 <- filter(freq_THP_3, flame_length_feet >= 11)

no3 <- round(sum(no_freq_THP_3$count)/sum(freq_THP_3$count), 2)
lo3 <- round(sum(low_freq_THP_3$count)/sum(freq_THP_3$count), 2)
me3 <- round(sum(med_freq_THP_3$count)/sum(freq_THP_3$count), 2)
hi3 <- round(sum(high_freq_THP_3$count)/sum(freq_THP_3$count), 2)

no_freq_THP_4 <- filter(freq_THP_4, flame_length_feet < 4)
low_freq_THP_4 <- filter(freq_THP_4, flame_length_feet >=4 & flame_length_feet <8)
med_freq_THP_4 <- filter(freq_THP_4, flame_length_feet >=8 & flame_length_feet <11)
high_freq_THP_4 <- filter(freq_THP_4, flame_length_feet >= 11)

no4 <- round(sum(no_freq_THP_4$count)/sum(freq_THP_4$count), 2)
lo4 <- round(sum(low_freq_THP_4$count)/sum(freq_THP_4$count), 2)
me4 <- round(sum(med_freq_THP_4$count)/sum(freq_THP_4$count), 2)
hi4 <- round(sum(high_freq_THP_4$count)/sum(freq_THP_4$count), 2)

no_freq_THP_5 <- filter(freq_THP_5, flame_length_feet < 4)
low_freq_THP_5 <- filter(freq_THP_5, flame_length_feet >=4 & flame_length_feet <8)
med_freq_THP_5 <- filter(freq_THP_5, flame_length_feet >=8 & flame_length_feet <11)
high_freq_THP_5 <- filter(freq_THP_5, flame_length_feet >= 11)

no5 <- round(sum(no_freq_THP_5$count)/sum(freq_THP_5$count), 2)
lo5 <- round(sum(low_freq_THP_5$count)/sum(freq_THP_5$count), 2)
me5 <- round(sum(med_freq_THP_5$count)/sum(freq_THP_5$count), 2)
hi5 <- round(sum(high_freq_THP_5$count)/sum(freq_THP_5$count), 2)

#Creates table of high and low flame lengths for each year

dif_table <- data.frame("year" = c(THP_1_year, THP_2_year, THP_3_year, THP_4_year, THP_5_year), 
                        "no_percent" = c(no1, no2, no3, no4, no5), 
                        "low_percent" = c(lo1, lo2, lo3, lo4, lo5), 
                        "med_percent" = c(me1, me2, me3, me4, me5), 
                        "high_percent" = c(hi1, hi2, hi3, hi4, hi5), 
                        "no_to_low_change" = c("", no2-no1, no3-lo1, lo4-lo1, lo5-lo1), 
                        "high_percent_change_from_pre_harvest" = c("", hi2-hi1, hi3-hi1, hi4-hi1, hi5-hi1))

#Because each raster has the same number of cells, uses a pooled-variance t-test to test for a significant difference in mean/median from baseline (pre-harvest)

#First, independent 2-group 2-tailed t-tests with equal variances

t_test_12 <- t.test(freq_THP_1$count, freq_THP_2$count, var.equal = TRUE)

t_test_13 <- t.test(freq_THP_1$count, freq_THP_3$count, var.equal = TRUE)

t_test_14 <- t.test(freq_THP_1$count, freq_THP_4$count, var.equal = TRUE)

t_test_15 <- t.test(freq_THP_1$count, freq_THP_5$count, var.equal = TRUE)

#Now, independent 2-group 1-tailed t-tests with equal variances 

t_test_12_less <- t.test(freq_THP_1$count, freq_THP_2$count, var.equal = TRUE, alternative="less")

t_test_12_greater <- t.test(freq_THP_1$count, freq_THP_2$count, var.equal = TRUE, alternative="greater")

t_test_13_less <- t.test(freq_THP_1$count, freq_THP_3$count, var.equal = TRUE, alternative="less")

t_test_13_greater <- t.test(freq_THP_1$count, freq_THP_3$count, var.equal = TRUE, alternative="greater")

t_test_14_less <- t.test(freq_THP_1$count, freq_THP_4$count, var.equal = TRUE, alternative="less")

t_test_14_greater <- t.test(freq_THP_1$count, freq_THP_4$count, var.equal = TRUE, alternative="greater")

t_test_15_less <- t.test(freq_THP_1$count, freq_THP_5$count, var.equal = TRUE, alternative="less")

t_test_15_greater <- t.test(freq_THP_1$count, freq_THP_5$count, var.equal = TRUE, alternative="greater")

t_test_summary <- map_df(list(t_test_12, t_test_13, t_test_14, t_test_15, t_test_12_less, t_test_12_greater, t_test_13_less, t_test_13_greater, t_test_14_less, t_test_14_greater, t_test_15_less, t_test_15_greater), tidy)

select_t_test_summary <- t_test_summary[c("p.value", "method", "alternative")]
select_t_test_summary$significant <- ifelse(select_t_test_summary$p.value < 0.05, "yes", "no")
select_t_test_summary$test_id <- c("t_test_12", 
                                   "t_test_13", 
                                   "t_test_14", 
                                   "t_test_15", 
                                   "t_test_12_less", 
                                   "t_test_12_greater", 
                                   "t_test_13_less", 
                                   "t_test_13_greater", 
                                   "t_test_14_less", 
                                   "t_test_14_greater", 
                                   "t_test_15_less", 
                                   "t_test_15_greater")

###NOTE: right now the only section that requires manual input is the t test summary output table. Will work on summary table that automates these results so no input necessary. 
