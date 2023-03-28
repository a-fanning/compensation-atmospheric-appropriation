#Set working directory in your local environment, load libraries, and data
setwd("C:/myFilePath")

#load libraries
library(jsonlite)
library(tidyverse)
library(zoo)
library(forecast)
library(ggpubr)
library(ggrepel)
#library(ggalluvial)

#Setup
options(tibble.width = Inf)

#-----------------------------------
#ggplot themes
#----------------------------------
# BASIC THEME
theme_chart <- 
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(plot.title = element_text(size=16, family="sans", face="bold", hjust=0, color="#666666")) +
  theme(panel.grid = element_line(colour = "#efefef")) +
  theme(axis.title = element_text(size=10, family="sans", color="#666666"))

# SCATTERPLOT THEME
theme_chart_SCATTER <- theme_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# HISTOGRAM THEME
theme_chart_HIST <- theme_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# SMALL MULTIPLE THEME
theme_chart_SMALLM <- theme_chart +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.background = element_rect(fill="white", colour="grey50")) +
  theme(strip.text = element_text(size=7, family="sans", color="#666666")) +
  theme(axis.text = element_text(size=7, family="sans", color="#666666")) +
  theme(axis.title = element_text(size=10, family="sans", color="#666666"))


#-----------------------------------
#Load population data files
#-----------------------------------
#World Bank country names
countryList <- read_csv("./cleanData/wbCountryNames.csv")

#Gapminder population data
popDataGap <- read_csv("./cleanData/pop_1850_2015.csv") %>%
	gather(date, population_Gap, -iso3c) %>%
	arrange(iso3c) %>% type_convert()

#Join World Bank country name and drop non-World Bank countries (N=217)
popDataGap1 <- inner_join(popDataGap, countryList, by="iso3c") %>%
	relocate(country) %>%
	filter(date <= 1969)

#UN population data
popDataUN <- read_csv("./cleanData/totalPopulation_WPP2019.csv") %>%
	select(-country)

#Join World Bank country name and drop non-World Bank countries (N=217)
popDataUN1 <- inner_join(popDataUN, countryList, by=c("iso3c")) %>%
	relocate(country) %>%
	filter(date <= 2050)

#Merge popData to create 1850-2050 dataframe
myData <- full_join(popDataGap1, popDataUN1, by=c("country", "iso3c", "date"))

#Create combined 1850-2050 population series
myData <- myData %>%
	rowwise() %>%
	mutate(population = ifelse(date <= 1970, population_Gap, population_UN)) %>%
	ungroup() %>%
	select(country, iso3c, date, population) %>%
	group_by(country, iso3c) %>%
	mutate(population = na.approx(population, na.rm="FALSE")) %>%
	ungroup()

#write_csv(myData, "./myData/1_population_1850-2050_GapminderAndWPP.csv")
rm(popDataGap, popDataUN, popDataGap1, popDataUN1)
	
#--------------------------------------------------------------------------------------------
#get primap territorial CO2 data (1850-2019) and munge
#--------------------------------------------------------------------------------------------
primapHist <- read_csv("./cleanData/PRIMAP-HISTTP-CO2-18502019.csv")

primapHist1 <- primapHist %>%
	select(-scenario, -category, -entity)

#Join World Bank country name and drop non-World Bank countries
myData <- left_join(myData, primapHist1, by=c("iso3c", "date"))

rm(primapHist, primapHist1)

#--------------------------------------------------------------------------------------------
#get old eora CO2 data (1970-2015) and munge
#--------------------------------------------------------------------------------------------
eoraPrimapTerr <- read_csv("./cleanData/eora2019-PRIMAPpba-19702015.csv") %>%
	select(-country) %>%
	gather(date, eoraOldTerr, -iso3c) %>%
	arrange(iso3c) %>% type_convert()

eoraPrimapFoot <- read_csv("./cleanData/eora2019-PRIMAPcba-19702015.csv") %>%
	select(-country) %>%
	gather(date, eoraOldFoot, -iso3c) %>%
	arrange(iso3c) %>% type_convert()

myData <- left_join(myData, eoraPrimapTerr, by=c("iso3c", "date"))
myData <- left_join(myData, eoraPrimapFoot, by=c("iso3c", "date"))

#--------------------------------------------------------------------------------------------
#get new eora CO2 data (1990-2019) and munge
#--------------------------------------------------------------------------------------------
eoraPrimapTerr <- read_csv("./cleanData/eora2021-PRIMAPpba-19902021.csv") %>%
	select(-country) %>%
	gather(date, eoraNewTerr, -iso3c) %>%
	arrange(iso3c) %>% type_convert() %>%
	filter(date <= 2019)

eoraPrimapFoot <- read_csv("./cleanData/eora2021-PRIMAPcba-19902021.csv") %>%
	select(-country) %>%
	gather(date, eoraNewFoot, -iso3c) %>%
	arrange(iso3c) %>% type_convert() %>%
	filter(date <= 2019)

myData <- left_join(myData, eoraPrimapTerr, by=c("iso3c", "date"))
myData <- left_join(myData, eoraPrimapFoot, by=c("iso3c", "date"))

#--------------------------------------------------------------------------------------------
#get merged and scaled eora CO2 data (1970-2019) and munge
#--------------------------------------------------------------------------------------------

eoraPrimapFoot <- read_csv("./cleanData/eoraScaled-PRIMAPcba-19702021.csv") %>%
	select(-country) %>%
	gather(date, eoraScaledFoot, -iso3c) %>%
	arrange(iso3c) %>% type_convert() %>%
	filter(date <= 2019)

myData <- left_join(myData, eoraPrimapFoot, by = c("iso3c", "date"))


rm(eoraPrimapTerr, eoraPrimapFoot)
##-----------------------------------
#	WRITE TO FILE
##-----------------------------------
#write_csv(myData, "./myData/1_colonizingAtmosphereData_21dec2021")



