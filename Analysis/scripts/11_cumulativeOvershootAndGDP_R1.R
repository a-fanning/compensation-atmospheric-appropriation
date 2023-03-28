## This script visualises historical cumulative GDP and CO2 emissions (with respect to fair shares), 1960-2018

#Read in datafiles from previous scripts (if needed)
#myData20 <- read_csv("./myData/07_myDataCO2convergenceWithGroups_v3.csv")

#overshoot <- read_csv("./myData/08_myDataOvershoot_v3.csv", col_types=list(col_double(), col_character(), col_character(), 
	col_character(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double()))


#--------------------------------------------------------------------------------------
#---------------------------------------------------------------------
#			PREP CUMULATIVE GDP calculations...
#----------------------------------------------------------------------
#Read in Maddison Project GDP data from 1960-2018 and join
gdp <- read_csv("./cleanData/GDPmaddisonRescaled_1960-2018_2010baseYear.csv")

#get world values
worldGDP <- gdp %>%
	group_by(date) %>%
	summarise(gdp = sum(gdp, na.rm=T)) %>%
	ungroup() %>%
	add_column(iso3c="WLD") %>%
	relocate(iso3c)

gdp1 <- rbind(gdp, worldGDP)

overshoot1 <- left_join(overshoot, gdp1, by=c("iso3c", "date"))
rm(overshoot, gdp, gdp1, worldGDP)

#get cumulative GDP by 1960 and 1992 start dates
co2gdp1960 <- overshoot1 %>%
	filter(startDate == 1960, date <= 2018, date >= 1960) %>%
	select(startDate, country, iso3c, date, overshoot350, overshoot15C, overshoot2C, gdp) %>%
	group_by(startDate, country, iso3c) %>%
	mutate(cumGDP = cumsum(ifelse(is.na(gdp), 0, gdp)) + gdp*0) %>%
	ungroup()

co2gdp1992 <- overshoot1 %>%
	filter(startDate == 1992, date <= 2018, date >= 1992) %>%
	select(startDate, country, iso3c, date, overshoot350, overshoot15C, overshoot2C, gdp) %>%
	group_by(startDate, country, iso3c) %>%
	mutate(cumGDP = cumsum(ifelse(is.na(gdp), 0, gdp)) + gdp*0) %>%
	ungroup()

co2gdp <- rbind(co2gdp1992, co2gdp1960)

rm(co2gdp1960, co2gdp1992)

#add population data
popData <- myData20 %>%
	select(country, iso3c, date, population)

co2gdp1 <- left_join(co2gdp, popData, by=c("country", "iso3c", "date"))
rm(co2gdp, popData)

co2gdp2 <- co2gdp1 %>%
	group_by(startDate, country, iso3c) %>%
	mutate(cumPop = cumsum(ifelse(is.na(population), 0, population)) + population*0) %>%
	ungroup()
rm(co2gdp1)


#------------------------------------------------------------------------------------------------------
#			WRITE TO FILE
#------------------------------------------------------------------------------------------------------
#write_csv(co2gdp2, "./myData/11_cumulativeGDPandCO2_v4.csv")
#write_csv(overshoot1, "./myData/11_myDataOvershootGDP_v4.csv")


